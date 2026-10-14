{-# LANGUAGE OverloadedStrings #-}

-- | Звіти: розклад, вільний доступ, завантаженість класів і робочих місць.
--   Кожен рядок звіту — окремий тип із екземплярами @QueryResults@ і 'Tabular',
--   тому вивід у вигляді таблиці робить одна універсальна функція 'showReport'.
module Reports
  ( reportScheduleByClassroom
  , reportScheduleByTeacher
  , reportScheduleByGroup
  , reportFullSchedule
  , reportFreeAccess
  , reportFreeNow
  , reportClassroomLoad
  , reportWorkstationLoad
  , ActiveSession (..)
  , activeSessions
  , reportActiveSessions
  ) where

import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Action, Param (..))
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Text.Printf (printf)

import Classes
import Database
import Domain
import Prompt

-- | Скільки годин на тиждень клас може бути відкритим (пн–пт, 08:00–20:00).
--   Використовується для відсотка завантаженості розкладом.
weekCapacityHours :: Double
weekCapacityHours = 5 * 12

showReport :: Tabular a => String -> [a] -> IO ()
showReport title rows = do
  putStrLn ("\n== " ++ title ++ " ==")
  if null rows then putStrLn "  (даних немає)" else putStr (renderTable rows)

--------------------------------------------------------------------------------
-- Розклад занять

data ScheduleRow = ScheduleRow
  { srDay :: WeekDay, srStart :: TimeOfDay, srEnd :: TimeOfDay
  , srSubject :: Text, srTeacher :: Text, srGroup :: Text, srClassroom :: Text }

instance QueryResults ScheduleRow where
  convertResults fs vs = case convertResults fs vs of
    (d, st, en, sb, t, g, c) -> ScheduleRow d st en sb t g c

instance Tabular ScheduleRow where
  tableHeaders _ = ["День", "Початок", "Кінець", "Дисципліна", "Викладач", "Група", "Клас"]
  tableCells r =
    [ enumLabel (srDay r), fmtTime (srStart r), fmtTime (srEnd r)
    , T.unpack (srSubject r), T.unpack (srTeacher r), T.unpack (srGroup r), T.unpack (srClassroom r) ]

scheduleRows :: Connection -> String -> [Action] -> IO [ScheduleRow]
scheduleRows conn cond params = query conn (fromString sql) params
  where
    sql = "SELECT l.week_day, l.start_time, l.end_time, s.name, u.full_name, l.group_name, c.name \
          \FROM schedule l \
          \JOIN classrooms c ON c.id = l.classroom_id \
          \JOIN subjects s ON s.id = l.subject_id \
          \JOIN teachers t ON t.id = l.teacher_id \
          \JOIN users u ON u.id = t.user_id " ++ cond ++
          " ORDER BY l.week_day, l.start_time, c.name"

reportScheduleByClassroom :: Connection -> IO ()
reportScheduleByClassroom conn = do
  c <- askRef conn (Proxy :: Proxy Classroom) "ID класу" Nothing
  rows <- scheduleRows conn "WHERE l.classroom_id = ?" [render c]
  showReport "Розклад класу" rows

reportScheduleByTeacher :: Connection -> IO ()
reportScheduleByTeacher conn = do
  t <- askRef conn (Proxy :: Proxy Teacher) "ID викладача" Nothing
  rows <- scheduleRows conn "WHERE l.teacher_id = ?" [render t]
  showReport "Розклад викладача" rows

reportScheduleByGroup :: Connection -> IO ()
reportScheduleByGroup conn = do
  g <- ask "Назва групи" (Nothing :: Maybe Text)
  rows <- scheduleRows conn "WHERE l.group_name = ?" [render g]
  showReport "Розклад групи" rows

reportFullSchedule :: Connection -> IO ()
reportFullSchedule conn = scheduleRows conn "" [] >>= showReport "Повний розклад занять"

--------------------------------------------------------------------------------
-- Вільний доступ

data FreeAccessRow = FreeAccessRow
  { faClassroom :: Text, faDay :: WeekDay, faStart :: TimeOfDay, faEnd :: TimeOfDay, faNote :: Maybe Text }

instance QueryResults FreeAccessRow where
  convertResults fs vs = case convertResults fs vs of
    (c, d, st, en, n) -> FreeAccessRow c d st en n

instance Tabular FreeAccessRow where
  tableHeaders _ = ["Клас", "День", "Початок", "Кінець", "Примітка"]
  tableCells r =
    [ T.unpack (faClassroom r), enumLabel (faDay r), fmtTime (faStart r), fmtTime (faEnd r)
    , maybe "" T.unpack (faNote r) ]

reportFreeAccess :: Connection -> IO ()
reportFreeAccess conn = do
  rows <- query_ conn
    "SELECT c.name, f.week_day, f.start_time, f.end_time, f.note FROM free_access f \
    \JOIN classrooms c ON c.id = f.classroom_id ORDER BY c.name, f.week_day, f.start_time"
  showReport "Час вільного доступу" (rows :: [FreeAccessRow])

-- | Де вільний доступ відкритий саме зараз і скільки вільних робочих місць.
data FreeNowRow = FreeNowRow
  { fnClassroom :: Text, fnStart :: TimeOfDay, fnEnd :: TimeOfDay, fnFree :: Int, fnTotal :: Int }

instance QueryResults FreeNowRow where
  convertResults fs vs = case convertResults fs vs of
    (c, st, en, fr, tot) -> FreeNowRow c st en fr tot

instance Tabular FreeNowRow where
  tableHeaders _ = ["Клас", "Вільний доступ", "Вільних місць", "Усього місць"]
  tableCells r =
    [ T.unpack (fnClassroom r), fmtTime (fnStart r) ++ "–" ++ fmtTime (fnEnd r)
    , show (fnFree r), show (fnTotal r) ]

reportFreeNow :: Connection -> IO ()
reportFreeNow conn = do
  now <- currentDbTime conn
  let day = weekDayOf now
      tod = timeToTimeOfDay (utctDayTime now)
  rows <- query conn
    "SELECT c.name, f.start_time, f.end_time, \
    \  (SELECT COUNT(*) FROM workstations w WHERE w.classroom_id = c.id AND w.status = 'WORKING' \
    \     AND NOT EXISTS (SELECT 1 FROM usage_log u WHERE u.workstation_id = w.id AND u.ended_at IS NULL)), \
    \  (SELECT COUNT(*) FROM workstations w WHERE w.classroom_id = c.id) \
    \FROM free_access f JOIN classrooms c ON c.id = f.classroom_id \
    \WHERE f.week_day = ? AND f.start_time <= ? AND ? < f.end_time ORDER BY c.name"
    (day, tod, tod)
  showReport ("Вільний доступ зараз (" ++ enumLabel day ++ " " ++ fmtTime tod ++ ")") (rows :: [FreeNowRow])

--------------------------------------------------------------------------------
-- Завантаженість

data ClassLoadRow = ClassLoadRow
  { clrName :: Text, clrWorkstations :: Int, clrLessons :: Int
  , clrLessonHours :: Double, clrFreeHours :: Double
  , clrSessions :: Int, clrUsedHours :: Double }

instance QueryResults ClassLoadRow where
  convertResults fs vs = case convertResults fs vs of
    (n, w, l, lh, fh, s, uh) -> ClassLoadRow n w l lh fh s uh

instance Tabular ClassLoadRow where
  tableHeaders _ =
    [ "Клас", "Роб. місць", "Занять/тиж", "Год. занять/тиж", "Год. вільного доступу/тиж"
    , "Завантаж. розкладом", "Сеансів", "Місце-годин" ]
  tableCells r =
    [ T.unpack (clrName r), show (clrWorkstations r), show (clrLessons r)
    , printf "%.1f" (clrLessonHours r), printf "%.1f" (clrFreeHours r)
    , printf "%.0f%%" (100 * clrLessonHours r / weekCapacityHours)
    , show (clrSessions r), printf "%.1f" (clrUsedHours r) ]

-- | Завантаженість класів: за розкладом (на тиждень) і фактична (за журналом
--   за останні @days@ днів).
reportClassroomLoad :: Connection -> IO ()
reportClassroomLoad conn = do
  days <- ask "За скільки останніх днів рахувати фактичне використання" (Just (30 :: Int))
  rows <- query conn
    "SELECT c.name, \
    \  (SELECT COUNT(*) FROM workstations w WHERE w.classroom_id = c.id), \
    \  (SELECT COUNT(*) FROM schedule l WHERE l.classroom_id = c.id), \
    \  COALESCE((SELECT SUM(TIME_TO_SEC(TIMEDIFF(l.end_time, l.start_time))) FROM schedule l \
    \            WHERE l.classroom_id = c.id), 0) / 3600, \
    \  COALESCE((SELECT SUM(TIME_TO_SEC(TIMEDIFF(f.end_time, f.start_time))) FROM free_access f \
    \            WHERE f.classroom_id = c.id), 0) / 3600, \
    \  (SELECT COUNT(*) FROM usage_log u JOIN workstations w ON w.id = u.workstation_id \
    \    WHERE w.classroom_id = c.id AND u.started_at >= NOW() - INTERVAL ? DAY), \
    \  COALESCE((SELECT SUM(TIMESTAMPDIFF(SECOND, u.started_at, COALESCE(u.ended_at, NOW()))) \
    \            FROM usage_log u JOIN workstations w ON w.id = u.workstation_id \
    \            WHERE w.classroom_id = c.id AND u.started_at >= NOW() - INTERVAL ? DAY), 0) / 3600 \
    \FROM classrooms c ORDER BY c.name"
    (days, days)
  showReport ("Завантаженість дисплейних класів (журнал за " ++ show days ++ " дн.)") (rows :: [ClassLoadRow])

data WsLoadRow = WsLoadRow
  { wlClassroom :: Text, wlInv :: Text, wlStatus :: WsStatus
  , wlSessions :: Int, wlHours :: Double, wlLast :: Maybe UTCTime }

instance QueryResults WsLoadRow where
  convertResults fs vs = case convertResults fs vs of
    (c, i, st, n, h, l) -> WsLoadRow c i st n h l

instance Tabular WsLoadRow where
  tableHeaders _ = ["Клас", "Робоче місце", "Стан", "Сеансів", "Годин", "Останнє використання"]
  tableCells r =
    [ T.unpack (wlClassroom r), T.unpack (wlInv r), enumLabel (wlStatus r)
    , show (wlSessions r), printf "%.1f" (wlHours r), maybe "—" fmtDateTime (wlLast r) ]

-- | Завантаженість окремих робочих місць за останні @days@ днів.
reportWorkstationLoad :: Connection -> IO ()
reportWorkstationLoad conn = do
  days <- ask "За скільки останніх днів рахувати використання" (Just (30 :: Int))
  rows <- query conn
    "SELECT c.name, w.inv_number, w.status, COUNT(u.id), \
    \  COALESCE(SUM(TIMESTAMPDIFF(SECOND, u.started_at, COALESCE(u.ended_at, NOW()))), 0) / 3600, \
    \  MAX(u.started_at) \
    \FROM workstations w JOIN classrooms c ON c.id = w.classroom_id \
    \LEFT JOIN usage_log u ON u.workstation_id = w.id AND u.started_at >= NOW() - INTERVAL ? DAY \
    \GROUP BY w.id, c.name, w.inv_number, w.status ORDER BY c.name, w.inv_number"
    (Only days)
  showReport ("Завантаженість робочих місць за " ++ show days ++ " дн.") (rows :: [WsLoadRow])

--------------------------------------------------------------------------------
-- Активні сеанси

data ActiveSession = ActiveSession
  { acId :: Int, acClassroom :: Text, acInv :: Text, acUser :: Text
  , acKind :: SessionKind, acSince :: UTCTime, acMinutes :: Int }

instance QueryResults ActiveSession where
  convertResults fs vs = case convertResults fs vs of
    (i, c, w, u, k, st, m) -> ActiveSession i c w u k st m

instance Tabular ActiveSession where
  tableHeaders _ = ["ID", "Клас", "Робоче місце", "Користувач", "Тип", "Початок", "Хвилин"]
  tableCells r =
    [ show (acId r), T.unpack (acClassroom r), T.unpack (acInv r), T.unpack (acUser r)
    , enumLabel (acKind r), fmtDateTime (acSince r), show (acMinutes r) ]

activeSessions :: Connection -> IO [ActiveSession]
activeSessions conn = query_ conn
  "SELECT s.id, c.name, w.inv_number, u.full_name, s.kind, s.started_at, \
  \  TIMESTAMPDIFF(MINUTE, s.started_at, NOW()) \
  \FROM usage_log s JOIN workstations w ON w.id = s.workstation_id \
  \JOIN classrooms c ON c.id = w.classroom_id JOIN users u ON u.id = s.user_id \
  \WHERE s.ended_at IS NULL ORDER BY c.name, w.inv_number"

reportActiveSessions :: Connection -> IO ()
reportActiveSessions conn = activeSessions conn >>= showReport "Активні сеанси (зараз за комп'ютерами)"
