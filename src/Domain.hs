{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Предметна область: типи сутностей і їхні екземпляри класів.
--
--   Кожна сутність відповідає таблиці БД і реалізує набір класів:
--   @QueryResults@ (читання рядка), 'Pretty', 'Validatable', 'Checked',
--   'Persistent' та 'Promptable'. Завдяки цьому одне й те саме універсальне
--   меню (див. "Menu") працює з усіма таблицями.
module Domain where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Action, Param (..))
import Database.MySQL.Simple.QueryParams (renderParams)
import Database.MySQL.Simple.QueryResults (QueryResults (..))
import Database.MySQL.Simple.Result (Result (..))
import Text.Printf (printf)

import Classes
import Database
import Prompt

--------------------------------------------------------------------------------
-- Перелічувані типи (ENUM у MySQL)

-- | Роль користувача.
data Role = RoleAdmin | RoleTeacher | RoleStudent | RoleTechnician
  deriving (Eq, Show, Enum, Bounded)

instance Enumerated Role where
  enumCode RoleAdmin      = "ADMIN"
  enumCode RoleTeacher    = "TEACHER"
  enumCode RoleStudent    = "STUDENT"
  enumCode RoleTechnician = "TECHNICIAN"
  enumLabel RoleAdmin      = "адміністратор"
  enumLabel RoleTeacher    = "викладач"
  enumLabel RoleStudent    = "студент"
  enumLabel RoleTechnician = "лаборант"

instance Param Role  where render  = renderEnum
instance Result Role where convert = convertEnum "Role"
instance Input Role where
  parseInput = parseEnum
  showInput  = enumLabel
  inputHint  = enumHint

-- | Стан робочого місця.
data WsStatus = WsWorking | WsRepair | WsDisabled
  deriving (Eq, Show, Enum, Bounded)

instance Enumerated WsStatus where
  enumCode WsWorking  = "WORKING"
  enumCode WsRepair   = "REPAIR"
  enumCode WsDisabled = "DISABLED"
  enumLabel WsWorking  = "працює"
  enumLabel WsRepair   = "на ремонті"
  enumLabel WsDisabled = "вимкнено"

instance Param WsStatus  where render  = renderEnum
instance Result WsStatus where convert = convertEnum "WsStatus"
instance Input WsStatus where
  parseInput = parseEnum
  showInput  = enumLabel
  inputHint  = enumHint

-- | Вид сеансу роботи за комп'ютером.
data SessionKind = KindLesson | KindFreeAccess
  deriving (Eq, Show, Enum, Bounded)

instance Enumerated SessionKind where
  enumCode KindLesson     = "LESSON"
  enumCode KindFreeAccess = "FREE_ACCESS"
  enumLabel KindLesson     = "заняття"
  enumLabel KindFreeAccess = "вільний доступ"

instance Param SessionKind  where render  = renderEnum
instance Result SessionKind where convert = convertEnum "SessionKind"
instance Input SessionKind where
  parseInput = parseEnum
  showInput  = enumLabel
  inputHint  = enumHint

-- | День тижня.
data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Enumerated WeekDay where
  enumCode  d = ["MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"] !! fromEnum d
  enumLabel d = ["Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Нд"] !! fromEnum d

instance Param WeekDay  where render  = renderEnum
instance Result WeekDay where convert = convertEnum "WeekDay"
instance Input WeekDay where
  parseInput = parseEnum
  showInput  = enumLabel
  inputHint  = enumHint

-- | День тижня для «наївного» часу, отриманого з БД.
weekDayOf :: UTCTime -> WeekDay
weekDayOf = toEnum . subtract 1 . fromEnum . dayOfWeek . utctDay

--------------------------------------------------------------------------------
-- Допоміжне

str :: Text -> String
str = T.unpack

blank :: Text -> Bool
blank = T.null . T.strip

-- | Кількість рядків, що повертає запит виду @SELECT COUNT(*) ...@.
countRows :: Connection -> Query -> [Action] -> IO Int
countRows conn q ps = do
  rs <- query conn q ps
  return $ case rs of
    [Only n] -> n
    _        -> 0

-- | «Нескінченно далекий» момент для сеансів, що ще не завершилися.
farFuture :: UTCTime
farFuture = UTCTime (fromGregorian 9999 12 31) 0

--------------------------------------------------------------------------------
-- Classroom — таблиця classrooms

data Classroom = Classroom
  { clId       :: Int
  , clName     :: Text
  , clBuilding :: Text
  , clCapacity :: Int
  } deriving (Show, Eq)

instance QueryResults Classroom where
  convertResults fs vs = case convertResults fs vs of
    (i, n, b, c) -> Classroom i n b c

instance Pretty Classroom where
  pretty c = printf "%s (%s), місткість: %d" (str (clName c)) (str (clBuilding c)) (clCapacity c)

instance Validatable Classroom where
  validate c =
    [ "Назва класу не може бути порожньою" | blank (clName c) ] ++
    [ "Корпус/адреса не може бути порожньою" | blank (clBuilding c) ] ++
    [ "Місткість має бути від 1 до 500" | clCapacity c < 1 || clCapacity c > 500 ]

-- | Місткість не може бути меншою за кількість уже встановлених робочих місць.
instance Checked Classroom where
  checkDb conn c = do
    n <- countRows conn "SELECT COUNT(*) FROM workstations WHERE classroom_id = ?" [render (clId c)]
    return [ "Місткість менша за кількість уже встановлених робочих місць (" ++ show n ++ ")"
           | n > clCapacity c ]

instance Persistent Classroom where
  tableName _   = "classrooms"
  entityTitle _ = "Дисплейні класи"
  dataColumns _ = ["name", "building", "capacity"]
  recordId      = clId
  recordParams c = renderParams (clName c, clBuilding c, clCapacity c)

instance Promptable Classroom where
  promptRecord _ old = do
    n <- ask "Назва класу" (clName <$> old)
    b <- ask "Корпус / адреса" (clBuilding <$> old)
    c <- ask "Місткість (макс. кількість робочих місць)" (clCapacity <$> old)
    return (Classroom (maybe 0 clId old) n b c)

--------------------------------------------------------------------------------
-- Workstation — таблиця workstations

data Workstation = Workstation
  { wsId          :: Int
  , wsClassroomId :: Int
  , wsInvNumber   :: Text
  , wsSpecs       :: Maybe Text
  , wsStatus      :: WsStatus
  } deriving (Show, Eq)

instance QueryResults Workstation where
  convertResults fs vs = case convertResults fs vs of
    (i, c, n, sp, st) -> Workstation i c n sp st

instance Pretty Workstation where
  pretty w = printf "%s [%s], клас #%d%s" (str (wsInvNumber w)) (enumLabel (wsStatus w)) (wsClassroomId w)
               (maybe "" (\sp -> ", " ++ str sp) (wsSpecs w))

instance Validatable Workstation where
  validate w = [ "Інвентарний номер не може бути порожнім" | blank (wsInvNumber w) ]

-- | У класі не може бути більше робочих місць, ніж його місткість.
instance Checked Workstation where
  checkDb conn w = do
    rows <- query conn
      "SELECT c.capacity, (SELECT COUNT(*) FROM workstations x WHERE x.classroom_id = c.id AND x.id <> ?) \
      \FROM classrooms c WHERE c.id = ?"
      (wsId w, wsClassroomId w)
    return [ "У класі вже встановлено максимальну кількість робочих місць (" ++ show cap ++ ")"
           | (cap, n) <- rows :: [(Int, Int)], n + 1 > cap ]

instance Persistent Workstation where
  tableName _   = "workstations"
  entityTitle _ = "Робочі місця"
  dataColumns _ = ["classroom_id", "inv_number", "specs", "status"]
  recordId      = wsId
  recordParams w = renderParams (wsClassroomId w, wsInvNumber w, wsSpecs w, wsStatus w)
  listing conn _ = do
    rows <- query_ conn
      "SELECT w.id, w.inv_number, c.name, w.status FROM workstations w \
      \JOIN classrooms c ON c.id = w.classroom_id ORDER BY c.name, w.inv_number"
    return [ (i, printf "%s — %s [%s]" (str inv) (str cn) (enumLabel st))
           | (i, inv, cn, st) <- rows :: [(Int, Text, Text, WsStatus)] ]

instance Promptable Workstation where
  promptRecord conn old = do
    c  <- askRef conn (Proxy :: Proxy Classroom) "ID класу" (wsClassroomId <$> old)
    n  <- ask "Інвентарний номер" (wsInvNumber <$> old)
    sp <- ask "Характеристики" (wsSpecs <$> old)
    st <- ask "Стан" (Just (maybe WsWorking wsStatus old))
    return (Workstation (maybe 0 wsId old) c n sp st)

--------------------------------------------------------------------------------
-- User — таблиця users

data User = User
  { usId       :: Int
  , usLogin    :: Text
  , usFullName :: Text
  , usRole     :: Role
  , usEmail    :: Maybe Text
  , usActive   :: Bool
  } deriving (Show, Eq)

instance QueryResults User where
  convertResults fs vs = case convertResults fs vs of
    (i, l, n, r, e, a) -> User i l n r e a

instance Pretty User where
  pretty u = printf "%s — %s (%s)%s" (str (usLogin u)) (str (usFullName u)) (enumLabel (usRole u))
               (if usActive u then "" else " [неактивний]" :: String)

instance Validatable User where
  validate u =
    [ "Логін не може бути порожнім" | blank (usLogin u) ] ++
    [ "Логін не може містити пробілів" | T.any (== ' ') (usLogin u) ] ++
    [ "ПІБ не може бути порожнім" | blank (usFullName u) ] ++
    [ "Некоректна адреса електронної пошти" | Just e <- [usEmail u], not ("@" `T.isInfixOf` e) ]

instance Checked User

instance Persistent User where
  tableName _   = "users"
  entityTitle _ = "Користувачі"
  dataColumns _ = ["login", "full_name", "role", "email", "is_active"]
  recordId      = usId
  recordParams u = renderParams (usLogin u, usFullName u, usRole u, usEmail u, usActive u)

instance Promptable User where
  promptRecord _ old = do
    l <- ask "Логін" (usLogin <$> old)
    n <- ask "ПІБ" (usFullName <$> old)
    r <- ask "Роль" (usRole <$> old)
    e <- ask "Електронна пошта" (usEmail <$> old)
    a <- ask "Активний" (Just (maybe True usActive old))
    return (User (maybe 0 usId old) l n r e a)

--------------------------------------------------------------------------------
-- Teacher — таблиця teachers (додаткові дані про користувача-викладача)

data Teacher = Teacher
  { thId         :: Int
  , thUserId     :: Int
  , thDepartment :: Text
  , thJobTitle   :: Text
  } deriving (Show, Eq)

instance QueryResults Teacher where
  convertResults fs vs = case convertResults fs vs of
    (i, u, d, j) -> Teacher i u d j

instance Pretty Teacher where
  pretty t = printf "користувач #%d, %s, %s" (thUserId t) (str (thDepartment t)) (str (thJobTitle t))

instance Validatable Teacher where
  validate t =
    [ "Кафедра не може бути порожньою" | blank (thDepartment t) ] ++
    [ "Посада не може бути порожньою" | blank (thJobTitle t) ]

-- | Користувач, пов'язаний із викладачем, повинен мати роль «викладач».
instance Checked Teacher where
  checkDb conn t = do
    n <- countRows conn "SELECT COUNT(*) FROM users WHERE id = ? AND role = 'TEACHER'" [render (thUserId t)]
    return [ "Користувач повинен мати роль «викладач»" | n == 0 ]

instance Persistent Teacher where
  tableName _   = "teachers"
  entityTitle _ = "Викладачі"
  dataColumns _ = ["user_id", "department", "job_title"]
  recordId      = thId
  recordParams t = renderParams (thUserId t, thDepartment t, thJobTitle t)
  listing conn _ = do
    rows <- query_ conn
      "SELECT t.id, u.full_name, t.department, t.job_title FROM teachers t \
      \JOIN users u ON u.id = t.user_id ORDER BY u.full_name"
    return [ (i, printf "%s — %s, %s" (str nm) (str d) (str j))
           | (i, nm, d, j) <- rows :: [(Int, Text, Text, Text)] ]

instance Promptable Teacher where
  promptRecord conn old = do
    u <- askRef conn (Proxy :: Proxy User) "ID користувача" (thUserId <$> old)
    d <- ask "Кафедра" (thDepartment <$> old)
    j <- ask "Посада" (thJobTitle <$> old)
    return (Teacher (maybe 0 thId old) u d j)

--------------------------------------------------------------------------------
-- Subject — таблиця subjects

data Subject = Subject
  { sbId   :: Int
  , sbCode :: Text
  , sbName :: Text
  } deriving (Show, Eq)

instance QueryResults Subject where
  convertResults fs vs = case convertResults fs vs of
    (i, c, n) -> Subject i c n

instance Pretty Subject where
  pretty s = printf "%s — %s" (str (sbCode s)) (str (sbName s))

instance Validatable Subject where
  validate s =
    [ "Код дисципліни не може бути порожнім" | blank (sbCode s) ] ++
    [ "Назва дисципліни не може бути порожньою" | blank (sbName s) ]

instance Checked Subject

instance Persistent Subject where
  tableName _   = "subjects"
  entityTitle _ = "Дисципліни"
  dataColumns _ = ["code", "name"]
  recordId      = sbId
  recordParams s = renderParams (sbCode s, sbName s)

instance Promptable Subject where
  promptRecord _ old = do
    c <- ask "Код" (sbCode <$> old)
    n <- ask "Назва" (sbName <$> old)
    return (Subject (maybe 0 sbId old) c n)

--------------------------------------------------------------------------------
-- Lesson — таблиця schedule (планові заняття)

data Lesson = Lesson
  { lsId          :: Int
  , lsClassroomId :: Int
  , lsTeacherId   :: Int
  , lsSubjectId   :: Int
  , lsGroup       :: Text
  , lsDay         :: WeekDay
  , lsStart       :: TimeOfDay
  , lsEnd         :: TimeOfDay
  } deriving (Show, Eq)

instance QueryResults Lesson where
  convertResults fs vs = case convertResults fs vs of
    (i, c, t, s, g, d, st, en) -> Lesson i c t s g d st en

instance Pretty Lesson where
  pretty l = printf "клас #%d, викладач #%d, дисципліна #%d, група %s, %s %s–%s"
               (lsClassroomId l) (lsTeacherId l) (lsSubjectId l) (str (lsGroup l))
               (enumLabel (lsDay l)) (fmtTime (lsStart l)) (fmtTime (lsEnd l))

instance Validatable Lesson where
  validate l =
    [ "Назва групи не може бути порожньою" | blank (lsGroup l) ] ++
    [ "Час закінчення має бути пізнішим за час початку" | lsEnd l <= lsStart l ]

-- | Заняття не можуть перетинатися за часом ні в класі, ні у викладача,
--   ні в групи, а також не можуть потрапляти на години вільного доступу.
instance Checked Lesson where
  checkDb conn l = do
    a <- clash "classroom_id" (render (lsClassroomId l))
           "Цей клас у вказаний час уже зайнятий іншим заняттям"
    b <- clash "teacher_id" (render (lsTeacherId l))
           "Викладач у цей час уже веде інше заняття"
    c <- clash "group_name" (render (lsGroup l))
           "У цієї групи в цей час уже є інше заняття"
    n <- countRows conn
           "SELECT COUNT(*) FROM free_access WHERE classroom_id = ? AND week_day = ? \
           \AND start_time < ? AND ? < end_time"
           [render (lsClassroomId l), render (lsDay l), render (lsEnd l), render (lsStart l)]
    return (a ++ b ++ c ++ [ "У цей час у класі встановлено вільний доступ — спершу змініть його розклад" | n > 0 ])
    where
      clash :: String -> Action -> String -> IO [String]
      clash col val msg = do
        n <- countRows conn
               (fromString ("SELECT COUNT(*) FROM schedule WHERE week_day = ? AND start_time < ? \
                            \AND ? < end_time AND id <> ? AND " ++ col ++ " = ?"))
               [render (lsDay l), render (lsEnd l), render (lsStart l), render (lsId l), val]
        return [msg | n > 0]

instance Persistent Lesson where
  tableName _   = "schedule"
  entityTitle _ = "Розклад планових занять"
  dataColumns _ = ["classroom_id", "teacher_id", "subject_id", "group_name", "week_day", "start_time", "end_time"]
  recordId      = lsId
  recordParams l = renderParams
    (lsClassroomId l, lsTeacherId l, lsSubjectId l, lsGroup l, lsDay l, lsStart l, lsEnd l)
  listing conn _ = do
    rows <- query_ conn
      "SELECT l.id, c.name, s.name, u.full_name, l.group_name, l.week_day, l.start_time, l.end_time \
      \FROM schedule l JOIN classrooms c ON c.id = l.classroom_id JOIN subjects s ON s.id = l.subject_id \
      \JOIN teachers t ON t.id = l.teacher_id JOIN users u ON u.id = t.user_id \
      \ORDER BY l.week_day, l.start_time, c.name"
    return [ (i, printf "%s %s–%s | %s | %s | %s | гр. %s"
                   (enumLabel d) (fmtTime st) (fmtTime en) (str cn) (str sn) (str tn) (str g))
           | (i, cn, sn, tn, g, d, st, en)
               <- rows :: [(Int, Text, Text, Text, Text, WeekDay, TimeOfDay, TimeOfDay)] ]

instance Promptable Lesson where
  promptRecord conn old = do
    c  <- askRef conn (Proxy :: Proxy Classroom) "ID класу" (lsClassroomId <$> old)
    t  <- askRef conn (Proxy :: Proxy Teacher) "ID викладача" (lsTeacherId <$> old)
    sb <- askRef conn (Proxy :: Proxy Subject) "ID дисципліни" (lsSubjectId <$> old)
    g  <- ask "Група" (lsGroup <$> old)
    d  <- ask "День тижня" (lsDay <$> old)
    st <- ask "Початок" (lsStart <$> old)
    en <- ask "Закінчення" (lsEnd <$> old)
    return (Lesson (maybe 0 lsId old) c t sb g d st en)

--------------------------------------------------------------------------------
-- FreeSlot — таблиця free_access (час вільного доступу)

data FreeSlot = FreeSlot
  { fsId          :: Int
  , fsClassroomId :: Int
  , fsDay         :: WeekDay
  , fsStart       :: TimeOfDay
  , fsEnd         :: TimeOfDay
  , fsNote        :: Maybe Text
  } deriving (Show, Eq)

instance QueryResults FreeSlot where
  convertResults fs vs = case convertResults fs vs of
    (i, c, d, st, en, n) -> FreeSlot i c d st en n

instance Pretty FreeSlot where
  pretty f = printf "клас #%d, %s %s–%s%s" (fsClassroomId f) (enumLabel (fsDay f))
               (fmtTime (fsStart f)) (fmtTime (fsEnd f)) (maybe "" (\n -> " (" ++ str n ++ ")") (fsNote f))

instance Validatable FreeSlot where
  validate f = [ "Час закінчення має бути пізнішим за час початку" | fsEnd f <= fsStart f ]

-- | Вільний доступ не може збігатися із заняттями або з іншим інтервалом вільного доступу.
instance Checked FreeSlot where
  checkDb conn f = do
    a <- countRows conn
           "SELECT COUNT(*) FROM schedule WHERE classroom_id = ? AND week_day = ? \
           \AND start_time < ? AND ? < end_time"
           [render (fsClassroomId f), render (fsDay f), render (fsEnd f), render (fsStart f)]
    b <- countRows conn
           "SELECT COUNT(*) FROM free_access WHERE classroom_id = ? AND week_day = ? \
           \AND start_time < ? AND ? < end_time AND id <> ?"
           [render (fsClassroomId f), render (fsDay f), render (fsEnd f), render (fsStart f), render (fsId f)]
    return $ [ "У цей час у класі проходить планове заняття" | a > 0 ]
          ++ [ "Інтервал перетинається з іншим інтервалом вільного доступу цього класу" | b > 0 ]

instance Persistent FreeSlot where
  tableName _   = "free_access"
  entityTitle _ = "Час вільного доступу"
  dataColumns _ = ["classroom_id", "week_day", "start_time", "end_time", "note"]
  recordId      = fsId
  recordParams f = renderParams (fsClassroomId f, fsDay f, fsStart f, fsEnd f, fsNote f)
  listing conn _ = do
    rows <- query_ conn
      "SELECT f.id, c.name, f.week_day, f.start_time, f.end_time, f.note FROM free_access f \
      \JOIN classrooms c ON c.id = f.classroom_id ORDER BY c.name, f.week_day, f.start_time"
    return [ (i, printf "%s | %s %s–%s%s" (str cn) (enumLabel d) (fmtTime st) (fmtTime en)
                   (maybe "" (\n -> " (" ++ str n ++ ")") nt))
           | (i, cn, d, st, en, nt)
               <- rows :: [(Int, Text, WeekDay, TimeOfDay, TimeOfDay, Maybe Text)] ]

instance Promptable FreeSlot where
  promptRecord conn old = do
    c  <- askRef conn (Proxy :: Proxy Classroom) "ID класу" (fsClassroomId <$> old)
    d  <- ask "День тижня" (fsDay <$> old)
    st <- ask "Початок" (fsStart <$> old)
    en <- ask "Закінчення" (fsEnd <$> old)
    n  <- ask "Примітка" (fsNote <$> old)
    return (FreeSlot (maybe 0 fsId old) c d st en n)

--------------------------------------------------------------------------------
-- Session — таблиця usage_log (журнал використання робочих місць)

data Session = Session
  { ssId            :: Int
  , ssWorkstationId :: Int
  , ssUserId        :: Int
  , ssLessonId      :: Maybe Int
  , ssKind          :: SessionKind
  , ssStart         :: UTCTime
  , ssEnd           :: Maybe UTCTime   -- ^ 'Nothing' — сеанс ще триває
  } deriving (Show, Eq)

instance QueryResults Session where
  convertResults fs vs = case convertResults fs vs of
    (i, w, u, l, k, st, en) -> Session i w u l k st en

instance Pretty Session where
  pretty s = printf "місце #%d, користувач #%d, %s, %s → %s" (ssWorkstationId s) (ssUserId s)
               (enumLabel (ssKind s)) (fmtDateTime (ssStart s)) (maybe "триває" fmtDateTime (ssEnd s))

instance Validatable Session where
  validate s =
    [ "Для сеансу типу «заняття» потрібно вказати заняття" | ssKind s == KindLesson, ssLessonId s == Nothing ] ++
    [ "Для вільного доступу заняття вказувати не потрібно" | ssKind s == KindFreeAccess, ssLessonId s /= Nothing ] ++
    [ "Кінець сеансу не може бути раніше за початок" | Just e <- [ssEnd s], e < ssStart s ]

-- | На одному місці не може бути двох сеансів одночасно;
--   заняття має проходити в тому ж класі, де стоїть робоче місце.
instance Checked Session where
  checkDb conn s = do
    a <- countRows conn
           "SELECT COUNT(*) FROM usage_log WHERE workstation_id = ? AND id <> ? \
           \AND started_at < ? AND COALESCE(ended_at, '9999-12-31 00:00:00') > ?"
           [render (ssWorkstationId s), render (ssId s), render (fromMaybe farFuture (ssEnd s)), render (ssStart s)]
    b <- case ssLessonId s of
           Nothing -> return 1
           Just l  -> countRows conn
             "SELECT COUNT(*) FROM schedule l JOIN workstations w ON w.classroom_id = l.classroom_id \
             \WHERE l.id = ? AND w.id = ?"
             [render l, render (ssWorkstationId s)]
    return $ [ "Це робоче місце вже зайняте в зазначений проміжок часу" | a > 0 ]
          ++ [ "Заняття проходить в іншому класі, ніж розташоване вибране робоче місце" | b == 0 ]

instance Persistent Session where
  tableName _   = "usage_log"
  entityTitle _ = "Журнал використання робочих місць (останні 50)"
  dataColumns _ = ["workstation_id", "user_id", "lesson_id", "kind", "started_at", "ended_at"]
  recordId      = ssId
  recordParams s = renderParams
    (ssWorkstationId s, ssUserId s, ssLessonId s, ssKind s, ssStart s, ssEnd s)
  listing conn _ = do
    rows <- query_ conn
      "SELECT s.id, w.inv_number, u.full_name, s.kind, s.started_at, s.ended_at FROM usage_log s \
      \JOIN workstations w ON w.id = s.workstation_id JOIN users u ON u.id = s.user_id \
      \ORDER BY s.started_at DESC LIMIT 50"
    return [ (i, printf "%s | %s | %s | %s → %s" (str inv) (str un) (enumLabel k)
                   (fmtDateTime st) (maybe "триває" fmtDateTime en))
           | (i, inv, un, k, st, en)
               <- rows :: [(Int, Text, Text, SessionKind, UTCTime, Maybe UTCTime)] ]

instance Promptable Session where
  promptRecord conn old = do
    w   <- askRef conn (Proxy :: Proxy Workstation) "ID робочого місця" (ssWorkstationId <$> old)
    u   <- askRef conn (Proxy :: Proxy User) "ID користувача" (ssUserId <$> old)
    k   <- ask "Тип сеансу" (Just (maybe KindFreeAccess ssKind old))
    l   <- if k == KindLesson
             then Just <$> askRef conn (Proxy :: Proxy Lesson) "ID заняття" (old >>= ssLessonId)
             else return Nothing
    now <- currentDbTime conn
    st  <- ask "Початок" (Just (maybe now ssStart old))
    en  <- ask "Кінець" (ssEnd <$> old)
    return (Session (maybe 0 ssId old) w u l k st en)
