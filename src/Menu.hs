{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Консольне меню. Функція 'crudMenu' одна на всі таблиці: вона вимагає від типу
--   лише екземплярів класів 'Managed' і більше нічого про нього не знає.
module Menu (mainMenu) where

import Control.Exception (IOException, try)
import Data.Proxy (Proxy (..))
import Data.Time (timeToTimeOfDay, utctDayTime)
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Param (..))
import System.IO.Error (ioeGetErrorString, isUserError)

import Classes
import Database
import Domain
import Prompt
import Reports

-- | Усе, що потрібно типу, щоб його можна було вводити, перевіряти,
--   зберігати в БД і редагувати через універсальне меню.
type Managed a = (Persistent a, Promptable a, Validatable a, Checked a)

--------------------------------------------------------------------------------
-- Загальні допоміжні функції

-- | Виконує дію меню; помилки БД та «людські» помилки (порожній довідник тощо)
--   показує повідомленням, а не завершує програму.
guarded :: IO () -> IO ()
guarded act = do
  r <- try (safely act)
  case r of
    Right (Right ()) -> return ()
    Right (Left msg) -> putStrLn ("  ! Помилка БД: " ++ msg)
    Left (e :: IOException)
      | isUserError e -> putStrLn ("  ! " ++ ioeGetErrorString e)
      | otherwise     -> ioError e

-- | Спочатку чисті перевірки ('validate'), і лише якщо вони пройдені — перевірки через БД.
allProblems :: (Validatable a, Checked a) => Connection -> a -> IO [String]
allProblems conn r = case validate r of
  [] -> checkDb conn r
  es -> return es

reportProblems :: String -> [String] -> IO ()
reportProblems verdict es = do
  mapM_ (putStrLn . ("  ! " ++)) es
  putStrLn ("  " ++ verdict)

--------------------------------------------------------------------------------
-- Універсальне меню «перегляд / додати / виправити / видалити»

crudMenu :: forall a. Managed a => Connection -> Proxy a -> IO ()
crudMenu conn p = loop
  where
    loop = do
      putStrLn ("\n--- " ++ entityTitle p ++ " ---")
      putStrLn "  1. Переглянути"
      putStrLn "  2. Додати"
      putStrLn "  3. Виправити"
      putStrLn "  4. Видалити"
      putStrLn "  0. Назад"
      c <- trim <$> askLine "Ваш вибір: "
      case c of
        "1" -> guarded (listAll conn p) >> loop
        "2" -> guarded (addRecord conn p) >> loop
        "3" -> guarded (editRecord conn p) >> loop
        "4" -> guarded (removeRecord conn p) >> loop
        "0" -> return ()
        _   -> putStrLn "  ! Невідомий пункт меню." >> loop

listAll :: Persistent a => Connection -> Proxy a -> IO ()
listAll conn p = do
  items <- listing conn p
  putStrLn ("\n" ++ entityTitle p ++ ":")
  if null items
    then putStrLn "  (порожньо)"
    else mapM_ (\(i, s) -> putStrLn ("  " ++ show i ++ ") " ++ s)) items

addRecord :: forall a. Managed a => Connection -> Proxy a -> IO ()
addRecord conn _ = do
  r    <- promptRecord conn (Nothing :: Maybe a)
  errs <- allProblems conn r
  if null errs
    then do
      i <- insertRecord conn r
      putStrLn ("  Додано (ID " ++ show i ++ "): " ++ pretty r)
    else reportProblems "Запис не збережено." errs

editRecord :: forall a. Managed a => Connection -> Proxy a -> IO ()
editRecord conn p = do
  listAll conn p
  i     <- ask "ID запису, який виправляємо" (Nothing :: Maybe Int)
  found <- selectById conn i :: IO (Maybe a)
  case found of
    Nothing  -> putStrLn "  ! Запису з таким ID немає."
    Just old -> do
      putStrLn ("  Поточні дані: " ++ pretty old)
      putStrLn "  (Enter — залишити значення без змін)"
      new  <- promptRecord conn (Just old)
      errs <- allProblems conn new
      if null errs
        then do
          ok <- updateRecord conn new
          putStrLn (if ok then "  Збережено: " ++ pretty new else "  ! Запис не знайдено.")
        else reportProblems "Зміни не збережено." errs

removeRecord :: Persistent a => Connection -> Proxy a -> IO ()
removeRecord conn p = do
  listAll conn p
  i    <- ask "ID запису, який видаляємо" (Nothing :: Maybe Int)
  sure <- confirm ("Видалити запис " ++ show i)
  if not sure
    then putStrLn "  Скасовано."
    else do
      ok <- deleteRecord conn p i
      putStrLn (if ok then "  Видалено." else "  ! Запису з таким ID немає.")

--------------------------------------------------------------------------------
-- Сеанси роботи за комп'ютерами

-- | Реєструє початок роботи користувача за робочим місцем (час — серверний).
startSession :: Connection -> IO ()
startSession conn = do
  w <- askRef conn (Proxy :: Proxy Workstation) "ID робочого місця" Nothing
  u <- askRef conn (Proxy :: Proxy User) "ID користувача" Nothing
  k <- ask "Тип сеансу" (Just KindFreeAccess)
  l <- if k == KindLesson
         then Just <$> askRef conn (Proxy :: Proxy Lesson) "ID заняття" Nothing
         else return Nothing
  now <- currentDbTime conn
  let sess = Session 0 w u l k now Nothing
  errs  <- allProblems conn sess
  extra <- if null errs then liveChecks conn sess else return []
  case errs ++ extra of
    [] -> do
      i <- insertRecord conn sess
      putStrLn ("  Сеанс розпочато (ID " ++ show i ++ ").")
    ps -> reportProblems "Сеанс не розпочато." ps

-- | Правила, які мають сенс лише для сеансу, що починається «зараз».
liveChecks :: Connection -> Session -> IO [String]
liveChecks conn s = do
  st <- query conn "SELECT status FROM workstations WHERE id = ?" (Only (ssWorkstationId s))
  let broken = [ "Робоче місце непридатне до роботи: " ++ enumLabel x | Only x <- st, x /= WsWorking ]
      tod    = timeToTimeOfDay (utctDayTime (ssStart s))
  open <- if ssKind s == KindFreeAccess
            then countRows conn
                   "SELECT COUNT(*) FROM free_access f JOIN workstations w ON w.classroom_id = f.classroom_id \
                   \WHERE w.id = ? AND f.week_day = ? AND f.start_time <= ? AND ? < f.end_time"
                   [render (ssWorkstationId s), render (weekDayOf (ssStart s)), render tod, render tod]
            else return 1
  return (broken ++ [ "Зараз у цьому класі не передбачено вільного доступу" | open == 0 ])

-- | Завершує один із активних сеансів (час закінчення — серверний).
endSession :: Connection -> IO ()
endSession conn = do
  active <- activeSessions conn
  if null active
    then putStrLn "  Активних сеансів немає."
    else do
      putStr (renderTable active)
      i <- ask "ID сеансу, який завершити" (Nothing :: Maybe Int)
      n <- execute conn "UPDATE usage_log SET ended_at = NOW() WHERE id = ? AND ended_at IS NULL" (Only i)
      putStrLn (if n > 0 then "  Сеанс завершено." else "  ! Активного сеансу з таким ID немає.")

--------------------------------------------------------------------------------
-- Звіти та головне меню

reportsMenu :: Connection -> IO ()
reportsMenu conn = loop
  where
    loop = do
      putStrLn "\n--- Звіти ---"
      putStrLn "  1. Розклад класу"
      putStrLn "  2. Розклад викладача"
      putStrLn "  3. Розклад групи"
      putStrLn "  4. Повний розклад занять"
      putStrLn "  5. Час вільного доступу"
      putStrLn "  6. Де вільний доступ відкритий зараз"
      putStrLn "  7. Завантаженість дисплейних класів"
      putStrLn "  8. Завантаженість робочих місць"
      putStrLn "  9. Активні сеанси"
      putStrLn "  0. Назад"
      c <- trim <$> askLine "Ваш вибір: "
      case c of
        "1" -> guarded (reportScheduleByClassroom conn) >> loop
        "2" -> guarded (reportScheduleByTeacher conn) >> loop
        "3" -> guarded (reportScheduleByGroup conn) >> loop
        "4" -> guarded (reportFullSchedule conn) >> loop
        "5" -> guarded (reportFreeAccess conn) >> loop
        "6" -> guarded (reportFreeNow conn) >> loop
        "7" -> guarded (reportClassroomLoad conn) >> loop
        "8" -> guarded (reportWorkstationLoad conn) >> loop
        "9" -> guarded (reportActiveSessions conn) >> loop
        "0" -> return ()
        _   -> putStrLn "  ! Невідомий пункт меню." >> loop

mainMenu :: Connection -> IO ()
mainMenu conn = loop
  where
    loop = do
      putStrLn ""
      putStrLn "=============================================="
      putStrLn "  Розклад роботи дисплейних класів факультету"
      putStrLn "=============================================="
      putStrLn "   1. Дисплейні класи"
      putStrLn "   2. Робочі місця"
      putStrLn "   3. Користувачі"
      putStrLn "   4. Викладачі"
      putStrLn "   5. Дисципліни"
      putStrLn "   6. Розклад планових занять"
      putStrLn "   7. Час вільного доступу"
      putStrLn "   8. Журнал використання робочих місць"
      putStrLn "   9. Почати сеанс роботи"
      putStrLn "  10. Завершити сеанс роботи"
      putStrLn "  11. Звіти"
      putStrLn "   0. Вихід"
      c <- trim <$> askLine "Ваш вибір: "
      case c of
        "1"  -> crudMenu conn (Proxy :: Proxy Classroom) >> loop
        "2"  -> crudMenu conn (Proxy :: Proxy Workstation) >> loop
        "3"  -> crudMenu conn (Proxy :: Proxy User) >> loop
        "4"  -> crudMenu conn (Proxy :: Proxy Teacher) >> loop
        "5"  -> crudMenu conn (Proxy :: Proxy Subject) >> loop
        "6"  -> crudMenu conn (Proxy :: Proxy Lesson) >> loop
        "7"  -> crudMenu conn (Proxy :: Proxy FreeSlot) >> loop
        "8"  -> crudMenu conn (Proxy :: Proxy Session) >> loop
        "9"  -> guarded (startSession conn) >> loop
        "10" -> guarded (endSession conn) >> loop
        "11" -> reportsMenu conn >> loop
        "0"  -> putStrLn "До побачення!"
        _    -> putStrLn "  ! Невідомий пункт меню." >> loop
