{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Робота з MySQL: клас 'Persistent' і універсальні CRUD-функції.
--   Жодна з функцій не знає про конкретні таблиці — усе потрібне
--   вона дізнається з екземпляра 'Persistent' відповідного типу.
module Database
  ( Persistent (..)
  , connectFromEnv
  , selectAll, selectById, insertRecord, updateRecord, deleteRecord
  , safely
  , currentDbTime
  ) where

import Control.Exception (Handler (..), catches)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Time (UTCTime)
import Data.Word (Word16)
import qualified Database.MySQL.Base as B
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (Action, Param (..))
import Database.MySQL.Simple.QueryResults (QueryResults)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Classes

--------------------------------------------------------------------------------
-- Клас Persistent

-- | Сутність, що зберігається в окремій таблиці MySQL.
--
--   Рядок таблиці читається через @QueryResults@ (стовпці: @id@, потім
--   'dataColumns' у тому ж порядку), а записується через 'recordParams'.
class (QueryResults a, Pretty a) => Persistent a where
  tableName    :: Proxy a -> String
  entityTitle  :: Proxy a -> String          -- ^ назва для меню
  dataColumns  :: Proxy a -> [String]        -- ^ стовпці без @id@
  recordId     :: a -> Int
  recordParams :: a -> [Action]              -- ^ значення для 'dataColumns'

  -- | Пари (id, підпис) для списків та вибору посилань.
  --   За замовчуванням — 'pretty'; типи з зовнішніми ключами перевизначають
  --   метод запитом із JOIN, щоб показувати імена замість числових id.
  listing :: Connection -> Proxy a -> IO [(Int, String)]
  listing conn _ = do
    rs <- selectAll conn
    return [(recordId r, pretty r) | r <- rs :: [a]]

--------------------------------------------------------------------------------
-- Універсальний CRUD

selectSql :: Persistent a => Proxy a -> String -> Query
selectSql p suffix = fromString $
  "SELECT id, " ++ intercalate ", " (dataColumns p) ++ " FROM " ++ tableName p ++ suffix

selectAll :: forall a. Persistent a => Connection -> IO [a]
selectAll conn = query_ conn (selectSql (Proxy :: Proxy a) " ORDER BY id")

selectById :: forall a. Persistent a => Connection -> Int -> IO (Maybe a)
selectById conn i =
  listToMaybe <$> query conn (selectSql (Proxy :: Proxy a) " WHERE id = ?") (Only i)

-- | Додає запис і повертає згенерований @id@.
insertRecord :: forall a. Persistent a => Connection -> a -> IO Int
insertRecord conn r = do
  let p    = Proxy :: Proxy a
      cols = dataColumns p
      sql  = "INSERT INTO " ++ tableName p ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
             ++ intercalate ", " (map (const "?") cols) ++ ")"
  _ <- execute conn (fromString sql) (recordParams r)
  fromIntegral <$> insertID conn

-- | Оновлює запис за його @id@; 'False', якщо такого запису немає.
updateRecord :: forall a. Persistent a => Connection -> a -> IO Bool
updateRecord conn r = do
  let p    = Proxy :: Proxy a
      sets = intercalate ", " [c ++ " = ?" | c <- dataColumns p]
      sql  = "UPDATE " ++ tableName p ++ " SET " ++ sets ++ " WHERE id = ?"
  n <- execute conn (fromString sql) (recordParams r ++ [render (recordId r)])
  return (n > 0)

deleteRecord :: forall a. Persistent a => Connection -> Proxy a -> Int -> IO Bool
deleteRecord conn p i =
  (> 0) <$> execute conn (fromString ("DELETE FROM " ++ tableName p ++ " WHERE id = ?")) (Only i)

--------------------------------------------------------------------------------
-- Підключення та помилки

-- | Параметри з'єднання беруться зі змінних середовища
--   DB_HOST, DB_PORT, DB_USER, DB_PASSWORD, DB_NAME.
connectFromEnv :: IO Connection
connectFromEnv = do
  host  <- env "DB_HOST" "localhost"
  port  <- env "DB_PORT" "3306"
  user  <- env "DB_USER" "root"
  pass  <- env "DB_PASSWORD" ""
  dbase <- env "DB_NAME" "display_classes"
  connect defaultConnectInfo
    { connectHost     = host
    , connectPort     = fromMaybe 3306 (readMaybe port :: Maybe Word16)
    , connectUser     = user
    , connectPassword = pass
    , connectDatabase = dbase
    , connectOptions  = [B.CharsetName "utf8mb4", B.FoundRows]
    }
  where
    env k d = fromMaybe d <$> lookupEnv k

-- | Виконує дію, перетворюючи помилки MySQL на зрозумілий рядок.
safely :: IO a -> IO (Either String a)
safely act = (Right <$> act) `catches`
  [ Handler (\(e :: B.MySQLError) -> return (Left (describeError e)))
  , Handler (\(e :: ResultError)  -> return (Left ("помилка перетворення даних: " ++ show e)))
  ]

describeError :: B.MySQLError -> String
describeError e = case B.errNumber e of
  1451 -> "запис використовується в інших таблицях — спочатку змініть або видаліть пов'язані записи"
  1452 -> "посилання на неіснуючий запис (порушено зовнішній ключ)"
  1062 -> "такий запис уже існує (порушено унікальність)"
  1048 -> "обов'язкове поле не заповнене"
  n    -> B.errMessage e ++ " (код " ++ show n ++ ")"

-- | Поточний час сервера БД як «наївний» час (без часових поясів).
currentDbTime :: Connection -> IO UTCTime
currentDbTime conn = do
  rows <- query_ conn "SELECT NOW()"
  case rows of
    [Only t] -> return t
    _        -> ioError (userError "не вдалося отримати час сервера БД")
