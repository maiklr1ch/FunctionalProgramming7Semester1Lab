{-# LANGUAGE ScopedTypeVariables #-}

-- | Класи типів, на яких побудована вся система, та допоміжні функції до них.
--
--   * 'Pretty'      — як показати значення користувачу;
--   * 'Validatable' — перевірка бізнес-правил без звернення до БД;
--   * 'Checked'     — перевірка правил цілісності, що потребують запитів до БД;
--   * 'Enumerated'  — перелічувані типи, що зберігаються як MySQL ENUM;
--   * 'Input'       — розбір і показ окремого поля форми введення;
--   * 'Tabular'     — рядок звіту, який можна вивести таблицею.
module Classes
  ( Pretty (..)
  , Validatable (..)
  , Checked (..)
  , Enumerated (..)
  , Input (..)
  , Tabular (..)
    -- * Допоміжні функції
  , enumValues, parseEnum, enumHint, renderEnum, convertEnum
  , renderTable
  , trim, fmtTime, fmtDateTime
  ) where

import Control.Exception (throw)
import Data.ByteString (ByteString)
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, find, intercalate, transpose)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeOfDay, UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple (Connection)
import Database.MySQL.Simple.Param (Action, Param (..))
import Database.MySQL.Simple.Result (Result (..), ResultError (..))

--------------------------------------------------------------------------------
-- Pretty

-- | Людиночитне подання значення (один рядок).
class Pretty a where
  pretty :: a -> String

--------------------------------------------------------------------------------
-- Validatable / Checked

-- | Перевірка значення без звернення до БД. Порожній список = помилок немає.
class Validatable a where
  validate :: a -> [String]

-- | Перевірка, якій потрібна БД (перетини в розкладі, ролі, зв'язки між таблицями).
--   Для типів без таких правил береться метод за замовчуванням.
class Checked a where
  checkDb :: Connection -> a -> IO [String]
  checkDb _ _ = return []

--------------------------------------------------------------------------------
-- Enumerated

-- | Скінченний перелік, що зберігається в БД як ENUM-код,
--   а користувачу показується українським підписом.
class (Bounded a, Enum a) => Enumerated a where
  enumCode  :: a -> String   -- ^ значення в MySQL ENUM
  enumLabel :: a -> String   -- ^ підпис для користувача

enumValues :: Enumerated a => [a]
enumValues = [minBound .. maxBound]

-- | Підказка виду "1=Пн, 2=Вт, ...".
enumHint :: forall a. Enumerated a => Proxy a -> String
enumHint _ = intercalate ", " [show i ++ "=" ++ enumLabel x | (i, x) <- zip [1 :: Int ..] (enumValues :: [a])]

-- | Приймає номер, ENUM-код або український підпис (без урахування регістру).
parseEnum :: forall a. Enumerated a => String -> Either String a
parseEnum raw =
  case [x | (i, x) <- zip [1 :: Int ..] (enumValues :: [a]), matches i x] of
    (x : _) -> Right x
    []      -> Left ("оберіть: " ++ enumHint (Proxy :: Proxy a))
  where
    s = map toLower (trim raw)
    matches i x = s == show i || s == map toLower (enumCode x) || s == map toLower (enumLabel x)

-- | Для реалізації @Param@ перелічуваних типів.
renderEnum :: Enumerated a => a -> Action
renderEnum = render . enumCode

-- | Для реалізації @Result@ перелічуваних типів.
convertEnum :: forall a. Enumerated a => String -> Field -> Maybe ByteString -> a
convertEnum tyName f v =
  case find ((== code) . enumCode) (enumValues :: [a]) of
    Just x  -> x
    Nothing -> throw ConversionFailed
      { errSQLType     = "ENUM"
      , errHaskellType = tyName
      , errFieldName   = ""
      , errMessage     = "невідоме значення: " ++ code
      }
  where
    code = T.unpack (convert f v :: Text)

--------------------------------------------------------------------------------
-- Input

-- | Поле форми введення: як розібрати рядок і як показати поточне значення.
class Input a where
  parseInput :: String -> Either String a
  showInput  :: a -> String
  inputHint  :: Proxy a -> String
  inputHint _ = ""

instance Input Text where
  parseInput s
    | null s    = Left "поле не може бути порожнім"
    | otherwise = Right (T.pack s)
  showInput = T.unpack

instance Input Int where
  parseInput s = case reads s of
    [(n, "")] -> Right n
    _         -> Left "потрібне ціле число"
  showInput = show

instance Input Bool where
  parseInput s
    | x `elem` ["так", "т", "y", "yes", "1", "true"]  = Right True
    | x `elem` ["ні", "н", "n", "no", "0", "false"]   = Right False
    | otherwise                                       = Left "введіть «так» або «ні»"
    where x = map toLower s
  showInput b = if b then "так" else "ні"
  inputHint _ = "так/ні"

instance Input TimeOfDay where
  parseInput s = maybe (Left "формат часу — ГГ:ХХ, наприклад 08:30") Right
                       (parseTimeM True defaultTimeLocale "%H:%M" s)
  showInput = fmtTime
  inputHint _ = "ГГ:ХХ"

instance Input UTCTime where
  parseInput s = maybe (Left "формат — РРРР-ММ-ДД ГГ:ХХ, наприклад 2026-10-05 10:30") Right
                       (parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M" s)
  showInput = fmtDateTime
  inputHint _ = "РРРР-ММ-ДД ГГ:ХХ"

-- | Необов'язкове поле: порожній ввід або «-» дає 'Nothing'.
instance forall a. Input a => Input (Maybe a) where
  parseInput s
    | s `elem` ["", "-"] = Right Nothing
    | otherwise          = Just <$> parseInput s
  showInput = maybe "—" showInput
  inputHint _ = case inputHint (Proxy :: Proxy a) of
    ""  -> "«-» = порожньо"
    h   -> h ++ "; «-» = порожньо"

--------------------------------------------------------------------------------
-- Tabular

-- | Рядок звіту: заголовки стовпців і значення клітинок.
class Tabular a where
  tableHeaders :: Proxy a -> [String]
  tableCells   :: a -> [String]

-- | Вирівняна текстова таблиця для довільного 'Tabular'.
renderTable :: forall a. Tabular a => [a] -> String
renderTable rows = unlines (line headers : sep : map line cellRows)
  where
    headers  = tableHeaders (Proxy :: Proxy a)
    cellRows = map tableCells rows
    widths   = [maximum (map length col) | col <- transpose (headers : cellRows)]
    line cs  = "  " ++ intercalate " | " [pad w c | (w, c) <- zip widths cs]
    sep      = "  " ++ intercalate "-+-" [replicate w '-' | w <- widths]
    pad w s  = s ++ replicate (w - length s) ' '

--------------------------------------------------------------------------------
-- Різне

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

fmtTime :: TimeOfDay -> String
fmtTime = formatTime defaultTimeLocale "%H:%M"

fmtDateTime :: UTCTime -> String
fmtDateTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"
