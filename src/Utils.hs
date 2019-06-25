module Utils where


import qualified Data.Char as Char
import           Data.Text as T

-------------------------------------------------------------------


uncapFst :: String -> String
uncapFst (head:tail) = Char.toLower head : tail
uncapFst []          = []
