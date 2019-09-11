module Main where

import Prelude hiding (sum, product, min, max, lines, words)

import System.Environment (getArgs)
import Control.Monad ((>=>))
-- import Control.Monad.State (State, evalState, get, put)
-- import qualified Control.Foldl as L
import qualified Data.Char as Char
-- import Data.Functor.Compose (Compose(..))
-- import Data.Functor.Const (Const(..))
-- import Data.Functor.Product (Product(..))
-- import qualified Control.Foldl as F
-- import qualified Control.Foldl.Text as T
import Data.Foldable
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Semigroup (Sum(..))
import Text.Printf


main :: IO ()
main = getArgs >>= traverse_ (BL.readFile >=> program)

program :: BL.ByteString -> IO ()
program = foldCounts & format & display

foldCounts :: BL.ByteString -> Counts
foldCounts = BL.foldl' (flip (mappend . count)) mempty


display :: Report -> IO ()
display (Report chs wds lns) = printf "Lines: %d Words: %d Chars: %d\n" lns wds chs

format :: Counts -> Report
format (Counts chars lns (Flux _ wrds _)) =
    Report (getSum chars) (getSum lns) (getSum wrds)

type Count = Sum Int

count :: Char -> Counts
count = Counts <$> (const (Sum 1)) <*> (Sum . fromEnum . (== '\n')) <*> flux


data Counts = Counts
    { characters :: !Count
    , lines :: !Count
    , words :: !Flux
    } deriving (Show)

instance Semigroup Counts where
     Counts charsA linesA wordsA <> Counts charsB linesB wordsB = Counts
        (charsA <> charsB)
        (linesA <> linesB)
        (wordsA <> wordsB)

instance Monoid Counts where
    mempty = Counts mempty mempty mempty


data CharType
    = AlphaNumeric
    | Whitespace
    deriving (Show)

data Flux = Flux
    { left :: !CharType
    , counter :: !Count
    , right :: !CharType
    } deriving (Show)

instance Semigroup Flux where
  Flux l n AlphaNumeric <> Flux AlphaNumeric n' r = Flux l (n <> n' <> Sum (-1)) r
  Flux l n _ <> Flux _ n' r = Flux l (n <> n') r

instance Monoid Flux where
    mempty = Flux Whitespace mempty Whitespace

flux :: Char -> Flux
flux c | Char.isSpace c = mempty
       | otherwise = Flux AlphaNumeric 1 AlphaNumeric


data Report = Report
    { characterCount :: Int
    , lineCount :: Int
    , wordCount :: Int
    } deriving (Show)

-- Helpers
(&) :: (a -> b) -> (b -> c) -> a -> c
(&) = flip (.)

(#) :: a -> (a -> b) -> b
(#) = flip ($)
