{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude hiding ((>>), sum, product, min, max, lines, words)
import System.Environment (getArgs)

import Control.Monad.State (State, evalState, get, put)
-- import qualified Control.Foldl as L
import Data.Char as Char
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Product (Product(..))
import Data.Foldable
import Data.Semigroup (Sum(..))


main :: IO ()
main = do
    strings <- getArgs
    case strings of
        [filePath] ->
            readFile filePath
            >>= program
            & runProgram
            & show
            & putStrLn
        _ -> pure ()


runProgram :: Product (Product (Const Count) (Const Count)) (Compose (State Bool) (Const Count)) () -> (Int, Int, Int)
runProgram (Pair (Pair chars lines) words) =
    ( getSum . getConst $ chars
    , getSum . getConst $ lines
    , getSum . getConst $ getCompose words `evalState` False
    )


program :: String -> Product (Product (Const Count) (Const Count)) (Compose (State Bool) (Const Count)) ()
program = traverse_ $ Pair <$> (Pair <$> countChar <*> countLine) <*> countWords

type Count = Sum Int

countChar :: a -> Const Count ()
countChar = const $ Const (Sum 1)

countLine :: Char -> Const Count ()
countLine = Const . Sum . fromEnum . (== '\n')

countWords :: Char -> Compose (State Bool) (Const Count) ()
countWords c = Compose $ do
    before <- get
    let after = not $ Char.isSpace c
    _ <- put after
    pure . Const . Sum . fromEnum $ (not before && after)


(&) :: (a -> b) -> (b -> c) -> a -> c
(&) = flip (.)

-- -- Swap tupling with more readable monoid eventually.

-- -- data WC = WC
-- --     { characters :: (Sum Int)
-- --     , lines :: (Sum Int)
-- --     , words :: (Sum Int)
-- --     }

-- -- instance Semigroup WC where
-- --     (<>) (WC charsA linesA wordsA) (WC charsB linesB wordsB) =
-- --         WC (charsA <> charsB) (linesA <> linesB) (wordsA <> wordsB)

-- -- instance Monoid WC where
-- --     mempty = WC mempty mempty mempty
