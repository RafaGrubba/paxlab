{-# LANGUAGE OverloadedStrings #-}

-- | Alinhamento global de duas sequências pelo algoritmo de
-- Needleman–Wunsch (programação dinâmica). Puro: a matriz de pontuação é
-- definida recursivamente e a Haskell-lazy resolve as dependências.
module PaxLab.Align
  ( Scoring(..)
  , Alignment(..)
  , defaultScoring
  , needlemanWunsch
  ) where

import           Data.Array
import           Data.Text  (Text)
import qualified Data.Text  as T

data Scoring = Scoring
  { matchScore    :: Int   -- ^ bônus por par igual
  , mismatchScore :: Int   -- ^ penalidade por par diferente
  , gapScore      :: Int   -- ^ penalidade por lacuna (gap)
  } deriving (Show, Eq)

-- Pontuação clássica simples.
defaultScoring :: Scoring
defaultScoring = Scoring 1 (-1) (-2)

data Alignment = Alignment
  { alignedA :: Text    -- ^ sequência A com lacunas ('-')
  , alignedB :: Text    -- ^ sequência B com lacunas ('-')
  , alignScore    :: Int
  , alignIdentity :: Double  -- ^ % de colunas idênticas
  } deriving (Show, Eq)

needlemanWunsch :: Scoring -> Text -> Text -> Alignment
needlemanWunsch sc a b = Alignment (T.pack alA) (T.pack alB) (h ! (n, m)) identity
  where
    s  = T.unpack (T.toUpper a)
    t  = T.unpack (T.toUpper b)
    n  = length s
    m  = length t
    sa = listArray (1, n) s :: Array Int Char
    ta = listArray (1, m) t :: Array Int Char
    g  = gapScore sc

    sub x y = if x == y then matchScore sc else mismatchScore sc

    -- Matriz de pontuação definida recursivamente (resolvida por laziness).
    h :: Array (Int, Int) Int
    h = array ((0, 0), (n, m))
          [ ((i, j), cell i j) | i <- [0 .. n], j <- [0 .. m] ]
    cell 0 0 = 0
    cell i 0 = g * i
    cell 0 j = g * j
    cell i j = maximum
      [ h ! (i - 1, j - 1) + sub (sa ! i) (ta ! j)
      , h ! (i - 1, j)     + g
      , h ! (i, j - 1)     + g
      ]

    -- Traceback: reconstrói o alinhamento a partir do canto (n, m).
    (alA, alB) = go n m [] []
    go i j accA accB
      | i == 0 && j == 0 = (accA, accB)
      | i > 0 && j > 0 && h ! (i, j) == h ! (i - 1, j - 1) + sub (sa ! i) (ta ! j)
          = go (i - 1) (j - 1) (sa ! i : accA) (ta ! j : accB)
      | i > 0 && h ! (i, j) == h ! (i - 1, j) + g
          = go (i - 1) j (sa ! i : accA) ('-' : accB)
      | otherwise
          = go i (j - 1) ('-' : accA) (ta ! j : accB)

    identity
      | null cols = 0
      | otherwise = 100 * fromIntegral same / fromIntegral (length cols)
      where
        cols = zip alA alB
        same = length [ () | (x, y) <- cols, x == y, x /= '-' ]
