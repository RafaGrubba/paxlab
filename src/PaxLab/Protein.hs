{-# LANGUAGE OverloadedStrings #-}

-- | Propriedades de uma proteína: comprimento, massa molecular aproximada e
-- composição de aminoácidos. Funções puras.
module PaxLab.Protein
  ( ProteinStats(..)
  , proteinStats
  , molecularWeight
  ) where

import           Data.List       (sortBy)
import qualified Data.Map.Strict as M
import           Data.Ord        (Down (..), comparing)
import           Data.Text       (Text)
import qualified Data.Text       as T

data ProteinStats = ProteinStats
  { psLength      :: Int            -- ^ número de aminoácidos
  , psWeightDa    :: Double         -- ^ massa molecular aproximada (Da)
  , psComposition :: [(Char, Int)]  -- ^ aminoácidos por frequência (desc)
  } deriving (Show, Eq)

-- Massa média de cada resíduo de aminoácido, em Dalton.
residueMass :: M.Map Char Double
residueMass = M.fromList
  [ ('A', 71.08), ('R',156.19), ('N',114.10), ('D',115.09), ('C',103.14)
  , ('E',129.12), ('Q',128.13), ('G', 57.05), ('H',137.14), ('I',113.16)
  , ('L',113.16), ('K',128.17), ('M',131.19), ('F',147.18), ('P', 97.12)
  , ('S', 87.08), ('T',101.10), ('W',186.21), ('Y',163.18), ('V', 99.13)
  ]

-- Mantém só os 20 aminoácidos padrão (ignora '*', 'X', espaços etc.).
cleanProt :: Text -> Text
cleanProt = T.filter (`M.member` residueMass) . T.toUpper

-- | Massa molecular aproximada: soma das massas dos resíduos + uma água.
molecularWeight :: Text -> Double
molecularWeight t
  | T.null s  = 0
  | otherwise = 18.02 + sum [ M.findWithDefault 0 c residueMass | c <- T.unpack s ]
  where s = cleanProt t

proteinStats :: Text -> ProteinStats
proteinStats t = ProteinStats
  { psLength      = T.length s
  , psWeightDa    = molecularWeight s
  , psComposition = sortBy (comparing (Down . snd)) (M.toList counts)
  }
  where
    s      = cleanProt t
    counts = M.fromListWith (+) [ (c, 1 :: Int) | c <- T.unpack s ]
