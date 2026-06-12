{-# LANGUAGE OverloadedStrings #-}

-- | Sítios de enzimas de restrição: trechos curtos de DNA que certas enzimas
-- reconhecem e cortam. Função pura — procura cada sítio na sequência.
module PaxLab.Restriction
  ( Enzyme(..)
  , restrictionSites
  ) where

import           Data.Text  (Text)
import qualified Data.Text  as T

import           PaxLab.Bio (normalize)

data Enzyme = Enzyme
  { enzName :: Text   -- ^ nome da enzima (ex.: EcoRI)
  , enzSite :: Text   -- ^ sítio de reconhecimento (ex.: GAATTC)
  } deriving (Show, Eq)

-- Algumas enzimas clássicas e seus sítios de reconhecimento (palindrômicos).
enzymes :: [Enzyme]
enzymes =
  [ Enzyme "EcoRI"   "GAATTC"
  , Enzyme "BamHI"   "GGATCC"
  , Enzyme "HindIII" "AAGCTT"
  , Enzyme "PstI"    "CTGCAG"
  , Enzyme "SmaI"    "CCCGGG"
  , Enzyme "XhoI"    "CTCGAG"
  , Enzyme "EcoRV"   "GATATC"
  , Enzyme "NotI"    "GCGGCCGC"
  ]

-- | Enzimas com pelo menos um sítio na sequência, junto das posições (0-based).
restrictionSites :: Text -> [(Enzyme, [Int])]
restrictionSites s0 =
  [ (e, ps) | e <- enzymes, let ps = occurrences (enzSite e), not (null ps) ]
  where
    s = normalize s0
    occurrences pat =
      [ i | i <- [0 .. T.length s - T.length pat], pat `T.isPrefixOf` T.drop i s ]
