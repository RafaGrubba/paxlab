{-# LANGUAGE OverloadedStrings #-}

-- | Visualizações geradas como SVG puro (string montada em Haskell, sem
-- dependência extra nem JavaScript). Embutidas no HTML via 'toHtmlRaw'.
module PaxLab.Viz
  ( orfMapSvg
  ) where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Text.Printf (printf)

import           PaxLab.Bio  (ORF (..))

-- | Mapa de ORFs: uma faixa por frame de leitura (+1, +2, +3), com blocos
-- coloridos posicionados pela faixa que cada ORF ocupa na sequência.
orfMapSvg :: Int -> [ORF] -> Text
orfMapSvg len orfs = T.concat
  [ "<svg viewBox=\"0 0 ", tshow w, " ", tshow h, "\" width=\"100%\" "
  , "preserveAspectRatio=\"none\" "
  , "style=\"background:#0f1c2b;border-radius:8px;display:block\">"
  , baseline
  , T.concat (map frameLabel [1, 2, 3])
  , T.concat (map orfRect orfs)
  , label (fromIntegral w - pad - 38) 12 (tshow len <> " nt")
  , "</svg>" ]
  where
    w      = 600 :: Int
    h      = 96  :: Int
    pad    = 14  :: Double
    innerW = fromIntegral w - 2 * pad

    sx :: Int -> Double
    sx x = pad + fromIntegral x / fromIntegral (max 1 len) * innerW

    frameY :: Int -> Double
    frameY f = 26 + fromIntegral (f - 1) * 22

    color :: Int -> Text
    color 1 = "#37b97f"
    color 2 = "#4f9de8"
    color _ = "#e0a64f"

    baseline = T.concat
      [ "<line x1=\"", f2 pad, "\" y1=\"14\" x2=\""
      , f2 (fromIntegral w - pad), "\" y2=\"14\" stroke=\"#33485f\"/>" ]

    frameLabel f = label 2 (frameY f + 10) ("+" <> tshow f)

    orfRect o = T.concat
      [ "<rect x=\"", f2 (sx (orfStart o))
      , "\" y=\"", f2 (frameY frm)
      , "\" width=\"", f2 (max 2 (sx (orfEnd o) - sx (orfStart o)))
      , "\" height=\"13\" rx=\"3\" fill=\"", color frm, "\">"
      , "<title>Frame +", tshow (orfFrame o), "  ", tshow (orfStart o)
      , "–", tshow (orfEnd o), " nt  (", tshow (T.length (orfProtein o)), " aa)"
      , "</title></rect>" ]
      where frm = ((orfFrame o - 1) `mod` 3) + 1

    label x y t = T.concat
      [ "<text x=\"", f2 x, "\" y=\"", f2 y
      , "\" fill=\"#8295a8\" font-size=\"10\" font-family=\"monospace\">"
      , t, "</text>" ]

    f2 :: Double -> Text
    f2 = T.pack . printf "%.1f"

    tshow :: Int -> Text
    tshow = T.pack . show
