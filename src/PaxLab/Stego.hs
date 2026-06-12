{-# LANGUAGE OverloadedStrings #-}

-- | Biocifra: cifra texto dentro de uma sequência de "DNA" usando uma senha.
-- Junta o lado segurança ao tema biológico.
--
-- Esquema: cada byte do texto é combinado (XOR) com um fluxo de chave derivado
-- da senha e então vira 4 bases (2 bits cada: A=00, C=01, G=10, T=11). Um byte
-- "mágico" no início serve de verificação — se a senha estiver errada, ele não
-- confere e a decifragem é recusada.
module PaxLab.Stego
  ( encodeDNA
  , decodeDNA
  ) where

import           Data.Bits          (shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString    as BS
import           Data.Char          (ord)
import           Data.List          (foldl')
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8', encodeUtf8)
import           Data.Word          (Word8, Word32)

-- Byte de verificação (detecta senha errada na decifragem).
magic :: Word8
magic = 0x9E

-- Fluxo de chave (infinito) derivado da senha por um gerador linear simples.
keystream :: Text -> [Word8]
keystream key = map toByte (tail (iterate step seed0))
  where
    seed0 :: Word32
    seed0 = foldl' (\h c -> h * 131 + fromIntegral (ord c)) 2166136261 (T.unpack key)
    step s = s * 1103515245 + 12345          -- LCG (Word32 já é modular)
    toByte s = fromIntegral (s `shiftR` 16)

baseOf :: Int -> Char
baseOf 0 = 'A'
baseOf 1 = 'C'
baseOf 2 = 'G'
baseOf _ = 'T'

valOf :: Char -> Int
valOf 'A' = 0
valOf 'C' = 1
valOf 'G' = 2
valOf _   = 3

-- | Cifra um texto em DNA usando a senha (cada byte vira 4 bases).
encodeDNA :: Text -> Text -> Text
encodeDNA key text = T.pack (concatMap encByte ciphered)
  where
    plain    = magic : BS.unpack (encodeUtf8 text)
    ciphered = zipWith xor plain (keystream key)
    encByte w =
      let n = fromIntegral w :: Int
      in [ baseOf ((n `shiftR` 6) .&. 3)
         , baseOf ((n `shiftR` 4) .&. 3)
         , baseOf ((n `shiftR` 2) .&. 3)
         , baseOf (n .&. 3) ]

-- | Decifra uma sequência de DNA com a senha. Senha errada => 'Left'.
decodeDNA :: Text -> Text -> Either Text Text
decodeDNA key input
  | not (null bad) =
      Left ("Caracteres não-ACGT encontrados: " <> T.pack (take 5 bad))
  | length bases `mod` 4 /= 0 =
      Left "Comprimento inválido: o número de bases precisa ser múltiplo de 4."
  | otherwise =
      case zipWith xor (toBytes bases) (keystream key) of
        (m : rest) | m == magic ->
          case decodeUtf8' (BS.pack rest) of
            Right txt -> Right txt
            Left _    -> Left "Senha incorreta ou sequência corrompida."
        _ -> Left "Senha incorreta ou sequência corrompida."
  where
    cleaned = T.toUpper (T.filter (not . (`elem` (" \n\r\t" :: String))) input)
    bases   = T.unpack cleaned
    bad     = filter (`notElem` ("ACGT" :: String)) bases
    toBytes :: String -> [Word8]
    toBytes (a : b : c : d : rest) =
      fromIntegral ( (valOf a `shiftL` 6)
                 .|. (valOf b `shiftL` 4)
                 .|. (valOf c `shiftL` 2)
                 .|.  valOf d ) : toBytes rest
    toBytes _ = []
