{-# LANGUAGE OverloadedStrings #-}

-- | Parser de FASTA escrito à mão. Em Haskell isso fica curto e seguro,
-- e os erros são amigáveis (apontam a linha problemática) — um dos
-- diferenciais de acessibilidade do PaxLab.
module PaxLab.Bio.Fasta
  ( FastaRecord(..)
  , parseFasta
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           PaxLab.Bio (invalidChars)

data FastaRecord = FastaRecord
  { faHeader   :: Text  -- ^ texto após o '>' (nome/descrição)
  , faResidues :: Text  -- ^ sequência concatenada, em maiúsculas
  } deriving (Show, Eq)

-- | Tenta parsear texto FASTA em registros. 'Left' traz mensagem em PT-BR.
parseFasta :: Text -> Either Text [FastaRecord]
parseFasta input
  | T.null (T.strip input) = Left "Entrada vazia: cole uma sequência FASTA."
  | not (T.isPrefixOf ">" (T.strip input)) =
      Left "FASTA deve começar com '>' seguido do nome da sequência."
  | otherwise =
      case go (zip [1 ..] (T.lines input)) [] of
        Left err   -> Left err
        Right recs -> validate (reverse recs)
  where
    go [] acc = Right acc
    go ((_, line) : rest) acc
      | T.isPrefixOf ">" line =
          go rest (FastaRecord (T.strip (T.drop 1 line)) "" : acc)
      | T.null (T.strip line) = go rest acc
      | otherwise = case acc of
          (FastaRecord h r : more) ->
            go rest (FastaRecord h (r <> T.toUpper (T.strip line)) : more)
          [] -> Left "Linha de sequência antes de qualquer cabeçalho '>'."

    validate recs =
      case filter (not . T.null . T.pack . invalidChars . faResidues) recs of
        []      -> Right recs
        (bad:_) ->
          let cs = invalidChars (faResidues bad)
          in Left $ T.concat
               [ "Sequência \"", faHeader bad
               , "\" contém caracteres inválidos: "
               , T.pack (take 5 cs)
               , ". Use apenas letras de nucleotídeos ou aminoácidos." ]
