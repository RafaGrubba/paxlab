{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Importação de sequências reais a partir do NCBI (E-utilities/efetch).
-- Recebe um número de acesso (ex.: NM_000207, o mRNA da insulina humana) e
-- devolve o FASTA correspondente.
module PaxLab.Fetch
  ( fetchNcbiFasta
  ) where

import           Control.Exception   (SomeException, try)
import           Data.Char           (isAlphaNum)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8')
import           Network.HTTP.Simple (getResponseBody, getResponseStatusCode,
                                      httpBS, parseRequest)

-- | Busca o FASTA de um acesso do NCBI (banco nuccore). 'Left' traz uma
-- mensagem amigável em PT-BR; 'Right' o texto FASTA pronto para o parser.
fetchNcbiFasta :: Text -> IO (Either Text Text)
fetchNcbiFasta acc
  | T.null cleaned =
      pure (Left "Digite um número de acesso do NCBI — ex.: NM_000207.")
  | not (T.all valido cleaned) =
      pure (Left ("\"" <> cleaned <> "\" não parece um número de acesso válido. "
                  <> "Use apenas letras, números, ponto e underscore — ex.: NM_000207."))
  | otherwise = do
      let url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
                <> "?db=nuccore&id=" <> T.unpack cleaned
                <> "&rettype=fasta&retmode=text"
      result <- try (parseRequest url >>= httpBS)
      pure $ case result of
        Left (_ :: SomeException) ->
          Left "Não foi possível conectar ao NCBI. Verifique sua conexão e tente de novo."
        Right resp
          | getResponseStatusCode resp /= 200 -> Left naoEncontrado
          | otherwise ->
              case decodeUtf8' (getResponseBody resp) of
                Left _ -> Left "A resposta do NCBI não pôde ser lida."
                Right txt
                  | T.isPrefixOf ">" (T.strip txt) -> Right (T.strip txt)
                  | otherwise -> Left naoEncontrado
  where
    cleaned   = T.strip acc
    valido c  = isAlphaNum c || c == '.' || c == '_'
    naoEncontrado =
      "Número de acesso \"" <> cleaned <> "\" não encontrado no NCBI. "
      <> "Confira o código e tente outro — exemplos válidos: "
      <> "NM_000207 (insulina humana), NR_046018, NC_012920."
