{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | O coração do diferencial do PaxLab: o "caderno reprodutível".
--
-- A ideia central:  uma 'Operation' é uma RECEITA serializável, e
-- 'runOperation' é uma função PURA. Guardamos a receita no banco; para
-- reproduzir uma análise qualquer no futuro, basta rodar a mesma função
-- com a mesma sequência. Reprodutibilidade não é um recurso que
-- "adicionamos" — é consequência de Haskell ser funcional.
module PaxLab.Analysis
  ( Operation(..)
  , AnalysisResult(..)
  , runOperation
  , parseOp
  , encodeOp
  , decodeOp
  , operationLabel
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           GHC.Generics         (Generic)
import           Text.Printf          (printf)

import           PaxLab.Bio
import           PaxLab.Protein       (molecularWeight)

-- | A receita de uma análise. Serializável (JSON) e portanto persistível.
data Operation
  = Transcribe
  | Translate
  | ReverseComplement
  | GCContent
  | FindORFs Int        -- ^ tamanho mínimo da proteína
  deriving (Show, Eq, Generic)

instance ToJSON   Operation
instance FromJSON Operation

-- | Resultado: o dado bruto + uma explicação em linguagem acessível
-- (o "modo educacional" do PaxLab).
data AnalysisResult = AnalysisResult
  { arText    :: Text   -- ^ resultado bruto
  , arSummary :: Text   -- ^ explicação para humanos
  } deriving (Show, Eq, Generic)

instance ToJSON   AnalysisResult
instance FromJSON AnalysisResult

operationLabel :: Operation -> Text
operationLabel op = case op of
  Transcribe        -> "Transcrição (DNA → RNA)"
  Translate         -> "Tradução (→ proteína)"
  ReverseComplement -> "Fita complementar reversa"
  GCContent         -> "Conteúdo GC"
  FindORFs n        -> "Buscar ORFs (mín. " <> T.pack (show n) <> " aa)"

-- | A função pura que faz tudo acontecer. Determinística por construção.
runOperation :: Operation -> Text -> AnalysisResult
runOperation op residues = case op of
  Transcribe ->
    AnalysisResult (transcribe r)
      "Transcrição DNA → RNA: cada timina (T) foi trocada por uracila (U), \
      \como faz a RNA-polimerase ao ler o gene."

  Translate ->
    let prot = translate r
    in AnalysisResult prot $ T.concat
         [ "Tradução pela tabela do código genético padrão: cada trinca de "
         , "bases (códon) virou um aminoácido. '*' marca um códon de parada. "
         , "Proteína resultante: ", T.pack (show (T.length prot)), " resíduos"
         , ", massa molecular ~", T.pack (printf "%.0f" (molecularWeight prot)), " Da." ]

  ReverseComplement ->
    AnalysisResult (reverseComplement r)
      "Fita complementar reversa: é a fita que pareia com a sua sequência \
      \(A-T, C-G), lida no sentido oposto. É o que a célula usa como molde."

  GCContent ->
    let p = gcContent r
    in AnalysisResult (T.pack (printf "%.1f%%" p)) $ T.pack $ printf
         "Conteúdo GC = %.1f%%. Pares G-C têm três pontes de hidrogênio \
         \(contra duas de A-T), então regiões ricas em GC são mais estáveis \
         \termicamente." p

  FindORFs minLen ->
    let orfs = findORFs minLen r
        body = if null orfs
                 then "Nenhuma ORF encontrada acima do tamanho mínimo."
                 else T.intercalate "\n" (map showORF orfs)
    in AnalysisResult body $ T.concat
         [ "Foram varridos os 3 frames diretos em busca de janelas de leitura "
         , "(de um ATG até um códon de parada). Encontradas: "
         , T.pack (show (length orfs)), " ORF(s)." ]
  where
    r = normalize residues

-- | Constrói uma 'Operation' a partir dos parâmetros do formulário web.
parseOp :: Text -> Maybe Int -> Maybe Operation
parseOp name mMinLen = case T.toLower name of
  "transcribe"        -> Just Transcribe
  "translate"         -> Just Translate
  "reversecomplement" -> Just ReverseComplement
  "gccontent"         -> Just GCContent
  "findorfs"          -> Just (FindORFs (maybe 30 id mMinLen))
  _                   -> Nothing

-- | Serializa a receita para guardar no banco (é isto que torna a
-- análise reproduzível depois).
encodeOp :: Operation -> Text
encodeOp = decodeUtf8 . BL.toStrict . encode

decodeOp :: Text -> Maybe Operation
decodeOp = decode . BL.fromStrict . encodeUtf8
