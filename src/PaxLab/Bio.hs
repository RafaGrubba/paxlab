{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Núcleo de bioinformática: funções PURAS, sem efeitos colaterais.
--
-- Esta é a parte que justifica usar Haskell. Cada função aqui é
-- determinística: mesma entrada => mesma saída, sempre. É exatamente
-- essa propriedade que torna o "caderno reprodutível" do PaxLab possível
-- de graça (ver 'PaxLab.Analysis').
module PaxLab.Bio
  ( SeqKind(..)
  , ORF(..)
  , normalize
  , detectKind
  , invalidChars
  , complementBase
  , reverseComplement
  , transcribe
  , translate
  , gcContent
  , findORFs
  , orfCoverage
  , showORF
  , toFasta
  , codonTable
  ) where

import           Data.Char       (isSpace)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import           GHC.Generics    (Generic)

-- | Tipo da sequência. Modelar isso no sistema de tipos é o exemplo
-- clássico de por que Haskell combina com biologia molecular.
data SeqKind = DNA | RNA | Protein
  deriving (Show, Read, Eq, Generic)

-- | Uma janela de leitura aberta (Open Reading Frame): de um ATG até um stop.
data ORF = ORF
  { orfFrame   :: Int   -- ^ 1, 2 ou 3 (frame de leitura)
  , orfStart   :: Int   -- ^ posição inicial (0-based, em nucleotídeos)
  , orfEnd     :: Int   -- ^ posição final (exclusiva)
  , orfProtein :: Text  -- ^ proteína traduzida
  } deriving (Show, Eq)

-- | Maiúsculas, sem espaços/quebras de linha.
normalize :: Text -> Text
normalize = T.toUpper . T.filter (not . isSpace)

-- | Caracteres que não pertencem a nenhum alfabeto biológico conhecido.
-- Usado para dar erros amigáveis em vez de "parse error" genérico.
invalidChars :: Text -> [Char]
invalidChars t =
  filter (`notElem` valid) (T.unpack (normalize t))
  where valid = "ACGTURYKMSWBDHVN*" ++ ['A'..'Z']  -- aminoácidos cobrem A-Z

-- | Adivinha o tipo da sequência pelo alfabeto presente.
detectKind :: Text -> SeqKind
detectKind t
  | T.any (== 'U') s                       = RNA
  | T.all (`elem` ("ACGTN" :: String)) s   = DNA
  | otherwise                              = Protein
  where s = normalize t

-- | Complemento de uma base de DNA (A<->T, C<->G).
complementBase :: Char -> Char
complementBase c = case c of
  'A' -> 'T'; 'T' -> 'A'
  'C' -> 'G'; 'G' -> 'C'
  'U' -> 'A'
  _   -> c

-- | Fita complementar reversa — o que de fato pareia com a fita original.
reverseComplement :: Text -> Text
reverseComplement = T.reverse . T.map complementBase . normalize

-- | Transcrição DNA -> RNA: cada T vira U.
transcribe :: Text -> Text
transcribe = T.map (\c -> if c == 'T' then 'U' else c) . normalize

-- | Conteúdo GC em porcentagem (0–100). Bases G e C são mais estáveis
-- termicamente, então este número diz algo sobre a sequência.
gcContent :: Text -> Double
gcContent t
  | total == 0 = 0
  | otherwise  = 100 * fromIntegral gc / fromIntegral total
  where
    s     = normalize t
    total = T.length (T.filter (`elem` ("ACGTU" :: String)) s)
    gc    = T.length (T.filter (`elem` ("GC" :: String)) s)

-- | Tradução pela tabela do código genético padrão. Stops viram '*'.
-- Aceita tanto T (DNA) quanto U (RNA).
translate :: Text -> Text
translate = T.pack . go . map dnaize . T.unpack . normalize
  where
    dnaize 'U' = 'T'
    dnaize c   = c
    go (a:b:c:rest) = M.findWithDefault 'X' [a,b,c] codonTable : go rest
    go _            = []

-- | Encontra ORFs nos três frames diretos com proteína de tamanho >= minLen.
findORFs :: Int -> Text -> [ORF]
findORFs minLen t = concatMap orfsInFrame [0, 1, 2]
  where
    s = T.unpack (map' (\c -> if c == 'U' then 'T' else c) (normalize t))
    map' f = T.map f
    orfsInFrame f = collect f (chunk3 (drop f s)) 0 Nothing []

    collect _ []       _ _      acc = reverse acc
    collect f (c:cs)   i mStart acc = case mStart of
      Nothing
        | c == "ATG" -> collect f cs (i + 1) (Just i) acc
        | otherwise  -> collect f cs (i + 1) Nothing  acc
      Just st
        | isStop c ->
            let startNt = f + st * 3
                lenNt   = (i - st) * 3
                prot    = translate (T.pack (take lenNt (drop startNt s)))
                orf     = ORF (f + 1) startNt (f + (i + 1) * 3) prot
            in if T.length prot >= minLen
                 then collect f cs (i + 1) Nothing (orf : acc)
                 else collect f cs (i + 1) Nothing acc
        | otherwise -> collect f cs (i + 1) mStart acc

    isStop x = x `elem` ["TAA", "TAG", "TGA"]

chunk3 :: [a] -> [[a]]
chunk3 xs = case splitAt 3 xs of
  (c, rest) | length c == 3 -> c : chunk3 rest
            | otherwise     -> []

-- | Para cada posição (nucleotídeo) coberta por alguma ORF, em qual frame
-- ela está (1, 2 ou 3). Útil para destacar as ORFs na sequência.
orfCoverage :: [ORF] -> M.Map Int Int
orfCoverage orfs = M.fromList
  [ (p, orfFrame o) | o <- orfs, p <- [orfStart o .. orfEnd o - 1] ]

-- | Empacota uma sequência no formato FASTA (cabeçalho + linhas de 60 bases).
toFasta :: Text -> Text -> Text
toFasta name residues =
  T.unlines (">" <> name : T.chunksOf 60 (normalize residues))

-- | Renderização curta de uma ORF para exibição.
showORF :: ORF -> Text
showORF o = T.concat
  [ "Frame +", T.pack (show (orfFrame o))
  , " | ", T.pack (show (orfStart o)), "–", T.pack (show (orfEnd o)), " nt"
  , " | ", T.pack (show (T.length (orfProtein o))), " aa"
  , " | ", T.take 30 (orfProtein o), if T.length (orfProtein o) > 30 then "…" else ""
  ]

-- | Código genético padrão (tabela 1). Códon de DNA (com T) -> aminoácido.
codonTable :: M.Map String Char
codonTable = M.fromList
  [ ("TTT",'F'),("TTC",'F'),("TTA",'L'),("TTG",'L')
  , ("CTT",'L'),("CTC",'L'),("CTA",'L'),("CTG",'L')
  , ("ATT",'I'),("ATC",'I'),("ATA",'I'),("ATG",'M')
  , ("GTT",'V'),("GTC",'V'),("GTA",'V'),("GTG",'V')
  , ("TCT",'S'),("TCC",'S'),("TCA",'S'),("TCG",'S')
  , ("CCT",'P'),("CCC",'P'),("CCA",'P'),("CCG",'P')
  , ("ACT",'T'),("ACC",'T'),("ACA",'T'),("ACG",'T')
  , ("GCT",'A'),("GCC",'A'),("GCA",'A'),("GCG",'A')
  , ("TAT",'Y'),("TAC",'Y'),("TAA",'*'),("TAG",'*')
  , ("CAT",'H'),("CAC",'H'),("CAA",'Q'),("CAG",'Q')
  , ("AAT",'N'),("AAC",'N'),("AAA",'K'),("AAG",'K')
  , ("GAT",'D'),("GAC",'D'),("GAA",'E'),("GAG",'E')
  , ("TGT",'C'),("TGC",'C'),("TGA",'*'),("TGG",'W')
  , ("CGT",'R'),("CGC",'R'),("CGA",'R'),("CGG",'R')
  , ("AGT",'S'),("AGC",'S'),("AGA",'R'),("AGG",'R')
  , ("GGT",'G'),("GGC",'G'),("GGA",'G'),("GGG",'G')
  ]
