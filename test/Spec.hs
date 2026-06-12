{-# LANGUAGE OverloadedStrings #-}

-- | Testes do núcleo puro. Como tudo aqui é função pura, testar é trivial
-- e — o mais importante para o projeto — É A MESMA garantia que torna o
-- caderno reprodutível confiável.
module Main (main) where

import           Control.Monad (unless)
import qualified Data.Map.Strict as M
import           System.Exit   (exitFailure)
import qualified Data.Text     as T

import           PaxLab.Align
import           PaxLab.Analysis
import           PaxLab.Bio
import           PaxLab.Bio.Fasta
import           PaxLab.Protein
import           PaxLab.Restriction
import           PaxLab.Stego

main :: IO ()
main = do
  results <- mapM run tests
  unless (and results) exitFailure
  putStrLn ("OK — " <> show (length tests) <> " testes passaram.")
  where
    run (name, ok) = do
      putStrLn ((if ok then "  ✓ " else "  ✗ ") <> name)
      pure ok

tests :: [(String, Bool)]
tests =
  [ ("transcribe troca T por U",
        transcribe "ATGC" == "AUGC")
  , ("reverseComplement de ATGC",
        reverseComplement "ATGC" == "GCAT")
  , ("translate ATG = M (Metionina)",
        translate "ATG" == "M")
  , ("translate reconhece stop como *",
        translate "TAA" == "*")
  , ("gcContent de GGCC = 100%",
        gcContent "GGCC" == 100)
  , ("gcContent de ATAT = 0%",
        gcContent "ATAT" == 0)
  , ("detectKind reconhece RNA pelo U",
        detectKind "AUGC" == RNA)
  , ("findORFs acha ATG...TAA",
        not (null (findORFs 1 "ATGAAATAA")))
  , ("parseFasta lê um registro",
        case parseFasta ">s1\nATGC" of
          Right [r] -> faResidues r == "ATGC"
          _         -> False)
  , ("parseFasta rejeita caractere inválido",
        case parseFasta ">s1\nATQZ!" of
          Left _ -> True
          _      -> False)
  , ("REPRODUTIBILIDADE: runOperation é determinístico",
        let r = "ATGGGCTAA"
        in runOperation Translate r == runOperation Translate r)
  , ("receita sobrevive a encode/decode",
        decodeOp (encodeOp (FindORFs 30)) == Just (FindORFs 30))
  , ("alinhamento de sequências idênticas dá 100% de identidade",
        alignIdentity (needlemanWunsch defaultScoring "ACGTACGT" "ACGTACGT") == 100)
  , ("alinhamento insere gap quando falta uma base",
        let aln = needlemanWunsch defaultScoring "ACGT" "AGT"
        in T.length (alignedA aln) == T.length (alignedB aln))
  , ("Biocifra: cifrar/decifrar com a mesma senha é ida-e-volta",
        decodeDNA "senha123" (encodeDNA "senha123" "PaxLab 🧬") == Right "PaxLab 🧬")
  , ("Biocifra: senha errada não decifra",
        case decodeDNA "errada" (encodeDNA "certa" "segredo") of
          Left _ -> True; _ -> False)
  , ("Biocifra: decifrar rejeita comprimento inválido",
        case decodeDNA "k" "ACG" of Left _ -> True; _ -> False)
  , ("toFasta monta cabeçalho e sequência",
        toFasta "seq1" "ATGC" == ">seq1\nATGC\n")
  , ("orfCoverage marca posições de uma ORF",
        not (M.null (orfCoverage (findORFs 1 "ATGAAATAA"))))
  , ("restrictionSites encontra EcoRI (GAATTC)",
        case restrictionSites "AAGAATTCAA" of
          ((e, ps) : _) -> enzName e == "EcoRI" && ps == [2]
          _             -> False)
  , ("molecularWeight de proteína vazia é 0",
        molecularWeight "" == 0)
  , ("molecularWeight cresce com mais resíduos",
        molecularWeight "AA" > molecularWeight "A")
  ]
