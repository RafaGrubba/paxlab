{-# LANGUAGE OverloadedStrings #-}

-- | Dados de exemplo. O "modo experimente sem trazer seus dados": se o
-- banco estiver vazio, cria um usuário demo e sequências reais para o
-- avaliador clicar e ver o site funcionando na hora.
module PaxLab.Seed
  ( seedIfEmpty
  ) where

import           Control.Monad        (unless, void)
import           Data.Text            (Text)
import           Data.Time            (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql (ConnectionPool)

import           PaxLab.Auth          (hashPw)
import           PaxLab.Bio           (SeqKind (..))
import           PaxLab.Db            (runDb)
import           PaxLab.Model

seedIfEmpty :: ConnectionPool -> IO ()
seedIfEmpty pool = do
  n <- runDb pool (count ([] :: [Filter User]))
  unless (n > 0) $ do
    now <- getCurrentTime
    h   <- hashPw "paxlab123"
    runDb pool $ do
      uid <- insert (User "demo@paxlab.bio" "Pesquisador Demo" h now)
      void $ insert (BioSequence uid "Insulina humana (INS, trecho)" DNA insulin now)
      void $ insert (BioSequence uid "Promotor rico em GC (exemplo)" DNA gcRich now)
      void $ insert (BioSequence uid "mRNA curto de exemplo" RNA shortRna now)
    putStrLn "Seed: usuário demo@paxlab.bio / senha paxlab123 criado."

-- Trecho do gene da insulina (códon de início + sequência ilustrativa).
insulin :: Text
insulin = "ATGGCCCTGTGGATGCGCCTCCTGCCCCTGCTGGCGCTGCTGGCCCTCTGGGGACCTGACCCAGCCGCAGCCTTTGTGAACCAACACCTGTGCGGCTCACACCTGGTGGAAGCTCTCTACCTAGTGTGCGGGGAACGAGGCTTCTTCTACACACCCAAGACCCGCCGGGAGGCAGAGGACCTGCAGTGA"

gcRich :: Text
gcRich = "GCGCGCGGCGGCGCGCGCATGCCGGCCGGCGGCGCGCCGCCGGCGCGCGCGCGGGCCGGCGCGCGCGCTGA"

shortRna :: Text
shortRna = "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
