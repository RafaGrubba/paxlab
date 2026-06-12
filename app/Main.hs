module Main (main) where

import qualified Data.Text          as T
import           System.Environment (lookupEnv)
import           System.IO          (hSetEncoding, stderr, stdout, utf8)
import           Text.Read          (readMaybe)

import           PaxLab.Api         (runApi)
import           PaxLab.Db          (mkPool, runMigrations)
import           PaxLab.Seed        (seedIfEmpty)

main :: IO ()
main = do
  -- Força UTF-8 no console (containers sobem com locale C/ASCII).
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  port   <- maybe 3000 id . (>>= readMaybe) <$> lookupEnv "PORT"
  -- Em produção usa DATABASE_URL (Postgres); senão, o caminho do SQLite.
  murl   <- lookupEnv "DATABASE_URL"
  dbConn <- case murl of
              Just url -> pure url
              Nothing  -> maybe "paxlab.sqlite3" id <$> lookupEnv "PAXLAB_DB"

  pool <- mkPool (T.pack dbConn)
  runMigrations pool
  seedIfEmpty pool

  putStrLn ("PaxLab (API) em http://localhost:" <> show port)
  runApi port pool
