module Main (main) where

import           Data.Char          (toLower)
import qualified Data.Text          as T
import           System.Environment (lookupEnv)
import           Text.Read          (readMaybe)

import           PaxLab.Db          (mkPool, runMigrations)
import           PaxLab.Seed        (seedIfEmpty)
import           PaxLab.Web         (runServer)

main :: IO ()
main = do
  -- Configuração por ambiente (com padrões para rodar localmente sem nada).
  port   <- maybe 3000 id . (>>= readMaybe) <$> lookupEnv "PORT"
  -- Em produção (Postgres) usa DATABASE_URL; senão, o caminho do SQLite.
  murl   <- lookupEnv "DATABASE_URL"
  dbConn <- case murl of
              Just url -> pure url
              Nothing  -> maybe "paxlab.sqlite3" id <$> lookupEnv "PAXLAB_DB"
  secure <- maybe False truthy <$> lookupEnv "PAXLAB_SECURE_COOKIES"

  pool <- mkPool (T.pack dbConn)
  runMigrations pool
  seedIfEmpty pool

  putStrLn ("PaxLab rodando em http://localhost:" <> show port
            <> "  (cookies seguros: " <> show secure <> ")")
  runServer port secure pool

-- | Interpreta valores como "1", "true", "yes", "on" (qualquer caixa) como verdadeiro.
truthy :: String -> Bool
truthy s = map toLower s `elem` ["1", "true", "yes", "on"]
