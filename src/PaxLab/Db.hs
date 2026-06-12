{-# LANGUAGE CPP              #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

-- | Pool de conexões e helpers de banco. O backend é escolhido em tempo de
-- compilação: SQLite por padrão (dev) ou PostgreSQL com o flag @postgres@
-- (produção). A string de conexão vem do ambiente (ver "Main").
module PaxLab.Db
  ( mkPool
  , runMigrations
  , runDb
  ) where

import           Control.Monad.Logger       (NoLoggingT, runNoLoggingT)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Text                  (Text)
import           Database.Persist.Sql       (ConnectionPool, SqlBackend,
                                             runMigration, runSqlPool)

#ifdef POSTGRES
import           Data.Text.Encoding         (encodeUtf8)
import           Database.Persist.Postgresql (createPostgresqlPool)
#else
import           Database.Persist.Sqlite    (createSqlitePool)
#endif

import           PaxLab.Model               (migrateAll)

-- | Cria o pool a partir da string de conexão (caminho SQLite ou URL Postgres).
mkPool :: Text -> IO ConnectionPool
#ifdef POSTGRES
mkPool conn = runNoLoggingT (createPostgresqlPool (encodeUtf8 conn) 10)
#else
mkPool path = runNoLoggingT (createSqlitePool path 5)
#endif

-- | Cria/atualiza as tabelas a partir do esquema em 'PaxLab.Model'.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool = runDb pool (runMigration migrateAll)

-- | Roda uma ação de banco usando o pool.
runDb :: ConnectionPool -> ReaderT SqlBackend (NoLoggingT IO) a -> IO a
runDb pool action = runNoLoggingT (runSqlPool action pool)
