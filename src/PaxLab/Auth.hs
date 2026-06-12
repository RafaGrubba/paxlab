{-# LANGUAGE OverloadedStrings #-}

-- | Autenticação: hashing de senha com bcrypt e geração de tokens.
--
-- Como o Rafael curte cybersegurança: senhas NUNCA são guardadas em texto
-- puro — só o hash bcrypt (com salt embutido). Para os tokens de sessão,
-- num projeto real troque 'System.Random' por um CSPRNG (ex.: pacote
-- @entropy@); aqui mantemos simples e deixamos a nota.
module PaxLab.Auth
  ( hashPw
  , verifyPw
  , genToken
  ) where

import           Control.Monad        (replicateM)
import           Data.Password.Bcrypt (Bcrypt, PasswordCheck (..), PasswordHash (..),
                                       checkPassword, hashPassword, mkPassword)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           System.Random        (randomRIO)

-- | Gera o hash bcrypt de uma senha em texto.
hashPw :: Text -> IO Text
hashPw raw = do
  h <- hashPassword (mkPassword raw)
  pure (unPasswordHash (h :: PasswordHash Bcrypt))

-- | Confere uma senha contra o hash guardado.
verifyPw :: Text -> Text -> Bool
verifyPw raw stored =
  case checkPassword (mkPassword raw) (PasswordHash stored :: PasswordHash Bcrypt) of
    PasswordCheckSuccess -> True
    PasswordCheckFail    -> False

-- | Token aleatório (sessão / permalink). Ver nota no topo do módulo.
genToken :: IO Text
genToken = T.pack <$> replicateM 32 pick
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
    pick = do
      i <- randomRIO (0, length alphabet - 1)
      pure (alphabet !! i)
