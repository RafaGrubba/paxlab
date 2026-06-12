{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Esquema do banco via persistent. Note o desenho do caderno reprodutível:
-- cada 'Analysis' guarda a RECEITA ('operation', em JSON) e a lineage
-- ('parentId') — é isso que permite replay, fork e permalink.
module PaxLab.Model where

import           Data.Text          (Text)
import           Data.Time          (UTCTime)
import           Database.Persist.TH
import           PaxLab.Bio         (SeqKind (..))

-- Faz 'SeqKind' ser armazenável (serializado como texto via Show/Read).
derivePersistField "SeqKind"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email        Text
    name         Text
    passwordHash Text
    createdAt    UTCTime
    UniqueEmail  email
    deriving Show

BioSequence
    ownerId   UserId
    name      Text
    kind      SeqKind
    residues  Text
    createdAt UTCTime
    deriving Show

Analysis
    ownerId       UserId
    sequenceId    BioSequenceId
    operation     Text            -- receita serializada (JSON) -> reprodutível
    resultText    Text            -- resultado registrado na hora
    resultSummary Text
    parentId      AnalysisId Maybe -- lineage: de qual análise esta foi forkada
    shareToken    Text            -- chave do permalink público
    createdAt     UTCTime
    favorite      Bool            -- marcada como favorita pelo usuário
    UniqueShareToken shareToken
    deriving Show

-- Alinhamento registrado: guarda os ids das duas sequências (a "receita"),
-- então o permalink pode re-rodar Needleman–Wunsch e provar reprodutibilidade.
AlignmentRecord
    ownerId    UserId
    seqAId     BioSequenceId
    seqBId     BioSequenceId
    seqAName   Text
    seqBName   Text
    score      Int
    identity   Double
    registeredA Text          -- alinhamento registrado (com gaps)
    registeredB Text
    matchS      Int           -- pontuação usada (parte da "receita")
    mismatchS   Int
    gapS        Int
    shareToken Text
    createdAt  UTCTime
    favorite   Bool
    UniqueAlignToken shareToken
    deriving Show

Session
    token     Text
    userId    UserId
    createdAt UTCTime
    UniqueSessionToken token
    deriving Show
|]
