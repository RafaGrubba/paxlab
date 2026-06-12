{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | API REST (Servant) do PaxLab. JSON em @/api/*@ + front-end estático.
-- Autenticação por token Bearer (tabela Session). Os handlers reaproveitam
-- toda a lógica pura e os modelos do app.
module PaxLab.Api
  ( runApi
  ) where

import           Control.Monad.Except         (throwError)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                  (FromJSON (..), Value, encode,
                                              object, toJSON, withObject, (.!=),
                                              (.:), (.:?), (.=))
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import           Database.Persist
import           Database.Persist.Sql        (ConnectionPool, fromSqlKey,
                                              toSqlKey)
import           Network.HTTP.Types.Status   (status500)
import           Network.Wai                 (responseLBS)
import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setOnExceptionResponse, setPort)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant
import           Servant.Server.StaticFiles  (serveDirectoryFileServer)

import           PaxLab.Align                (Alignment (..), Scoring (..),
                                              needlemanWunsch)
import           PaxLab.Analysis             (arSummary, arText, decodeOp,
                                              encodeOp, operationLabel, parseOp,
                                              runOperation)
import           PaxLab.Auth                 (genToken, hashPw, verifyPw)
import           PaxLab.Bio                  (ORF (..), SeqKind (..), detectKind,
                                              findORFs, invalidChars, normalize)
import           PaxLab.Bio.Fasta            (FastaRecord (..), parseFasta)
import           PaxLab.Db                   (runDb)
import           PaxLab.Fetch                (fetchNcbiFasta)
import           PaxLab.Model
import           PaxLab.Protein              (ProteinStats (..), proteinStats)
import           PaxLab.Restriction          (Enzyme (..), restrictionSites)
import           PaxLab.Stego                (decodeDNA, encodeDNA)

-- Corpos de requisição -------------------------------------------------------
data RegisterReq = RegisterReq Text Text Text
data LoginReq    = LoginReq Text Text
data SeqReq      = SeqReq Text Text
data AnalyzeReq  = AnalyzeReq Text (Maybe Int)
data AlignReq    = AlignReq Int64 Int64 Int Int Int
data ImportReq   = ImportReq Text
data EncodeReq   = EncodeReq Text Text
data DecodeReq   = DecodeReq Text Text

instance FromJSON RegisterReq where
  parseJSON = withObject "RegisterReq" $ \o ->
    RegisterReq <$> o .: "email" <*> o .: "name" <*> o .: "password"
instance FromJSON LoginReq where
  parseJSON = withObject "LoginReq" $ \o ->
    LoginReq <$> o .: "email" <*> o .: "password"
instance FromJSON SeqReq where
  parseJSON = withObject "SeqReq" $ \o -> SeqReq <$> o .: "name" <*> o .: "data"
instance FromJSON AnalyzeReq where
  parseJSON = withObject "AnalyzeReq" $ \o ->
    AnalyzeReq <$> o .: "op" <*> o .:? "minlen"
instance FromJSON AlignReq where
  parseJSON = withObject "AlignReq" $ \o ->
    AlignReq <$> o .: "seqA" <*> o .: "seqB"
             <*> (o .:? "match" .!= 1) <*> (o .:? "mismatch" .!= (-1))
             <*> (o .:? "gap" .!= (-2))
instance FromJSON ImportReq where
  parseJSON = withObject "ImportReq" $ \o -> ImportReq <$> o .: "accession"
instance FromJSON EncodeReq where
  parseJSON = withObject "EncodeReq" $ \o -> EncodeReq <$> o .: "text" <*> o .: "key"
instance FromJSON DecodeReq where
  parseJSON = withObject "DecodeReq" $ \o -> DecodeReq <$> o .: "dna" <*> o .: "key"

-- API ------------------------------------------------------------------------
type AuthH = Header "Authorization" Text
type Cap   = Capture "id" Int64
type CapT  = Capture "token" Text

type API =
       "api" :> "health"   :> Get '[JSON] Value
  :<|> "api" :> "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] Value
  :<|> "api" :> "login"    :> ReqBody '[JSON] LoginReq    :> Post '[JSON] Value
  -- Sequências (CRUD)
  :<|> "api" :> "sequences" :> AuthH :> QueryParam "q" Text :> Get '[JSON] Value
  :<|> "api" :> "sequences" :> AuthH :> ReqBody '[JSON] SeqReq :> Post '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> AuthH :> Get '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> AuthH :> ReqBody '[JSON] SeqReq :> Put '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> AuthH :> Delete '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> "analyze"     :> AuthH :> ReqBody '[JSON] AnalyzeReq :> Post '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> "analyses"    :> AuthH :> Get '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> "orfs"        :> AuthH :> Get '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> "restriction" :> AuthH :> Get '[JSON] Value
  :<|> "api" :> "sequences" :> Cap :> "protein"     :> AuthH :> Get '[JSON] Value
  -- Análises
  :<|> "api" :> "analyses" :> CapT :> Get '[JSON] Value
  :<|> "api" :> "analyses" :> CapT :> "favorite" :> AuthH :> Post '[JSON] Value
  :<|> "api" :> "analyses" :> CapT :> "fork"     :> AuthH :> Post '[JSON] Value
  :<|> "api" :> "analyses" :> CapT :> AuthH :> Delete '[JSON] Value
  -- Alinhamentos
  :<|> "api" :> "align"      :> AuthH :> ReqBody '[JSON] AlignReq :> Post '[JSON] Value
  :<|> "api" :> "alignments" :> AuthH :> Get '[JSON] Value
  :<|> "api" :> "alignments" :> CapT :> Get '[JSON] Value
  :<|> "api" :> "alignments" :> CapT :> "favorite" :> AuthH :> Post '[JSON] Value
  :<|> "api" :> "alignments" :> CapT :> AuthH :> Delete '[JSON] Value
  -- Importação NCBI e Biocifra
  :<|> "api" :> "import"          :> AuthH :> ReqBody '[JSON] ImportReq :> Post '[JSON] Value
  :<|> "api" :> "biocifra" :> "encode" :> ReqBody '[JSON] EncodeReq :> Post '[JSON] Value
  :<|> "api" :> "biocifra" :> "decode" :> ReqBody '[JSON] DecodeReq :> Post '[JSON] Value
  :<|> "api" :> "favorites" :> AuthH :> Get '[JSON] Value
  :<|> Raw

api :: Proxy API
api = Proxy

server :: ConnectionPool -> Server API
server pool =
       health
  :<|> register :<|> login
  :<|> listSeqs :<|> createSeq :<|> getSeq :<|> updateSeq :<|> deleteSeq
  :<|> analyze :<|> historySeq :<|> orfsSeq :<|> restrictionSeq :<|> proteinSeq
  :<|> permalink :<|> favAnalysis :<|> forkAnalysis :<|> delAnalysis
  :<|> createAlign :<|> listAligns :<|> alignPermalink :<|> favAlign :<|> delAlign
  :<|> importNcbi :<|> cipherEncode :<|> cipherDecode
  :<|> favoritesH
  :<|> serveDirectoryFileServer "frontend"
  where
    health = pure (object ["status" .= ("ok" :: Text)])

    register (RegisterReq email name pw) = do
      existing <- liftIO $ runDb pool $ getBy (UniqueEmail email)
      case existing of
        Just _  -> throwError (jsonErr err409 "E-mail já cadastrado.")
        Nothing -> do
          h   <- liftIO (hashPw pw)
          now <- liftIO getCurrentTime
          uid <- liftIO $ runDb pool $ insert (User email name h now)
          tok <- newSession uid
          pure (object ["token" .= tok, "name" .= name])

    login (LoginReq email pw) = do
      mu <- liftIO $ runDb pool $ getBy (UniqueEmail email)
      case mu of
        Just (Entity uid u) | verifyPw pw (userPasswordHash u) -> do
          tok <- newSession uid
          pure (object ["token" .= tok, "name" .= userName u])
        _ -> throwError (jsonErr err401 "E-mail ou senha inválidos.")

    listSeqs mauth mq = do
      u <- requireUser pool mauth
      let needle = maybe "" (T.toLower . T.strip) mq
      todas <- liftIO $ runDb pool $
        selectList [BioSequenceOwnerId ==. entityKey u] [Desc BioSequenceCreatedAt]
      let seqs = if T.null needle then todas
                 else filter (T.isInfixOf needle . T.toLower
                                . bioSequenceName . entityVal) todas
      pure (toJSON (map seqJSON seqs))

    createSeq mauth (SeqReq name dat) = do
      u <- requireUser pool mauth
      residues <- either (throwError . jsonErr err400) pure (extractResidues dat)
      now <- liftIO getCurrentTime
      let row = BioSequence (entityKey u) name (detectKind residues) residues now
      k <- liftIO $ runDb pool $ insert row
      pure (seqJSON (Entity k row))

    getSeq i mauth = do
      u <- requireUser pool mauth
      Entity k s <- ownedSeq pool u i
      pure (seqJSON (Entity k s))

    updateSeq i mauth (SeqReq name dat) = do
      u <- requireUser pool mauth
      Entity k _ <- ownedSeq pool u i
      residues <- either (throwError . jsonErr err400) pure (extractResidues dat)
      liftIO $ runDb pool $ update k
        [ BioSequenceName =. name, BioSequenceResidues =. residues
        , BioSequenceKind =. detectKind residues ]
      ms <- liftIO $ runDb pool $ get k
      maybe (throwError (jsonErr err404 "Sequência não encontrada."))
            (\s -> pure (seqJSON (Entity k s))) ms

    deleteSeq i mauth = do
      u <- requireUser pool mauth
      Entity k _ <- ownedSeq pool u i
      liftIO $ runDb pool $ do
        deleteWhere [AnalysisSequenceId ==. k]
        deleteWhere [AlignmentRecordSeqAId ==. k]
        deleteWhere [AlignmentRecordSeqBId ==. k]
        delete k
      pure (object ["deleted" .= True])

    analyze i mauth (AnalyzeReq opName mMin) = do
      u <- requireUser pool mauth
      Entity k s <- ownedSeq pool u i
      case parseOp opName mMin of
        Nothing -> throwError (jsonErr err400 "Operação inválida.")
        Just op -> do
          let res = runOperation op (bioSequenceResidues s)
          tok <- liftIO genToken
          now <- liftIO getCurrentTime
          let row = Analysis (entityKey u) k (encodeOp op)
                             (arText res) (arSummary res) Nothing tok now False
          ak <- liftIO $ runDb pool $ insert row
          pure (analysisJSON (Entity ak row))

    historySeq i mauth = do
      u <- requireUser pool mauth
      Entity k _ <- ownedSeq pool u i
      as <- liftIO $ runDb pool $
        selectList [AnalysisSequenceId ==. k] [Desc AnalysisCreatedAt]
      pure (toJSON (map analysisJSON as))

    orfsSeq i mauth = do
      u <- requireUser pool mauth
      Entity _ s <- ownedSeq pool u i
      let res  = bioSequenceResidues s
          orfs = if bioSequenceKind s == Protein then [] else findORFs 15 res
      pure (object ["length" .= T.length res, "orfs" .= map orfJSON orfs])

    restrictionSeq i mauth = do
      u <- requireUser pool mauth
      Entity _ s <- ownedSeq pool u i
      pure (toJSON
        [ object ["name" .= enzName e, "site" .= enzSite e, "positions" .= ps]
        | (e, ps) <- restrictionSites (bioSequenceResidues s) ])

    proteinSeq i mauth = do
      u <- requireUser pool mauth
      Entity _ s <- ownedSeq pool u i
      let ps = proteinStats (bioSequenceResidues s)
      pure (object
        [ "length" .= psLength ps, "weight" .= psWeightDa ps
        , "composition" .=
            [ object ["aa" .= T.singleton c, "count" .= n]
            | (c, n) <- psComposition ps ] ])

    permalink tok = do
      ma <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
      case ma of
        Nothing -> throwError (jsonErr err404 "Análise não encontrada.")
        Just ea@(Entity _ a) -> do
          ms <- liftIO $ runDb pool $ get (analysisSequenceId a)
          case (ms, decodeOp (analysisOperation a)) of
            (Just s, Just op) -> do
              let rep     = runOperation op (bioSequenceResidues s)
                  matches = arText rep == analysisResultText a
              pure (object
                [ "analysis" .= analysisJSON ea
                , "sequenceName" .= bioSequenceName s
                , "reproduced" .= object ["text" .= arText rep, "summary" .= arSummary rep]
                , "matches" .= matches ])
            _ -> throwError (jsonErr err500 "Não foi possível reproduzir.")

    favAnalysis tok mauth = do
      u <- requireUser pool mauth
      ma <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
      case ma of
        Just (Entity aid a) | analysisOwnerId a == entityKey u -> do
          let nf = not (analysisFavorite a)
          liftIO $ runDb pool $ update aid [AnalysisFavorite =. nf]
          pure (analysisJSON (Entity aid a { analysisFavorite = nf }))
        _ -> throwError (jsonErr err404 "Análise não encontrada.")

    forkAnalysis tok mauth = do
      u <- requireUser pool mauth
      ma <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
      case ma of
        Just (Entity aid a) -> do
          newTok <- liftIO genToken
          now    <- liftIO getCurrentTime
          let copy = a { analysisOwnerId = entityKey u, analysisParentId = Just aid
                       , analysisShareToken = newTok, analysisCreatedAt = now
                       , analysisFavorite = False }
          k <- liftIO $ runDb pool $ insert copy
          pure (analysisJSON (Entity k copy))
        Nothing -> throwError (jsonErr err404 "Análise não encontrada.")

    delAnalysis tok mauth = do
      u <- requireUser pool mauth
      ma <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
      case ma of
        Just (Entity aid a) | analysisOwnerId a == entityKey u -> do
          liftIO $ runDb pool $ do
            updateWhere [AnalysisParentId ==. Just aid] [AnalysisParentId =. Nothing]
            delete aid
          pure (object ["deleted" .= True])
        _ -> throwError (jsonErr err404 "Análise não encontrada.")

    createAlign mauth (AlignReq ia ib mt mm gp) = do
      u <- requireUser pool mauth
      let ka = toSqlKey ia :: BioSequenceId
          kb = toSqlKey ib :: BioSequenceId
      msa <- liftIO $ runDb pool $ get ka
      msb <- liftIO $ runDb pool $ get kb
      case (msa, msb) of
        (Just sa, Just sb)
          | bioSequenceOwnerId sa == entityKey u
          , bioSequenceOwnerId sb == entityKey u -> do
              let sc  = Scoring mt mm gp
                  aln = needlemanWunsch sc (capLen (bioSequenceResidues sa))
                                           (capLen (bioSequenceResidues sb))
              tok <- liftIO genToken
              now <- liftIO getCurrentTime
              let row = AlignmentRecord (entityKey u) ka kb
                          (bioSequenceName sa) (bioSequenceName sb)
                          (alignScore aln) (alignIdentity aln)
                          (alignedA aln) (alignedB aln) mt mm gp tok now False
              k <- liftIO $ runDb pool $ insert row
              pure (object
                [ "record" .= alignmentJSON (Entity k row)
                , "alignedA" .= alignedA aln, "alignedB" .= alignedB aln ])
        _ -> throwError (jsonErr err400 "Sequências inválidas.")

    listAligns mauth = do
      u <- requireUser pool mauth
      rs <- liftIO $ runDb pool $
        selectList [AlignmentRecordOwnerId ==. entityKey u] [Desc AlignmentRecordCreatedAt]
      pure (toJSON (map alignmentJSON rs))

    alignPermalink tok = do
      ma <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
      case ma of
        Nothing -> throwError (jsonErr err404 "Alinhamento não encontrado.")
        Just er@(Entity _ r) -> do
          msa <- liftIO $ runDb pool $ get (alignmentRecordSeqAId r)
          msb <- liftIO $ runDb pool $ get (alignmentRecordSeqBId r)
          case (msa, msb) of
            (Just sa, Just sb) -> do
              let sc  = Scoring (alignmentRecordMatchS r) (alignmentRecordMismatchS r)
                                (alignmentRecordGapS r)
                  aln = needlemanWunsch sc (capLen (bioSequenceResidues sa))
                                           (capLen (bioSequenceResidues sb))
              pure (object
                [ "record" .= alignmentJSON er
                , "alignedA" .= alignedA aln, "alignedB" .= alignedB aln ])
            _ -> throwError (jsonErr err500 "Sequência do alinhamento não existe mais.")

    favAlign tok mauth = do
      u <- requireUser pool mauth
      ma <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
      case ma of
        Just (Entity rid r) | alignmentRecordOwnerId r == entityKey u -> do
          let nf = not (alignmentRecordFavorite r)
          liftIO $ runDb pool $ update rid [AlignmentRecordFavorite =. nf]
          pure (alignmentJSON (Entity rid r { alignmentRecordFavorite = nf }))
        _ -> throwError (jsonErr err404 "Alinhamento não encontrado.")

    delAlign tok mauth = do
      u <- requireUser pool mauth
      ma <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
      case ma of
        Just (Entity rid r) | alignmentRecordOwnerId r == entityKey u -> do
          liftIO $ runDb pool $ delete rid
          pure (object ["deleted" .= True])
        _ -> throwError (jsonErr err404 "Alinhamento não encontrado.")

    importNcbi mauth (ImportReq acc) = do
      u <- requireUser pool mauth
      result <- liftIO (fetchNcbiFasta acc)
      case result of
        Left e -> throwError (jsonErr err400 e)
        Right fasta -> case parseFasta fasta of
          Right (r : _) -> do
            now <- liftIO getCurrentTime
            let row = BioSequence (entityKey u) (faHeader r)
                        (detectKind (faResidues r)) (faResidues r) now
            k <- liftIO $ runDb pool $ insert row
            pure (seqJSON (Entity k row))
          _ -> throwError (jsonErr err400 "O FASTA do NCBI não pôde ser lido.")

    cipherEncode (EncodeReq text key) =
      pure (object ["dna" .= encodeDNA key text])

    cipherDecode (DecodeReq dna key) =
      case decodeDNA key dna of
        Right t -> pure (object ["text" .= t])
        Left e  -> throwError (jsonErr err400 e)

    favoritesH mauth = do
      u <- requireUser pool mauth
      as <- liftIO $ runDb pool $
        selectList [AnalysisOwnerId ==. entityKey u, AnalysisFavorite ==. True]
                   [Desc AnalysisCreatedAt]
      rs <- liftIO $ runDb pool $
        selectList [AlignmentRecordOwnerId ==. entityKey u, AlignmentRecordFavorite ==. True]
                   [Desc AlignmentRecordCreatedAt]
      pure (object ["analyses" .= map analysisJSON as, "alignments" .= map alignmentJSON rs])

    newSession uid = do
      tok <- liftIO genToken
      now <- liftIO getCurrentTime
      _   <- liftIO $ runDb pool $ insert (Session tok uid now)
      pure tok

-- Helpers --------------------------------------------------------------------

requireUser :: ConnectionPool -> Maybe Text -> Handler (Entity User)
requireUser pool mauth =
  case mauth >>= T.stripPrefix "Bearer " of
    Nothing  -> throwError (jsonErr err401 "Token ausente.")
    Just tok -> do
      mu <- liftIO $ runDb pool $ do
        ms <- getBy (UniqueSessionToken tok)
        case ms of
          Nothing              -> pure Nothing
          Just (Entity _ sess) -> fmap (Entity (sessionUserId sess))
                                    <$> get (sessionUserId sess)
      maybe (throwError (jsonErr err401 "Token inválido.")) pure mu

ownedSeq :: ConnectionPool -> Entity User -> Int64 -> Handler (Entity BioSequence)
ownedSeq pool u i = do
  let k = toSqlKey i :: BioSequenceId
  ms <- liftIO $ runDb pool $ get k
  case ms of
    Just s | bioSequenceOwnerId s == entityKey u -> pure (Entity k s)
    _ -> throwError (jsonErr err404 "Sequência não encontrada.")

capLen :: Text -> Text
capLen = T.take 1000

seqJSON :: Entity BioSequence -> Value
seqJSON (Entity k s) = object
  [ "id" .= fromSqlKey k, "name" .= bioSequenceName s
  , "kind" .= show (bioSequenceKind s), "residues" .= bioSequenceResidues s
  , "length" .= T.length (bioSequenceResidues s), "createdAt" .= bioSequenceCreatedAt s ]

analysisJSON :: Entity Analysis -> Value
analysisJSON (Entity k a) = object
  [ "id" .= fromSqlKey k
  , "operation" .= maybe (analysisOperation a) operationLabel (decodeOp (analysisOperation a))
  , "resultText" .= analysisResultText a, "summary" .= analysisResultSummary a
  , "token" .= analysisShareToken a, "favorite" .= analysisFavorite a
  , "createdAt" .= analysisCreatedAt a ]

alignmentJSON :: Entity AlignmentRecord -> Value
alignmentJSON (Entity k r) = object
  [ "id" .= fromSqlKey k, "token" .= alignmentRecordShareToken r
  , "seqAName" .= alignmentRecordSeqAName r, "seqBName" .= alignmentRecordSeqBName r
  , "score" .= alignmentRecordScore r, "identity" .= alignmentRecordIdentity r
  , "match" .= alignmentRecordMatchS r, "mismatch" .= alignmentRecordMismatchS r
  , "gap" .= alignmentRecordGapS r, "favorite" .= alignmentRecordFavorite r
  , "createdAt" .= alignmentRecordCreatedAt r ]

orfJSON :: ORF -> Value
orfJSON o = object
  [ "frame" .= orfFrame o, "start" .= orfStart o
  , "end" .= orfEnd o, "protein" .= orfProtein o ]

jsonErr :: ServerError -> Text -> ServerError
jsonErr e msg = e
  { errBody    = encode (object ["error" .= msg])
  , errHeaders = [("Content-Type", "application/json")] }

extractResidues :: Text -> Either Text Text
extractResidues dat
  | T.isPrefixOf ">" trimmed =
      case parseFasta trimmed of
        Right (r : _)
          | T.null (faResidues r) -> Left "A sequência está vazia."
          | otherwise             -> Right (faResidues r)
        Right []      -> Left "Nenhuma sequência encontrada no FASTA."
        Left err      -> Left err
  | T.null trimmed = Left "Cole uma sequência."
  | otherwise =
      let r = normalize trimmed; bad = invalidChars r
      in if null bad then Right r
         else Left ("Caracteres inválidos: " <> T.pack (take 5 bad))
  where trimmed = T.strip dat

runApi :: Int -> ConnectionPool -> IO ()
runApi port pool = runSettings settings (simpleCors (serve api (server pool)))
  where
    settings = setPort port (setOnExceptionResponse onErr defaultSettings)
    -- Exceções não tratadas viram um 500 com corpo JSON (não texto cru).
    onErr _ = responseLBS status500
                [("Content-Type", "application/json")]
                "{\"error\":\"Erro interno no servidor.\"}"
