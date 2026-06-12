{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Camada web (Scotty). Rotas, sessões por cookie e o fluxo do
-- caderno reprodutível (analisar → registrar → permalink → fork).
module PaxLab.Web
  ( runServer
  ) where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (object, (.=))
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import           Data.Time                (getCurrentTime)
import           Database.Persist         hiding (get)
import qualified Database.Persist         as P (delete, get)
import           Database.Persist.Sql     (ConnectionPool, fromSqlKey, toSqlKey)
import           Lucid                    (Html, renderText)
import           Network.HTTP.Types.Status (status400, status404, status500)
import           Text.Read                (readMaybe)
import           Web.Scotty

import           PaxLab.Align             (Alignment (..), Scoring (..),
                                          needlemanWunsch)
import           PaxLab.Analysis
import           PaxLab.Auth
import           PaxLab.Bio               (invalidChars, detectKind, normalize, toFasta)
import           PaxLab.Bio.Fasta         (FastaRecord (..), parseFasta)
import           PaxLab.Db                (runDb)
import           PaxLab.Fetch             (fetchNcbiFasta)
import           PaxLab.Model
import           PaxLab.Stego             (decodeDNA, encodeDNA)
import           PaxLab.Views

-- | @runServer porta cookieSeguro pool@. 'cookieSeguro' liga a flag Secure no
-- cookie de sessão (use em produção, sob HTTPS).
runServer :: Int -> Bool -> ConnectionPool -> IO ()
runServer port secure pool = scotty port (routes secure pool)

routes :: Bool -> ConnectionPool -> ScottyM ()
routes secure pool = do
  -- Landing só para deslogados; logado vai direto ao Início.
  get "/" $ do
    mu <- currentUser pool
    case mu of
      Just _  -> redirect "/home"
      Nothing -> render Nothing (landingPage (pure ()))

  get "/register" $ redirectIfAuth pool $
    render Nothing (authPage "Criar conta" "/register" Nothing)
  get "/login" $ redirectIfAuth pool $
    render Nothing (authPage "Entrar" "/login" Nothing)

  post "/register" $ do
    name  <- formParam "name" :: ActionM Text
    email <- formParam "email" :: ActionM Text
    pw    <- formParam "password" :: ActionM Text
    existing <- liftIO $ runDb pool $ getBy (UniqueEmail email)
    case existing of
      Just _ -> render Nothing (authPage "Criar conta" "/register"
                                  (Just "Esse e-mail já está cadastrado."))
      Nothing -> do
        h   <- liftIO (hashPw pw)
        now <- liftIO getCurrentTime
        uid <- liftIO $ runDb pool $ insert (User email name h now)
        startSession secure pool uid
        redirect "/home"

  post "/login" $ do
    email <- formParam "email" :: ActionM Text
    pw    <- formParam "password" :: ActionM Text
    mu <- liftIO $ runDb pool $ getBy (UniqueEmail email)
    case mu of
      Just (Entity uid u) | verifyPw pw (userPasswordHash u) -> do
        startSession secure pool uid
        redirect "/home"
      _ -> render Nothing (authPage "Entrar" "/login"
                             (Just "E-mail ou senha inválidos."))

  post "/logout" $ do
    mc <- header "Cookie"
    case mc >>= lookupCookie "session" . TL.toStrict of
      Just tok -> liftIO $ runDb pool $ deleteBy (UniqueSessionToken tok)
      Nothing  -> pure ()
    setHeader "Set-Cookie" "session=; Path=/; Max-Age=0"
    redirect "/"

  -- Dashboard pós-login.
  get "/home" $ requireAuth pool $ \u -> do
    base    <- baseUrl
    nSeqs   <- liftIO $ runDb pool $ count [BioSequenceOwnerId ==. entityKey u]
    nAs     <- liftIO $ runDb pool $ count [AnalysisOwnerId ==. entityKey u]
    nAls    <- liftIO $ runDb pool $ count [AlignmentRecordOwnerId ==. entityKey u]
    recents <- liftIO $ runDb pool $
      selectList [AnalysisOwnerId ==. entityKey u] [Desc AnalysisCreatedAt, LimitTo 5]
    render (Just u) (dashboardPage base u nSeqs nAs nAls recents)

  -- Coleção de favoritos (análises + alinhamentos), acessível só pelo Início.
  get "/favorites" $ requireAuth pool $ \u -> do
    base  <- baseUrl
    favA  <- liftIO $ runDb pool $
      selectList [AnalysisOwnerId ==. entityKey u, AnalysisFavorite ==. True]
                 [Desc AnalysisCreatedAt]
    favAl <- liftIO $ runDb pool $
      selectList [AlignmentRecordOwnerId ==. entityKey u, AlignmentRecordFavorite ==. True]
                 [Desc AlignmentRecordCreatedAt]
    render (Just u) (favoritesPage base favA favAl)

  -- Alinhamento global de duas sequências.
  get "/align" $ requireAuth pool $ \u -> do
    base <- baseUrl
    qps  <- queryParams
    let pageSize = 10
        page = maybe 1 (max 1) (lookup "p" qps >>= readMaybe . TL.unpack)
    seqs    <- liftIO $ runDb pool $
      selectList [BioSequenceOwnerId ==. entityKey u] [Desc BioSequenceCreatedAt]
    total   <- liftIO $ runDb pool $ count [AlignmentRecordOwnerId ==. entityKey u]
    recents <- liftIO $ runDb pool $
      selectList [AlignmentRecordOwnerId ==. entityKey u]
                 [Desc AlignmentRecordCreatedAt, LimitTo pageSize,
                  OffsetBy ((page - 1) * pageSize)]
    let totalPages = max 1 ((total + pageSize - 1) `div` pageSize)
    render (Just u) (alignPage base seqs recents page totalPages)

  post "/align" $ requireAuth pool $ \u -> do
    ia <- formParam "seqa" :: ActionM Int
    ib <- formParam "seqb" :: ActionM Int
    sc <- Scoring <$> (formParam "match" :: ActionM Int)
                  <*> (formParam "mismatch" :: ActionM Int)
                  <*> (formParam "gap" :: ActionM Int)
    let ka = toSqlKey (fromIntegral ia) :: BioSequenceId
        kb = toSqlKey (fromIntegral ib) :: BioSequenceId
    ma <- liftIO $ runDb pool $ P.get ka
    mb <- liftIO $ runDb pool $ P.get kb
    case (ma, mb) of
      (Just sa, Just sb)
        | bioSequenceOwnerId sa == entityKey u
        , bioSequenceOwnerId sb == entityKey u -> do
            let aln = needlemanWunsch sc
                        (capLen (bioSequenceResidues sa))
                        (capLen (bioSequenceResidues sb))
            tok <- liftIO genToken
            now <- liftIO getCurrentTime
            _ <- liftIO $ runDb pool $ insert
                   (AlignmentRecord (entityKey u) ka kb
                      (bioSequenceName sa) (bioSequenceName sb)
                      (alignScore aln) (alignIdentity aln)
                      (alignedA aln) (alignedB aln)
                      (matchScore sc) (mismatchScore sc) (gapScore sc)
                      tok now False)
            redirect (TL.fromStrict ("/al/" <> tok))
      _ -> redirect "/align"

  -- Importação de sequências reais do NCBI.
  get "/import" $ requireAuth pool $ \u ->
    render (Just u) (importPage Nothing)

  post "/import" $ requireAuth pool $ \u -> do
    acc    <- formParam "accession" :: ActionM Text
    result <- liftIO (fetchNcbiFasta acc)
    case result of
      Left err -> render (Just u) (importPage (Just err))
      Right fasta -> case parseFasta fasta of
        Right (r : _) -> do
          now <- liftIO getCurrentTime
          _ <- liftIO $ runDb pool $ insert
                 (BioSequence (entityKey u) (faHeader r)
                              (detectKind (faResidues r)) (faResidues r) now)
          redirect "/sequences"
        _ -> render (Just u) (importPage (Just "O FASTA do NCBI não pôde ser lido."))

  -- Biocifra: cifrar / decifrar mensagens em DNA com senha.
  get "/biocifra" $ requireAuth pool $ \u ->
    render (Just u) (biocifraPage Nothing Nothing)

  post "/biocifra/encode" $ requireAuth pool $ \u -> do
    txt <- formParam "text" :: ActionM Text
    key <- formParam "key" :: ActionM Text
    render (Just u) (biocifraPage (Just (encodeDNA key txt)) Nothing)

  post "/biocifra/decode" $ requireAuth pool $ \u -> do
    dna <- formParam "dna" :: ActionM Text
    key <- formParam "key" :: ActionM Text
    render (Just u) (biocifraPage Nothing (Just (decodeDNA key dna)))

  -- Compêndio: referência estática de conceitos.
  get "/compendio" $ requireAuth pool $ \u ->
    render (Just u) compendiumPage

  get "/sequences" $ requireAuth pool $ \u -> do
    qps <- queryParams
    let q = maybe "" TL.toStrict (lookup "q" qps)
        needle = T.toLower (T.strip q)
    todas <- liftIO $ runDb pool $
      selectList [BioSequenceOwnerId ==. entityKey u] [Desc BioSequenceCreatedAt]
    let seqs = if T.null needle
                 then todas
                 else filter (T.isInfixOf needle . T.toLower . bioSequenceName . entityVal) todas
    render (Just u) (sequencesPage q seqs)

  post "/sequences" $ requireAuth pool $ \u -> do
    name <- formParam "name" :: ActionM Text
    dat  <- formParam "data" :: ActionM Text
    case extractResidues dat of
      Left err -> do
        seqs <- liftIO $ runDb pool $
          selectList [BioSequenceOwnerId ==. entityKey u] [Desc BioSequenceCreatedAt]
        render (Just u) (flashError err >> sequencesPage "" seqs)
      Right residues -> do
        now <- liftIO getCurrentTime
        _ <- liftIO $ runDb pool $
          insert (BioSequence (entityKey u) name (detectKind residues) residues now)
        redirect "/sequences"

  get "/sequences/:sid" $ requireAuth pool $ \u -> do
    base <- baseUrl
    i    <- captureParam "sid" :: ActionM Int
    qps  <- queryParams
    let pageSize = 5
        page = maybe 1 (max 1) (lookup "p" qps >>= readMaybe . TL.unpack)
        key  = toSqlKey (fromIntegral i) :: BioSequenceId
    ms <- liftIO $ runDb pool $ P.get key
    case ms of
      Just s | bioSequenceOwnerId s == entityKey u -> do
        total <- liftIO $ runDb pool $ count [AnalysisSequenceId ==. key]
        as <- liftIO $ runDb pool $
          selectList [AnalysisSequenceId ==. key]
            [Desc AnalysisCreatedAt, LimitTo pageSize, OffsetBy ((page - 1) * pageSize)]
        let totalPages = max 1 ((total + pageSize - 1) `div` pageSize)
        render (Just u) (sequenceDetail base (Entity key s) as page totalPages)
      _ -> do
        status status404
        render (Just u) (flashError "Sequência não encontrada.")

  -- Roda a análise e devolve só o fragmento (htmx insere no histórico).
  post "/sequences/:sid/analyze" $ requireAuth pool $ \u -> do
    base   <- baseUrl
    i      <- captureParam "sid" :: ActionM Int
    opName <- formParam "op" :: ActionM Text
    -- O formulário sempre envia minlen (input com valor padrão 30).
    mMin   <- Just <$> (formParam "minlen" :: ActionM Int)
    let key = toSqlKey (fromIntegral i) :: BioSequenceId
    ms <- liftIO $ runDb pool $ P.get key
    case (ms, parseOp opName mMin) of
      (Just s, Just op) | bioSequenceOwnerId s == entityKey u -> do
        let res = runOperation op (bioSequenceResidues s)
        tok <- liftIO genToken
        now <- liftIO getCurrentTime
        let a = Analysis (entityKey u) key (encodeOp op)
                         (arText res) (arSummary res) Nothing tok now False
        aid <- liftIO $ runDb pool $ insert a
        html (renderText (analysisCard base (Entity aid a)))
      _ -> do
        status status400
        html "<p class='flash'>Operação inválida.</p>"

  -- Permalink reproduzível: re-roda a receita e compara com o registrado.
  get "/a/:tok" $ do
    tok  <- captureParam "tok" :: ActionM Text
    mu   <- currentUser pool
    base <- baseUrl
    ma   <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
    case ma of
      Nothing -> do
        status status404
        render mu (flashError "Link não encontrado.")
      Just ea@(Entity _ a) -> do
        ms <- liftIO $ runDb pool $ P.get (analysisSequenceId a)
        case (ms, decodeOp (analysisOperation a)) of
          (Just s, Just op) -> do
            let reproduced = runOperation op (bioSequenceResidues s)
                matches    = arText reproduced == analysisResultText a
            render mu (permalinkPage base ea reproduced matches)
          _ -> do
            status status500
            render mu (flashError "Não foi possível reproduzir esta análise.")

  -- Permalink de alinhamento: re-roda Needleman–Wunsch das sequências atuais.
  get "/al/:tok" $ do
    tok  <- captureParam "tok" :: ActionM Text
    mu   <- currentUser pool
    base <- baseUrl
    ma   <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
    case ma of
      Nothing -> do
        status status404
        render mu (flashError "Alinhamento não encontrado.")
      Just er@(Entity _ r) -> do
        msa <- liftIO $ runDb pool $ P.get (alignmentRecordSeqAId r)
        msb <- liftIO $ runDb pool $ P.get (alignmentRecordSeqBId r)
        case (msa, msb) of
          (Just sa, Just sb) -> do
            let sc  = Scoring (alignmentRecordMatchS r)
                              (alignmentRecordMismatchS r)
                              (alignmentRecordGapS r)
                aln = needlemanWunsch sc
                        (capLen (bioSequenceResidues sa))
                        (capLen (bioSequenceResidues sb))
            render mu (alignPermalinkPage base er aln)
          _ -> do
            status status500
            render mu (flashError "Uma das sequências deste alinhamento não existe mais.")

  post "/a/:tok/fork" $ requireAuth pool $ \u -> do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
    case ma of
      Just (Entity aid a) -> do
        newTok <- liftIO genToken
        now    <- liftIO getCurrentTime
        let copy = a { analysisOwnerId    = entityKey u
                     , analysisParentId   = Just aid
                     , analysisShareToken = newTok
                     , analysisCreatedAt  = now
                     , analysisFavorite   = False }
        _ <- liftIO $ runDb pool $ insert copy
        redirect (TL.fromStrict ("/a/" <> newTok))
      Nothing -> do
        status status404
        render (Just u) (flashError "Link não encontrado.")

  -- Favoritar / desfavoritar uma análise (devolve o card atualizado p/ htmx).
  post "/a/:tok/favorite" $ requireAuth pool $ \u -> do
    base <- baseUrl
    tok  <- captureParam "tok" :: ActionM Text
    ma   <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
    case ma of
      Just (Entity aid a) | analysisOwnerId a == entityKey u -> do
        let newFav = not (analysisFavorite a)
        liftIO $ runDb pool $ update aid [AnalysisFavorite =. newFav]
        html (renderText (analysisCard base (Entity aid a { analysisFavorite = newFav })))
      _ -> do
        status status404
        html "<p class='flash'>Análise não encontrada.</p>"

  -- Excluir uma análise individual (htmx remove o card; senão redireciona).
  post "/a/:tok/delete" $ requireAuth pool $ \u -> do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
    case ma of
      Just (Entity aid a) | analysisOwnerId a == entityKey u -> do
        liftIO $ runDb pool $ do
          updateWhere [AnalysisParentId ==. Just aid] [AnalysisParentId =. Nothing]
          P.delete aid
        isHx <- header "HX-Request"
        maybe (redirect "/home") (const (html "")) isHx
      _ -> do
        status status404
        html "<p class='flash'>Análise não encontrada.</p>"

  -- Favoritar / desfavoritar um alinhamento.
  post "/al/:tok/favorite" $ requireAuth pool $ \u -> do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
    case ma of
      Just (Entity rid r) | alignmentRecordOwnerId r == entityKey u -> do
        liftIO $ runDb pool $
          update rid [AlignmentRecordFavorite =. not (alignmentRecordFavorite r)]
        redirectBack (TL.fromStrict ("/al/" <> tok))
      _ -> do
        status status404
        render (Just u) (flashError "Alinhamento não encontrado.")

  -- Excluir um alinhamento.
  post "/al/:tok/delete" $ requireAuth pool $ \u -> do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
    case ma of
      Just (Entity rid r) | alignmentRecordOwnerId r == entityKey u -> do
        liftIO $ runDb pool $ P.delete rid
        redirect "/align"
      _ -> do
        status status404
        render (Just u) (flashError "Alinhamento não encontrado.")

  -- Exportar a trilha reprodutível como JSON citável.
  get "/a/:tok/export.json" $ do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueShareToken tok)
    case ma of
      Nothing -> status status404 >> json (object ["erro" .= ("não encontrada" :: Text)])
      Just (Entity _ a) -> do
        setHeader "Content-Disposition" "attachment; filename=\"analise.json\""
        json $ object
          [ "tipo"       .= ("analise" :: Text)
          , "permalink"  .= ("/a/" <> analysisShareToken a)
          , "operacao"   .= analysisOperation a
          , "resultado"  .= analysisResultText a
          , "explicacao" .= analysisResultSummary a
          , "criadaEm"   .= analysisCreatedAt a ]

  get "/al/:tok/export.json" $ do
    tok <- captureParam "tok" :: ActionM Text
    ma  <- liftIO $ runDb pool $ getBy (UniqueAlignToken tok)
    case ma of
      Nothing -> status status404 >> json (object ["erro" .= ("não encontrado" :: Text)])
      Just (Entity _ r) -> do
        setHeader "Content-Disposition" "attachment; filename=\"alinhamento.json\""
        json $ object
          [ "tipo"       .= ("alinhamento" :: Text)
          , "permalink"  .= ("/al/" <> alignmentRecordShareToken r)
          , "sequenciaA" .= alignmentRecordSeqAName r
          , "sequenciaB" .= alignmentRecordSeqBName r
          , "score"      .= alignmentRecordScore r
          , "identidade" .= alignmentRecordIdentity r
          , "pontuacao"  .= object [ "match"    .= alignmentRecordMatchS r
                                   , "mismatch" .= alignmentRecordMismatchS r
                                   , "gap"      .= alignmentRecordGapS r ]
          , "alinhadoA"  .= alignmentRecordRegisteredA r
          , "alinhadoB"  .= alignmentRecordRegisteredB r
          , "criadoEm"   .= alignmentRecordCreatedAt r ]

  -- Renomear / reeditar uma sequência.
  post "/sequences/:sid/edit" $ requireAuth pool $ \u -> do
    base <- baseUrl
    i    <- captureParam "sid" :: ActionM Int
    name <- formParam "name" :: ActionM Text
    dat  <- formParam "data" :: ActionM Text
    let key = toSqlKey (fromIntegral i) :: BioSequenceId
    ms <- liftIO $ runDb pool $ P.get key
    case ms of
      Just s | bioSequenceOwnerId s == entityKey u ->
        case extractResidues dat of
          Right residues -> do
            liftIO $ runDb pool $ update key
              [ BioSequenceName     =. name
              , BioSequenceResidues =. residues
              , BioSequenceKind     =. detectKind residues ]
            redirect (TL.fromStrict ("/sequences/" <> T.pack (show i)))
          Left err -> do
            total <- liftIO $ runDb pool $ count [AnalysisSequenceId ==. key]
            as <- liftIO $ runDb pool $
              selectList [AnalysisSequenceId ==. key] [Desc AnalysisCreatedAt, LimitTo 5]
            let tp = max 1 ((total + 4) `div` 5)
            render (Just u) (flashError err >> sequenceDetail base (Entity key s) as 1 tp)
      _ -> do
        status status404
        render (Just u) (flashError "Sequência não encontrada.")

  -- Excluir uma sequência (e o que dependia dela).
  post "/sequences/:sid/delete" $ requireAuth pool $ \u -> do
    i <- captureParam "sid" :: ActionM Int
    let key = toSqlKey (fromIntegral i) :: BioSequenceId
    ms <- liftIO $ runDb pool $ P.get key
    case ms of
      Just s | bioSequenceOwnerId s == entityKey u -> do
        liftIO $ runDb pool $ do
          deleteWhere [AnalysisSequenceId ==. key]
          deleteWhere [AlignmentRecordSeqAId ==. key]
          deleteWhere [AlignmentRecordSeqBId ==. key]
          P.delete key
        redirect "/sequences"
      _ -> do
        status status404
        render (Just u) (flashError "Sequência não encontrada.")

  -- Baixar a sequência em formato FASTA.
  get "/sequences/:sid/fasta" $ requireAuth pool $ \u -> do
    i <- captureParam "sid" :: ActionM Int
    let key = toSqlKey (fromIntegral i) :: BioSequenceId
    ms <- liftIO $ runDb pool $ P.get key
    case ms of
      Just s | bioSequenceOwnerId s == entityKey u -> do
        let fname = T.map (\c -> if c == ' ' then '_' else c)
                          (T.filter (/= '"') (bioSequenceName s))
        setHeader "Content-Type" "text/plain; charset=utf-8"
        setHeader "Content-Disposition"
          (TL.fromStrict ("attachment; filename=\"" <> fname <> ".fasta\""))
        text (TL.fromStrict (toFasta (bioSequenceName s) (bioSequenceResidues s)))
      _ -> do
        status status404
        text "Sequência não encontrada."

-- Helpers -------------------------------------------------------------------

render :: Maybe (Entity User) -> Html () -> ActionM ()
render mu content = do
  -- Sem cache: garante que "voltar" rebusque a página (listas sempre frescas,
  -- ex.: o alinhamento recém-criado já aparece ao voltar).
  setHeader "Cache-Control" "no-store"
  html (renderText (layout mu content))

requireAuth :: ConnectionPool -> (Entity User -> ActionM ()) -> ActionM ()
requireAuth pool act = do
  mu <- currentUser pool
  maybe (redirect "/login") act mu

-- | Para telas de login/registro: se já está logado, manda para o Início.
redirectIfAuth :: ConnectionPool -> ActionM () -> ActionM ()
redirectIfAuth pool act = do
  mu <- currentUser pool
  maybe act (const (redirect "/home")) mu

-- | URL base (esquema + host) para montar permalinks absolutos copiáveis.
baseUrl :: ActionM Text
baseUrl = do
  mh <- header "Host"
  pure ("http://" <> maybe "localhost:3000" TL.toStrict mh)

-- | Volta para a página de origem (Referer), com um destino padrão.
redirectBack :: TL.Text -> ActionM ()
redirectBack def = header "Referer" >>= redirect . maybe def id

currentUser :: ConnectionPool -> ActionM (Maybe (Entity User))
currentUser pool = do
  mc <- header "Cookie"
  case mc >>= lookupCookie "session" . TL.toStrict of
    Nothing  -> pure Nothing
    Just tok -> liftIO $ runDb pool $ do
      mSess <- getBy (UniqueSessionToken tok)
      case mSess of
        Nothing               -> pure Nothing
        Just (Entity _ sess)  -> do
          mu <- P.get (sessionUserId sess)
          pure (Entity (sessionUserId sess) <$> mu)

startSession :: Bool -> ConnectionPool -> UserId -> ActionM ()
startSession secure pool uid = do
  tok <- liftIO genToken
  now <- liftIO getCurrentTime
  _   <- liftIO $ runDb pool $ insert (Session tok uid now)
  let secureFlag = if secure then "; Secure" else ""
  setHeader "Set-Cookie"
    (TL.fromStrict ("session=" <> tok <> "; Path=/; HttpOnly; SameSite=Lax" <> secureFlag))

lookupCookie :: Text -> Text -> Maybe Text
lookupCookie key raw =
  lookup key
    [ (T.takeWhile (/= '=') p, T.drop 1 (T.dropWhile (/= '=') p))
    | p <- map T.strip (T.splitOn ";" raw) ]

-- | Limita o tamanho da sequência para o alinhamento (NW é O(n·m)).
capLen :: Text -> Text
capLen = T.take 1000

-- | Extrai os resíduos de um envio: aceita FASTA (começa com '>') ou texto cru.
extractResidues :: Text -> Either Text Text
extractResidues dat
  | T.isPrefixOf ">" trimmed =
      case parseFasta trimmed of
        Right (r : _) -> Right (faResidues r)
        Right []      -> Left "Nenhuma sequência encontrada no FASTA."
        Left err      -> Left err
  | T.null trimmed = Left "Cole uma sequência."
  | otherwise =
      let r   = normalize trimmed
          bad = invalidChars r
      in if null bad
           then Right r
           else Left ("Caracteres inválidos: " <> T.pack (take 5 bad)
                      <> ". Use só nucleotídeos ou aminoácidos.")
  where trimmed = T.strip dat
