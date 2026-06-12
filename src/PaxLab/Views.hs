{-# LANGUAGE OverloadedStrings #-}

-- | Camada de apresentação com Lucid (HTML tipado em Haskell) + htmx.
-- O site inteiro é renderizado pelo servidor; htmx dá interatividade sem
-- escrever JavaScript.
module PaxLab.Views
  ( layout
  , landingPage
  , authPage
  , dashboardPage
  , sequencesPage
  , sequenceDetail
  , analysisCard
  , permalinkPage
  , alignPage
  , alignPermalinkPage
  , favoritesPage
  , biocifraPage
  , compendiumPage
  , importPage
  , flashError
  ) where

import           Control.Monad       (when)
import           Data.Int            (Int64)
import qualified Data.Map.Strict     as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Database.Persist.Sql (Entity (..), fromSqlKey)
import           Lucid
import           Lucid.Base          (makeAttribute)
import           Text.Printf         (printf)

import           PaxLab.Align        (Alignment (..))
import           PaxLab.Analysis
import           PaxLab.Bio          (ORF, SeqKind (..), findORFs, orfCoverage)
import           PaxLab.Model
import           PaxLab.Protein      (ProteinStats (..), proteinStats)
import           PaxLab.Restriction  (Enzyme (..), restrictionSites)
import           PaxLab.Viz          (orfMapSvg)

-- htmx attributes -----------------------------------------------------------
hx :: Text -> Text -> Attribute
hx k = makeAttribute ("hx-" <> k)

sid :: Entity BioSequence -> Int64
sid = fromSqlKey . entityKey

-- Layout --------------------------------------------------------------------
layout :: Maybe (Entity User) -> Html () -> Html ()
layout mUser content = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "PaxLab — bioinformática reprodutível"
    script_ [src_ "https://unpkg.com/htmx.org@1.9.12"] ("" :: Text)
    style_ css
  body_ $ do
    nav_ $ do
      -- Logado, a marca leva ao Início; deslogado, à landing.
      a_ [href_ (maybe "/" (const "/home") mUser), class_ "brand"] "🧬 PaxLab"
      div_ [class_ "spacer"] ""
      case mUser of
        Just u -> do
          a_ [href_ "/home"] "Início"
          a_ [href_ "/sequences"] "Sequências"
          a_ [href_ "/align"] "Alinhar"
          a_ [href_ "/import"] "Importar"
          a_ [href_ "/compendio"] "Compêndio"
          span_ [class_ "muted"] (toHtml (userName (entityVal u)))
          form_ [action_ "/logout", method_ "post", class_ "inline"] $
            button_ [class_ "linklike"] "Sair"
        Nothing -> do
          a_ [href_ "/login"] "Entrar"
          a_ [href_ "/register", class_ "btn"] "Criar conta"
    main_ content
    footer_ $ small_ "PaxLab · análises puras, resultados reproduzíveis."

-- Landing -------------------------------------------------------------------
landingPage :: Html () -> Html ()
landingPage _ = do
  section_ [class_ "hero"] $ do
    h1_ "Bioinformática que você pode reproduzir."
    p_ [class_ "lead"] $ do
      "Analise sequências de DNA, RNA e proteína — e cada análise vira "
      "um registro permanente e reproduzível. Compartilhe um link; "
      "qualquer pessoa refaz exatamente os mesmos passos."
    div_ [class_ "cta"] $ do
      a_ [href_ "/register", class_ "btn big"] "Começar"
      a_ [href_ "/login", class_ "btn ghost big"] "Entrar"
  section_ [class_ "features"] $ do
    feature "📓" "Caderno reprodutível"
      "Toda análise guarda a receita exata. Replay e permalink inclusos."
    feature "🎓" "Modo educacional"
      "Cada resultado vem explicado em linguagem acessível."
    feature "⚙️" "Núcleo funcional"
      "Funções puras em Haskell: mesma entrada, mesma saída, sempre."
  where
    feature :: Html () -> Html () -> Html () -> Html ()
    feature ico ttl txt = div_ [class_ "feature"] $ do
      div_ [class_ "ico"] ico
      h3_ ttl
      p_ [class_ "muted"] txt

-- Auth ----------------------------------------------------------------------
authPage :: Text -> Text -> Maybe Text -> Html ()
authPage title action mErr = section_ [class_ "card narrow"] $ do
  h2_ (toHtml title)
  maybe (pure ()) flashError mErr
  form_ [action_ action, method_ "post"] $ do
    when (action == "/register") $ do
      label_ "Nome"
      input_ [type_ "text", name_ "name", placeholder_ "Como devemos te chamar?",
              required_ "required", autofocus_]
    label_ "E-mail"
    input_ ([type_ "email", name_ "email", required_ "required"]
            ++ [autofocus_ | action == "/login"])
    label_ "Senha"
    input_ [type_ "password", name_ "password", required_ "required"]
    button_ [class_ "btn", type_ "submit"] (toHtml title)
  if action == "/login"
    then p_ [class_ "muted"] $ do
           "Não tem conta? "; a_ [href_ "/register"] "Criar conta"
    else p_ [class_ "muted"] $ do
           "Já tem conta? "; a_ [href_ "/login"] "Entrar"

-- Lista de sequências -------------------------------------------------------
sequencesPage :: Text -> [Entity BioSequence] -> Html ()
sequencesPage q seqs = do
  section_ [class_ "card"] $ do
    h2_ "Nova sequência"
    p_ [class_ "muted"] "Cole texto FASTA ou resíduos crus (DNA, RNA ou proteína)."
    form_ [action_ "/sequences", method_ "post"] $ do
      label_ "Nome"
      input_ [type_ "text", name_ "name", placeholder_ "Ex.: Insulina humana (INS)", required_ "required"]
      label_ "Sequência / FASTA"
      textarea_ [name_ "data", rows_ "6", placeholder_ ">minha_seq\nATGGCC...", required_ "required"] ""
      button_ [class_ "btn", type_ "submit"] "Salvar sequência"
  section_ [class_ "card"] $ do
    h2_ "Minhas sequências"
    form_ [action_ "/sequences", method_ "get", class_ "searchbar"] $ do
      input_ [type_ "search", name_ "q", value_ q, placeholder_ "Buscar por nome…"]
      button_ [class_ "btn ghost", type_ "submit"] "Buscar"
    if null seqs
      then p_ [class_ "muted"]
             (if T.null (T.strip q)
                then "Nenhuma sequência ainda. Crie a primeira acima."
                else "Nenhuma sequência encontrada para essa busca.")
      else ul_ [class_ "list"] $ mapM_ row seqs
  where
    row :: Entity BioSequence -> Html ()
    row e = li_ $ do
      a_ [href_ ("/sequences/" <> T.pack (show (sid e)))]
         (toHtml (bioSequenceName (entityVal e)))
      span_ [class_ "tag"] (toHtml (show (bioSequenceKind (entityVal e))))
      span_ [class_ "muted"]
        (toHtml (T.pack (show (T.length (bioSequenceResidues (entityVal e)))) <> " resíduos"))

-- Detalhe da sequência + painel de análise ----------------------------------
sequenceDetail :: Text -> Entity BioSequence -> [Entity Analysis] -> Int -> Int -> Html ()
sequenceDetail base e analyses page totalPages = do
  let s      = entityVal e
      res    = bioSequenceResidues s
      isProt = bioSequenceKind s == Protein
      orfs   = if isProt then [] else findORFs 15 res
      self   = "/sequences/" <> T.pack (show (sid e))
  backButton
  section_ [class_ "card"] $ do
    h2_ (toHtml (bioSequenceName s))
    span_ [class_ "tag"] (toHtml (show (bioSequenceKind s)))
    highlightedSeq res orfs
    -- ORFs são conceito de DNA/RNA — o mapa some para proteínas.
    when (not isProt) $ do
      h3_ "Mapa de ORFs"
      if null orfs
        then p_ [class_ "muted"] "Nenhuma ORF (≥15 aa) encontrada nos frames diretos."
        else do
          toHtmlRaw (orfMapSvg (T.length res) orfs)
          p_ [class_ "muted orf-legend"]
             "Acima e na sequência, as cores marcam ORFs por frame (+1, +2, +3)."
    div_ [class_ "card-foot"] $
      a_ [href_ (self <> "/fasta"), class_ "muted dl"] "baixar FASTA"
  if isProt then proteinSection res else restrictionSection res
  section_ [class_ "card"] $ do
    h3_ "Rodar análise"
    form_ [ hx "post" ("/sequences/" <> T.pack (show (sid e)) <> "/analyze")
          , hx "target" "#results", hx "swap" "afterbegin" ] $ do
      -- O campo de tamanho mínimo só aparece para "Buscar ORFs".
      select_ [ name_ "op", onchange_
                  "document.getElementById('minlen-wrap').style.display=\
                  \this.value==='findorfs'?'inline-block':'none'" ] $ do
        option_ [value_ "transcribe"] "Transcrição (DNA → RNA)"
        option_ [value_ "translate"] "Tradução (→ proteína)"
        option_ [value_ "reversecomplement"] "Fita complementar reversa"
        option_ [value_ "gccontent"] "Conteúdo GC"
        option_ [value_ "findorfs"] "Buscar ORFs"
      span_ [id_ "minlen-wrap", style_ "display:none"] $
        input_ [type_ "number", name_ "minlen", value_ "30",
                title_ "tamanho mín. de ORF (aa)"]
      button_ [class_ "btn", type_ "submit"] "Analisar"
    h3_ "Histórico"
    div_ [id_ "results"] $
      if null analyses
        then p_ [class_ "muted"] "Ainda sem análises. Rode a primeira acima."
        else mapM_ (analysisCard base) analyses
    when (totalPages > 1) $ div_ [class_ "pager"] $ do
      if page > 1
        then a_ [href_ (self <> "?p=" <> T.pack (show (page - 1))), class_ "btn ghost"]
                "← anteriores"
        else span_ [] ""
      span_ [class_ "muted"]
        (toHtml ("página " <> T.pack (show page) <> " de " <> T.pack (show totalPages)))
      if page < totalPages
        then a_ [href_ (self <> "?p=" <> T.pack (show (page + 1))), class_ "btn ghost"]
                "próximas →"
        else span_ [] ""
  section_ [class_ "card"] $ do
    details_ $ do
      summary_ "Editar / excluir"
      form_ [action_ ("/sequences/" <> T.pack (show (sid e)) <> "/edit"),
             method_ "post"] $ do
        label_ "Nome"
        input_ [type_ "text", name_ "name", value_ (bioSequenceName s),
                required_ "required"]
        label_ "Sequência / FASTA"
        textarea_ [name_ "data", rows_ "5", required_ "required"] (toHtml res)
        button_ [class_ "btn", type_ "submit"] "Salvar alterações"
      form_ [action_ ("/sequences/" <> T.pack (show (sid e)) <> "/delete"),
             method_ "post", class_ "danger",
             onsubmit_ "return confirm('Excluir esta sequência e suas análises?')"] $
        button_ [class_ "btn danger"] "Excluir sequência"

-- Cartão de uma análise (também é o fragmento devolvido ao htmx) -------------
analysisCard :: Text -> Entity Analysis -> Html ()
analysisCard base e = do
  let a   = entityVal e
      tok = analysisShareToken a
      fav = analysisFavorite a
  div_ [class_ "result"] $ do
    div_ [class_ "result-head"] $ do
      strong_ (toHtml (opLabel (analysisOperation a)))
      details_ [class_ "kebab"] $ do
        summary_ "⋮"
        div_ [class_ "kebab-menu"] $ do
          button_ [ hx "post" ("/a/" <> tok <> "/favorite")
                  , hx "target" "closest .result", hx "swap" "outerHTML"
                  , class_ "kebab-item" ]
                  (if fav then "☆ Desfavoritar" else "⭐ Favoritar")
          button_ [ hx "post" ("/a/" <> tok <> "/delete")
                  , hx "target" "closest .result", hx "swap" "outerHTML"
                  , makeAttribute "hx-confirm" "Excluir esta análise?"
                  , class_ "kebab-item danger" ]
                  "🗑 Excluir"
    pre_ [class_ "out"] (toHtml (analysisResultText a))
    p_ [class_ "muted"] (toHtml (analysisResultSummary a))
    div_ [class_ "result-actions"] $ do
      permalinkBox base ("/a/" <> tok)
      a_ [href_ ("/a/" <> tok <> "/export.json"), class_ "muted dl"] "baixar JSON"
  where
    opLabel raw = maybe raw operationLabel (decodeOp raw)

-- Permalink: prova de reprodutibilidade --------------------------------------
permalinkPage :: Text -> Entity Analysis -> AnalysisResult -> Bool -> Html ()
permalinkPage base e reproduced matches = do
  let a = entityVal e
  backButton
  section_ [class_ "card"] $ do
    h2_ "Análise compartilhada"
    p_ [class_ "muted"] $ do
      "Operação: "; strong_ (toHtml (maybe (analysisOperation a) operationLabel
                                            (decodeOp (analysisOperation a))))
    verdictBox matches
    h3_ "Resultado registrado"
    pre_ [class_ "out"] (toHtml (analysisResultText a))
    h3_ "Resultado reproduzido agora"
    pre_ [class_ "out"] (toHtml (arText reproduced))
    p_ [class_ "muted"] (toHtml (arSummary reproduced))
    permalinkBox base ("/a/" <> analysisShareToken a)
    a_ [href_ ("/a/" <> analysisShareToken a <> "/export.json"), class_ "muted dl"] "baixar JSON"
    div_ [class_ "perma-actions"] $ do
      form_ [action_ ("/a/" <> analysisShareToken a <> "/fork"), method_ "post"] $
        button_ [class_ "btn"] "Forkar para minhas análises"
      form_ [ action_ ("/a/" <> analysisShareToken a <> "/delete"), method_ "post"
            , onsubmit_ "return confirm('Excluir esta análise?')" ] $
        button_ [class_ "btn danger"] "Excluir análise"

-- Dashboard pós-login --------------------------------------------------------
dashboardPage :: Text -> Entity User -> Int -> Int -> Int
              -> [Entity Analysis] -> Html ()
dashboardPage base u nSeqs nAnalyses nAligns recents = do
  section_ [class_ "card"] $ do
    h2_ (toHtml ("Olá, " <> userName (entityVal u)))
    p_ [class_ "muted"] "Seu laboratório de bioinformática reprodutível."
    div_ [class_ "stats"] $ do
      stat (T.pack (show nSeqs)) "sequências"
      stat (T.pack (show nAnalyses)) "análises registradas"
      stat (T.pack (show nAligns)) "alinhamentos"
  section_ [class_ "card"] $ do
    h3_ "Ferramentas"
    div_ [class_ "features"] $ do
      tool "/sequences" "🧬" "Sequências" "Cadastre e analise DNA/RNA/proteína."
      tool "/align" "🔗" "Alinhar" "Compare duas sequências (Needleman–Wunsch)."
      tool "/import" "🌐" "Importar do NCBI" "Traga genes reais por número de acesso."
      tool "/biocifra" "🔐" "Biocifra" "Cifre mensagens dentro do DNA com uma senha."
      tool "/compendio" "📖" "Compêndio" "Conceitos de bioinformática e biologia molecular."
      tool "/favorites" "⭐" "Favoritos" "Suas análises e alinhamentos favoritos."
  section_ [class_ "card"] $ do
    h3_ "Análises recentes"
    if null recents
      then p_ [class_ "muted"] "Nenhuma análise ainda. Comece pelas Sequências."
      else mapM_ (analysisCard base) recents
  where
    stat :: Text -> Text -> Html ()
    stat n lbl = div_ [class_ "stat"] $ do
      span_ [class_ "stat-n"] (toHtml n)
      span_ [class_ "muted"] (toHtml lbl)
    tool :: Text -> Html () -> Html () -> Html () -> Html ()
    tool href ico ttl txt = a_ [href_ href, class_ "feature toollink"] $ do
      div_ [class_ "ico"] ico
      h3_ ttl
      p_ [class_ "muted"] txt

-- Alinhamento ----------------------------------------------------------------
alignPage :: Text -> [Entity BioSequence] -> [Entity AlignmentRecord]
          -> Int -> Int -> Html ()
alignPage _ seqs recents page totalPages = do
  section_ [class_ "card"] $ do
    h2_ "Alinhamento global (Needleman–Wunsch)"
    if length seqs < 2
      then p_ [class_ "muted"] "Você precisa de pelo menos 2 sequências cadastradas."
      else form_ [action_ "/align", method_ "post"] $ do
        -- Por padrão já vêm pré-selecionadas as duas sequências mais recentes.
        label_ "Sequência A"
        selectFor "seqa" 0
        label_ "Sequência B"
        selectFor "seqb" (min 1 (length seqs - 1))
        label_ "Pontuação"
        div_ [class_ "scorerow"] $ do
          scoreInput "match" "igual (+)" "1"
          scoreInput "mismatch" "diferente" "-1"
          scoreInput "gap" "lacuna" "-2"
        button_ [class_ "btn", type_ "submit"] "Alinhar e registrar"
  section_ [class_ "card"] $ do
    h3_ "Alinhamentos registrados"
    if null recents
      then p_ [class_ "muted"] "Nenhum alinhamento ainda."
      else ul_ [class_ "list"] (mapM_ recRow recents)
    when (totalPages > 1) $ div_ [class_ "pager"] $ do
      if page > 1
        then a_ [href_ ("/align?p=" <> T.pack (show (page - 1))), class_ "btn ghost"]
                "← anteriores"
        else span_ [] ""
      span_ [class_ "muted"]
        (toHtml ("página " <> T.pack (show page) <> " de " <> T.pack (show totalPages)))
      if page < totalPages
        then a_ [href_ ("/align?p=" <> T.pack (show (page + 1))), class_ "btn ghost"]
                "próximas →"
        else span_ [] ""
  where
    selectFor :: Text -> Int -> Html ()
    selectFor nm selIdx = select_ [name_ nm] $
      mapM_ (\(i, e) ->
               option_ ([value_ (T.pack (show (sid e)))]
                        ++ [makeAttribute "selected" "selected" | i == selIdx])
                       (toHtml (bioSequenceName (entityVal e))))
            (zip [0 ..] seqs)
    scoreInput :: Text -> Text -> Text -> Html ()
    scoreInput nm lbl val = span_ [class_ "scorefield"] $ do
      small_ (toHtml lbl)
      input_ [type_ "number", name_ nm, value_ val]
    recRow :: Entity AlignmentRecord -> Html ()
    recRow = alignRow

-- Permalink de alinhamento (re-roda NW com a pontuação registrada) -----------
alignPermalinkPage :: Text -> Entity AlignmentRecord -> Alignment -> Html ()
alignPermalinkPage base e reproduced = do
  let r   = entityVal e
      tok = alignmentRecordShareToken r
  backTo "/align" "← Alinhamentos"
  section_ [class_ "card"] $ do
    div_ [class_ "result-head"] $ do
      h2_ (toHtml (alignmentRecordSeqAName r <> " × " <> alignmentRecordSeqBName r))
      alignKebab r
    p_ [class_ "muted"] $ do
      strong_ "Score: "; toHtml (show (alignmentRecordScore r))
      " · "
      strong_ "Identidade: "
      toHtml (T.pack (printf "%.1f%%" (alignmentRecordIdentity r)))
      " · "
      strong_ "Pontuação: "
      toHtml (T.pack (printf "igual %+d / dif %+d / gap %+d"
                        (alignmentRecordMatchS r) (alignmentRecordMismatchS r)
                        (alignmentRecordGapS r)))
    h3_ "Alinhamento"
    p_ [class_ "muted"] "Verde = igual · laranja = diferente · vermelho = lacuna (gap)."
    alignViz (alignedA reproduced) (alignedB reproduced)
    div_ [class_ "result-actions"] $ do
      permalinkBox base ("/al/" <> tok)
      a_ [href_ ("/al/" <> tok <> "/export.json"), class_ "muted dl"] "baixar JSON"

-- Coleção de favoritos (acessada só pelo Início) -----------------------------
favoritesPage :: Text -> [Entity Analysis] -> [Entity AlignmentRecord] -> Html ()
favoritesPage base favA favAl = do
  backButton
  section_ [class_ "card"] $ do
    h2_ "⭐ Favoritos"
    p_ [class_ "muted"] "Suas análises e alinhamentos marcados como favoritos."
  section_ [class_ "card"] $ do
    h3_ "Análises favoritas"
    if null favA
      then p_ [class_ "muted"] "Nenhuma análise favoritada ainda."
      else mapM_ (analysisCard base) favA
  section_ [class_ "card"] $ do
    h3_ "Alinhamentos favoritos"
    if null favAl
      then p_ [class_ "muted"] "Nenhum alinhamento favoritado ainda."
      else ul_ [class_ "list"] (mapM_ alignRow favAl)

-- Linha de alinhamento numa lista, com menu de 3 pontos (favoritar/excluir).
alignRow :: Entity AlignmentRecord -> Html ()
alignRow e = let r = entityVal e in li_ $ do
  when (alignmentRecordFavorite r) (span_ [title_ "favorito"] "⭐")
  a_ [href_ ("/al/" <> alignmentRecordShareToken r)]
     (toHtml (alignmentRecordSeqAName r <> " × " <> alignmentRecordSeqBName r))
  span_ [class_ "tag"] (toHtml ("score " <> T.pack (show (alignmentRecordScore r))))
  span_ [class_ "muted"] (toHtml (T.pack (printf "%.1f%%" (alignmentRecordIdentity r))))
  alignKebab r

-- Menu de 3 pontos de um alinhamento (formulários, pois a lista é server-side).
alignKebab :: AlignmentRecord -> Html ()
alignKebab r = details_ [class_ "kebab"] $ do
  summary_ "⋮"
  div_ [class_ "kebab-menu"] $ do
    form_ [action_ ("/al/" <> tok <> "/favorite"), method_ "post"] $
      button_ [class_ "kebab-item"]
              (if alignmentRecordFavorite r then "☆ Desfavoritar" else "⭐ Favoritar")
    form_ [ action_ ("/al/" <> tok <> "/delete"), method_ "post"
          , onsubmit_ "return confirm('Excluir este alinhamento?')" ] $
      button_ [class_ "kebab-item danger"] "🗑 Excluir"
  where tok = alignmentRecordShareToken r

-- Visualização colorida do alinhamento ---------------------------------------
alignViz :: Text -> Text -> Html ()
alignViz a b = div_ [class_ "alnviz"] $ do
  pre_ [class_ "aln"] (vizRow a b)
  pre_ [class_ "aln"] (vizRow b a)
  where
    vizRow :: Text -> Text -> Html ()
    vizRow self other =
      mapM_ (\(x, y) -> span_ [class_ (cls x y)] (toHtml (T.singleton x)))
            (zip (T.unpack self) (T.unpack other))
    cls x y
      | x == '-' || y == '-' = "g"
      | x == y               = "m"
      | otherwise            = "x"

-- Biocifra -------------------------------------------------------------------
biocifraPage :: Maybe Text -> Maybe (Either Text Text) -> Html ()
biocifraPage mEnc mDec = do
  section_ [class_ "card"] $ do
    h2_ "🔐 Biocifra — mensagens cifradas em DNA"
    p_ [class_ "muted"] "Cada byte do texto é combinado com a sua senha e vira 4 \
                        \bases (A=00, C=01, G=10, T=11). Sem a senha certa, a \
                        \sequência não revela nada."
  section_ [class_ "card"] $ do
    h3_ "Cifrar texto em DNA"
    form_ [action_ "/biocifra/encode", method_ "post"] $ do
      textarea_ [name_ "text", rows_ "3", placeholder_ "Sua mensagem secreta…",
                 required_ "required"] ""
      label_ "Senha"
      input_ [type_ "text", name_ "key", placeholder_ "uma senha", required_ "required"]
      button_ [class_ "btn", type_ "submit"] "Cifrar"
    case mEnc of
      Nothing -> pure ()
      Just d  -> div_ [class_ "encbox"] $ do
        textarea_ [id_ "enc-out", class_ "out", readonly_ "readonly", rows_ "3"]
                  (toHtml d)
        button_ [ type_ "button", class_ "btn ghost"
                , onclick_ "navigator.clipboard.writeText(\
                           \document.getElementById('enc-out').value);\
                           \this.textContent='Copiado!'" ]
                "Copiar"
  section_ [class_ "card"] $ do
    h3_ "Decifrar uma sequência"
    form_ [action_ "/biocifra/decode", method_ "post"] $ do
      textarea_ [name_ "dna", rows_ "3", placeholder_ "ACGTACGT…", required_ "required"] ""
      label_ "Senha"
      input_ [type_ "text", name_ "key", placeholder_ "a mesma senha", required_ "required"]
      button_ [class_ "btn", type_ "submit"] "Decifrar"
    case mDec of
      Nothing          -> pure ()
      Just (Right msg) -> pre_ [class_ "out"] (toHtml msg)
      Just (Left err)  -> flashError err

-- Compêndio ------------------------------------------------------------------
compendiumPage :: Html ()
compendiumPage = do
  backButton
  section_ [class_ "card"] $ do
    h2_ "📖 Compêndio"
    p_ [class_ "muted"] "Os conceitos de biologia molecular e bioinformática \
                        \necessários para entender o que o PaxLab mostra."
  section_ [class_ "card"] $ do
    h3_ "As moléculas e o código"
    item "DNA" "A molécula que armazena a informação genética da célula, em fita \
               \dupla. Seu alfabeto tem quatro bases — A, C, G e T — e é a ordem \
               \delas que carrega a informação. As duas fitas são complementares: \
               \A sempre pareia com T, e C com G."
    item "RNA" "Uma cópia de trabalho de um trecho do DNA, em geral de fita simples. \
               \Usa U (uracila) no lugar de T e serve de intermediário entre o gene \
               \e a proteína — o RNA mensageiro leva a receita do gene até o ribossomo."
    item "Proteína" "O que de fato executa o trabalho na célula: enzimas, estrutura, \
                    \transporte, sinalização. É uma cadeia de aminoácidos (existem 20, \
                    \cada um com uma letra), e a ordem deles determina como a proteína \
                    \se dobra e qual função ela assume."
    item "Base nitrogenada" "Cada \"letra\" da sequência (A, C, G, T no DNA; U no RNA). \
                            \A sequência de bases É a informação — ler, comparar e \
                            \transformar essas letras é tudo o que o PaxLab faz."
    item "Códon" "Uma trinca de bases lida de uma vez. Como há 4 bases, existem \
                 \4³ = 64 códons possíveis, e cada um corresponde a um aminoácido ou \
                 \a um sinal de parada. É a unidade de leitura do código genético."
    item "Código genético" "A tabela que diz qual aminoácido cada um dos 64 códons \
                           \representa. É redundante (vários códons dão o mesmo \
                           \aminoácido) e quase universal entre os seres vivos. \
                           \AUG (ATG no DNA) marca o início e codifica metionina."
  section_ [class_ "card"] $ do
    h3_ "As operações do PaxLab"
    item "Transcrição" "A passagem de DNA para RNA, feita pela enzima RNA-polimerase. \
                       \Na prática, copia a sequência trocando cada T por U; é o \
                       \primeiro passo da expressão de um gene. (operação Transcrição)"
    item "Tradução" "A leitura dos códons, um a um, para montar a proteína no \
                    \ribossomo. Vai do códon de início (AUG) até encontrar um códon de \
                    \parada, que o PaxLab marca com '*'. (operação Tradução)"
    item "Fita complementar reversa" "A fita que pareia com a sua sequência (A-T, C-G), \
                                      \lida no sentido oposto. É o molde que a célula \
                                      \usa para copiar o DNA e é essencial para procurar \
                                      \genes que ficam na fita de trás."
    item "Conteúdo GC" "A porcentagem de bases que são G ou C. Como o par G-C tem três \
                       \pontes de hidrogênio (contra duas do A-T), regiões ricas em GC \
                       \são mais estáveis e \"derretem\" a temperaturas mais altas — o \
                       \número dá pistas sobre a estrutura e sobre o organismo."
    item "ORF (janela de leitura aberta)" "Um trecho que vai de um códon de início \
                                          \(ATG) até um de parada, sem paradas no meio. \
                                          \É uma candidata a região que codifica \
                                          \proteína, e localizá-las é um primeiro passo \
                                          \para achar genes numa sequência nova."
    item "Frame de leitura" "Como cada códon tem 3 bases, a mesma fita pode ser lida de \
                            \três formas, dependendo de onde se começa (posição 1, 2 ou \
                            \3). Cada uma é um frame — o PaxLab varre os três frames \
                            \diretos ao buscar ORFs."
  section_ [class_ "card"] $ do
    h3_ "Formatos, comparação e reprodutibilidade"
    item "FASTA" "O formato de texto mais comum para sequências. Cada registro tem uma \
                 \linha de cabeçalho começando com \">\" (nome/descrição) seguida das \
                 \linhas com a sequência. É o que o NCBI devolve e o que o PaxLab \
                 \importa e analisa."
    item "Alinhamento" "Encaixar duas sequências, posição a posição, para revelar os \
                       \trechos em comum. Onde uma tem uma base que a outra não tem, \
                       \entra uma lacuna (gap), representando uma inserção ou deleção \
                       \ao longo da evolução."
    item "Identidade (%)" "A proporção de colunas do alinhamento em que as duas \
                          \sequências têm exatamente a mesma letra. É uma medida rápida \
                          \de quão parecidas elas são: quanto maior, mais próximas."
    item "Needleman–Wunsch" "O algoritmo clássico de alinhamento global: encaixa as \
                            \sequências inteiras maximizando uma pontuação (prêmio para \
                            \iguais, penalidade para diferentes e para lacunas), por \
                            \programação dinâmica. No PaxLab a pontuação é configurável."
    item "Reprodutibilidade" "Conseguir refazer uma análise e obter exatamente o mesmo \
                             \resultado — um problema sério na ciência atual. No PaxLab \
                             \cada análise guarda a sua \"receita\" e o permalink a \
                             \re-executa na hora para provar. É simples aqui porque as \
                             \funções de análise são puras: mesma entrada, mesma saída."
  where
    item :: Text -> Text -> Html ()
    item ttl txt = div_ [class_ "comp-item"] $ do
      h4_ (toHtml ttl)
      p_ [class_ "muted"] (toHtml txt)

-- Importação do NCBI ---------------------------------------------------------
importPage :: Maybe Text -> Html ()
importPage mErr = section_ [class_ "card narrow"] $ do
  h2_ "Importar do NCBI"
  p_ [class_ "muted"] "Número de acesso do GenBank (ex.: NM_000207 — insulina humana)."
  maybe (pure ()) flashError mErr
  form_ [action_ "/import", method_ "post"] $ do
    label_ "Número de acesso"
    input_ [type_ "text", name_ "accession", placeholder_ "NM_000207", required_ "required"]
    button_ [class_ "btn", type_ "submit"] "Buscar e salvar"

-- Botão "voltar" (telas que não são seções principais). Uma linha de JS
-- inline só para a navegação de histórico do navegador.
backButton :: Html ()
backButton = button_ [class_ "btn ghost back", onclick_ "history.back()"] "← Voltar"

-- | Voltar para um destino fixo (navegação GET nova, sempre atualizada — ao
-- contrário do history.back(), que o navegador pode servir do cache).
backTo :: Text -> Text -> Html ()
backTo url label = a_ [href_ url, class_ "btn ghost back"] (toHtml label)

-- Caixa de permalink: clicar revela o link completo, pronto para copiar.
-- Usa <details>/<summary> nativos do HTML — sem JavaScript.
permalinkBox :: Text -> Text -> Html ()
permalinkBox base path =
  details_ [class_ "permabox"] $ do
    summary_ "🔗 permalink"
    div_ [class_ "copyrow"] $ do
      input_ [class_ "copyfield", type_ "text", readonly_ "readonly",
              value_ (base <> path), onclick_ "this.select()"]
      a_ [href_ path, class_ "muted", target_ "_blank"] "abrir"

verdictBox :: Bool -> Html ()
verdictBox matches =
  div_ [class_ (if matches then "verdict ok" else "verdict bad")] $
    if matches
      then "✓ Reproduzido: rodar a receita agora deu exatamente o mesmo resultado."
      else "⚠ Divergência entre o resultado registrado e o reproduzido."

flashError :: Text -> Html ()
flashError msg = div_ [class_ "flash"] (toHtml msg)

-- Helpers -------------------------------------------------------------------
-- Sequência exibida em grupos de 10, com as bases das ORFs destacadas por frame.
highlightedSeq :: Text -> [ORF] -> Html ()
highlightedSeq res orfs =
  pre_ [class_ "seq"] (mapM_ renderPos (zip [0 :: Int ..] (T.unpack res)))
  where
    cov = orfCoverage orfs
    renderPos (i, c) = do
      when (i > 0 && i `mod` 10 == 0) (toHtml (" " :: Text))
      case M.lookup i cov of
        Just f  -> span_ [class_ ("orf f" <> T.pack (show f))] (toHtml (T.singleton c))
        Nothing -> toHtml (T.singleton c)

-- Seção de sítios de restrição (para sequências de DNA/RNA).
restrictionSection :: Text -> Html ()
restrictionSection res = section_ [class_ "card"] $ do
  h3_ "Sítios de restrição"
  let sites = restrictionSites res
  if null sites
    then p_ [class_ "muted"] "Nenhum sítio das enzimas conhecidas nesta sequência."
    else ul_ [class_ "list"] (mapM_ enzRow sites)
  where
    enzRow :: (Enzyme, [Int]) -> Html ()
    enzRow (e, ps) = li_ $ do
      strong_ (toHtml (enzName e))
      span_ [class_ "tag"] (toHtml (enzSite e))
      span_ [class_ "muted"] $ toHtml $
        T.pack (show (length ps)) <> " sítio(s) — pos. "
        <> T.intercalate ", " (map (T.pack . show) ps)

-- Seção de propriedades de uma proteína.
proteinSection :: Text -> Html ()
proteinSection res = section_ [class_ "card"] $ do
  let ps = proteinStats res
  h3_ "Propriedades da proteína"
  p_ [class_ "muted"] $ do
    strong_ "Comprimento: "
    toHtml (show (psLength ps) <> " aa")
    " · "
    strong_ "Massa molecular: "
    toHtml (T.pack (printf "%.0f Da" (psWeightDa ps)))
  h4_ "Aminoácidos mais frequentes"
  ul_ [class_ "list"] (mapM_ compRow (take 8 (psComposition ps)))
  where
    compRow :: (Char, Int) -> Html ()
    compRow (c, n) = li_ $ do
      span_ [class_ "tag"] (toHtml (T.singleton c))
      span_ [class_ "muted"] (toHtml (show n <> "×"))

css :: Text
css = T.pack
  "*{box-sizing:border-box}body{margin:0;font-family:system-ui,sans-serif;\
  \color:#14202b;background:#f4f7fb;line-height:1.5}\
  \nav{display:flex;align-items:center;gap:1rem;padding:.8rem 1.4rem;\
  \background:#0e2a47;color:#fff}nav a{color:#cfe0f5;text-decoration:none}\
  \nav a.btn,a.btn{background:#1e8e5a;color:#fff;padding:.45rem .9rem;\
  \border-radius:7px;text-decoration:none;display:inline-block}\
  \.brand{font-weight:700;color:#fff!important;font-size:1.1rem}\
  \.spacer{flex:1}.linklike{background:none;border:0;color:#cfe0f5;cursor:pointer;\
  \font:inherit;padding:0}\
  \.inline{display:inline}main{max-width:880px;margin:2rem auto;padding:0 1rem}\
  \.hero{text-align:center;padding:2rem 0}.hero h1{font-size:2.2rem;margin:.3rem 0}\
  \.lead{font-size:1.15rem;color:#41566b;max-width:620px;margin:1rem auto}\
  \.cta{display:flex;gap:.8rem;justify-content:center;margin-top:1.4rem}\
  \.btn.big{padding:.7rem 1.4rem;font-size:1.05rem}.btn.ghost{background:#fff;\
  \color:#0e2a47;border:1px solid #cdd9e6}.features{display:grid;\
  \grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:1rem;margin-top:2rem}\
  \.feature{background:#fff;border:1px solid #e3eaf2;border-radius:12px;padding:1.2rem}\
  \.feature .ico{font-size:1.8rem}.card{background:#fff;border:1px solid #e3eaf2;\
  \border-radius:12px;padding:1.4rem;margin-bottom:1.4rem}.card.narrow{max-width:420px;\
  \margin:2rem auto}label{display:block;margin:.7rem 0 .25rem;font-weight:600}\
  \input,textarea,select{width:100%;padding:.55rem;border:1px solid #cdd9e6;\
  \border-radius:7px;font:inherit}button.btn{margin-top:1rem;border:0;cursor:pointer;\
  \font-size:1rem}.muted{color:#6b7d90}.tag{background:#e7f1ff;color:#1f5fa8;\
  \border-radius:20px;padding:.1rem .6rem;font-size:.8rem;margin:0 .5rem}\
  \.list{list-style:none;padding:0}.list li{display:flex;align-items:center;gap:.4rem;\
  \padding:.6rem 0;border-bottom:1px solid #eef2f7}.seq,.out{background:#0f1c2b;\
  \color:#9ef0c8;padding:.8rem;border-radius:8px;overflow-x:auto;font-size:.85rem;\
  \white-space:pre-wrap;word-break:break-all}.result{border:1px solid #e3eaf2;\
  \border-radius:10px;padding:1rem;margin:.8rem 0}.result-head{display:flex;\
  \justify-content:space-between;align-items:center}.permalink{text-decoration:none;\
  \font-size:.85rem}.flash{background:#fdecec;color:#a12020;border:1px solid #f4caca;\
  \padding:.6rem .9rem;border-radius:8px;margin:.6rem 0}.verdict{padding:.7rem;\
  \border-radius:8px;margin:.8rem 0;font-weight:600}.verdict.ok{background:#e6f7ee;\
  \color:#11663b}.verdict.bad{background:#fdecec;color:#a12020}\
  \.stats{display:flex;gap:1.6rem;margin-top:1rem}.stat{display:flex;\
  \flex-direction:column}.stat-n{font-size:1.8rem;font-weight:700;color:#1e8e5a}\
  \.toollink{text-decoration:none;color:inherit;display:block}\
  \.toollink:hover{border-color:#1e8e5a}.btn.back{margin:0 0 1rem;padding:.4rem .9rem}\
  \.permabox{margin-top:.6rem}.permabox summary{cursor:pointer;color:#1f5fa8;\
  \font-size:.9rem}.copyrow{display:flex;gap:.5rem;align-items:center;margin-top:.5rem}\
  \.copyfield{font-family:monospace;font-size:.8rem;background:#f4f7fb}\
  \.alnviz{overflow-x:auto}.aln{background:#0f1c2b;margin:.2rem 0;padding:.6rem;\
  \border-radius:6px;font-size:.85rem;letter-spacing:1px;white-space:pre;\
  \display:block;width:max-content;min-width:100%}\
  \.aln span.m{color:#37b97f}.aln span.x{color:#e0a64f}\
  \.aln span.g{color:#fc8181;background:#3a1f24}\
  \.result-head{display:flex;justify-content:space-between;align-items:center}\
  \.star{background:none;border:0;font-size:1.3rem;cursor:pointer;color:#c7d2dd;\
  \line-height:1}.star.on{color:#f0b429}.result-actions{display:flex;gap:1rem;\
  \align-items:center;margin-top:.5rem;flex-wrap:wrap;\
  \justify-content:space-between}.dl{font-size:.85rem;text-decoration:none;\
  \margin-left:auto;align-self:flex-end}.scorerow{display:flex;gap:.8rem}.scorefield{display:flex;\
  \flex-direction:column;flex:1}.scorefield small{color:#6b7d90}.scorefield input{\
  \width:100%}.encbox{margin-top:.6rem}.encbox textarea{width:100%}\
  \.btn.danger{background:#c53030}form.danger{margin-top:.8rem}\
  \.card-btns{display:flex;gap:.4rem;align-items:center}.del{background:none;\
  \border:0;font-size:1.05rem;cursor:pointer;opacity:.55}.del:hover{opacity:1}\
  \.pager{display:flex;justify-content:space-between;align-items:center;\
  \margin-top:1rem;gap:1rem}.pager .btn{padding:.4rem .9rem}\
  \.list form.inline{margin-left:auto;display:flex}.list .kebab{margin-left:auto}\
  \.kebab{position:relative}.kebab>summary{list-style:none;cursor:pointer;\
  \padding:0 .4rem;color:#6b7d90;font-size:1.4rem;line-height:1;border-radius:6px}\
  \.kebab>summary::-webkit-details-marker{display:none}\
  \.kebab[open]>summary{background:#eef2f7;color:#14202b}.kebab-menu{position:absolute;\
  \right:0;top:1.9rem;background:#fff;border:1px solid #e3eaf2;border-radius:8px;\
  \box-shadow:0 6px 18px rgba(0,0,0,.14);z-index:20;min-width:170px;overflow:hidden}\
  \.kebab-menu form{margin:0}.kebab-item{display:block;width:100%;text-align:left;\
  \background:none;border:0;padding:.6rem .9rem;cursor:pointer;font:inherit;\
  \color:#14202b}.kebab-item:hover{background:#f4f7fb}.kebab-item.danger{color:#c53030}\
  \.comp-item{padding:.55rem 0;border-bottom:1px solid #eef2f7}.comp-item:last-child{\
  \border-bottom:0}.comp-item h4{margin:0 0 .2rem;color:#0e2a47}\
  \.searchbar{display:flex;gap:.5rem;margin-bottom:1rem}.searchbar input{flex:1;\
  \margin:0}.searchbar .btn{margin:0}.seq .orf{border-radius:2px;padding:0 1px}\
  \.seq .orf.f1{background:rgba(55,185,127,.40)}.seq .orf.f2{background:rgba(79,157,232,.45)}\
  \.seq .orf.f3{background:rgba(224,166,79,.45)}.orf-legend{margin-top:.4rem}\
  \.card-foot{display:flex;justify-content:flex-end;margin-top:.8rem}\
  \.perma-actions{display:flex;gap:.8rem;flex-wrap:wrap;margin-top:.8rem}\
  \details summary{cursor:pointer;font-weight:600;color:#1f5fa8}footer{\
  \text-align:center;color:#8295a8;padding:2rem}"
