# 🧬 PaxLab

> 🔗 **Site no ar:** _a preencher após o deploy_ &nbsp;·&nbsp;
> **Login de demonstração:** `demo@paxlab.bio` / `paxlab123`

Plataforma de bioinformática em **Haskell** onde cada análise vira um
**registro reproduzível e compartilhável**. Analise sequências de DNA, RNA
e proteína; cada operação guarda a *receita* exata e pode ser refeita por
qualquer pessoa através de um permalink.

## Por que é diferente das ferramentas existentes

As ferramentas do ramo (NCBI BLAST, Expasy, Benchling…) tratam cada análise
como descartável. O PaxLab grava a **receita** de cada análise e, como o
núcleo é feito de **funções puras** (mesma entrada → mesma saída, sempre),
reproduzir é só re-executar. Reprodutibilidade não é um recurso adicionado —
é consequência de Haskell ser funcional. Essa é a "feature de venda" do
projeto e o argumento central da apresentação.

## Funcionalidades

- **Autenticação** com senha em bcrypt e sessão por cookie.
- **Dashboard pós-login** (`/home`) com estatísticas, atalhos e análises recentes.
- **CRUD de sequências** com parser FASTA próprio e validação amigável.
- **Análises**: transcrição, tradução, fita complementar reversa,
  conteúdo GC e busca de ORFs.
- **Caderno reprodutível**: toda análise tem permalink (`/a/<token>`) que
  re-executa a receita e prova que o resultado bate. Botão de *fork*.
- **Alinhamento global** de duas sequências (Needleman–Wunsch) com score e %
  de identidade — **registrado no caderno** (permalink `/al/<token>` que re-roda
  o algoritmo e prova reprodutibilidade) e com **visualização colorida**
  (verde = igual, laranja = diferente, vermelho = lacuna).
- **Visualização gráfica**: mapa de ORFs em SVG (gerado em Haskell, sem JS) e as
  ORFs destacadas dentro da própria sequência, no detalhe de cada sequência.
- **Baixar sequência como FASTA** e **busca por nome** na lista.
- **Sítios de enzimas de restrição** (EcoRI, BamHI, …) para sequências de DNA.
- **Propriedades da proteína** (comprimento, massa molecular, composição de
  aminoácidos); a massa também aparece no resultado da Tradução.
- **Importação do NCBI** (`/import`): traz genes reais por número de acesso
  do GenBank (ex.: `NM_000207` = insulina humana) via E-utilities.
- **Biocifra** (`/biocifra`): cifra e decifra mensagens dentro de uma sequência de
  DNA usando uma **senha** (keystream XOR + verificação) — o lado segurança do
  projeto. Botão de copiar.
- **Compêndio** (`/compendio`): referência dos conceitos de biologia molecular e
  bioinformática usados no site (na navegação e como card no Início).
- **Favoritar** análises e alinhamentos; a página **Favoritos** (acessível pelo
  card no Início) reúne os dois tipos.
- **Excluir** análises (htmx remove o card na hora) e alinhamentos, com confirmação.
- **Listas paginadas**: histórico de análises (5/página) e alinhamentos
  registrados (10/página).
- **Editar e excluir sequências** (renomear / reeditar resíduos; excluir remove
  também análises e alinhamentos dependentes).
- **Exportar trilha** como JSON citável (`/a/<token>/export.json`,
  `/al/<token>/export.json`).
- **Pontuação de alinhamento configurável** (match / mismatch / gap), guardada
  junto do registro para reprodução fiel.
- **Modo educacional**: cada resultado vem explicado em PT-BR.
- **Dados de exemplo** (usuário demo) para avaliar sem precisar trazer dados.

## Arquitetura

| Camada            | Arquivo                | Papel                                   |
|-------------------|------------------------|-----------------------------------------|
| Núcleo puro       | `src/PaxLab/Bio.hs`    | Algoritmos (transcrição, tradução, GC…) |
| Parser            | `src/PaxLab/Bio/Fasta.hs` | FASTA com erros amigáveis            |
| Alinhamento       | `src/PaxLab/Align.hs`  | Needleman–Wunsch (puro, matriz lazy)    |
| Biocifra          | `src/PaxLab/Stego.hs`  | Cifra texto em DNA com senha (puro)     |
| Restrição         | `src/PaxLab/Restriction.hs` | Sítios de enzimas de restrição     |
| Proteína          | `src/PaxLab/Protein.hs` | Massa molecular e composição (puro)    |
| Importação        | `src/PaxLab/Fetch.hs`  | Busca FASTA no NCBI (E-utilities)       |
| Caderno           | `src/PaxLab/Analysis.hs`  | `Operation` (receita) + `runOperation` (pura) |
| Banco             | `src/PaxLab/Model.hs`, `Db.hs` | Esquema persistent + SQLite        |
| Autenticação      | `src/PaxLab/Auth.hs`   | bcrypt + tokens                         |
| Web               | `src/PaxLab/Web.hs`    | Rotas Scotty + sessões                  |
| Views             | `src/PaxLab/Views.hs`  | HTML com Lucid + htmx                   |

Stack: **Scotty** (servidor) · **persistent + SQLite** (banco) ·
**Lucid** (HTML tipado) · **htmx** (interatividade sem JS).

## Como rodar

```bash
cd ~/paxlab
stack build          # primeira vez baixa as dependências (demora)
stack exec paxlab    # sobe em http://localhost:3000
```

Login de demonstração: **demo@paxlab.bio** / **paxlab123**

### Variáveis de ambiente

| Variável                | Padrão            | Para que serve                                  |
|-------------------------|-------------------|-------------------------------------------------|
| `PORT`                  | `3000`            | Porta do servidor                               |
| `PAXLAB_DB`             | `paxlab.sqlite3`  | Caminho do arquivo SQLite                       |
| `PAXLAB_SECURE_COOKIES` | (desligado)       | `1`/`true` em produção (HTTPS) para o cookie de sessão usar a flag `Secure` |

Rodar os testes (núcleo, alinhamento e Bio-CTF):

```bash
stack test
```

> **Nota sobre este ambiente:** falta o pacote `libgmp-dev` (só há o
> runtime `libgmp.so.10`). Há duas opções:
> 1. `sudo apt-get install libgmp-dev` (corrige de vez), ou
> 2. usar o symlink local já criado, prefixando os comandos com
>    `LIBRARY_PATH=~/.local/ghc-libs:$LIBRARY_PATH` e `--system-ghc`, ex.:
>    `LIBRARY_PATH=~/.local/ghc-libs stack build --system-ghc`.

## Deploy

> **A Vercel não serve para este projeto.** Ela roda funções serverless e sites
> estáticos, não um servidor Haskell de processo contínuo, e seu sistema de
> arquivos é efêmero (zera o SQLite). Use um host que roda containers.

O banco é escolhido em **tempo de compilação** por um flag do Cabal:

- **Local (dev)** → SQLite, sem flag: `stack build` / `stack exec paxlab`.
- **Produção** → PostgreSQL, com `--flag paxlab:postgres` (é o que o Dockerfile faz).
  A conexão vem da env **`DATABASE_URL`**.

O `Dockerfile` compila com Postgres e gera uma imagem mínima. Há config pronta
para dois hosts:

### Fly.io
```bash
fly launch --no-deploy          # usa o fly.toml deste repo
fly postgres create             # cria um Postgres gerenciado
fly postgres attach <nome-pg>   # define DATABASE_URL automaticamente
fly deploy
```

### Render
Suba o repo no GitHub e crie um **Blueprint** apontando para o `render.yaml`
(ele já declara o serviço web em Docker + um Postgres `paxlab-db` e injeta o
`DATABASE_URL`).

Em ambos, `PAXLAB_SECURE_COOKIES=1` já está configurado (cookie de sessão com
flag `Secure` sob HTTPS).

## Ideias para subir a nota (próximos passos)

- Visualização gráfica da sequência / mapa de ORFs.
- Alinhamento entre duas sequências (Needleman–Wunsch).
- Consumir uma API pública (NCBI/Ensembl) para importar genes reais.
- "Bio-CTF": esconder mensagens codificadas em DNA (junta o lado segurança).
- Exportar a trilha reprodutível como JSON/PDF citável.
