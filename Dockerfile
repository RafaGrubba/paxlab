# ---- Build (compila com o backend PostgreSQL) ----
FROM haskell:9.6.7 AS build

# libpq-dev é necessária para compilar o persistent-postgresql.
RUN apt-get update \
    && apt-get install -y --no-install-recommends libpq-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .
RUN stack build --system-ghc --flag paxlab:postgres \
        --copy-bins --local-bin-path /app/bin

# ---- Runtime (imagem mínima só com o binário) ----
FROM debian:bookworm-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
         ca-certificates libpq5 libgmp10 zlib1g \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY --from=build /app/bin/paxlab /usr/local/bin/paxlab

# Locale UTF-8 (evita erro de encoding em logs com acento).
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# A porta efetiva vem da env PORT (definida pela plataforma).
EXPOSE 8080
CMD ["paxlab"]
