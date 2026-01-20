###########################################################
# Stage 1: Build the Haskell builder image with deps

FROM haskell:9.10.1-bullseye AS wrench-builder

RUN apt-get update && apt-get install -y \
    libgmp-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package.yaml stack.yaml stack.yaml.lock wrench.cabal /app/

RUN stack setup --install-ghc
RUN stack build --only-dependencies

###########################################################
# Stage 2.1: Build the Haskell application

FROM ryukzak/wrench-builder AS wrench-build
COPY . /app
ARG VERSION_SUFFIX=""
RUN echo "Building with VERSION_SUFFIX=${VERSION_SUFFIX}" && \
    stack build --ghc-options -O2 --copy-bins --local-bin-path /app/.local/bin

###########################################################
# Stage 2.2: Generate variants

FROM python:3.13-alpine3.21 AS wrench-variants

WORKDIR /app
COPY script /app

RUN [ "python", "/app/variants.py" ]

###########################################################
# Stage 2.3: Prebuild example reports and index

FROM wrench-build AS wrench-examples
COPY --from=wrench-variants /app/variants /app/variants

RUN chmod +x script/build_examples.sh
RUN EXAMPLE_PORT=8090 \
    EXAMPLE_ROOT=/app/example \
    EXAMPLE_OUTPUT=/app/example-reports \
    EXAMPLES_JSON=/app/static/assets/examples.json \
    WRENCH_SERV_BIN=/app/.local/bin/wrench-serv \
    WRENCH_BIN=/app/.local/bin/wrench \
    VARIANTS=/app/variants \
    bash ./script/build_examples.sh

###########################################################
# Stage 4: Create a minimal runtime container

FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libgmp10 libc6-dev locales ca-certificates \
    && echo "en_US.UTF-8 UTF-8" > /etc/locale.gen \
    && echo "ru_RU.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8
ENV VARIANTS=/app/variants
ENV WRENCH_EXEC=wrench
ENV STORAGE_PATH=/data

WORKDIR /app
COPY --from=wrench-build /app/.local/bin/wrench /app/.local/bin/wrench-serv /app/.local/bin/wrench-fmt /bin/
COPY --from=wrench-variants /app/variants /app/variants
COPY --from=wrench-examples /app/static /app/static
COPY --from=wrench-examples /app/example-reports /app/example-reports
COPY docker/entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

EXPOSE 8080

ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["wrench-serv"]
