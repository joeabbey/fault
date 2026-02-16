# syntax=docker/dockerfile:1

# Stage 1: Build SvelteKit frontend
FROM node:22-alpine AS web-builder

WORKDIR /web

# Install dependencies first (cache layer)
COPY web/package.json web/package-lock.json ./
COPY web/vendor ./vendor
RUN npm ci

# Copy source and build
COPY web/ .
RUN npm run build

# Stage 2: Build Go binary
FROM golang:1.22-alpine AS builder

RUN apk add --no-cache git openssh-client

WORKDIR /app

ENV GOPRIVATE=github.com/joeabbey/magma
ENV GONOSUMCHECK=github.com/joeabbey/magma

# Download dependencies first (cache layer)
COPY go.mod go.sum ./
RUN --mount=type=secret,id=magma_ssh \
    mkdir -p ~/.ssh && \
    ssh-keyscan github.com >> ~/.ssh/known_hosts 2>/dev/null && \
    if [ -f /run/secrets/magma_ssh ]; then \
      cp /run/secrets/magma_ssh ~/.ssh/id_ed25519 && \
      chmod 600 ~/.ssh/id_ed25519 && \
      git config --global url."git@github.com:".insteadOf "https://github.com/"; \
    fi && \
    go mod download

# Copy source and build
COPY . .
ARG VERSION=dev
RUN --mount=type=secret,id=magma_ssh \
    if [ -f /run/secrets/magma_ssh ]; then \
      mkdir -p ~/.ssh && \
      cp /run/secrets/magma_ssh ~/.ssh/id_ed25519 && \
      chmod 600 ~/.ssh/id_ed25519 && \
      ssh-keyscan github.com >> ~/.ssh/known_hosts 2>/dev/null && \
      git config --global url."git@github.com:".insteadOf "https://github.com/"; \
    fi && \
    CGO_ENABLED=0 go build -ldflags="-s -w -X main.Version=${VERSION}" -o /fault-cloud ./cmd/fault-cloud

# Stage 3: Final image
FROM alpine:3.19

RUN apk add --no-cache ca-certificates

COPY --from=builder /fault-cloud /usr/local/bin/fault-cloud
COPY --from=web-builder /web/build /srv/web

ENV WEB_DIR=/srv/web

EXPOSE 8082

CMD ["fault-cloud"]
