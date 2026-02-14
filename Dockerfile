FROM golang:1.22-alpine AS builder

WORKDIR /app

# Download dependencies first (cache layer)
COPY go.mod go.sum ./
RUN go mod download

# Copy source and build
COPY . .
RUN CGO_ENABLED=0 go build -ldflags="-s -w" -o /fault-cloud ./cmd/fault-cloud

FROM alpine:3.19

RUN apk add --no-cache ca-certificates

COPY --from=builder /fault-cloud /usr/local/bin/fault-cloud

EXPOSE 8082

CMD ["fault-cloud"]
