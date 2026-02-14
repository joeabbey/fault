VERSION ?= $(shell git describe --tags --always --dirty 2>/dev/null || echo "0.1.0-dev")
LDFLAGS = -ldflags "-X main.Version=$(VERSION)"

.PHONY: build build-cloud test lint clean release

build:
	go build $(LDFLAGS) -o fault ./cmd/fault

build-cloud:
	go build $(LDFLAGS) -o fault-cloud ./cmd/fault-cloud

test:
	go test ./... -race

lint:
	golangci-lint run

clean:
	rm -f fault fault-cloud
	rm -rf dist/

release: clean
	mkdir -p dist
	GOOS=linux GOARCH=amd64 go build $(LDFLAGS) -o dist/fault-linux-amd64 ./cmd/fault
	GOOS=linux GOARCH=arm64 go build $(LDFLAGS) -o dist/fault-linux-arm64 ./cmd/fault
	GOOS=darwin GOARCH=arm64 go build $(LDFLAGS) -o dist/fault-darwin-arm64 ./cmd/fault
	GOOS=darwin GOARCH=amd64 go build $(LDFLAGS) -o dist/fault-darwin-amd64 ./cmd/fault
