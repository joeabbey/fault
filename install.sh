#!/bin/sh
# Fault installer — detects OS/arch, downloads from GitHub Releases
set -e

REPO="joeabbey/fault"
BINARY="fault"

# Detect OS
OS="$(uname -s | tr '[:upper:]' '[:lower:]')"
case "$OS" in
    linux)  OS="linux" ;;
    darwin) OS="darwin" ;;
    *)
        echo "error: unsupported operating system: $OS"
        exit 1
        ;;
esac

# Detect architecture
ARCH="$(uname -m)"
case "$ARCH" in
    x86_64|amd64)   ARCH="amd64" ;;
    aarch64|arm64)   ARCH="arm64" ;;
    *)
        echo "error: unsupported architecture: $ARCH"
        exit 1
        ;;
esac

# Determine latest version
if [ -z "$VERSION" ]; then
    VERSION="$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
        | grep '"tag_name"' | head -1 | cut -d'"' -f4)"
    if [ -z "$VERSION" ]; then
        echo "error: could not determine latest version"
        exit 1
    fi
fi

FILENAME="${BINARY}-${OS}-${ARCH}"
URL="https://github.com/${REPO}/releases/download/${VERSION}/${FILENAME}"

echo "Downloading fault ${VERSION} for ${OS}/${ARCH}..."

# Create temp directory
TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

# Download
if ! curl -fsSL -o "${TMPDIR}/${BINARY}" "$URL"; then
    echo "error: failed to download ${URL}"
    exit 1
fi

chmod +x "${TMPDIR}/${BINARY}"

# Determine install location
INSTALL_DIR="/usr/local/bin"
if [ ! -w "$INSTALL_DIR" ]; then
    INSTALL_DIR="${HOME}/.local/bin"
    mkdir -p "$INSTALL_DIR"
fi

mv "${TMPDIR}/${BINARY}" "${INSTALL_DIR}/${BINARY}"

echo ""
echo "Installed fault ${VERSION} to ${INSTALL_DIR}/${BINARY}"
echo ""

# Check if install dir is in PATH
case ":$PATH:" in
    *":${INSTALL_DIR}:"*) ;;
    *)
        echo "Add ${INSTALL_DIR} to your PATH:"
        echo "  export PATH=\"${INSTALL_DIR}:\$PATH\""
        echo ""
        ;;
esac

echo "Get started:"
echo "  fault init        # Create .fault.yaml config"
echo "  fault hook install # Install pre-commit hook"
echo "  fault check       # Run analysis on changed files"
