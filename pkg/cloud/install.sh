#!/bin/sh
# Fault installer — detects OS/arch, downloads from GitHub Releases
# Usage: curl -sSf https://fault.jabbey.io/install.sh | sh
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
        echo "hint: set VERSION=v0.1.0 to install a specific version"
        exit 1
    fi
fi

FILENAME="${BINARY}-${OS}-${ARCH}"
URL="https://github.com/${REPO}/releases/download/${VERSION}/${FILENAME}"

echo "Downloading fault ${VERSION} for ${OS}/${ARCH}..."

# Create temp directory
WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

# Download binary
if ! curl -fsSL -o "${WORK_DIR}/${BINARY}" "$URL"; then
    echo "error: failed to download ${URL}"
    echo "hint: check that version ${VERSION} exists at https://github.com/${REPO}/releases"
    exit 1
fi

chmod +x "${WORK_DIR}/${BINARY}"

# Verify the binary is a valid executable
if ! file "${WORK_DIR}/${BINARY}" | grep -q "executable"; then
    echo "error: downloaded file is not a valid executable"
    echo "hint: this may indicate a corrupted download"
    exit 1
fi

# Determine install location
INSTALL_DIR="/usr/local/bin"
if [ ! -w "$INSTALL_DIR" ]; then
    INSTALL_DIR="${HOME}/.local/bin"
    mkdir -p "$INSTALL_DIR"
fi

mv "${WORK_DIR}/${BINARY}" "${INSTALL_DIR}/${BINARY}"

echo ""
echo "Installed fault ${VERSION} to ${INSTALL_DIR}/${BINARY}"
echo ""

# Check if install dir is in PATH
case ":$PATH:" in
    *":${INSTALL_DIR}:"*) ;;
    *)
        echo "Note: ${INSTALL_DIR} is not in your PATH."
        echo "Add it with:"
        echo "  export PATH=\"${INSTALL_DIR}:\$PATH\""
        echo ""
        ;;
esac

echo "Get started:"
echo "  fault init        # Create .fault.yaml config"
echo "  fault hook install # Install pre-commit hook"
echo "  fault check       # Run analysis on changed files"
