#!/bin/bash

# Source other scripts
source ./lib/utils.sh
source /etc/profile.d/env.sh
. ./helpers.sh
. /opt/scripts/common.sh

# Exported variables
export DATABASE_URL="postgres://localhost:5432/mydb"
export API_KEY="secret-key-123"
export MAX_RETRIES=3

# Local/declare variables
declare -r CONFIG_FILE="/etc/app/config.yml"
declare -i RETRY_COUNT=0
local_var="not exported"

# Function declarations (name() { style)
setup_database() {
    echo "Setting up database..."
    createdb myapp
}

cleanup_temp_files() {
    rm -rf /tmp/myapp_*
}

# Function declarations (function keyword style)
function run_migrations {
    echo "Running migrations..."
    psql -f migrations.sql
}

function deploy_application {
    local env="${1:-production}"
    echo "Deploying to $env..."
}

# Function with complex signature
process_batch() {
    local input_file="$1"
    local output_dir="$2"
    local batch_size="${3:-100}"

    while IFS= read -r line; do
        echo "$line" >> "$output_dir/output.txt"
    done < "$input_file"
}

# Nested function (still detected at declaration point)
function main {
    setup_database
    run_migrations

    inner_helper() {
        echo "helper"
    }

    deploy_application "staging"
}

# Exported function-like variable
export -f setup_database

main "$@"
