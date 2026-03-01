#!/bin/bash
# Manual benchmark script for synapse (no hyperfine required)
#
# This script runs simple time measurements for synapse operations

set -euo pipefail

# Configuration
SUBSTRATE_HOST="${SUBSTRATE_HOST:-127.0.0.1}"
SUBSTRATE_PORT="${SUBSTRATE_PORT:-4444}"
RUNS=5

# Find synapse binary
SYNAPSE_BIN="/workspace/hypermemetic/synapse/dist-newstyle/build/aarch64-linux/ghc-9.4.8/plexus-synapse-0.3.0.0/x/synapse/build/synapse/synapse"

if [ ! -f "$SYNAPSE_BIN" ]; then
    echo "ERROR: synapse binary not found at $SYNAPSE_BIN"
    echo "Build with: cd synapse && cabal build"
    exit 1
fi

echo "=== Manual Synapse Performance Benchmarks ==="
echo ""
echo "Configuration:"
echo "  Synapse: $SYNAPSE_BIN"
echo "  Substrate: ${SUBSTRATE_HOST}:${SUBSTRATE_PORT}"
echo "  Runs per test: ${RUNS}"
echo ""

# Check substrate connectivity
if ! timeout 5 bash -c "exec 3<>/dev/tcp/${SUBSTRATE_HOST}/${SUBSTRATE_PORT}" 2>/dev/null; then
    echo "ERROR: Cannot connect to substrate at ${SUBSTRATE_HOST}:${SUBSTRATE_PORT}"
    echo "Start substrate with: cd plexus-substrate && cargo run --release"
    exit 1
fi

echo "✓ Substrate is running"
echo ""

# Helper function to run and time a command
time_command() {
    local name="$1"
    local cmd="$2"
    local times=()

    echo "Testing: $name"
    echo "Command: $cmd"

    for i in $(seq 1 $RUNS); do
        echo -n "  Run $i/$RUNS... "
        start=$(date +%s%N)
        eval "$cmd" > /dev/null 2>&1
        end=$(date +%s%N)
        elapsed=$((($end - $start) / 1000000)) # Convert to milliseconds
        times+=($elapsed)
        echo "${elapsed}ms"
    done

    # Calculate average
    total=0
    for t in "${times[@]}"; do
        total=$((total + t))
    done
    avg=$((total / RUNS))

    # Calculate min/max
    min=${times[0]}
    max=${times[0]}
    for t in "${times[@]}"; do
        if [ $t -lt $min ]; then min=$t; fi
        if [ $t -gt $max ]; then max=$t; fi
    done

    echo "  → Average: ${avg}ms (min: ${min}ms, max: ${max}ms)"
    echo ""
}

# Benchmark 1: Parameter-less method (fast path)
time_command \
    "Parameter-less method (substrate.hash)" \
    "$SYNAPSE_BIN -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate hash"

# Benchmark 2: Method with parameters
time_command \
    "Method with parameters (substrate.echo)" \
    "$SYNAPSE_BIN -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate echo once --message test"

# Benchmark 3: Full IR generation
time_command \
    "Full IR generation (synapse -i substrate)" \
    "$SYNAPSE_BIN -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} -i substrate"

# Benchmark 4: Method help (needs IR)
time_command \
    "Method help (substrate.echo --help)" \
    "$SYNAPSE_BIN -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate echo --help"

echo "=== Benchmark Complete ==="
echo ""
echo "Expected results (from Phase 1 optimization):"
echo "  Parameter-less method: <1s (800-900ms)"
echo "  Method with params: 1-2s"
echo "  Full IR generation: 7-8s"
echo "  Method help: 1-2s"
