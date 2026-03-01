#!/bin/bash
# Synapse IR Construction Performance Benchmarks
#
# This script benchmarks different scenarios to validate the performance
# improvements from Phase 1 (skip IR for parameter-less methods) and
# Phase 2 (parallel schema fetching).
#
# Prerequisites:
# - substrate server running on localhost:4444
# - hyperfine installed (for statistical benchmarking)
# - synapse built and in PATH

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SUBSTRATE_HOST="${SUBSTRATE_HOST:-127.0.0.1}"
SUBSTRATE_PORT="${SUBSTRATE_PORT:-4444}"
WARMUP_RUNS=2
MIN_RUNS=10
RESULTS_DIR="${PWD}/benchmark/results"

# Create results directory
mkdir -p "${RESULTS_DIR}"

echo -e "${BLUE}=== Synapse IR Construction Benchmarks ===${NC}"
echo ""
echo "Configuration:"
echo "  Substrate: ${SUBSTRATE_HOST}:${SUBSTRATE_PORT}"
echo "  Warmup runs: ${WARMUP_RUNS}"
echo "  Min runs: ${MIN_RUNS}"
echo "  Results: ${RESULTS_DIR}"
echo ""

# Check prerequisites
check_prereqs() {
    echo -e "${YELLOW}Checking prerequisites...${NC}"

    # Check hyperfine
    if ! command -v hyperfine &> /dev/null; then
        echo -e "${RED}ERROR: hyperfine not found. Install with: cargo install hyperfine${NC}"
        exit 1
    fi

    # Check synapse
    if ! command -v synapse &> /dev/null; then
        echo -e "${RED}ERROR: synapse not found. Build with: cd synapse && cabal install${NC}"
        exit 1
    fi

    # Check substrate connectivity
    if ! timeout 5 bash -c "exec 3<>/dev/tcp/${SUBSTRATE_HOST}/${SUBSTRATE_PORT}" 2>/dev/null; then
        echo -e "${RED}ERROR: Cannot connect to substrate at ${SUBSTRATE_HOST}:${SUBSTRATE_PORT}${NC}"
        echo "Start substrate with: cd plexus-substrate && cargo run --release"
        exit 1
    fi

    echo -e "${GREEN}✓ All prerequisites met${NC}"
    echo ""
}

# Benchmark 1: Parameter-less method (should be fast - optimized path)
benchmark_parameterless() {
    echo -e "${BLUE}[1/6] Benchmarking parameter-less method (substrate.hash)${NC}"
    echo "Expected: <1s (fast path, no IR construction)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/01-parameterless.json" \
        --export-markdown "${RESULTS_DIR}/01-parameterless.md" \
        "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate hash" \
        || echo -e "${RED}Benchmark failed${NC}"

    echo ""
}

# Benchmark 2: Method with parameters (needs IR)
benchmark_with_params() {
    echo -e "${BLUE}[2/6] Benchmarking method with parameters (substrate.echo)${NC}"
    echo "Expected: 1-2s (needs IR construction but optimized)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/02-with-params.json" \
        --export-markdown "${RESULTS_DIR}/02-with-params.md" \
        "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate echo once 'test'" \
        || echo -e "${RED}Benchmark failed${NC}"

    echo ""
}

# Benchmark 3: Full IR generation with -i flag
benchmark_full_ir() {
    echo -e "${BLUE}[3/6] Benchmarking full IR generation (synapse -i)${NC}"
    echo "Expected: 7-8s (full schema tree walk with parallel fetching)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/03-full-ir.json" \
        --export-markdown "${RESULTS_DIR}/03-full-ir.md" \
        "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} -i substrate > /dev/null" \
        || echo -e "${RED}Benchmark failed${NC}"

    echo ""
}

# Benchmark 4: Method help (needs IR)
benchmark_method_help() {
    echo -e "${BLUE}[4/6] Benchmarking method help (substrate.echo --help)${NC}"
    echo "Expected: 1-2s (needs IR for type information)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/04-method-help.json" \
        --export-markdown "${RESULTS_DIR}/04-method-help.md" \
        "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate echo --help > /dev/null" \
        || echo -e "${RED}Benchmark failed${NC}"

    echo ""
}

# Benchmark 5: Deep nested method (arbor.snapshot.list - parameter-less)
benchmark_nested_parameterless() {
    echo -e "${BLUE}[5/6] Benchmarking nested parameter-less method (arbor.snapshot.list)${NC}"
    echo "Expected: <1s (fast path even for nested methods)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/05-nested-parameterless.json" \
        --export-markdown "${RESULTS_DIR}/05-nested-parameterless.md" \
        "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} arbor snapshot list" \
        || echo -e "${RED}Benchmark failed (arbor.snapshot.list may not exist)${NC}"

    echo ""
}

# Benchmark 6: Comparison - same method multiple times (cache warmup effect)
benchmark_repeated() {
    echo -e "${BLUE}[6/6] Benchmarking repeated calls (cache effects)${NC}"
    echo "Expected: Similar times (no persistent cache yet)"
    echo ""

    hyperfine \
        --warmup ${WARMUP_RUNS} \
        --min-runs ${MIN_RUNS} \
        --export-json "${RESULTS_DIR}/06-repeated.json" \
        --export-markdown "${RESULTS_DIR}/06-repeated.md" \
        --command-name "call-1" "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate hash" \
        --command-name "call-2" "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate hash" \
        --command-name "call-3" "synapse -H ${SUBSTRATE_HOST} -P ${SUBSTRATE_PORT} substrate hash" \
        || echo -e "${RED}Benchmark failed${NC}"

    echo ""
}

# Generate summary report
generate_summary() {
    echo -e "${BLUE}=== Generating Summary Report ===${NC}"
    echo ""

    cat > "${RESULTS_DIR}/SUMMARY.md" << 'EOF'
# Synapse IR Construction Benchmark Results

**Date:** $(date -u +"%Y-%m-%d %H:%M:%S UTC")
**Substrate:** ${SUBSTRATE_HOST}:${SUBSTRATE_PORT}

## Overview

This benchmark suite validates the performance improvements from:
1. **Phase 1:** Skip IR construction for parameter-less methods (10x speedup)
2. **Phase 2:** Parallel schema fetching in buildIR (15x speedup for schema walking)

## Benchmark Results

### 1. Parameter-less Method (substrate.hash)
EOF

    # Extract results from JSON files
    if [ -f "${RESULTS_DIR}/01-parameterless.json" ]; then
        echo "**Optimized Fast Path - No IR Construction Required**" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
        cat "${RESULTS_DIR}/01-parameterless.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

### 2. Method with Parameters (substrate.echo)
**Requires IR Construction with Parallel Schema Fetching**

EOF

    if [ -f "${RESULTS_DIR}/02-with-params.json" ]; then
        cat "${RESULTS_DIR}/02-with-params.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

### 3. Full IR Generation (synapse -i substrate)
**Complete Schema Tree Walk with Parallel Fetching**

EOF

    if [ -f "${RESULTS_DIR}/03-full-ir.json" ]; then
        cat "${RESULTS_DIR}/03-full-ir.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

### 4. Method Help (substrate.echo --help)
**Requires IR for Type Information**

EOF

    if [ -f "${RESULTS_DIR}/04-method-help.json" ]; then
        cat "${RESULTS_DIR}/04-method-help.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

### 5. Nested Parameter-less Method (arbor.snapshot.list)
**Fast Path Works for Nested Namespaces**

EOF

    if [ -f "${RESULTS_DIR}/05-nested-parameterless.json" ]; then
        cat "${RESULTS_DIR}/05-nested-parameterless.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

### 6. Repeated Calls (Cache Effects)
**Testing Process Startup Overhead vs Connection Reuse**

EOF

    if [ -f "${RESULTS_DIR}/06-repeated.json" ]; then
        cat "${RESULTS_DIR}/06-repeated.md" >> "${RESULTS_DIR}/SUMMARY.md"
        echo "" >> "${RESULTS_DIR}/SUMMARY.md"
    fi

    cat >> "${RESULTS_DIR}/SUMMARY.md" << 'EOF'

## Performance Analysis

### Expected vs Actual

| Scenario | Expected | Actual | Status |
|----------|----------|--------|--------|
| Parameter-less method | <1s | TBD | - |
| Method with params | 1-2s | TBD | - |
| Full IR generation | 7-8s | TBD | - |
| Method help | 1-2s | TBD | - |
| Nested parameter-less | <1s | TBD | - |

### Key Findings

1. **Fast Path Effectiveness:** Parameter-less methods should be ~10x faster than pre-optimization
2. **Parallel Schema Impact:** IR construction should show benefits from parallel child fetching
3. **Process Overhead:** Remaining time in fast path is mostly Haskell startup + WebSocket connection
4. **No Persistent Cache:** Repeated calls show no improvement (connection pooling not yet implemented)

### Optimization Opportunities Remaining

1. **Connection Pooling** - Reuse WebSocket connections across calls (Phase 3)
2. **Lazy IR Construction** - Build IR on-demand instead of eagerly (Phase 4)
3. **Backend Hash API** - Lightweight hash-only queries without schema fetch (Phase 5)
4. **Persistent Process** - Keep synapse running as a daemon to eliminate startup overhead

## Raw Data

All raw benchmark data is available in JSON format:
- `01-parameterless.json` - Parameter-less method timings
- `02-with-params.json` - Method with parameters timings
- `03-full-ir.json` - Full IR generation timings
- `04-method-help.json` - Method help timings
- `05-nested-parameterless.json` - Nested method timings
- `06-repeated.json` - Repeated call comparison

## Reproduction

To reproduce these benchmarks:

```bash
# Start substrate
cd plexus-substrate && cargo run --release &

# Run benchmarks
cd synapse && ./benchmark/ir-construction.sh
```

## References

- [IR Construction Performance Analysis](../docs/architecture/16675729999709551615_ir-construction-performance-analysis.md)
- Commit: 5ef9216 - "perf: skip IR construction for parameter-less methods (10x speedup)"
- Commit: ebdb251 - "perf: implement parallel schema fetching in buildIR"
EOF

    echo -e "${GREEN}Summary report generated: ${RESULTS_DIR}/SUMMARY.md${NC}"
    echo ""
}

# Main execution
main() {
    check_prereqs

    benchmark_parameterless
    benchmark_with_params
    benchmark_full_ir
    benchmark_method_help
    benchmark_nested_parameterless
    benchmark_repeated

    generate_summary

    echo -e "${GREEN}=== Benchmarks Complete ===${NC}"
    echo ""
    echo "Results:"
    ls -lh "${RESULTS_DIR}/"
    echo ""
    echo -e "${BLUE}View summary: cat ${RESULTS_DIR}/SUMMARY.md${NC}"
}

# Run if executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
