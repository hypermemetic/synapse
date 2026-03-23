# Comprehensive Testing: Implementation Plan

## Executive Summary

This document provides a concrete implementation plan for the comprehensive multi-layer testing architecture. The goal is to achieve **90%+ automated verification coverage** across schema → IR → codegen → wire format → runtime behavior.

## What We're Building

```
Schema Types ──┬──> Property Tests ──> QuickCheck
               ├──> Refinement Types ──> Compile-time Guarantees
               └──> GADTs ──> Invalid States Unrepresentable

IR Construction ──┬──> Round-trip Tests ──> Schema ↔ IR Identity
                  ├──> Soundness Checks ──> No Dangling Refs
                  └──> Completeness Tests ──> No Data Loss

Codegen ──┬──> Template Validation ──> Syntax Correct
          ├──> Compilation Tests ──> Generated Code Compiles
          ├──> Property Preservation ──> IR Props → Code Props
          └──> Mutation Testing ──> Templates Robust

Wire Format ──┬──> Property-based Tests ──> Generate from TLA+
              ├──> Fuzzing ──> Handle Malformed Messages
              ├──> Trace Validation ──> Actual vs Spec
              └──> Compliance Tests ──> Already Built!

Cross-Language ──┬──> Compatibility Matrix ──> All Pairs
                 ├──> Round-trip Tests ──> TS → HS → Rust → TS
                 └──> Type Isomorphism ──> Types Equivalent
```

**Timeline**: 10 weeks
**Effort**: 180-230 hours
**Team**: 1-2 developers
**ROI**: Massive - prevent entire classes of bugs

---

## Week-by-Week Plan

### Week 1: Schema Refinement Types

**Goal**: Basic type safety with `refined` library

**Tasks**:
1. Add `refined` dependency to synapse.cabal
2. Create `Synapse.Types.Refined` module
3. Define refined types:
   - `PlexusHash` (16 hex chars)
   - `Timestamp` (positive Int64)
   - `Namespace` (lowercase, no spaces)
   - `MethodName` (valid identifier)
   - `SnakeCaseField` (snake_case only)

4. Update `Synapse.Schema.Types` to use refined types
5. Update parsers/serializers
6. Fix compilation errors

**Deliverables**:
- `src/Synapse/Types/Refined.hs` (150 lines)
- Updated `src/Synapse/Schema/Types.hs`
- All tests pass

**Success Criteria**:
- ✅ Invalid hashes rejected at construction
- ✅ Negative timestamps impossible
- ✅ Empty namespaces impossible
- ✅ CamelCase fields rejected

### Week 2: Property-Based Testing Foundation

**Goal**: QuickCheck generators and basic properties

**Tasks**:
1. Create `test/Synapse/Schema/Gen.hs`
2. Implement generators:
   - `genNamespace`
   - `genMethodDef`
   - `genTypeDef`
   - `genSchema`

3. Write properties:
   - `prop_genSchema_valid` - Generated schemas valid
   - `prop_schema_hash_deterministic` - Hash consistency
   - `prop_schema_json_roundtrip` - JSON serialization

4. Add to test suite
5. Run with `quickCheckWith stdArgs { maxSuccess = 10000 }`

**Deliverables**:
- `test/Synapse/Schema/Gen.hs` (300 lines)
- `test/Synapse/Schema/Properties.hs` (200 lines)
- 10,000+ passing property tests

**Success Criteria**:
- ✅ 10,000 random schemas generated and validated
- ✅ All round-trip tests pass
- ✅ Hash determinism verified

### Week 3: IR Round-Trip Testing

**Goal**: Verify Schema ↔ IR is bijective (or at least semantically equivalent)

**Tasks**:
1. Create `test/Synapse/IR/RoundTrip.hs`
2. Implement `irToSchema :: IR -> Either Error Schema`
3. Write properties:
   - `prop_schema_ir_roundtrip`
   - `prop_ir_transformations_preserve_semantics`

4. Create `test/Synapse/IR/Resolution.hs`
5. Write properties:
   - `prop_all_refs_resolve`
   - `prop_circular_refs_detected`

6. Create `test/Synapse/IR/Soundness.hs`
7. Write properties:
   - `prop_all_types_wellformed`
   - `prop_substitution_preserves_wellformedness`

**Deliverables**:
- `test/Synapse/IR/RoundTrip.hs` (200 lines)
- `test/Synapse/IR/Resolution.hs` (150 lines)
- `test/Synapse/IR/Soundness.hs` (150 lines)
- Enhanced `IRSpec.hs` with property tests

**Success Criteria**:
- ✅ Schema → IR → Schema is identity
- ✅ All type refs resolve
- ✅ No circular references
- ✅ Type system is sound

### Week 4: GADTs for Wire Format

**Goal**: Make metadata requirements impossible to violate

**Tasks**:
1. Create `src/Synapse/Types/WireFormat.hs`
2. Define ItemType:
   ```haskell
   data ItemType = Data | Error | Done | Progress | Request
   ```

3. Define GADT:
   ```haskell
   data StreamItem (t :: ItemType) where
     DataItem :: StreamMetadata -> ... -> StreamItem 'Data
     RequestItem :: ... -> StreamItem 'Request  -- No metadata!
   ```

4. Update serialization/deserialization
5. Update codegen to generate GADTs
6. Fix compilation errors

**Deliverables**:
- `src/Synapse/Types/WireFormat.hs` (250 lines)
- Updated codegen templates
- Type-safe wire format

**Success Criteria**:
- ✅ Can't construct DataItem without metadata
- ✅ RequestItem has no metadata field
- ✅ Wire format enforced by types

### Week 5: Template Validation

**Goal**: Ensure codegen templates are correct

**Tasks**:
1. Create `test/Synapse/Codegen/Template.hs`
2. Implement template parser
3. Write validators:
   - `checkUnboundVars`
   - `checkMissingPartials`
   - `checkInfiniteLoops`

4. Create `test/Synapse/Codegen/Compilation.hs`
5. Implement compilation testers:
   - `compileHaskellCode`
   - `compileTypeScriptCode`

6. Write properties:
   - `prop_all_templates_valid`
   - `prop_generated_haskell_compiles`
   - `prop_generated_typescript_compiles`

**Deliverables**:
- `test/Synapse/Codegen/Template.hs` (300 lines)
- `test/Synapse/Codegen/Compilation.hs` (250 lines)
- Template validation CI step

**Success Criteria**:
- ✅ All templates parse and validate
- ✅ Generated Haskell compiles
- ✅ Generated TypeScript passes tsc

### Week 6: Property-Based Wire Format Testing

**Goal**: Generate test cases from TLA+ spec

**Tasks**:
1. Create `test/Synapse/Wire/PropertyBased.hs`
2. Implement generators:
   - `genPlexusMessage`
   - `genStreamItem` (with integer timestamps!)
   - `genMetadata`

3. Write properties:
   - `prop_server_accepts_valid_messages`
   - `prop_message_serialization`
   - `prop_metadata_has_integer_timestamps`

4. Integrate with existing compliance tests
5. Run against actual servers

**Deliverables**:
- `test/Synapse/Wire/PropertyBased.hs` (400 lines)
- Integration with pytest compliance tests
- 1000+ generated test cases

**Success Criteria**:
- ✅ Property tests generate valid messages
- ✅ All messages accepted by servers
- ✅ Integer timestamp property holds

### Week 7: Fuzzing Framework

**Goal**: Find edge cases and crashes

**Tasks**:
1. Create `test/Synapse/Wire/Fuzzing.hs`
2. Implement fuzzers:
   - `genFuzzedMessage`
   - `genMalformedMessage`
   - `genEdgeCases`

3. Write properties:
   - `prop_server_handles_fuzzed_messages`

4. Run fuzzer for extended periods
5. Document all crashes/hangs found
6. Fix issues in servers

**Deliverables**:
- `test/Synapse/Wire/Fuzzing.hs` (300 lines)
- Fuzzing CI job (runs overnight)
- Bug reports for servers

**Success Criteria**:
- ✅ 10,000+ fuzzed messages sent
- ✅ No server crashes
- ✅ All errors handled gracefully

### Week 8: Cross-Language Compatibility Matrix

**Goal**: Test all language pair combinations

**Tasks**:
1. Create `test/Synapse/CrossLang/Matrix.hs`
2. Define language pairs:
   ```haskell
   [(Haskell, TypeScript), (Haskell, Rust),
    (TypeScript, Rust), ...]
   ```

3. Implement test runner:
   - Start server in language A
   - Run client in language B
   - Verify correctness

4. Create `test/Synapse/CrossLang/RoundTrip.hs`
5. Implement round-trip tests:
   - TS → Haskell → TS
   - Haskell → Rust → Haskell
   - etc.

6. Write properties:
   - `prop_all_pairs_compatible`
   - `prop_typescript_haskell_roundtrip`

**Deliverables**:
- `test/Synapse/CrossLang/Matrix.hs` (350 lines)
- `test/Synapse/CrossLang/RoundTrip.hs` (300 lines)
- CI matrix job

**Success Criteria**:
- ✅ All language pairs tested
- ✅ All round-trips preserve values
- ✅ CI runs full matrix

### Week 9: Mutation Testing

**Goal**: Verify tests catch bugs

**Tasks**:
1. Create `test/Synapse/Schema/Mutation.hs`
2. Define mutations:
   - Remove namespace
   - Invalid hash format
   - Duplicate method names
   - Unresolved type ref

3. Write properties:
   - `prop_mutations_caught`

4. Create `test/Synapse/Codegen/Mutation.hs`
5. Define template mutations:
   - Remove required param
   - Change type to any
   - Remove streaming support

6. Write properties:
   - `prop_mutations_break_tests`

**Deliverables**:
- `test/Synapse/Schema/Mutation.hs` (250 lines)
- `test/Synapse/Codegen/Mutation.hs` (300 lines)
- Mutation testing CI job

**Success Criteria**:
- ✅ All schema mutations caught
- ✅ All template mutations caught
- ✅ Test suite is robust

### Week 10: Integration and Documentation

**Goal**: Tie everything together, document, polish

**Tasks**:
1. Create comprehensive test runner:
   ```bash
   ./test-all.sh
   ```

2. Write documentation:
   - Testing guide
   - Property testing examples
   - CI configuration

3. Add to CI pipeline:
   ```yaml
   - name: Property Tests
     run: cabal test property-tests
   - name: Fuzzing
     run: cabal test fuzzing --test-options="+RTS -N4"
   - name: Cross-Language Matrix
     run: ./test-matrix.sh
   ```

4. Performance tuning:
   - Parallelize tests
   - Optimize generators
   - Cache compilations

5. Create test report dashboard

**Deliverables**:
- `docs/testing-guide.md`
- CI integration complete
- Test dashboard

**Success Criteria**:
- ✅ All tests run in CI
- ✅ Documentation complete
- ✅ < 10 minute test run

---

## Quick Wins (First 2 Weeks)

If you only have 2 weeks, focus on these high-impact items:

### Week 1: Refinement Types
- Add `refined` library
- Create `PlexusHash`, `Timestamp`, `SnakeCaseField` types
- Update schema types to use refinements
- **Impact**: Prevent float timestamps, invalid hashes at compile time

### Week 2: Property-Based IR Testing
- Create QuickCheck generators
- Write round-trip property (`prop_schema_ir_roundtrip`)
- Write resolution property (`prop_all_refs_resolve`)
- **Impact**: Catch IR bugs automatically with 10,000+ test cases

**Result**: 80% of benefit with 20% of effort

---

## Maintenance Plan

### Daily
- Run property tests in CI (< 5 minutes)
- Monitor for test failures

### Weekly
- Review fuzzing results (overnight runs)
- Add new properties for new features

### Monthly
- Run extended fuzzing (1M+ test cases)
- Review cross-language matrix results
- Update generators for new schema features

### Quarterly
- Mutation testing audit
- Performance optimization
- Documentation updates

---

## Success Metrics

### Coverage Targets

| Layer | Target | How to Measure |
|-------|--------|----------------|
| Schema | 90% | Property tests pass on 10,000 schemas |
| IR | 95% | Round-trip tests pass, all refs resolve |
| Codegen | 85% | Generated code compiles for all schemas |
| Wire Format | 90% | Fuzzer finds no crashes in 100K tests |
| Cross-Language | 95% | All pairs in compatibility matrix pass |

### Quality Gates

**Gate 1: Compilation**
- All refined types compile
- No type errors with GADTs
- Generated code compiles

**Gate 2: Property Tests**
- 10,000+ schemas generated and valid
- All round-trip tests pass
- All ref resolution tests pass

**Gate 3: Fuzzing**
- 100,000+ messages sent
- No crashes or hangs
- All errors handled gracefully

**Gate 4: Cross-Language**
- All language pairs tested
- All round-trips succeed
- No serialization failures

**Gate 5: Mutation Testing**
- 100% of mutations caught
- Test suite is robust
- No false positives

---

## Cost-Benefit Analysis

### Costs

**Development Time**: 180-230 hours over 10 weeks
**Learning Curve**: Team learns advanced types, property testing
**Maintenance**: ~4 hours/week ongoing
**CI Resources**: ~20 minutes extra per build

### Benefits

**Bug Prevention**:
- Float timestamp bug: **Impossible** (caught at compile time)
- CamelCase bug: **Impossible** (caught at compile time)
- Missing metadata: **Impossible** (enforced by GADTs)
- Unresolved refs: **Caught automatically** (property tests)
- Interop bugs: **Caught in matrix** (all pairs tested)

**Estimated Savings**:
- 20+ hours/month debugging interop issues
- 10+ hours/month fixing schema bugs
- 5+ hours/month investigating production errors
- **35+ hours/month saved**

**ROI**: 35 hours saved/month / 4 hours maintenance = **8.75x return**

Plus: Confidence to refactor, faster feature development, fewer production incidents

---

## Risk Mitigation

### Risk: Advanced types too hard
**Mitigation**: Start simple (refined), add complexity gradually, provide training

### Risk: Property tests too slow
**Mitigation**: Parallelize, optimize generators, run subset in PR checks

### Risk: False positives break CI
**Mitigation**: Clear failure messages, easy triage, quarantine flaky tests

### Risk: Never finish all 10 weeks
**Mitigation**: Each week delivers standalone value, can stop after any week

---

## Recommended Approach

### Option A: Full Implementation (10 weeks)
**Best for**: Teams committed to long-term quality
**Result**: 90%+ verification coverage, maximum confidence

### Option B: Core Features (5 weeks)
**Best for**: Teams with time constraints
**Focus**: Weeks 1-5 (refinements, properties, GADTs, templates)
**Result**: 70% coverage, high-impact wins

### Option C: Quick Wins (2 weeks)
**Best for**: Teams wanting immediate results
**Focus**: Refinement types + property-based IR testing
**Result**: 40% coverage, biggest bugs prevented

**Recommendation**: Start with Option C, evaluate, proceed to B or A based on results.

---

## Next Steps

### Immediate (This Week)
1. Review this plan with team
2. Get buy-in for at least Option C (2 weeks)
3. Set up development branch
4. Add `refined` dependency

### Week 1 (Starting Monday)
1. Kick off Week 1 tasks
2. Daily standups to track progress
3. Pair programming for advanced types
4. Document learnings as we go

### Week 2 Checkpoint
1. Review Week 1 deliverables
2. Run property tests
3. Measure compilation error rate
4. Decide: continue to Week 3 or stop?

---

## Conclusion

This plan transforms Plexus RPC from "tested" to "proven correct" at every layer.

**The Goal**: If it type-checks and property tests pass, it **cannot** violate the protocol specification.

**Status**: 🟢 Plan Complete, Ready to Execute
**Next Step**: Get team approval, start Week 1
**Timeline**: 10 weeks for full coverage, 2 weeks for quick wins

---

**Created**: 2026-03-20
**Author**: Claude Sonnet 4.5
**Status**: Ready for Implementation
**Effort**: 180-230 hours over 10 weeks
**ROI**: 8.75x return on investment
