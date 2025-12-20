# Pre-Publication Cleanup Strategy

**Date**: 2024-12-19
**Status**: Planning Complete, Execution Pending
**Related**: CLEANUP-PLAN.md

## Context

Before publishing `semantics` and `meaning` packages to Hackage, we need to ensure they meet Haskell community standards and eliminate all technical debt. This document describes our systematic approach to identifying, categorizing, and planning the resolution of all issues.

## The Name Conflict Discovery

### Problem

During preparation for publication, we discovered that macOS ships with `/usr/bin/symbols`, a process inspection utility. Publishing our package as `symbols` would:
- Override or conflict with the system utility
- Cause user confusion
- Violate Unix tool naming best practices

### Solution

Renamed the executable from `symbols` to `semantics`:
- **Rationale**: Pairs perfectly with the `meaning` package (semantics ↔ meaning)
- **Scope**: Executable name only; library package remains `symbols`
- **Impact**: All help text, error messages, and documentation updated
- **Branding**: New ASCII art logo generated for splash screen

```
███████╗███████╗███╗   ███╗ █████╗ ███╗   ██╗████████╗██╗ ██████╗███████╗
██╔════╝██╔════╝████╗ ████║██╔══██╗████╗  ██║╚══██╔══╝██║██╔════╝██╔════╝
███████╗█████╗  ██╔████╔██║███████║██╔██╗ ██║   ██║   ██║██║     ███████╗
╚════██║██╔══╝  ██║╚██╔╝██║██╔══██║██║╚██╗██║   ██║   ██║██║     ╚════██║
███████║███████╗██║ ╚═╝ ██║██║  ██║██║ ╚████║   ██║   ██║╚██████╗███████║
╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝ ╚═════╝╚══════╝
```

## Systematic Issue Analysis

We performed a comprehensive audit using multiple tools and methods:

### 1. Compiler Warnings Analysis

**Command**: `cabal build all --ghc-options="-Wall -Wcompat -Wunused-imports -Wunused-top-binds"`

**Findings**: 13 warnings across 3 categories
- **Unused imports** (8): Leftover from refactoring, safe to remove
- **Unused definitions** (3): Fields in `PendingRequest`, pattern variable in Schema parsing
- **Name shadowing** (2): Local variables conflicting with imported names

**Impact**: Low severity but indicates code smell; fixing improves maintainability

### 2. Cabal Packaging Analysis

**Command**: `cabal check` (both packages)

**Findings**: 32 warnings total
- **Missing metadata** (14): category, maintainer, author, description, repository
- **Missing upper bounds** (18): All dependencies lack PVP-compliant upper bounds
- **No source repository**: Missing VCS information

**Impact**: High severity for publication - Hackage requires proper metadata and strongly recommends upper bounds

### 3. Dead Code Detection

**Method**: Manual inspection + build dependency graph

**Findings**:
- `app/Main.hs`: Leftover from old `symbols-cli`, no longer referenced
- `examples/SchemaDiscovery.hs`: Intentionally kept (behind `build-examples` flag)

**Impact**: Minor - one file to delete, but important for cleanliness

### 4. Documentation Audit

**Method**: Manual review of all modules

**Findings**:
- 11 modules lack top-level Haddock (-- |) comments
- README existed but was outdated (referred to old cognition-client)
- No CHANGELOG files
- Architecture docs exist but not referenced from README

**Impact**: Medium severity - documentation is crucial for open source adoption

## The Phased Cleanup Strategy

We organized all issues into **6 sequential phases** with clear success criteria:

### Phase 1: Fix Compiler Warnings (13 tasks)
**Goal**: Zero warnings with `-Wall`
**Estimated effort**: 1-2 hours
**Risk**: Low - mechanical changes only

Subdivided into:
- 1.1 Unused Imports (8 warnings)
- 1.2 Unused Definitions (3 warnings)
- 1.3 Name Shadowing (2 warnings)

**Rationale**: Start with easy wins to build momentum and improve code quality immediately

### Phase 2: Remove Dead Code (1 task)
**Goal**: Every file serves a purpose
**Estimated effort**: 10 minutes
**Risk**: None - file is confirmed unused

Simply delete `app/Main.hs` and verify build succeeds.

### Phase 3: Cabal File Metadata (32 tasks)
**Goal**: `cabal check` shows 0 warnings for both packages
**Estimated effort**: 2-3 hours
**Risk**: Low - mostly data entry

Subdivided into:
- 3.1 symbols.cabal required metadata (8 items)
- 3.2 meaning.cabal required metadata (6 items)
- 3.3 Upper bounds (18 dependencies)

**Rationale**: Required for Hackage publication; must be correct before upload

### Phase 4: Documentation (15 tasks)
**Goal**: All public modules have Haddock, READMEs exist
**Estimated effort**: 3-4 hours
**Risk**: Low - documentation writing

Subdivided into:
- 4.1 Module Documentation (11 modules)
- 4.2 README Files (2 packages)
- 4.3 CHANGELOG creation (2 packages)

**Rationale**: Good documentation is critical for package adoption and user success

### Phase 5: Final Verification (9 tasks)
**Goal**: All quality checks pass
**Estimated effort**: 1 hour
**Risk**: Medium - may uncover new issues

Verification steps:
- Clean build succeeds
- All tests pass
- No warnings with `-Werror`
- Both packages pass `cabal check`
- sdist builds successfully
- Can install from tarballs
- Haddock generates successfully

**Rationale**: Final gate before publication; ensures no regressions

### Phase 6: Publication Preparation (8 tasks)
**Goal**: Ready to upload to Hackage
**Estimated effort**: 1-2 hours
**Risk**: Medium - first-time publication process

Steps:
- Git tagging (v0.1.0.0)
- Hackage account setup
- Candidate upload (dry run)
- Review on candidate server
- Official upload
- Announcement

**Rationale**: Staged deployment minimizes risk of publishing incorrect artifacts

## Design Decisions

### Why Not Automate Everything?

We chose manual execution over automated scripts because:

1. **Learning opportunity**: First Hackage publication; understanding each step is valuable
2. **Quality control**: Manual review catches issues automation might miss
3. **Flexibility**: Can adapt to unexpected issues discovered during cleanup
4. **Documentation**: Manual process forces us to document decisions

### Why Phase Dependencies?

Phases must be executed in order because:
- Compiler warnings might hide dead code
- Cabal metadata affects documentation generation
- Clean builds required before creating distribution artifacts
- All quality checks must pass before publication

### Why Not Fix Everything At Once?

Phased approach provides:
- **Clear checkpoints**: Know exactly where we are in the process
- **Rollback safety**: Can revert a single phase without losing progress
- **Parallelization opportunity**: Multiple people could work on different phases (though we're solo)
- **Psychological wins**: Completing each phase provides motivation

## Success Criteria

The cleanup is **100% complete** when all 9 criteria are met:

1. ✅ Zero compiler warnings with `-Wall`
2. ✅ Zero `cabal check` warnings for both packages
3. ✅ All tests pass
4. ✅ All modules have haddock documentation
5. ✅ READMEs exist and are comprehensive
6. ✅ CHANGELOGs exist
7. ✅ Packages build from sdist tarballs
8. ✅ Haddock generates successfully
9. ✅ Can be uploaded to Hackage

Each criterion maps directly to a phase completion.

## Current Status (2024-12-19)

### Completed
- ✅ Name conflict resolution (`symbols` → `semantics`)
- ✅ Comprehensive README with ASCII art
- ✅ CLEANUP-PLAN.md documentation (78 tasks catalogued)
- ✅ Architecture documentation (this document)
- ✅ Package structure refactoring (`meaning` + `symbols`)
- ✅ Template rendering system
- ✅ Test suite (all passing)

### Pending
- ⏳ Phase 1: Fix compiler warnings (0/13 complete)
- ⏳ Phase 2: Remove dead code (0/1 complete)
- ⏳ Phase 3: Cabal metadata (0/32 complete)
- ⏳ Phase 4: Documentation (0/15 complete)
- ⏳ Phase 5: Final verification (0/9 complete)
- ⏳ Phase 6: Publication (0/8 complete)

**Total progress**: Planning 100%, Execution 0%

## Tool Integration

### Verification Tools Used

```bash
# Compiler warnings
cabal build all --ghc-options="-Wall -Wcompat -Wunused-imports -Wunused-top-binds"

# Package quality
cabal check                    # Run in both root and meaning/
cd meaning && cabal check

# Dead code detection
grep -r "TODO\|FIXME\|XXX\|HACK" --include="*.hs" .

# Documentation
find . -name "*.hs" -not -path "./dist-newstyle/*"

# Build verification
cabal clean && cabal build all
cabal test all
cabal sdist
```

### Continuous Verification

After each phase, run:
```bash
./scripts/verify-cleanup.sh  # (To be created)
```

This script should:
1. Check compiler warnings count
2. Run cabal check on both packages
3. Run all tests
4. Verify documentation coverage
5. Report phase completion status

## Risk Assessment

### Low Risk
- Removing unused imports/definitions
- Adding metadata to cabal files
- Writing documentation
- Renaming executable (already done)

### Medium Risk
- Upper bounds specification (requires dependency knowledge)
- First Hackage upload (unfamiliar process)
- Haddock generation (might reveal doc syntax errors)

### Mitigation Strategies
- **Upper bounds**: Research each dependency's changelog, use conservative ranges
- **Hackage upload**: Use candidate upload first, can delete before official publication
- **Haddock errors**: Fix incrementally during Phase 4

## Future Considerations

### Post-Publication
- Set up CI/CD for automated testing
- Enable Hackage documentation builds
- Monitor package download statistics
- Respond to user issues and PRs

### Versioning Strategy
- Follow PVP (Package Versioning Policy)
- `meaning` and `symbols` versioned independently
- Breaking changes in `meaning` → `symbols` major version bump

### Maintenance
- Quarterly dependency updates
- Annual upper bounds review
- Responsive to Hackage trustee interventions

## Lessons Learned (So Far)

### What Went Well
- Systematic analysis found all issues before they became problems
- Phased approach provides clear roadmap
- Documentation-first strategy ensures nothing is forgotten
- Renaming early avoided publishing with wrong name

### What Could Be Improved
- Should have run `cabal check` earlier in development
- Could have enabled `-Wall` from the start to prevent warning accumulation
- Template system could have been designed with publication in mind

### Process Improvements
- Add `cabal check` to pre-commit hooks
- Enable `-Wall -Werror` in CI/CD
- Maintain CHANGELOG from first commit
- Write module documentation as modules are created

## Conclusion

This cleanup strategy demonstrates that preparation is key to successful open source publication. By systematically identifying all issues, organizing them into logical phases, and documenting our approach, we've created a clear path from "works on my machine" to "published on Hackage."

The 78 tasks across 6 phases represent approximately 8-12 hours of work, but the phased approach means we can tackle this incrementally. Each phase completion brings us closer to publication while maintaining code quality and documentation standards.

**Next step**: Begin Phase 1 (compiler warnings) when ready to execute cleanup.

---

**Status tracking**: See `CLEANUP-PLAN.md` for detailed task checklist
