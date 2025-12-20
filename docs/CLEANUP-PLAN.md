# Semantics Package Cleanup Plan

This document outlines the cleanup tasks required before publishing to Hackage.

**Note**: The main executable has been renamed from `symbols` to `semantics` to avoid conflicting with `/usr/bin/symbols` on macOS.

## Summary

- **Total Haskell Files**: 18
- **Compiler Warnings**: 13
- **Cabal Check Issues**: 23 (symbols) + 9 (meaning)
- **Dead Code**: App/Main.hs (unused)
- **Missing Documentation**: Multiple modules

## Phase 1: Fix Compiler Warnings ✓ Complete when: `cabal build all --ghc-options="-Wall" shows 0 warnings`

### 1.1 Unused Imports (8 warnings)

**meaning/src/Plexus/Schema/Cache.hs**
- [ ] Remove redundant qualified import of `Plexus.Schema` (line 30)
- [ ] Remove unused imports: `EnrichedSchema`, `MethodSchema`, `PlexusHash`, `PlexusHashEvent`, `extractHashEvent`, `parseMethodSchemas` (line 31)
- [ ] Remove unused import `Plexus.Types` (line 32)

**src/Plexus/Client.hs**
- [ ] Remove unused import `void` from `Control.Monad` (line 28)

**src/Activation/Cone.hs**
- [ ] Remove unused import `Object` from `Data.Aeson.Types` (line 38)

**app/Dyn.hs**
- [ ] Remove unused import `Data.Maybe` (line 17)
- [ ] Remove unused import `withArgs` from `System.Environment` (line 23)
- [ ] Remove unused qualified import `System.Directory` (line 27)
- [ ] Remove unused imports from `Plexus.Schema`: `ActivationSchemaEvent`, `MethodSchema`, `extractActivationSchemaEvent`, `parseMethodSchemas` (line 33)

### 1.2 Unused Definitions (3 warnings)

**meaning/src/Plexus/Schema.hs**
- [ ] Fix unused pattern variable `val` at line 214 (use `_` or remove)

**src/Plexus/Client.hs**
- [ ] Remove or document unused fields in `PendingRequest`:
  - [ ] `prQueue` (line 61)
  - [ ] `prResponse` (line 62)

### 1.3 Name Shadowing (2 warnings)

**meaning/src/Plexus/Schema/Cache.hs**
- [ ] Rename local variable `pairs` at line 179 (conflicts with `Data.Aeson.pairs`)

**app/Dyn.hs**
- [ ] Rename parameter `action` at line 737 (conflicts with `Options.Applicative.action`)
- [ ] Rename local function `find` at line 763 (shadows outer `find` from line 742)

## Phase 2: Remove Dead Code ✓ Complete when: All files serve a purpose

### 2.1 Unused Executables

**app/Main.hs**
- [ ] Remove `app/Main.hs` (was old symbols-cli, no longer referenced in cabal file)
- [ ] Verify removal: `rm app/Main.hs && cabal build all` succeeds

## Phase 3: Cabal File Metadata ✓ Complete when: `cabal check` shows 0 errors

### 3.1 symbols.cabal Required Metadata

- [ ] Add `category` field (suggestion: "Network, Development, LLM")
- [ ] Add `maintainer` field
- [ ] Add `author` field
- [ ] Add `description` field (multi-line detailed description)
- [ ] Add `homepage` field (link to Codeberg)
- [ ] Add `bug-reports` field (link to Codeberg issues)
- [ ] Add `source-repository` section:
  ```cabal
  source-repository head
    type:     git
    location: https://codeberg.org/hypermemetic/symbols
  ```

### 3.2 meaning.cabal Required Metadata

- [ ] Add `category` field (suggestion: "Network, Data, Types")
- [ ] Add `maintainer` field
- [ ] Add `author` field
- [ ] Add `description` field
- [ ] Add `homepage` field
- [ ] Add `bug-reports` field
- [ ] Add `source-repository` section (same repo, subdirectory: meaning/)

### 3.3 Upper Bounds (PVP Compliance)

**symbols.cabal** - Add upper bounds to all dependencies:
- [ ] meaning (e.g., `meaning >= 0.1 && < 0.2`)
- [ ] aeson (e.g., `>= 2.0 && < 2.3`)
- [ ] aeson-pretty
- [ ] text
- [ ] bytestring
- [ ] websockets
- [ ] network
- [ ] async
- [ ] stm
- [ ] streaming
- [ ] mtl
- [ ] containers
- [ ] scientific
- [ ] vector
- [ ] directory
- [ ] time
- [ ] filepath
- [ ] optparse-applicative
- [ ] stache
- [ ] process

**meaning.cabal** - Add upper bounds:
- [ ] aeson
- [ ] text
- [ ] bytestring
- [ ] containers
- [ ] scientific
- [ ] vector
- [ ] directory
- [ ] time
- [ ] filepath

## Phase 4: Documentation ✓ Complete when: All public modules have haddock

### 4.1 Module Documentation

Add module-level haddock (-- |) to:

**meaning package:**
- [ ] meaning/src/Plexus/Types.hs
- [ ] meaning/src/Plexus/Schema.hs
- [ ] meaning/src/Plexus/Schema/Cache.hs

**symbols package:**
- [ ] src/Plexus.hs
- [ ] src/Plexus/Client.hs
- [ ] src/Plexus/Dynamic.hs
- [ ] src/Plexus/Renderer.hs
- [ ] src/Activation/Arbor.hs
- [ ] src/Activation/Bash.hs
- [ ] src/Activation/Cone.hs
- [ ] src/Activation/Health.hs

### 4.2 README Files

- [ ] Create `meaning/README.md` explaining the package purpose
- [ ] Enhance `README.md` in root with:
  - [ ] Installation instructions
  - [ ] Quick start guide
  - [ ] Example usage
  - [ ] Link to Hackage docs (once published)

### 4.3 CHANGELOG

- [ ] Create `CHANGELOG.md` for symbols (version 0.1.0.0 initial release)
- [ ] Create `meaning/CHANGELOG.md`

## Phase 5: Final Verification ✓ Complete when: All checks pass

### 5.1 Build Verification

- [ ] Clean build succeeds: `cabal clean && cabal build all`
- [ ] All tests pass: `cabal test all`
- [ ] No warnings: `cabal build all --ghc-options="-Wall -Werror"`
- [ ] Cabal check passes: `cabal check` (0 warnings)
- [ ] meaning check passes: `cd meaning && cabal check` (0 warnings)

### 5.2 Package Building

- [ ] sdist builds: `cabal sdist`
- [ ] meaning sdist builds: `cd meaning && cabal sdist`
- [ ] Can install from tarball:
  ```bash
  cabal install dist-newstyle/sdist/symbols-0.1.0.0.tar.gz
  cabal install meaning/dist-newstyle/sdist/meaning-0.1.0.0.tar.gz
  ```

### 5.3 Documentation Generation

- [ ] Haddock builds: `cabal haddock all`
- [ ] Review generated docs look good

## Phase 6: Publication Preparation ✓ Complete when: Ready for Hackage

### 6.1 Git Tagging

- [ ] Tag release: `git tag -a v0.1.0.0 -m "Initial release"`
- [ ] Push tags: `git push origin --tags`

### 6.2 Hackage Account Setup

- [ ] Create Hackage account (if needed)
- [ ] Verify email
- [ ] Set up package maintainer group (if multi-person)

### 6.3 Upload (Dry Run First!)

- [ ] Upload candidate to Hackage (dry run):
  ```bash
  cabal upload --publish meaning/dist-newstyle/sdist/meaning-0.1.0.0.tar.gz
  cabal upload --publish dist-newstyle/sdist/symbols-0.1.0.0.tar.gz
  ```
- [ ] Review package on Hackage candidate server
- [ ] Verify documentation rendered correctly
- [ ] Test installation from Hackage candidate

### 6.4 Official Upload

- [ ] Upload meaning to Hackage (for real)
- [ ] Upload symbols to Hackage (for real)
- [ ] Announce on Haskell Discourse / Reddit / etc.

## Success Criteria

The cleanup is **100% complete** when:

1. ✅ Zero compiler warnings with `-Wall`
2. ✅ Zero `cabal check` warnings for both packages
3. ✅ All tests pass
4. ✅ All modules have haddock documentation
5. ✅ READMEs exist and are comprehensive
6. ✅ CHANGELOGs exist
7. ✅ Packages build from sdist tarballs
8. ✅ Haddock generates successfully
9. ✅ Can be uploaded to Hackage

## Notes

- **Demo code preservation**: `examples/SchemaDiscovery.hs` is kept (gated behind build-examples flag)
- **Test code**: All test files are appropriate and well-structured
- **Architecture docs**: Keep all docs/ files as-is (they're helpful)
