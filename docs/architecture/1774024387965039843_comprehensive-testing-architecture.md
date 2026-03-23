# Comprehensive Multi-Layer Testing Architecture

## Vision

**Goal**: Achieve mathematical certainty that schema → IR → codegen → implementation chain is correct at every layer, with automated verification that catches bugs before they reach production.

**Philosophy**: Test properties, not examples. Prove correctness, don't just exercise code.

## Executive Summary

This document outlines a comprehensive testing strategy spanning all layers of the Plexus RPC ecosystem:

```
┌─────────────────────────────────────────────────────────────────────┐
│                     COMPREHENSIVE TESTING LAYERS                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  Layer 1: Schema Definition                                          │
│  ├─ Type system enforcement (refinement types)                       │
│  ├─ Schema validation (JSON Schema + custom rules)                   │
│  └─ Property-based schema generation                                 │
│                                                                       │
│  Layer 2: IR Construction                                            │
│  ├─ Round-trip testing (Schema → IR → Schema)                        │
│  ├─ Reference resolution verification                                │
│  ├─ Type soundness checking                                          │
│  └─ Completeness proofs (all schema info preserved)                  │
│                                                                       │
│  Layer 3: Code Generation                                            │
│  ├─ Template validation (syntax, logic)                              │
│  ├─ Generated code compilation                                       │
│  ├─ Property preservation (IR properties → code properties)          │
│  └─ Mutation testing (change template, should break tests)           │
│                                                                       │
│  Layer 4: Wire Format                                                │
│  ├─ Protocol conformance (TLA+ spec compliance)                      │
│  ├─ Fuzzing (malformed messages, edge cases)                         │
│  ├─ Property-based testing (generate from TLA+ spec)                 │
│  └─ Cross-implementation validation                                  │
│                                                                       │
│  Layer 5: Runtime Behavior                                           │
│  ├─ Trace validation (actual traces vs TLA+ spec)                    │
│  ├─ Property-based integration tests                                 │
│  ├─ Refinement checking (code refines spec)                          │
│  └─ Continuous monitoring (production trace validation)              │
│                                                                       │
│  Layer 6: Cross-Language Compatibility                               │
│  ├─ Compatibility matrix (all pairs tested)                          │
│  ├─ Round-trip testing (TS → Haskell → Rust → TS)                   │
│  ├─ Type isomorphism verification                                    │
│  └─ Interop regression suite                                         │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

**Status**: 🟡 25% implemented (IR tests exist, compliance tests created)
**Target**: 🟢 90% automated verification coverage
**Timeline**: 6-8 weeks for full implementation

---

## Layer 1: Schema Definition Testing

### 1.1 Type System Enforcement

**Goal**: Use Haskell's type system to enforce schema validity at compile time.

#### Refinement Types for Schema Constraints

```haskell
-- Use LiquidHaskell or refined library
module Synapse.Schema.Refined where

import Refined

-- Hash must be exactly 16 hex characters
type PlexusHash = Refined (SizeEqualTo 16 && Hex) Text

-- Timestamp must be positive integer (Unix epoch)
type Timestamp = Refined Positive Int64

-- Namespace must be non-empty, lowercase, no spaces
type Namespace = Refined (NonEmpty && LowerCase && NoSpaces) Text

-- Method name must be valid identifier
type MethodName = Refined ValidIdentifier Text

-- Schema definition with refined types
data SchemaMetadata = SchemaMetadata
  { smPlexusHash :: PlexusHash        -- Compile-time validated!
  , smTimestamp  :: Timestamp         -- Cannot be negative!
  , smProvenance :: NonEmpty Text     -- Cannot be empty!
  , smNamespace  :: Namespace         -- Cannot have spaces!
  }

-- Constructor that enforces constraints
mkSchemaMetadata :: Text -> Int64 -> NonEmpty Text -> Text -> Either RefineException SchemaMetadata
mkSchemaMetadata hash ts prov ns = do
  hash' <- refine hash
  ts'   <- refine ts
  ns'   <- refine ns
  pure $ SchemaMetadata hash' ts' prov ns'

-- This won't compile:
-- bad = SchemaMetadata "too-long-hash" (-42) [] "Has Spaces"
```

#### Dependent Types for Protocol Properties

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Synapse.Schema.Dependent where

import GHC.TypeLits
import Data.Proxy

-- Stream item must have metadata (except request type)
data StreamItemType = Data | Error | Done | Progress | Request

-- Type-level predicate: does this item type require metadata?
type family RequiresMetadata (t :: StreamItemType) :: Bool where
  RequiresMetadata 'Request = 'False
  RequiresMetadata _        = 'True

-- GADT that enforces metadata requirement at type level
data StreamItem (t :: StreamItemType) where
  DataItem ::
    { diMetadata :: StreamMetadata
    , diContentType :: Text
    , diContent :: Value
    } -> StreamItem 'Data

  ErrorItem ::
    { eiMetadata :: StreamMetadata
    , eiMessage :: Text
    , eiCode :: Maybe Text
    } -> StreamItem 'Error

  DoneItem ::
    { diMetadata :: StreamMetadata
    } -> StreamItem 'Done

  ProgressItem ::
    { piMetadata :: StreamMetadata
    , piMessage :: Text
    , piPercentage :: Refined (GreaterThan 0 && LessThan 101) Int
    } -> StreamItem 'Progress

  RequestItem ::
    { riRequestId :: Text
    , riRequestData :: Value
    , riTimeoutMs :: Refined Positive Int
    } -> StreamItem 'Request  -- No metadata field!

-- Cannot construct invalid items:
-- bad = RequestItem metadata "id" val 1000  -- Type error: RequestItem doesn't take metadata!
```

#### Schema DSL with Type-Safe Builders

```haskell
module Synapse.Schema.Builder where

-- Phantom types for validation state
data Validated
data Unvalidated

data SchemaBuilder a = SchemaBuilder
  { sbNamespace :: Maybe Text
  , sbVersion :: Maybe Text
  , sbMethods :: [MethodDef]
  , sbTypes :: [TypeDef]
  }

-- Start with unvalidated builder
newSchema :: SchemaBuilder Unvalidated
newSchema = SchemaBuilder Nothing Nothing [] []

-- Validation functions that change phantom type
setNamespace :: Text -> SchemaBuilder Unvalidated -> SchemaBuilder Unvalidated
setNamespace ns sb = sb { sbNamespace = Just ns }

setVersion :: Text -> SchemaBuilder Unvalidated -> SchemaBuilder Unvalidated
setVersion v sb = sb { sbVersion = Just v }

-- Can only build if validated
build :: SchemaBuilder Validated -> Schema
build sb = Schema
  { sNamespace = fromJust (sbNamespace sb)  -- Safe: type guarantees Just
  , sVersion = fromJust (sbVersion sb)
  , sMethods = sbMethods sb
  , sTypes = sbTypes sb
  }

-- Validation step changes phantom type
validate :: SchemaBuilder Unvalidated -> Either ValidationError (SchemaBuilder Validated)
validate sb
  | isNothing (sbNamespace sb) = Left "Missing namespace"
  | isNothing (sbVersion sb) = Left "Missing version"
  | null (sbMethods sb) = Left "No methods defined"
  | otherwise = Right (unsafeCoerce sb)  -- Phantom type change

-- Usage enforces validation:
example = do
  schema <- validate $ newSchema
    & setNamespace "cone"
    & setVersion "1.0.0"
    & addMethod (method "chat" ...)

  return $ build schema  -- OK: validated

-- This won't compile:
-- bad = build newSchema  -- Type error: expected Validated, got Unvalidated
```

### 1.2 Schema Property Testing

**Goal**: Generate valid schemas, test properties, find edge cases.

#### QuickCheck Generators for Schemas

```haskell
module Synapse.Schema.Gen where

import Test.QuickCheck

-- Generate valid namespace
genNamespace :: Gen Text
genNamespace = do
  parts <- listOf1 $ resize 10 $ listOf1 $ elements ['a'..'z']
  return $ T.intercalate "." $ map T.pack parts

-- Generate valid hash (16 hex chars)
genPlexusHash :: Gen Text
genPlexusHash = do
  chars <- vectorOf 16 $ elements "0123456789abcdef"
  return $ T.pack chars

-- Generate valid method definition
genMethodDef :: Gen MethodDef
genMethodDef = do
  name <- genMethodName
  params <- listOf genParamDef
  returns <- genTypeRef
  streaming <- arbitrary
  return $ MethodDef name "" params returns streaming False

-- Generate entire schema
genSchema :: Gen Schema
genSchema = do
  namespace <- genNamespace
  version <- genVersion
  methods <- listOf1 genMethodDef  -- At least one method
  types <- listOf genTypeDef
  return $ Schema namespace version "" methods types

-- Property: All generated schemas are valid
prop_genSchema_valid :: Property
prop_genSchema_valid = forAll genSchema $ \schema ->
  case validateSchema schema of
    Left err -> counterexample (show err) False
    Right _ -> True

-- Property: Schema hash is deterministic
prop_schema_hash_deterministic :: Property
prop_schema_hash_deterministic = forAll genSchema $ \schema ->
  let hash1 = computeSchemaHash schema
      hash2 = computeSchemaHash schema
  in hash1 === hash2

-- Property: Schema round-trips through JSON
prop_schema_json_roundtrip :: Property
prop_schema_json_roundtrip = forAll genSchema $ \schema ->
  case decode (encode schema) of
    Nothing -> counterexample "Failed to decode" False
    Just schema' -> schema === schema'
```

### 1.3 Schema Mutation Testing

**Goal**: Intentionally break schemas, verify validators catch it.

```haskell
module Synapse.Schema.Mutation where

-- Mutators that produce invalid schemas
data SchemaMutation
  = RemoveNamespace
  | EmptyNamespace
  | InvalidHashFormat
  | DuplicateMethodNames
  | UnresolvedTypeRef
  | CyclicTypeDef
  deriving (Enum, Bounded)

-- Apply mutation to schema
mutateSchema :: SchemaMutation -> Schema -> Schema
mutateSchema RemoveNamespace s = s { sNamespace = "" }
mutateSchema EmptyNamespace s = s { sNamespace = "" }
mutateSchema InvalidHashFormat s = s { sHash = "invalid" }
mutateSchema DuplicateMethodNames s =
  let m = head (sMethods s)
  in s { sMethods = m : m : tail (sMethods s) }
-- ... more mutations ...

-- Property: All mutations should be caught by validator
prop_mutations_caught :: Schema -> SchemaMutation -> Property
prop_mutations_caught validSchema mutation =
  let mutated = mutateSchema mutation validSchema
      result = validateSchema mutated
  in counterexample (show mutation) $
    case result of
      Left _ -> property True  -- Good: caught the mutation
      Right _ -> property False -- Bad: mutation not caught!

-- Run all mutations
testAllMutations :: Schema -> IO ()
testAllMutations schema = do
  forM_ [minBound..maxBound] $ \mutation -> do
    let mutated = mutateSchema mutation schema
    case validateSchema mutated of
      Left err -> putStrLn $ "✓ " <> show mutation <> ": caught"
      Right _ -> error $ "✗ " <> show mutation <> ": NOT CAUGHT!"
```

---

## Layer 2: IR Construction Testing

### 2.1 Round-Trip Testing

**Goal**: Schema → IR → Schema should be identity (or at least semantically equivalent).

```haskell
module Synapse.IR.RoundTrip where

-- Test: Schema → IR → Schema
prop_schema_ir_roundtrip :: Property
prop_schema_ir_roundtrip = forAll genSchema $ \schema ->
  case buildIR schema of
    Left err -> counterexample (show err) False
    Right ir ->
      case irToSchema ir of
        Left err -> counterexample (show err) False
        Right schema' -> schema `semanticalllyEqual` schema'

-- Semantic equality (not structural - may reorder things)
semanticallyEqual :: Schema -> Schema -> Bool
semanticallyEqual s1 s2 =
  and [ sNamespace s1 == sNamespace s2
      , sVersion s1 == sVersion s2
      , Set.fromList (sMethods s1) == Set.fromList (sMethods s2)
      , Set.fromList (sTypes s1) == Set.fromList (sTypes s2)
      ]

-- Test: IR transformations preserve semantics
prop_ir_transformations_preserve_semantics :: Property
prop_ir_transformations_preserve_semantics = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      let ir' = optimizeIR ir  -- Some transformation
          schema' = irToSchema ir'
          schema'' = irToSchema ir
      in schema' `semanticallyEqual` schema''
```

### 2.2 Reference Resolution Verification

**Goal**: All TypeRef references resolve, no dangling pointers.

```haskell
module Synapse.IR.Resolution where

-- Property: All type refs resolve
prop_all_refs_resolve :: Property
prop_all_refs_resolve = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      let unresolvedRefs = findUnresolvedRefs ir
      in counterexample (show unresolvedRefs) $
        null unresolvedRefs

-- Find all unresolved references
findUnresolvedRefs :: IR -> [QualifiedName]
findUnresolvedRefs ir =
  let allRefs = gatherAllTypeRefs ir
      defined = Map.keysSet (irTypes ir)
  in filter (`Set.notMember` defined) allRefs

-- Gather all type refs from IR
gatherAllTypeRefs :: IR -> [QualifiedName]
gatherAllTypeRefs ir = concat
  [ extractRefs (mdParams method) <> extractRefs [mdReturns method]
  | method <- Map.elems (irMethods ir)
  ]
  where
    extractRefs :: [ParamDef] -> [QualifiedName]
    extractRefs = concatMap (extractFromTypeRef . pdType)

    extractFromTypeRef :: TypeRef -> [QualifiedName]
    extractFromTypeRef (RefNamed qn) = [qn]
    extractFromTypeRef (RefArray inner) = extractFromTypeRef inner
    extractFromTypeRef (RefOptional inner) = extractFromTypeRef inner
    extractFromTypeRef _ = []

-- Property: Circular references are detected
prop_circular_refs_detected :: Property
prop_circular_refs_detected = forAll genSchemaWithCircularRef $ \schema ->
  case buildIR schema of
    Left (CircularReference _) -> property True  -- Good: detected
    Left _ -> counterexample "Wrong error" False
    Right _ -> counterexample "Circular ref not detected!" False
```

### 2.3 Type Soundness Checking

**Goal**: IR type system is sound (no runtime type errors).

```haskell
module Synapse.IR.Soundness where

-- Well-formedness of types
isWellFormedType :: IR -> TypeRef -> Bool
isWellFormedType ir = \case
  RefNamed qn -> Map.member (qualifiedNameFull qn) (irTypes ir)
  RefArray inner -> isWellFormedType ir inner
  RefOptional inner -> isWellFormedType ir inner
  RefPrimitive _ _ -> True
  RefAny -> True
  RefUnknown -> True

-- Property: All types in IR are well-formed
prop_all_types_wellformed :: Property
prop_all_types_wellformed = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      let allTypeRefs = gatherAllTypeRefs ir
      in all (isWellFormedType ir) allTypeRefs

-- Property: Type substitution preserves well-formedness
prop_substitution_preserves_wellformedness :: Property
prop_substitution_preserves_wellformedness =
  forAll ((,) <$> genSchema <*> genTypeSubstitution) $ \(schema, subst) ->
    case buildIR schema of
      Left _ -> discard
      Right ir ->
        let ir' = applySubstitution subst ir
        in prop_all_types_wellformed ir'
```

### 2.4 Completeness Testing

**Goal**: IR preserves all information from schema (no data loss).

```haskell
module Synapse.IR.Completeness where

-- Property: IR contains all methods from schema
prop_ir_contains_all_methods :: Property
prop_ir_contains_all_methods = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      let schemaMethods = Set.fromList $ map mdName (sMethods schema)
          irMethods = Set.fromList $ map mdFullPath (Map.elems (irMethods ir))
      in schemaMethods `Set.isSubsetOf` irMethods

-- Property: IR contains all types from schema
prop_ir_contains_all_types :: Property
prop_ir_contains_all_types = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      let schemaTypes = Set.fromList $ map tdName (sTypes schema)
          irTypes = Set.fromList $ Map.keys (irTypes ir)
      in schemaTypes `Set.isSubsetOf` irTypes

-- Property: Method descriptions preserved
prop_method_descriptions_preserved :: Property
prop_method_descriptions_preserved = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir ->
      all (\method ->
        let fullPath = sNamespace schema <> "." <> mdName method
        in case Map.lookup fullPath (irMethods ir) of
          Nothing -> False
          Just irMethod -> mdDescription method == mdDescription irMethod
      ) (sMethods schema)
```

---

## Layer 3: Code Generation Testing

### 3.1 Template Validation

**Goal**: Templates are syntactically valid and produce correct code.

```haskell
module Synapse.Codegen.Template where

-- Parse template and check for errors
validateTemplate :: FilePath -> IO (Either TemplateError ())
validateTemplate templatePath = do
  template <- readFile templatePath
  case parseTemplate template of
    Left err -> return $ Left err
    Right ast -> checkTemplateAST ast

-- Check template AST for common errors
checkTemplateAST :: TemplateAST -> Either TemplateError ()
checkTemplateAST ast = do
  -- Check all variables are bound
  unboundVars <- findUnboundVars ast
  unless (null unboundVars) $
    Left $ UnboundVariables unboundVars

  -- Check all partials exist
  missingPartials <- findMissingPartials ast
  unless (null missingPartials) $
    Left $ MissingPartials missingPartials

  -- Check no infinite loops
  when (hasInfiniteLoop ast) $
    Left InfiniteLoop

  return ()

-- Property: All templates are valid
prop_all_templates_valid :: IO Property
prop_all_templates_valid = do
  templates <- findAllTemplates "src/Synapse/Codegen/Templates"
  results <- mapM validateTemplate templates
  return $ conjoin
    [ counterexample (show path <> ": " <> show err) False
    | (path, Left err) <- zip templates results
    ]
```

### 3.2 Generated Code Compilation

**Goal**: Generated code compiles without errors.

```haskell
module Synapse.Codegen.Compilation where

-- Test: Generated Haskell code compiles
prop_generated_haskell_compiles :: Property
prop_generated_haskell_compiles = forAll genSchema $ \schema ->
  ioProperty $ do
    case buildIR schema of
      Left _ -> return discard
      Right ir -> do
        let code = generateHaskellCode ir
        result <- compileHaskellCode code
        return $ counterexample (show result) $
          case result of
            CompileSuccess -> property True
            CompileError errs -> counterexample (show errs) False

compileHaskellCode :: Text -> IO CompileResult
compileHaskellCode code = do
  withSystemTempDirectory "codegen-test" $ \tmpDir -> do
    let srcFile = tmpDir </> "Generated.hs"
    writeFile srcFile code
    (exitCode, stdout, stderr) <- readProcessWithExitCode "ghc"
      ["-c", srcFile, "-outputdir", tmpDir] ""
    return $ case exitCode of
      ExitSuccess -> CompileSuccess
      ExitFailure _ -> CompileError stderr

-- Property: Generated TypeScript code passes tsc
prop_generated_typescript_compiles :: Property
prop_generated_typescript_compiles = forAll genSchema $ \schema ->
  ioProperty $ do
    case buildIR schema of
      Left _ -> return discard
      Right ir -> do
        let code = generateTypeScriptCode ir
        result <- compileTypeScriptCode code
        return $ case result of
          CompileSuccess -> property True
          CompileError errs -> counterexample (show errs) False
```

### 3.3 Property Preservation

**Goal**: Properties true in IR must be true in generated code.

```haskell
module Synapse.Codegen.Properties where

-- Property: If IR method is streaming, generated code has stream type
prop_streaming_preserved :: Property
prop_streaming_preserved = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir -> ioProperty $ do
      code <- generateHaskellCode ir
      let methods = extractMethodSignatures code
      return $ all (\method ->
        let irMethod = lookupIRMethod ir (methodName method)
        in case irMethod of
          Just m | mdStreaming m -> hasStreamingType method
          Just m | not (mdStreaming m) -> not (hasStreamingType method)
          Nothing -> False
      ) methods

-- Property: Required parameters in IR → required in generated code
prop_required_params_preserved :: Property
prop_required_params_preserved = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir -> ioProperty $ do
      code <- generateHaskellCode ir
      let funcs = extractFunctionDefs code
      return $ all (\func ->
        let irMethod = lookupIRMethod ir (funcName func)
        in case irMethod of
          Just m ->
            let requiredParams = filter (not . pdOptional) (mdParams m)
                funcParams = funcParameters func
            in length requiredParams == length (filter (not . paramOptional) funcParams)
          Nothing -> False
      ) funcs

-- Property: Type refs in IR → correct types in generated code
prop_types_correct :: Property
prop_types_correct = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir -> ioProperty $ do
      hsCode <- generateHaskellCode ir
      tsCode <- generateTypeScriptCode ir
      return $ conjoin
        [ prop_haskell_types_match ir hsCode
        , prop_typescript_types_match ir tsCode
        ]
```

### 3.4 Mutation Testing for Templates

**Goal**: Change template, tests should break.

```haskell
module Synapse.Codegen.Mutation where

-- Mutate template in various ways
data TemplateMutation
  = RemoveRequiredParam
  | ChangeTypeToAny
  | RemoveStreamingSupport
  | IntroduceSyntaxError
  | SwapParamOrder
  deriving (Enum, Bounded)

-- Apply mutation to template
mutateTemplate :: TemplateMutation -> Text -> Text
mutateTemplate RemoveRequiredParam tmpl =
  T.replace "{{#each params}}" "{{#each (optional params)}}" tmpl
mutateTemplate ChangeTypeToAny tmpl =
  T.replace "{{typeName}}" "any" tmpl
-- ... more mutations ...

-- Property: Mutations cause test failures
prop_mutations_break_tests :: Property
prop_mutations_break_tests = forAll genSchema $ \schema ->
  ioProperty $ do
    case buildIR schema of
      Left _ -> return discard
      Right ir -> do
        forM_ [minBound..maxBound] $ \mutation -> do
          -- Generate code with mutated template
          let template = loadTemplate "Method.hs.mustache"
          let mutated = mutateTemplate mutation template
          code <- generateCodeWithTemplate mutated ir

          -- Run tests against generated code
          result <- runTestSuite code

          -- Expect at least one test to fail
          case result of
            AllPassed -> error $ "Mutation not caught: " <> show mutation
            SomeFailed _ -> return ()  -- Good!

        return $ property True
```

---

## Layer 4: Wire Format Testing

### 4.1 Property-Based Wire Format Testing

**Goal**: Generate test cases from TLA+ spec, run against implementations.

```haskell
module Synapse.Wire.PropertyBased where

-- Generate valid Plexus RPC messages from TLA+ traces
genPlexusMessage :: Gen PlexusMessage
genPlexusMessage = oneof
  [ genCallRequest
  , genAckResponse
  , genStreamNotification
  , genErrorResponse
  ]

-- Generate call request
genCallRequest :: Gen PlexusMessage
genCallRequest = do
  reqId <- arbitrary `suchThat` (> 0)
  method <- genMethodName
  params <- arbitrary
  return $ CallRequest
    { crJsonrpc = "2.0"
    , crId = reqId
    , crMethod = method
    , crParams = params
    }

-- Generate stream notification
genStreamNotification :: Gen PlexusMessage
genStreamNotification = do
  subId <- arbitrary `suchThat` (>= 0)
  item <- genStreamItem
  return $ StreamNotification
    { snJsonrpc = "2.0"
    , snMethod = "subscription"
    , snParams = SubscriptionParams subId item
    }

-- Generate stream item with valid metadata
genStreamItem :: Gen StreamItem
genStreamItem = frequency
  [ (5, DataItem <$> genMetadata <*> arbitrary <*> arbitrary)
  , (1, ProgressItem <$> genMetadata <*> arbitrary <*> choose (0, 100))
  , (1, DoneItem <$> genMetadata)
  , (1, ErrorItem <$> genMetadata <*> arbitrary <*> arbitrary <*> arbitrary)
  ]

-- Generate metadata with INTEGER timestamps (not floats!)
genMetadata :: Gen StreamMetadata
genMetadata = StreamMetadata
  <$> listOf1 genNamespace  -- provenance (non-empty)
  <*> genPlexusHash          -- 16 hex chars
  <*> (getPositive <$> arbitrary :: Gen Int64)  -- INTEGER timestamp

-- Property: Server accepts all valid messages
prop_server_accepts_valid_messages :: ServerURL -> Property
prop_server_accepts_valid_messages url = forAll genPlexusMessage $ \msg ->
  ioProperty $ do
    result <- sendToServer url msg
    return $ case result of
      Left (ParseError _) -> counterexample "Server rejected valid message" False
      Left (ProtocolError _) -> counterexample "Protocol error on valid message" False
      Right _ -> property True

-- Property: Generated messages serialize correctly
prop_message_serialization :: Property
prop_message_serialization = forAll genPlexusMessage $ \msg ->
  let json = encode msg
      decoded = decode json
  in decoded === Just msg

-- Property: Metadata always has integer timestamps
prop_metadata_has_integer_timestamps :: Property
prop_metadata_has_integer_timestamps = forAll genStreamItem $ \item ->
  case item of
    DataItem meta _ _ -> isInteger (smTimestamp meta)
    ErrorItem meta _ _ _ -> isInteger (smTimestamp meta)
    DoneItem meta -> isInteger (smTimestamp meta)
    ProgressItem meta _ _ -> isInteger (smTimestamp meta)
    RequestItem _ _ _ -> property True  -- No metadata
  where
    isInteger n = n == floor (toRational n)
```

### 4.2 Fuzzing

**Goal**: Send malformed/edge-case messages, ensure graceful handling.

```haskell
module Synapse.Wire.Fuzzing where

-- Fuzzer that generates semi-valid messages
genFuzzedMessage :: Gen Value
genFuzzedMessage = oneof
  [ genValidMessage  -- 50% valid baseline
  , genMalformedMessage  -- 50% intentionally broken
  ]

-- Generate intentionally malformed messages
genMalformedMessage :: Gen Value
genMalformedMessage = oneof
  [ genMissingFields
  , genWrongTypes
  , genExtraFields
  , genInvalidValues
  , genEdgeCases
  ]

-- Missing required fields
genMissingFields :: Gen Value
genMissingFields = do
  obj <- arbitrary :: Gen Object
  field <- elements ["jsonrpc", "id", "method", "params"]
  return $ Object $ KeyMap.delete field obj

-- Wrong types for fields
genWrongTypes :: Gen Value
genWrongTypes = object
  [ "jsonrpc" ==> Number 2.0  -- Should be string "2.0"
  , "id" ==> String "not-a-number"  -- Should be integer
  , "method" ==> Number 42  -- Should be string
  , "params" ==> String "not-an-object"  -- Should be object
  ]

-- Edge cases
genEdgeCases :: Gen Value
genEdgeCases = oneof
  [ -- Huge nested structures
    genDeeplyNested 1000

    -- Very large strings
  , object ["method" ==> String (T.replicate 100000 "x")]

    -- Special characters
  , object ["method" ==> String "\0\n\r\t\"\\"]

    -- Unicode edge cases
  , object ["method" ==> String "🚀\x0000\xFFFF"]

    -- Numeric edge cases
  , object ["id" ==> Number (fromIntegral (maxBound :: Int64) + 1)]
  ]

-- Property: Server handles all fuzzed messages gracefully
prop_server_handles_fuzzed_messages :: ServerURL -> Property
prop_server_handles_fuzzed_messages url = forAll genFuzzedMessage $ \msg ->
  ioProperty $ do
    result <- sendRawJSON url msg
    return $ case result of
      -- Server must respond, even if with error
      NoResponse -> counterexample "Server didn't respond" False
      ServerCrash -> counterexample "Server crashed!" False
      Response _ -> property True  -- Any response is fine
```

### 4.3 TLA+ Trace Validation

**Goal**: Run actual server, record traces, validate against TLA+ spec.

```haskell
module Synapse.Wire.TraceValidation where

-- Record a trace of actual server behavior
data Trace = Trace
  { tEvents :: [Event]
  , tInitialState :: State
  , tFinalState :: State
  }

data Event
  = ClientConnect ConnectionId
  | ClientSendRequest RequestId Method Params
  | ServerSendAck RequestId SubscriptionId
  | ServerSendStreamItem SubscriptionId StreamItem
  | ClientDisconnect ConnectionId

-- Validate trace against TLA+ spec
validateTrace :: Trace -> Either TraceViolation ()
validateTrace trace = do
  -- Convert to TLA+ trace format
  let tlaTrace = convertToTLATrace trace

  -- Run TLC with trace checking
  result <- runTLC "PlexusRPCEnhanced.tla" tlaTrace

  case result of
    TraceValid -> Right ()
    TraceInvalid violation -> Left violation

-- Property: All recorded traces are valid
prop_traces_satisfy_spec :: ServerURL -> Property
prop_traces_satisfy_spec url = monadicIO $ do
  -- Run series of random operations
  operations <- pick $ listOf genOperation

  -- Execute against server and record trace
  trace <- run $ recordTrace url operations

  -- Validate trace
  result <- run $ validateTrace trace

  case result of
    Left violation -> do
      monitor $ counterexample ("Trace violation: " <> show violation)
      assert False
    Right () -> assert True

-- Generate random operations
genOperation :: Gen Operation
genOperation = oneof
  [ Connect <$> arbitrary
  , SendRequest <$> arbitrary <*> genMethodName <*> arbitrary
  , Disconnect <$> arbitrary
  ]
```

---

## Layer 5: Runtime Behavior Testing

### 5.1 Refinement Checking

**Goal**: Prove implementation refines TLA+ specification.

```haskell
module Synapse.Runtime.Refinement where

-- Check if implementation refines spec
-- This requires extracting implementation behavior and comparing to TLA+ semantics
checkRefinement :: Implementation -> TLASpec -> IO (Either RefinementViolation ())
checkRefinement impl spec = do
  -- Generate symbolic execution traces from implementation
  traces <- extractSymbolicTraces impl

  -- For each trace, check if it's valid in TLA+ spec
  violations <- filterM (not . isValidTrace spec) traces

  case violations of
    [] -> return $ Right ()
    (v:_) -> return $ Left $ RefinementViolation v

-- Extract symbolic execution traces
-- This is complex and might require tools like KLEE, SymEx, or manual instrumentation
extractSymbolicTraces :: Implementation -> IO [Trace]
extractSymbolicTraces impl = do
  -- Instrument implementation with trace recording
  instrumented <- instrumentCode impl

  -- Run with symbolic inputs
  symbolicEngine <- initSymbolicEngine
  traces <- runSymbolic symbolicEngine instrumented

  return traces

-- Property: Implementation refines specification
prop_implementation_refines_spec :: Property
prop_implementation_refines_spec = ioProperty $ do
  impl <- loadImplementation "plexus-rpc-ts"
  spec <- loadTLASpec "PlexusRPCEnhanced.tla"

  result <- checkRefinement impl spec

  return $ case result of
    Left violation -> counterexample (show violation) False
    Right () -> property True
```

### 5.2 Continuous Monitoring

**Goal**: Validate production traces in real-time.

```haskell
module Synapse.Runtime.Monitoring where

-- Stream of production events
type EventStream = ConduitT () Event IO ()

-- Monitor that validates events against spec
specMonitor :: TLASpec -> ConduitT Event Violation IO ()
specMonitor spec = do
  state <- lift $ initialState spec

  forever $ do
    event <- await
    case event of
      Nothing -> return ()
      Just e -> do
        -- Check if event is valid in current state
        case validateEvent spec state e of
          Left violation -> yield violation
          Right state' -> lift $ writeIORef stateRef state'

-- Property: Production traces satisfy spec (over time window)
prop_production_traces_valid :: Duration -> Property
prop_production_traces_valid window = ioProperty $ do
  -- Connect to production telemetry
  eventStream <- connectToProduction

  -- Collect events for specified window
  events <- runConduit $
    eventStream
    .| takeWhileC (\e -> eventTime e < window)
    .| sinkList

  -- Build trace
  let trace = eventsToTrace events

  -- Validate
  result <- validateTrace trace

  return $ case result of
    Left violation -> counterexample (show violation) False
    Right () -> property True
```

---

## Layer 6: Cross-Language Compatibility Testing

### 6.1 Compatibility Matrix

**Goal**: Test all pairwise language combinations.

```haskell
module Synapse.CrossLang.Matrix where

data Language = Haskell | TypeScript | Rust | Python
  deriving (Eq, Ord, Enum, Bounded)

-- All pairs of languages to test
languagePairs :: [(Language, Language)]
languagePairs =
  [ (l1, l2)
  | l1 <- [minBound..maxBound]
  , l2 <- [minBound..maxBound]
  , l1 /= l2
  ]

-- Test case: Client in lang1, Server in lang2
data CrossLangTest = CrossLangTest
  { cltClient :: Language
  , cltServer :: Language
  , cltOperation :: Operation
  , cltExpectedResult :: Expected Result
  }

-- Generate test cases for all pairs
genCrossLangTests :: Gen [CrossLangTest]
genCrossLangTests = do
  operations <- listOf genOperation
  return
    [ CrossLangTest client server op expected
    | (client, server) <- languagePairs
    , op <- operations
    , let expected = computeExpected op
    ]

-- Property: All language pairs are compatible
prop_all_pairs_compatible :: Property
prop_all_pairs_compatible = forAll genCrossLangTests $ \tests ->
  ioProperty $ do
    results <- mapM runCrossLangTest tests
    let failures = filter (not . testPassed) results
    return $ counterexample (show failures) $ null failures

-- Run a single cross-language test
runCrossLangTest :: CrossLangTest -> IO TestResult
runCrossLangTest test = do
  -- Start server in target language
  server <- startServer (cltServer test)

  -- Run client in source language
  result <- runClient (cltClient test) server (cltOperation test)

  -- Check result
  let passed = result == cltExpectedResult test

  stopServer server

  return $ TestResult
    { trClient = cltClient test
    , trServer = cltServer test
    , trOperation = cltOperation test
    , trResult = result
    , trPassed = passed
    }
```

### 6.2 Round-Trip Type Testing

**Goal**: Value serialized in one language deserializes correctly in another.

```haskell
module Synapse.CrossLang.RoundTrip where

-- Property: TS → Haskell → TS preserves value
prop_typescript_haskell_roundtrip :: Property
prop_typescript_haskell_roundtrip = forAll arbitrary $ \(value :: TypeScriptValue) ->
  ioProperty $ do
    -- Serialize in TypeScript
    json <- serializeTypeScript value

    -- Deserialize in Haskell
    haskellValue <- deserializeHaskell json

    -- Serialize in Haskell
    json' <- serializeHaskell haskellValue

    -- Deserialize in TypeScript
    value' <- deserializeTypeScript json'

    -- Check equality
    return $ value === value'

-- Property: All types round-trip correctly
prop_all_types_roundtrip :: Property
prop_all_types_roundtrip = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir -> ioProperty $ do
      forM_ (Map.elems $ irTypes ir) $ \typeDef -> do
        -- Generate value of this type
        value <- generateValue typeDef

        -- Test all language pairs
        forM_ languagePairs $ \(lang1, lang2) -> do
          json1 <- serialize lang1 value
          value2 <- deserialize lang2 json1
          json2 <- serialize lang2 value2
          value1' <- deserialize lang1 json2

          assertEqual "Round-trip failed" value value1'

      return $ property True
```

### 6.3 Type Isomorphism Verification

**Goal**: Types in different languages are truly equivalent.

```haskell
module Synapse.CrossLang.Isomorphism where

-- Check if two types are isomorphic
checkIsomorphism :: Type1 -> Type2 -> IO (Either IsomorphismError ())
checkIsomorphism t1 t2 = do
  -- Check structural equivalence
  unless (sameStructure t1 t2) $
    return $ Left $ StructureMismatch t1 t2

  -- Check all values can be converted both ways
  forM_ (generateValuesOfType t1) $ \v1 -> do
    case convertType t1 t2 v1 of
      Nothing -> return $ Left $ ConversionFailed v1
      Just v2 -> do
        case convertType t2 t1 v2 of
          Nothing -> return $ Left $ RoundTripFailed v1
          Just v1' ->
            unless (v1 == v1') $
              return $ Left $ ValueMismatch v1 v1'

  return $ Right ()

-- Property: Generated types are isomorphic across languages
prop_generated_types_isomorphic :: Property
prop_generated_types_isomorphic = forAll genSchema $ \schema ->
  case buildIR schema of
    Left _ -> discard
    Right ir -> ioProperty $ do
      hsTypes <- generateHaskellTypes ir
      tsTypes <- generateTypeScriptTypes ir
      rsTypes <- generateRustTypes ir

      -- Check all pairwise isomorphisms
      results <- sequence
        [ checkIsomorphism hsType tsType
        | (name, hsType) <- Map.toList hsTypes
        , Just tsType <- [Map.lookup name tsTypes]
        ]

      let failures = lefts results
      return $ counterexample (show failures) $ null failures
```

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)
**Priority**: CRITICAL
**Effort**: 40-60 hours

- [ ] Set up QuickCheck generators for schemas
- [ ] Implement basic property tests for IR
- [ ] Create schema validation with refinement types
- [ ] Add round-trip testing (Schema ↔ IR)
- [ ] Document type system enforcement patterns

**Deliverables**:
- `Synapse.Schema.Gen` module
- `Synapse.IR.Properties` module
- `Synapse.Schema.Refined` module
- Updated architecture docs

### Phase 2: Codegen Validation (Weeks 3-4)
**Priority**: HIGH
**Effort**: 30-40 hours

- [ ] Template validation framework
- [ ] Generated code compilation tests
- [ ] Property preservation checks
- [ ] Mutation testing for templates

**Deliverables**:
- `Synapse.Codegen.Template` module
- `Synapse.Codegen.Properties` module
- Mutation testing suite

### Phase 3: Wire Format Testing (Weeks 5-6)
**Priority**: HIGH
**Effort**: 30-40 hours

- [ ] Property-based wire format tests
- [ ] Fuzzing framework
- [ ] TLA+ trace validation
- [ ] Integration with existing compliance tests

**Deliverables**:
- `Synapse.Wire.PropertyBased` module
- `Synapse.Wire.Fuzzing` module
- `Synapse.Wire.TraceValidation` module

### Phase 4: Cross-Language Testing (Weeks 7-8)
**Priority**: MEDIUM
**Effort**: 30-40 hours

- [ ] Compatibility matrix implementation
- [ ] Round-trip testing framework
- [ ] Type isomorphism verification
- [ ] Automated multi-language test runs

**Deliverables**:
- `Synapse.CrossLang.Matrix` module
- `Synapse.CrossLang.RoundTrip` module
- `Synapse.CrossLang.Isomorphism` module
- CI integration

### Phase 5: Runtime Verification (Weeks 9-10)
**Priority**: LOW (advanced)
**Effort**: 40-50 hours

- [ ] Refinement checking framework
- [ ] Production trace monitoring
- [ ] Continuous verification
- [ ] Symbolic execution integration (if feasible)

**Deliverables**:
- `Synapse.Runtime.Refinement` module (best-effort)
- `Synapse.Runtime.Monitoring` module
- Production telemetry integration

---

## Success Metrics

### Coverage Targets

| Layer | Current | Target | Verification Method |
|-------|---------|--------|---------------------|
| Schema Validation | 20% | 90% | Property tests pass |
| IR Construction | 40% | 95% | Round-trip tests pass |
| Code Generation | 10% | 85% | Generated code compiles |
| Wire Format | 25% | 90% | Fuzzing finds no crashes |
| Runtime Behavior | 5% | 70% | Trace validation passes |
| Cross-Language | 30% | 95% | All pairs test clean |

### Quality Gates

**Phase 1 Complete When**:
- ✅ 1000+ property tests passing
- ✅ All schemas round-trip through IR
- ✅ Zero unresolved type references
- ✅ Refinement types enforce constraints

**Phase 2 Complete When**:
- ✅ All templates validate
- ✅ Generated code compiles (Haskell, TypeScript, Rust)
- ✅ Properties preserved from IR to code
- ✅ Mutations caught by tests

**Phase 3 Complete When**:
- ✅ 10,000+ fuzzed messages handled gracefully
- ✅ All TLA+ traces validate
- ✅ Integration with existing compliance tests
- ✅ Wire format bugs impossible to write

**Phase 4 Complete When**:
- ✅ All language pairs tested
- ✅ Round-trip testing covers all types
- ✅ Type isomorphisms verified
- ✅ CI runs full compatibility matrix

**Phase 5 Complete When**:
- ✅ Production traces continuously validated
- ✅ Refinement checking automated (if implemented)
- ✅ Zero protocol violations in production
- ✅ Telemetry integrated with verification

---

## Tools and Dependencies

### Haskell
- `QuickCheck` - Property-based testing
- `refined` or `LiquidHaskell` - Refinement types
- `hspec` - Test framework
- `tasty` - Test runner
- `hedgehog` - Alternative property testing

### TLA+
- `tla2tools.jar` - TLC model checker (already installed)
- `apalache` - Symbolic model checker (optional)

### Fuzzing
- `QuickFuzz` - Haskell fuzzing
- `AFL` - American Fuzzy Lop (for C/Rust)
- Custom fuzzer for JSON-RPC

### Cross-Language
- Docker containers for isolation
- `testcontainers-hs` - Container management
- Language-specific test runners

### CI/CD
- GitHub Actions workflows
- Nix for reproducible builds
- Test result aggregation

---

## Risk Mitigation

### Technical Risks

**Risk**: Refinement types too restrictive
**Mitigation**: Start with `refined` library (runtime checks), graduate to LiquidHaskell incrementally

**Risk**: Property tests too slow
**Mitigation**: Parallelize tests, use QuickCheck's `withMaxSuccess`, profile and optimize

**Risk**: Cross-language testing flaky
**Mitigation**: Use Docker for isolation, retry transient failures, careful cleanup

**Risk**: Fuzzing finds too many edge cases
**Mitigation**: Prioritize crashes/security issues, document known limitations

### Process Risks

**Risk**: Too ambitious, never finish
**Mitigation**: Phased approach, each phase delivers value independently

**Risk**: Tests maintenance burden
**Mitigation**: Generate tests from specs, minimize hand-written tests

**Risk**: False positives break CI
**Mitigation**: Clear failure messages, easy to triage, quarantine flaky tests

---

## Conclusion

This comprehensive testing architecture provides:

1. **Mathematical Certainty**: Properties proven at type level
2. **Automated Discovery**: Bugs found by generators, not humans
3. **Continuous Verification**: Production behavior checked against spec
4. **Cross-Language Confidence**: All implementations compatible
5. **Maintainability**: Tests generated from specs, not hand-written

**Expected Outcomes**:
- 90%+ reduction in interoperability bugs
- Type-level guarantees prevent entire classes of bugs
- Automated test generation reduces maintenance
- Continuous verification catches regressions immediately
- Cross-language compatibility guaranteed

**Timeline**: 10 weeks for full implementation
**ROI**: Massive - prevents entire categories of bugs, enables confident refactoring

---

**Created**: 2026-03-20
**Status**: 🟡 Planning Complete, Implementation Phase 1 Ready
**Next Step**: Begin Phase 1 - Foundation (Schema + IR property testing)
