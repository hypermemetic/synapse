# Type System Enforcement for Protocol Correctness

## Philosophy: Make Illegal States Unrepresentable

**Core Idea**: Use Haskell's type system to encode protocol invariants so that **violating the spec doesn't type-check**.

Instead of runtime checks that can be forgotten:
```haskell
-- ❌ Runtime checking (can forget)
validateMetadata :: Metadata -> Either Error ()
validateMetadata m = do
  unless (length (mPlexusHash m) == 16) $
    Left "Hash must be 16 chars"
  unless (mTimestamp m > 0) $
    Left "Timestamp must be positive"
  -- ... easy to forget a check
```

Use types that make invalid values impossible:
```haskell
-- ✅ Type-level enforcement (impossible to forget)
data Metadata = Metadata
  { mPlexusHash :: Hash16     -- Can only be 16 chars!
  , mTimestamp  :: Positive Int64  -- Can only be positive!
  , mProvenance :: NonEmpty Text   -- Cannot be empty!
  }
  -- Invalid values don't type-check!
```

---

## Layer 1: Basic Refinement Types

### Using the `refined` Library

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Synapse.Types.Refined where

import Refined
import Refined.Unsafe (reallyUnsafeRefine)

-- Plexus hash: exactly 16 hex chars
type PlexusHash = Refined (SizeEqualTo 16 && Hex) Text

mkPlexusHash :: Text -> Either RefineException PlexusHash
mkPlexusHash = refine

-- Example usage
validHash :: Either RefineException PlexusHash
validHash = mkPlexusHash "abc123def456789a"  -- ✅ OK

invalidHash :: Either RefineException PlexusHash
invalidHash = mkPlexusHash "too-long-hash"  -- ❌ Type error!

-- Unix timestamp: positive integer
type Timestamp = Refined Positive Int64

mkTimestamp :: Int64 -> Either RefineException Timestamp
mkTimestamp = refine

-- ❌ This fails at runtime:
badTimestamp :: Either RefineException Timestamp
badTimestamp = mkTimestamp (-42)  -- Refinement fails

-- ✅ This works:
goodTimestamp :: Either RefineException Timestamp
goodTimestamp = mkTimestamp 1735052400

-- Namespace: lowercase, no spaces, non-empty
type Namespace = Refined (NonEmpty && LowerCase && NoSpaces) Text

-- Method name: valid identifier
type MethodName = Refined ValidIdentifier Text

-- Subscription ID: non-negative
type SubscriptionId = Refined NonNegative Int

-- Provenance: non-empty list
type Provenance = NonEmpty Text  -- From Data.List.NonEmpty
```

### Custom Refinements

```haskell
-- Custom predicate for hex strings
data Hex

instance Predicate Hex Text where
  validate p txt =
    if T.all (`elem` "0123456789abcdef") txt
    then Nothing
    else Just "Not a hex string"

-- Custom predicate for valid identifiers
data ValidIdentifier

instance Predicate ValidIdentifier Text where
  validate p txt =
    if isValidIdent txt
    then Nothing
    else Just "Not a valid identifier"
    where
      isValidIdent t =
        not (T.null t) &&
        T.head t `elem` ['a'..'z'] &&
        T.all (\c -> isAlphaNum c || c == '_') t

-- Custom predicate for snake_case
data SnakeCase

instance Predicate SnakeCase Text where
  validate p txt =
    if all (\c -> isLower c || isDigit c || c == '_') (T.unpack txt)
    then Nothing
    else Just "Not snake_case"

-- Use it:
type SnakeCaseField = Refined SnakeCase Text

-- ✅ Valid
plexusHash :: SnakeCaseField
plexusHash = $(refineTH "plexus_hash")  -- Checked at compile time!

-- ❌ Won't compile
camelCase :: SnakeCaseField
camelCase = $(refineTH "plexusHash")  -- Compile error!
```

---

## Layer 2: Phantom Types for State Machines

### Encode Protocol State in Types

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Synapse.Types.StateMachine where

import GHC.TypeLits

-- Protocol states as types
data SubscriptionState = Acked | Streaming | Terminated

-- Subscription with state phantom type
data Subscription (s :: SubscriptionState) = Subscription
  { subId :: SubscriptionId
  , subMethod :: MethodName
  , subConnection :: ConnectionId
  }

-- State transitions are functions that change phantom type
ackSubscription
  :: RequestId
  -> IO (Subscription 'Acked)
ackSubscription reqId = do
  subId <- generateSubscriptionId
  return $ Subscription subId method conn

-- Can only send items if acked or streaming
sendStreamItem
  :: Subscription s
  -> StreamItem
  -> IO (Subscription 'Streaming)
sendStreamItem sub item = do
  -- Implementation...
  return $ unsafeCoerce sub  -- Transition to Streaming

-- Can only terminate if streaming
terminateSubscription
  :: Subscription 'Streaming
  -> IO (Subscription 'Terminated)
terminateSubscription sub = do
  sendDone (subId sub)
  return $ unsafeCoerce sub  -- Transition to Terminated

-- ❌ This won't type-check:
-- bad = terminateSubscription (ackSubscription reqId)
--       ^^^ Type error: expected 'Streaming, got 'Acked

-- ✅ This type-checks:
good :: IO ()
good = do
  sub1 <- ackSubscription 1           -- Subscription 'Acked
  sub2 <- sendStreamItem sub1 item    -- Subscription 'Streaming
  sub3 <- terminateSubscription sub2  -- Subscription 'Terminated
  -- ✅ Correct state transitions enforced by types!
```

### Indexed Monads for Protocol Sequences

```haskell
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Synapse.Types.Indexed where

-- Indexed monad for protocol sequences
data Protocol (i :: SubscriptionState) (o :: SubscriptionState) a where
  Return :: a -> Protocol s s a
  Bind :: Protocol s t a -> (a -> Protocol t u b) -> Protocol s u b

-- Operations that transition states
ack :: Protocol 'Acked 'Acked SubscriptionId
sendData :: StreamItem -> Protocol 'Streaming 'Streaming ()
done :: Protocol 'Streaming 'Terminated ()

-- ✅ Valid protocol sequence
validSequence :: Protocol 'Acked 'Terminated ()
validSequence = do
  subId <- ack
  sendData item1
  sendData item2
  done

-- ❌ Invalid sequence (missing ack)
invalidSequence :: Protocol 'Acked 'Terminated ()
invalidSequence = do
  sendData item  -- Type error! Can't send data before ack
  done
```

---

## Layer 3: GADTs for Wire Format Correctness

### Encode Metadata Requirements

```haskell
{-# LANGUAGE GADTs #-}

module Synapse.Types.WireFormat where

-- Stream item types at type level
data ItemType = Data | Error | Done | Progress | Request

-- GADT: metadata requirement encoded in type
data StreamItem (t :: ItemType) where
  DataItem ::
    { diMetadata :: StreamMetadata    -- Metadata required!
    , diContentType :: SnakeCaseField  -- snake_case enforced!
    , diContent :: Value
    } -> StreamItem 'Data

  ErrorItem ::
    { eiMetadata :: StreamMetadata    -- Metadata required!
    , eiMessage :: Text
    , eiCode :: Maybe Text
    , eiRecoverable :: Bool
    } -> StreamItem 'Error

  DoneItem ::
    { doneMetadata :: StreamMetadata  -- Metadata required!
    } -> StreamItem 'Done

  ProgressItem ::
    { piMetadata :: StreamMetadata     -- Metadata required!
    , piMessage :: Text
    , piPercentage :: Refined (From 0 `And` To 100) Int
    } -> StreamItem 'Progress

  RequestItem ::
    { riRequestId :: Text
    , riRequestData :: Value
    , riTimeoutMs :: Refined Positive Int
    } -> StreamItem 'Request
    -- ☝️ No metadata field! Type enforces protocol rule.

-- Can't construct item without metadata:
-- bad = DataItem "content_type" value  -- Type error! Missing metadata

-- Must provide metadata:
good = DataItem metadata "content_type" value  -- ✅ Type-checks

-- Serialize to JSON with correct fields
toJSON :: StreamItem t -> Value
toJSON (DataItem meta ct content) = object
  [ "type" .= ("data" :: Text)
  , "metadata" .= meta
  , "content_type" .= ct  -- snake_case!
  , "content" .= content
  ]
toJSON (RequestItem rid rdata timeout) = object
  [ "type" .= ("request" :: Text)
  , "request_id" .= rid    -- snake_case!
  , "request_data" .= rdata  -- snake_case!
  , "timeout_ms" .= timeout  -- snake_case!
  -- No metadata field!
  ]
```

### Type-Safe Field Names

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Synapse.Types.Fields where

import GHC.TypeLits

-- Phantom type for field naming convention
data FieldCase = SnakeCase | CamelCase

-- Type-level function: wire format requires snake_case
type family WireFormat (c :: FieldCase) :: Constraint where
  WireFormat 'SnakeCase = ()
  WireFormat 'CamelCase = TypeError
    ('Text "Wire format must use snake_case, not camelCase!")

-- Field name with case marker
data Field (c :: FieldCase) = Field Text

-- Only snake_case fields can be serialized
toWire :: WireFormat c => Field c -> Text
toWire (Field name) = name

-- ✅ Valid:
plexusHashField :: Field 'SnakeCase
plexusHashField = Field "plexus_hash"

wireFormat :: Text
wireFormat = toWire plexusHashField  -- ✅ OK

-- ❌ Invalid (won't compile):
camelCaseField :: Field 'CamelCase
camelCaseField = Field "plexusHash"

-- bad = toWire camelCaseField
--       ^^^ Type error: camelCase not allowed on wire!
```

---

## Layer 4: Dependent Types (Advanced)

### LiquidHaskell Refinements

```haskell
{-@ LIQUID "--exact-data-cons" @-}

module Synapse.Types.Liquid where

-- Refined data type with liquid annotations
{-@ data StreamMetadata = StreamMetadata
      { smPlexusHash :: {v:Text | len v == 16}
      , smTimestamp  :: {v:Int | v > 0}
      , smProvenance :: {v:[Text] | len v > 0}
      }
  @-}
data StreamMetadata = StreamMetadata
  { smPlexusHash :: Text
  , smTimestamp  :: Int
  , smProvenance :: [Text]
  }

-- LiquidHaskell proves this at compile time:
{-@ mkMetadata :: h:{Text | len h == 16} -> t:{Int | t > 0} -> p:{[Text] | len p > 0} -> StreamMetadata @-}
mkMetadata :: Text -> Int -> [Text] -> StreamMetadata
mkMetadata = StreamMetadata

-- ✅ This verifies:
validMetadata :: StreamMetadata
validMetadata = mkMetadata "abc123def456789a" 1735052400 ["substrate"]

-- ❌ This fails LiquidHaskell verification:
-- invalidMetadata = mkMetadata "short" (-42) []
--                              ^^^^^^  ^^^^  ^^
--                              wrong   wrong empty!
```

### Dependent Pairs for Bounded Lists

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Synapse.Types.Bounded where

import GHC.TypeLits
import Data.Proxy

-- Vector: list with length in type
data Vec (n :: Nat) a where
  VNil :: Vec 0 a
  VCons :: a -> Vec n a -> Vec (n + 1) a

-- Provenance: non-empty vector
type ProvenanceChain = Vec (n + 1) Text

-- ❌ Can't construct empty provenance:
-- emptyProvenance :: ProvenanceChain
-- emptyProvenance = VNil  -- Type error! Expected (n+1), got 0

-- ✅ Must have at least one element:
validProvenance :: Vec 1 Text
validProvenance = VCons "substrate" VNil

-- Bounded provenance (max 10 elements)
type BoundedProvenance = Vec n Text where n <= 10

-- Type-level proof that length <= max
data LTE (n :: Nat) (m :: Nat) where
  LTEZero :: LTE 0 m
  LTESucc :: LTE n m -> LTE (n + 1) (m + 1)

-- Can only extend if under limit
extendProvenance
  :: LTE n 10
  -> Vec n Text
  -> Text
  -> Vec (n + 1) Text
extendProvenance _ vec name = VCons name vec

-- ❌ This fails at compile time if chain too long:
-- tooLong = extendProvenance proof vec11 "more"
--           ^^^ Type error: 11 > 10!
```

---

## Layer 5: Session Types for Bidirectional Protocols

### Encode Full Protocol in Types

```haskell
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Synapse.Types.Session where

-- Session type language
data SessionType
  = Send MessageType SessionType
  | Recv MessageType SessionType
  | Choose [SessionType]
  | Offer [SessionType]
  | End

-- Plexus RPC subscription session
type SubscriptionSession =
  Recv CallRequest
    (Send AckResponse
      (Choose
        [ Send DataItem (SendMore DataItem)  -- Can send more data
        , Send ErrorItem End                  -- Or terminate with error
        , Send DoneItem End                   -- Or terminate successfully
        ]))

-- Bidirectional session
type BidirectionalSession =
  Recv CallRequest
    (Send AckResponse
      (Interleave
        (SendMany DataItem)
        (RecvMany RequestItem)))

-- Session-typed channel
data Chan (s :: SessionType) where
  -- ...

-- Operations follow session type
send :: Chan ('Send msg s) -> msg -> IO (Chan s)
recv :: Chan ('Recv msg s) -> IO (msg, Chan s)

-- Example protocol implementation
handleSubscription :: Chan SubscriptionSession -> IO ()
handleSubscription ch = do
  (req, ch1) <- recv ch
  ch2 <- send ch1 (AckResponse subId)
  -- Now must either send data, error, or done
  ch3 <- send ch2 (DataItem meta content)
  -- Can continue sending data or terminate
  ch4 <- send ch3 (DoneItem meta)
  -- Session ends here (type enforces it!)
```

---

## Layer 6: Proof-Carrying Code

### Embed Proofs in Types

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Synapse.Types.Proofs where

-- Proof that a type ref resolves
data Resolves (ir :: IR) (ref :: TypeRef) where
  ResolvesProof :: Lookup ir ref ~ Just typeDef -> Resolves ir ref

-- Can only build method if all refs resolve
data MethodWithProofs = MethodWithProofs
  { mwpMethod :: MethodDef
  , mwpParamProofs :: [Resolves ir (ParamType param)]
  , mwpReturnProof :: Resolves ir (ReturnType method)
  }

-- Building method requires providing proofs
buildMethod
  :: MethodDef
  -> (forall ref. TypeRef -> Maybe (Resolves ir ref))
  -> Either BuildError MethodWithProofs
buildMethod method prover = do
  paramProofs <- forM (mdParams method) $ \param ->
    case prover (pdType param) of
      Nothing -> Left $ UnresolvedRef (pdType param)
      Just proof -> Right proof

  returnProof <- case prover (mdReturns method) of
    Nothing -> Left $ UnresolvedRef (mdReturns method)
    Just proof -> Right proof

  return $ MethodWithProofs method paramProofs returnProof

-- ❌ Can't construct MethodWithProofs without proofs:
-- bad = MethodWithProofs method [] []  -- Type error!

-- ✅ Must provide proofs:
-- good = buildMethod method resolver
```

---

## Implementation Strategy

### Phase 1: Basic Refinements (Week 1)

```haskell
-- Start with refined library
module Synapse.Types.V1 where

import Refined

type PlexusHash = Refined (SizeEqualTo 16 && Hex) Text
type Timestamp = Refined Positive Int64
type Namespace = Refined (NonEmpty && LowerCase) Text

-- Update existing types gradually
data StreamMetadata = StreamMetadata
  { smPlexusHash :: PlexusHash  -- Changed from Text
  , smTimestamp :: Timestamp     -- Changed from Int
  , smProvenance :: NonEmpty Text  -- Changed from [Text]
  }
```

### Phase 2: GADTs for Wire Format (Week 2)

```haskell
-- Introduce GADT for stream items
data StreamItem (t :: ItemType) where
  DataItem :: { ... } -> StreamItem 'Data
  ErrorItem :: { ... } -> StreamItem 'Error
  -- ...

-- Update codegen to use GADTs
generateHaskellType :: TypeDef -> Text
generateHaskellType def =
  case def of
    StreamItemDef -> generateGADT def
    _ -> generateRecord def
```

### Phase 3: Phantom Types for State (Week 3)

```haskell
-- Add phantom types to subscriptions
data Subscription (s :: SubscriptionState) = ...

-- Update IR to track states
data IR = IR
  { irSubscriptions :: Map SubscriptionId (SomeSubscription)
  , ...
  }

data SomeSubscription where
  SomeSubscription :: Subscription s -> SomeSubscription
```

### Phase 4: Advanced Types (Weeks 4-5)

```haskell
-- Add LiquidHaskell annotations
{-@ data StreamMetadata = StreamMetadata
      { smPlexusHash :: {v:Text | len v == 16}
      , smTimestamp  :: {v:Int | v > 0}
      , smProvenance :: {v:[Text] | len v > 0}
      }
  @-}

-- Add dependent types for bounds
type BoundedStreamItems = Vec n StreamItem where n <= MaxStreamItems

-- Add session types for protocols
type SubscriptionSession = Send AckResponse (RecvMany StreamItem)
```

---

## Testing the Type System

### Property: Invalid Values Don't Compile

```haskell
-- These should not compile:
-- badHash :: PlexusHash
-- badHash = $(refineTH "invalid")  -- Compile error!

-- badTimestamp :: Timestamp
-- badTimestamp = $(refineTH (-42))  -- Compile error!

-- badProvenance :: NonEmpty Text
-- badProvenance = []  -- Compile error!
```

### Property: State Transitions Enforced

```haskell
-- This should not compile:
-- badTransition :: IO ()
-- badTransition = do
--   sub <- ackSubscription reqId       -- Subscription 'Acked
--   terminateSubscription sub          -- Type error! Expected 'Streaming
```

### Property: Metadata Required

```haskell
-- This should not compile:
-- noMetadata :: StreamItem 'Data
-- noMetadata = DataItem "content_type" value  -- Type error! Missing metadata
```

---

## Benefits

### Compile-Time Guarantees

- ✅ **Invalid hashes don't compile** (not just runtime error)
- ✅ **Float timestamps don't compile** (not discovered in production)
- ✅ **Missing metadata doesn't compile** (not found by tests)
- ✅ **Wrong state transitions don't compile** (not protocol violations)
- ✅ **CamelCase wire format doesn't compile** (not interop bugs)

### Refactoring Confidence

When you change a type:
- Compiler tells you **every place** that needs updating
- No need to search for string literals
- No need to run full test suite
- **If it compiles, it's correct** (for encoded invariants)

### Documentation

Types **are** documentation:
```haskell
-- This says it all:
sendStreamItem
  :: Subscription 'Streaming
  -> StreamItem t
  -> IO (Subscription 'Streaming)

-- No need to document:
-- "Can only call if subscription is streaming"
-- "Item must have metadata if type is Data/Error/Done/Progress"
-- Types enforce it!
```

---

## Limitations and Trade-offs

### What Types CAN'T Enforce

- **Network behavior** (timeouts, retries)
- **Performance** (response time, throughput)
- **Business logic** (what data to send)
- **External system behavior** (other servers)

### Complexity Trade-off

More type safety = more complex types:
```haskell
-- Simple but unsafe:
sendItem :: Subscription -> StreamItem -> IO ()

-- Safe but complex:
sendItem
  :: Subscription 'Streaming
  -> StreamItem t
  -> Requires (HasMetadata t)
  -> IO (Subscription 'Streaming)
```

**Decision**: Use advanced types for **protocol correctness**, not business logic.

### Learning Curve

Team needs to learn:
- Refinement types
- GADTs
- Phantom types
- Type families
- (Optionally) Dependent types

**Mitigation**: Introduce gradually, provide training, document patterns.

---

## Conclusion

By encoding protocol invariants in the type system, we transform **runtime bugs** into **compile-time errors**.

**The Ultimate Goal**: If the Plexus RPC code compiles, it **cannot** violate the protocol specification (for properties we've encoded).

**Status**: 🔴 Not yet implemented
**Next Step**: Phase 1 - Add refinement types for basic invariants
**Timeline**: 5 weeks for full implementation

---

**Created**: 2026-03-20
**Status**: 🟡 Design Complete, Ready for Implementation
**Dependencies**: `refined`, `liquidhaskell` (optional), GHC 9.6+
