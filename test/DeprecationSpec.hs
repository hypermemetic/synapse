{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for IR-6 and IR-14 deprecation rendering.
--
--   IR-6: per-method and per-activation decoration in 'renderSchema'.
--   IR-14: per-parameter decoration in both 'renderMethodFull' (reads
--          'methodParamSchemas' directly) and 'renderMethodHelpWith'
--          (reads 'ParamDef.pdDeprecation' after the IR builder has
--          lifted the info across).
--
--   Invocation-time stderr notices are deferred to a follow-up ticket.
module Main where

import Data.Aeson (object, (.=), Value)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Plexus.Schema.Recursive
  ( DeprecationInfo(..)
  , MethodRole(..)
  , MethodSchema(..)
  , ParamSchema(..)
  , PluginSchema(..)
  )
import Synapse.Algebra.Render
  ( deprecationMarker
  , renderMethodFull
  , renderSchema
  )
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.IR.Builder (extractMethodDef)
import Synapse.IR.Types (IR(..), emptyIR)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

baseMethod :: MethodSchema
baseMethod = MethodSchema
  { methodName           = "move_doc"
  , methodDescription    = "Relocate a document."
  , methodHash           = "hash-move-doc"
  , methodParams         = Nothing
  , methodReturns        = Nothing
  , methodStreaming      = False
  , methodBidirectional  = False
  , methodRequestType    = Nothing
  , methodResponseType   = Nothing
  , methodDeprecation    = Nothing
  , methodParamSchemas   = Nothing
  , methodRole           = MethodRoleRpc
  }

deprecatedMethod :: MethodSchema
deprecatedMethod = baseMethod
  { methodDeprecation = Just DeprecationInfo
      { depSince     = "0.5"
      , depRemovedIn = "0.7"
      , depMessage   = "use move_doc"
      }
  }

plainMethod :: MethodSchema
plainMethod = baseMethod { methodName = "list_docs" }

basePlugin :: PluginSchema
basePlugin = PluginSchema
  { psNamespace       = "docs"
  , psVersion         = "1.0"
  , psDescription     = "Document activation"
  , psLongDescription = Nothing
  , psHash            = "hash-docs"
  , psMethods         = [deprecatedMethod, plainMethod]
  , psChildren        = Nothing
  , psDeprecation     = Nothing
  }

deprecatedPlugin :: PluginSchema
deprecatedPlugin = basePlugin
  { psDeprecation = Just DeprecationInfo
      { depSince     = "0.4"
      , depRemovedIn = "0.8"
      , depMessage   = "use docs_v2"
      }
  }

-- ---------------------------------------------------------------------------
-- IR-14 Fixtures: param-level deprecation
-- ---------------------------------------------------------------------------

-- | JSON Schema for a method with two parameters: 'path' (deprecated, the
--   one IR-14 decorates) and 'target' (not deprecated, the control).
--
--   This matches the minimum shape 'renderParamsFull' / 'extractParams'
--   actually inspect: an object with @properties@ and @required@.
paramsSchemaJSON :: Value
paramsSchemaJSON = object
  [ "type" .= ("object" :: T.Text)
  , "properties" .= object
      [ "path"   .= object
          [ "type"        .= ("string" :: T.Text)
          , "description" .= ("Old document path (deprecated)." :: T.Text)
          ]
      , "target" .= object
          [ "type"        .= ("string" :: T.Text)
          , "description" .= ("Destination identifier." :: T.Text)
          ]
      ]
  , "required" .= (["path", "target"] :: [T.Text])
  ]

-- | ParamSchema list carrying the per-param deprecation info for 'path'
--   and leaving 'target' clean. This is the IR-5 wire-level payload.
paramSchemas :: [ParamSchema]
paramSchemas =
  [ ParamSchema
      { paramName        = "path"
      , paramDescription = Just "Old document path (deprecated)."
      , paramRequired    = True
      , paramDeprecation = Just DeprecationInfo
          { depSince     = "0.5"
          , depRemovedIn = "0.7"
          , depMessage   = "use target"
          }
      }
  , ParamSchema
      { paramName        = "target"
      , paramDescription = Just "Destination identifier."
      , paramRequired    = True
      , paramDeprecation = Nothing
      }
  ]

-- | Method with a deprecated parameter. Used to drive both
--   'renderMethodFull' (Render.hs) and 'renderMethodHelp' (Help.hs).
methodWithDeprecatedParam :: MethodSchema
methodWithDeprecatedParam = baseMethod
  { methodName         = "relocate"
  , methodDescription  = "Relocate a document."
  , methodHash         = "hash-relocate"
  , methodParams       = Just paramsSchemaJSON
  , methodParamSchemas = Just paramSchemas
  }

-- | Build a minimal IR containing just this method, so we can exercise
--   'renderMethodHelp' which consumes the IR 'MethodDef' (where
--   'pdDeprecation' lives, lifted there by 'extractMethodDef').
helpFixture :: (IR, T.Text)
helpFixture =
  let (_types, mdef) = extractMethodDef "docs" "docs" methodWithDeprecatedParam
      fullPath       = "docs.relocate"
      ir = emptyIR { irMethods = Map.singleton fullPath mdef }
  in (ir, renderMethodHelp ir mdef)

main :: IO ()
main = hspec $ do
  describe "renderSchema (IR-6 method-level deprecation)" $ do
    it "prepends the warning marker to deprecated method names" $ do
      let rendered = renderSchema basePlugin
      rendered `shouldSatisfy` T.isInfixOf deprecationMarker

    it "emits the full DEPRECATED detail line for deprecated methods" $ do
      let rendered = renderSchema basePlugin
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use move_doc"

    it "leaves non-deprecated methods unchanged (no marker on their name line)" $ do
      let rendered = renderSchema basePlugin
          ls       = T.lines rendered
          -- Lines containing list_docs must NOT contain the marker.
          listDocLines = filter (T.isInfixOf "list_docs") ls
      all (not . T.isInfixOf deprecationMarker) listDocLines
        `shouldBe` True

    it "emits no deprecation markers for plugins whose methods all lack deprecation" $ do
      let cleanPlugin = basePlugin { psMethods = [plainMethod] }
          rendered    = renderSchema cleanPlugin
      rendered `shouldSatisfy` (not . T.isInfixOf deprecationMarker)
      rendered `shouldSatisfy` (not . T.isInfixOf "DEPRECATED")

  describe "renderSchema (IR-6 activation-level deprecation)" $ do
    it "renders a warning marker on the activation heading" $ do
      let rendered = renderSchema deprecatedPlugin
          ls       = T.lines rendered
          -- Some early line must contain both the namespace and the marker.
          decoratedHeading =
            any (\l -> T.isInfixOf "docs" l && T.isInfixOf deprecationMarker l)
                (take 3 ls)
      decoratedHeading `shouldBe` True

    it "includes the activation-level deprecation detail string" $ do
      let rendered = renderSchema deprecatedPlugin
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.4"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.8"
      rendered `shouldSatisfy` T.isInfixOf "use docs_v2"

    it "keeps activation rendering unchanged when psDeprecation is Nothing" $ do
      let rendered = renderSchema (basePlugin { psMethods = [plainMethod] })
      rendered `shouldSatisfy` (not . T.isInfixOf "use docs_v2")

  describe "renderMethodFull (IR-14 per-parameter deprecation in Render.hs)" $ do
    it "prepends the warning marker on the deprecated parameter's flag line" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
          ls       = T.lines rendered
          -- Line that mentions --path must carry the marker.
          pathLines = filter (T.isInfixOf "--path") ls
      pathLines `shouldSatisfy` (not . null)
      all (T.isInfixOf deprecationMarker) pathLines `shouldBe` True

    it "emits the DEPRECATED detail line for the deprecated parameter" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use target"

    it "leaves non-deprecated sibling parameter undecorated" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
          ls       = T.lines rendered
          -- Lines mentioning --target must NOT carry the marker.  We also
          -- filter out lines that happen to contain 'use target' (the
          -- deprecation detail line for --path), since that substring
          -- would false-match.
          targetLines =
            [ l | l <- ls
                , T.isInfixOf "--target" l
                , not (T.isInfixOf "use target" l)
            ]
      targetLines `shouldSatisfy` (not . null)
      all (not . T.isInfixOf deprecationMarker) targetLines `shouldBe` True

  describe "renderMethodHelp (IR-14 per-parameter deprecation in Help.hs)" $ do
    it "prepends the warning marker on the deprecated parameter's flag line" $ do
      let (_, rendered) = helpFixture
          ls            = T.lines rendered
          pathLines     = filter (T.isInfixOf "--path") ls
      pathLines `shouldSatisfy` (not . null)
      all (T.isInfixOf deprecationMarker) pathLines `shouldBe` True

    it "emits the DEPRECATED detail line for the deprecated parameter" $ do
      let (_, rendered) = helpFixture
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use target"

    it "leaves non-deprecated sibling parameter undecorated" $ do
      let (_, rendered) = helpFixture
          ls            = T.lines rendered
          targetLines =
            [ l | l <- ls
                , T.isInfixOf "--target" l
                , not (T.isInfixOf "use target" l)
            ]
      targetLines `shouldSatisfy` (not . null)
      all (not . T.isInfixOf deprecationMarker) targetLines `shouldBe` True
