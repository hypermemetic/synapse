{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for IR-6 deprecation rendering.
--
--   Covers the rendering side only. Invocation-time stderr notices
--   are deferred to a follow-up ticket per the IR-6 retry scope.
module Main where

import qualified Data.Text as T
import Test.Hspec

import Plexus.Schema.Recursive
  ( DeprecationInfo(..)
  , MethodSchema(..)
  , PluginSchema(..)
  )
import Synapse.Algebra.Render
  ( deprecationMarker
  , renderSchema
  )

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
  , methodRole           = Nothing
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
