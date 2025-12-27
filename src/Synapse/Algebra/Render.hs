-- | Render algebra for PluginSchema
module Synapse.Algebra.Render
  ( -- * Rendering Functions
    renderSchema
  , renderSchemaWith
  , renderMethod
  , renderMethodFull
  , renderChild
  , renderParams

    -- * Configuration
  , RenderStyle(..)
  , defaultStyle
  , compactStyle
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (intersperse, sortOn)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Synapse.Schema.Types

-- | Rendering style configuration
data RenderStyle = RenderStyle
  { rsIndent    :: !Int   -- ^ Spaces per indent level
  , rsShowHash  :: !Bool  -- ^ Show plugin/method hashes
  , rsShowTypes :: !Bool  -- ^ Show parameter types
  , rsCompact   :: !Bool  -- ^ Compact single-line format
  }
  deriving stock (Show, Eq)

-- | Default style: readable multi-line output
defaultStyle :: RenderStyle
defaultStyle = RenderStyle
  { rsIndent    = 2
  , rsShowHash  = False
  , rsShowTypes = True
  , rsCompact   = False
  }

-- | Compact style: single-line descriptions
compactStyle :: RenderStyle
compactStyle = defaultStyle { rsCompact = True }

-- | Render a PluginSchema
renderSchema :: PluginSchema -> Text
renderSchema = renderSchemaWith defaultStyle

-- | Render with custom style
renderSchemaWith :: RenderStyle -> PluginSchema -> Text
renderSchemaWith _style PluginSchema{..}
  | null psMethods && maybe True null psChildren = headerText
  | otherwise = renderStrict $ layoutPretty layoutOpts doc
  where
    layoutOpts = LayoutOptions (AvailablePerLine 80 1.0)

    headerText = psNamespace <> " v" <> psVersion <> "\n" <> psDescription <> "\n"

    doc :: Doc ann
    doc = vsep
      [ pretty psNamespace <+> pretty ("v" <> psVersion)
      , emptyDoc
      , indent 2 $ align $ fillSep $ map pretty $ T.words psDescription
      , emptyDoc
      , childrenDoc
      , methodsDoc
      ]

    childrenDoc = case psChildren of
      Nothing -> emptyDoc
      Just [] -> emptyDoc
      Just children -> vsep
        [ pretty ("activations" :: Text)
        , emptyDoc
        , indent 2 $ vsep $ map renderChildDoc (sortOn csNamespace children)
        , emptyDoc
        ]

    methodsDoc
      | null psMethods = emptyDoc
      | otherwise = vsep
        [ pretty ("methods" :: Text)
        , emptyDoc
        , indent 2 $ vsep $ intersperse emptyDoc $ map renderMethodDoc (sortOn methodName psMethods)
        ]

    renderChildDoc child = fillBreak 12 (pretty $ csNamespace child)
      <+> align (fillSep $ map pretty $ T.words $ csDescription child)

    renderMethodDoc method = vsep $
      [ fillBreak 12 (pretty $ methodName method)
          <+> align (fillSep $ map pretty $ T.words $ methodDescription method)
      ] ++ paramsDocs (methodParams method)

    paramsDocs Nothing = []
    paramsDocs (Just (Object o)) = case KM.lookup "properties" o of
      Just (Object props) ->
        let reqList = case KM.lookup "required" o of
              Just (Array arr) -> [t | String t <- foldr (:) [] arr]
              _ -> []
            propList = KM.toList props
            sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
        in [indent 12 $ vsep $ map (renderParamDoc reqList) sorted]
      _ -> []
    paramsDocs _ = []

    renderParamDoc :: [Text] -> (K.Key, Value) -> Doc ann
    renderParamDoc required (name, propSchema) =
      let nameText = K.toText name
          isReq = nameText `elem` required
          (typ, desc) = extractTypeDesc propSchema
          flag = "--" <> T.replace "_" "-" nameText
          typStr = "<" <> typ <> ">" <> if isReq then "" else "?"
          descWords = if T.null desc then [] else map pretty (T.words desc)
      in fillBreak 20 (pretty flag <+> pretty typStr)
         <+> align (fillSep descWords)

-- | Render a method (short form)
renderMethod :: MethodSchema -> Text
renderMethod = renderMethodWith defaultStyle

-- | Render a method with style
renderMethodWith :: RenderStyle -> MethodSchema -> Text
renderMethodWith RenderStyle{..} m =
  "  " <> padRight 16 (methodName m) <> methodDescription m
    <> if rsShowTypes then renderParams (methodParams m) else ""

-- | Render a method (full form with all params)
renderMethodFull :: MethodSchema -> Text
renderMethodFull m = T.unlines $
  [ methodName m <> " - " <> methodDescription m
  , ""
  ] <> paramLines
  where
    paramLines = case methodParams m of
      Nothing -> ["  (no parameters)"]
      Just schema -> renderParamsFull schema

-- | Render a child summary
renderChild :: ChildSummary -> Text
renderChild child =
  "  " <> padRight 16 (csNamespace child) <> csDescription child

-- | Render parameters inline
renderParams :: Maybe Value -> Text
renderParams Nothing = ""
renderParams (Just (Object o)) = case KM.lookup "properties" o of
  Just (Object props) ->
    let reqList = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- foldr (:) [] arr]
          _ -> []
        propList = KM.toList props
        sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
        rendered = map (renderParam reqList) sorted
    in if null rendered then "" else "\n" <> T.intercalate "\n" rendered
  _ -> ""
renderParams _ = ""

-- | Render a single parameter
renderParam :: [Text] -> (K.Key, Value) -> Text
renderParam required (name, propSchema) =
  let nameText = K.toText name
      isReq = nameText `elem` required
      flagName = T.replace "_" "-" nameText
      (typ, desc) = extractTypeDesc propSchema
      reqMarker = if isReq then "" else "?"
  in "      --" <> flagName <> reqMarker <> " <" <> typ <> ">  " <> desc

-- | Render parameters in full (for method help)
renderParamsFull :: Value -> [Text]
renderParamsFull (Object o) = case KM.lookup "properties" o of
  Just (Object props) ->
    let reqList = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- foldr (:) [] arr]
          _ -> []
        propList = KM.toList props
        sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
    in map (renderParamFull reqList) sorted
  _ -> []
renderParamsFull _ = []

renderParamFull :: [Text] -> (K.Key, Value) -> Text
renderParamFull required (name, propSchema) =
  let nameText = K.toText name
      isReq = nameText `elem` required
      (typ, desc) = extractTypeDesc propSchema
      reqText = if isReq then " (required)" else " (optional)"
  in "  --" <> nameText <> " : " <> typ <> reqText <> "\n      " <> desc

-- | Extract type and description from property schema
extractTypeDesc :: Value -> (Text, Text)
extractTypeDesc (Object po) =
  ( case KM.lookup "type" po of { Just (String t) -> t; _ -> "any" }
  , case KM.lookup "description" po of { Just (String d) -> d; _ -> "" }
  )
extractTypeDesc _ = ("any", "")

-- | Pad text to a minimum width
padRight :: Int -> Text -> Text
padRight n t
  | T.length t >= n = t <> " "
  | otherwise = t <> T.replicate (n - T.length t) " "
