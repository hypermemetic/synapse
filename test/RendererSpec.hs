{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Template renderer tests with real response samples
module Main (main) where

import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Plexus.Renderer (renderValue, loadTemplateText)

-- ============================================================================
-- Sample Responses (from actual claudecode chat)
-- ============================================================================

-- | ClaudeCode chat content event
sampleContentEvent :: Value
sampleContentEvent = [aesonQQ|{
  "id": "d3dbb40a-8f1a-4789-84d4-dc278c6a7457",
  "type": "content",
  "text": "Hi! How can I help you?"
}|]

-- | ClaudeCode Read tool use (with enrichment - tool_name merged into tool_input)
sampleReadToolUse :: Value
sampleReadToolUse = [aesonQQ|{
  "id": "03b52558-8ed6-4e38-82d2-47be82434ed5",
  "tool_input": {
    "file_path": "/Users/shmendez/dev/controlflow/substrate/Cargo.toml",
    "tool_name": "Read"
  },
  "tool_name": "Read",
  "tool_use_id": "toolu_01EATfK9kFskYzSWKpXCLwZF",
  "type": "tool_use"
}|]

-- | ClaudeCode Bash tool use (with enrichment)
sampleBashToolUse :: Value
sampleBashToolUse = [aesonQQ|{
  "id": "21549858-e33a-464c-be7f-956f33376c76",
  "tool_input": {
    "command": "ls -la /Users/shmendez/dev/controlflow/substrate",
    "description": "List substrate directory contents",
    "tool_name": "Bash"
  },
  "tool_name": "Bash",
  "tool_use_id": "toolu_01VKkUBYvA1Y1jprFCUSsZQQ",
  "type": "tool_use"
}|]

-- | ClaudeCode Edit tool use (with enrichment)
sampleEditToolUse :: Value
sampleEditToolUse = [aesonQQ|{
  "id": "test-id",
  "tool_input": {
    "file_path": "/path/to/file.rs",
    "old_string": "fn old() {}",
    "new_string": "fn new() {}",
    "tool_name": "Edit"
  },
  "tool_name": "Edit",
  "tool_use_id": "toolu_edit123",
  "type": "tool_use"
}|]

-- | ClaudeCode Grep tool use (with enrichment)
sampleGrepToolUse :: Value
sampleGrepToolUse = [aesonQQ|{
  "id": "test-id",
  "tool_input": {
    "pattern": "fn main",
    "path": "/path/to/src",
    "tool_name": "Grep"
  },
  "tool_name": "Grep",
  "tool_use_id": "toolu_grep123",
  "type": "tool_use"
}|]

-- | ClaudeCode chat start
sampleStartEvent :: Value
sampleStartEvent = [aesonQQ|{
  "id": "d3dbb40a-8f1a-4789-84d4-dc278c6a7457",
  "type": "start",
  "user_position": {
    "node_id": "fe940a44-79e2-453c-88e2-651ecd279d00",
    "tree_id": "795aa963-8028-43cc-990c-22d13940c527"
  }
}|]

-- | ClaudeCode chat complete
sampleCompleteEvent :: Value
sampleCompleteEvent = [aesonQQ|{
  "claude_session_id": "796fdda2-3982-42f6-a0ea-6de5bf69b11f",
  "new_head": {
    "node_id": "98874969-420e-4c7a-9b68-748f3a6b3d1e",
    "tree_id": "795aa963-8028-43cc-990c-22d13940c527"
  },
  "type": "complete",
  "usage": {
    "cost_usd": null,
    "input_tokens": null,
    "num_turns": 1,
    "output_tokens": null
  }
}|]

-- | Cone chat content event (for comparison)
sampleConeContentEvent :: Value
sampleConeContentEvent = [aesonQQ|{
  "cone_id": "b7442c8a-3c6d-4efe-b2d0-47bb8173a135",
  "content": "Hi there!",
  "type": "chat_content"
}|]

-- ============================================================================
-- Templates
-- ============================================================================

claudecodeTemplate :: Text
claudecodeTemplate = "{{{text}}}{{#tool_input}}\n\n🔧 {{tool_name}} {{file_path}}{{#command}}\"{{.}}\"{{/command}}{{pattern}}\n{{/tool_input}}"

coneTemplate :: Text
coneTemplate = "{{{content}}}"

-- ============================================================================
-- Tests
-- ============================================================================

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Template Renderer" $ do

  describe "ClaudeCode chat template" $ do
    let template = loadTemplateText claudecodeTemplate

    it "renders content events correctly" $ do
      case renderValue template sampleContentEvent of
        Right result -> result `shouldBe` "Hi! How can I help you?"
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders Read tool use correctly" $ do
      case renderValue template sampleReadToolUse of
        Right result -> do
          T.unpack result `shouldContain` "🔧 Read"
          T.unpack result `shouldContain` "/Users/shmendez/dev/controlflow/substrate/Cargo.toml"
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders Bash tool use correctly" $ do
      case renderValue template sampleBashToolUse of
        Right result -> do
          T.unpack result `shouldContain` "🔧 Bash"
          T.unpack result `shouldContain` "\"ls -la /Users/shmendez/dev/controlflow/substrate\""
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders Edit tool use correctly" $ do
      case renderValue template sampleEditToolUse of
        Right result -> do
          T.unpack result `shouldContain` "🔧 Edit"
          T.unpack result `shouldContain` "/path/to/file.rs"
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders Grep tool use correctly" $ do
      case renderValue template sampleGrepToolUse of
        Right result -> do
          T.unpack result `shouldContain` "🔧 Grep"
          T.unpack result `shouldContain` "fn main"
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders start events as empty (no visible output)" $ do
      case renderValue template sampleStartEvent of
        Right result -> result `shouldBe` ""
        Left err -> expectationFailure $ "Render failed: " <> show err

    it "renders complete events as empty (no visible output)" $ do
      case renderValue template sampleCompleteEvent of
        Right result -> result `shouldBe` ""
        Left err -> expectationFailure $ "Render failed: " <> show err

  describe "Cone chat template" $ do
    let template = loadTemplateText coneTemplate

    it "renders content events correctly" $ do
      case renderValue template sampleConeContentEvent of
        Right result -> result `shouldBe` "Hi there!"
        Left err -> expectationFailure $ "Render failed: " <> show err

