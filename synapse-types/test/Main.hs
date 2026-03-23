module Main (main) where

import Test.Hspec
import qualified Synapse.Types.RefinedSpec
import qualified Synapse.Types.ProtocolSpec
import qualified Synapse.Types.RegressionSpec

main :: IO ()
main = hspec $ do
  describe "Synapse.Types.Refined" Synapse.Types.RefinedSpec.spec
  describe "Synapse.Types.Protocol" Synapse.Types.ProtocolSpec.spec
  describe "Synapse.Types.Regression" Synapse.Types.RegressionSpec.spec
