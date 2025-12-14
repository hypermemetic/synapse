-- | Dynamic CLI - discovers capabilities from the substrate at runtime
module Main where

import Plexus (connect, disconnect, defaultConfig)
import Plexus.Client (plexusRpc)
import Plexus.Types (PlexusStreamItem(..))
import qualified Activation.Cone as Cone
import qualified Data.Aeson
import Data.Aeson (toJSON, Value, fromJSON, Result(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Streaming.Prelude as S
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["registry"] -> showRegistry
    ["models"] -> showModels
    ["models", family] -> showModelsByFamily (T.pack family)
    ["services"] -> showServices
    ["families"] -> showFamilies
    ["debug"] -> debugRaw
    _ -> usage

usage :: IO ()
usage = do
  putStrLn "symbols-dyn - Dynamic CLI for Plexus"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  symbols-dyn registry         Show full registry info"
  putStrLn "  symbols-dyn models           List all available models"
  putStrLn "  symbols-dyn models <family>  List models for a specific family"
  putStrLn "  symbols-dyn services         List available services"
  putStrLn "  symbols-dyn families         List model families"
  putStrLn ""
  putStrLn "This CLI discovers available capabilities from the running substrate."

showRegistry :: IO ()
showRegistry = do
  putStrLn "Fetching registry from substrate..."
  hFlush stdout
  conn <- connect defaultConfig
  mRegistry <- fetchRegistry conn
  disconnect conn
  case mRegistry of
    Nothing -> putStrLn "Failed to fetch registry"
    Just reg -> do
      let stats = Cone.registryStats reg
      putStrLn $ "Registry: "
        <> show (Cone.statsModelCount stats) <> " models, "
        <> show (Cone.statsServiceCount stats) <> " services, "
        <> show (Cone.statsFamilyCount stats) <> " families"
      putStrLn ""
      putStrLn "Services:"
      mapM_ printService (Cone.registryServices reg)
      putStrLn ""
      putStrLn "Families:"
      mapM_ (\f -> putStrLn $ "  " <> T.unpack f) (Cone.registryFamilies reg)

showModels :: IO ()
showModels = do
  conn <- connect defaultConfig
  mRegistry <- fetchRegistry conn
  disconnect conn
  case mRegistry of
    Nothing -> putStrLn "Failed to fetch registry"
    Just reg -> do
      putStrLn "Available models:"
      putStrLn ""
      mapM_ printModel (Cone.registryModels reg)

showModelsByFamily :: Text -> IO ()
showModelsByFamily family = do
  conn <- connect defaultConfig
  mRegistry <- fetchRegistry conn
  disconnect conn
  case mRegistry of
    Nothing -> putStrLn "Failed to fetch registry"
    Just reg -> do
      let models = filter (\m -> Cone.modelFamily m == family) (Cone.registryModels reg)
      if null models
        then putStrLn $ "No models found for family: " <> T.unpack family
        else do
          putStrLn $ "Models in family '" <> T.unpack family <> "':"
          putStrLn ""
          mapM_ printModel models

showServices :: IO ()
showServices = do
  conn <- connect defaultConfig
  mRegistry <- fetchRegistry conn
  disconnect conn
  case mRegistry of
    Nothing -> putStrLn "Failed to fetch registry"
    Just reg -> do
      putStrLn "Available services:"
      putStrLn ""
      mapM_ printService (Cone.registryServices reg)

showFamilies :: IO ()
showFamilies = do
  conn <- connect defaultConfig
  mRegistry <- fetchRegistry conn
  disconnect conn
  case mRegistry of
    Nothing -> putStrLn "Failed to fetch registry"
    Just reg -> do
      putStrLn "Model families:"
      mapM_ (\f -> putStrLn $ "  " <> T.unpack f) (Cone.registryFamilies reg)

-- | Fetch registry from the plexus
fetchRegistry :: Cone.PlexusConnection -> IO (Maybe Cone.RegistryExport)
fetchRegistry conn = do
  result <- S.head_ $ Cone.coneRegistry conn
  case result of
    Just (Cone.RegistryData reg) -> pure $ Just reg
    Just other -> do
      putStrLn $ "Got unexpected event: " <> show other
      pure Nothing
    Nothing -> do
      putStrLn "No events received from coneRegistry"
      pure Nothing

-- | Debug: show raw stream items
debugRaw :: IO ()
debugRaw = do
  putStrLn "Fetching raw stream items from cone_registry..."
  conn <- connect defaultConfig
  S.mapM_ printStreamItem $ plexusRpc conn "cone_registry" (toJSON ([] :: [Value]))
  disconnect conn

printStreamItem :: PlexusStreamItem -> IO ()
printStreamItem (StreamProgress prov msg pct) =
  putStrLn $ "PROGRESS: " <> T.unpack msg
printStreamItem (StreamData prov contentType dat) = do
  putStrLn $ "DATA [" <> T.unpack contentType <> "]"
  -- Try parsing as ConeEvent
  case fromJSON dat :: Result Cone.ConeEvent of
    Success evt -> putStrLn $ "  Parsed as ConeEvent"
    Error err -> putStrLn $ "  Parse error: " <> err
printStreamItem (StreamError prov err rec) =
  putStrLn $ "ERROR: " <> T.unpack err
printStreamItem (StreamDone prov) =
  putStrLn "DONE"

printService :: Cone.ServiceExport -> IO ()
printService svc = do
  putStrLn $ "  " <> T.unpack (Cone.serviceName svc)
  case Cone.serviceBaseUrl svc of
    Just url -> putStrLn $ "    URL: " <> T.unpack url
    Nothing -> pure ()
  case Cone.serviceMessageBuilder svc of
    Just builder -> putStrLn $ "    Builder: " <> T.unpack builder
    Nothing -> pure ()

printModel :: Cone.ModelExport -> IO ()
printModel m = do
  let caps = Cone.modelCapabilities m
  let pricing = Cone.modelPricing m
  putStrLn $ "  " <> T.unpack (Cone.modelId m)
  putStrLn $ "    Family: " <> T.unpack (Cone.modelFamily m)
    <> " | Service: " <> T.unpack (Cone.modelService m)
    <> " | Status: " <> T.unpack (Cone.modelStatus m)
  case (Cone.capContextWindow caps, Cone.capMaxOutputTokens caps) of
    (Just ctx, Just out) ->
      putStrLn $ "    Context: " <> show ctx <> " | Max output: " <> show out
    (Just ctx, Nothing) ->
      putStrLn $ "    Context: " <> show ctx
    _ -> pure ()
  case (Cone.pricingInputPer1k pricing, Cone.pricingOutputPer1k pricing) of
    (Just inp, Just out) ->
      putStrLn $ "    Pricing: $" <> show inp <> "/1k in, $" <> show out <> "/1k out"
    _ -> pure ()
