{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "\n=== SYNAPSE Control Aesthetics Preview ==="
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Smooth RGB Gradient (Cyan → Blue)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: True Color RGB Gradient (Cyan → Electric Blue)"
  putStrLn ""
  printRgbGradient 0 255 255  0 100 255  6  -- cyan to blue
  putStrLn "\ESC[90m    ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯\ESC[0m"
  putStrLn "    PRECISION ORCHESTRATION · ADAPTIVE CONTROL"
  putStrLn ""
  waitForEnter

  -- Option 2: Tighter gradient with border
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: Framed Control (Sharp borders, gradient fill)"
  putStrLn ""
  putStrLn "\ESC[90m    ╔═══════════════════════════════════════════════════════════════╗\ESC[0m"
  printRgbGradient 0 200 255  50 50 200  6
  putStrLn "\ESC[90m    ╚═══════════════════════════════════════════════════════════════╝\ESC[0m"
  putStrLn "\ESC[96m         Command · Control · Orchestrate · Execute\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: Purple → Blue (deeper control)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: Deep Neural Control (Purple → Blue)"
  putStrLn ""
  printRgbGradient 150 50 255  50 100 255  6  -- purple to blue
  putStrLn "\ESC[35m    ▸ Synaptic Control Layer\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: Ice blue (cold precision)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: Ice Control (Cold precision, white → cyan)"
  putStrLn ""
  printRgbGradient 200 240 255  0 180 255  6  -- ice to cyan
  putStrLn "\ESC[96m    ⚡ ZERO-LATENCY ORCHESTRATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: Gradient with grid overlay
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: Grid Control (Technical precision)"
  putStrLn ""
  putStrLn "\ESC[90m    ┌─────────────────────────────────────────────────────────────┐\ESC[0m"
  printRgbGradient 0 255 200  0 100 255  6
  putStrLn "\ESC[90m    └─────────────────────────────────────────────────────────────┘\ESC[0m"
  putStrLn "\ESC[36m    [ ADAPTIVE INTERFACE ] [ RUNTIME DISCOVERY ] [ TYPE-SAFE ]\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: Minimal with accent
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: Minimal Control (Clean authority)"
  putStrLn ""
  printRgbGradient 0 200 255  0 120 255  6
  putStrLn ""
  putStrLn "\ESC[96;1m    ▸ CONTROL SUBSTRATE FROM THE COMMAND LINE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: Electric gradient (high energy)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: Electric Control (High voltage aesthetic)"
  putStrLn ""
  printRgbGradient 100 255 255  0 150 255  6  -- bright cyan to deep blue
  putStrLn "\ESC[96m    ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\ESC[0m"
  putStrLn "\ESC[96m    SYNAPTIC COMMAND INTERFACE • PLEXUS CONTROL LAYER\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 8: Gradient blocks (smooth texture)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 8: Textured Gradient (Block density variation)"
  putStrLn ""
  printGradientBlocks
  putStrLn "\ESC[36m         Precision Orchestration at the Neural Layer\ESC[0m"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nWhich aesthetic best conveys CONTROL?"

-- Print the SYNAPSE logo with smooth RGB gradient
printRgbGradient :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
printRgbGradient r1 g1 b1 r2 g2 b2 lines = do
  let logo = [ "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
             , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
             , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
             , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
             , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
             , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
             ]
  mapM_ (putStrLn . addIndent) $ zipWith (colorLine r1 g1 b1 r2 g2 b2 (length logo)) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

-- Color a single line with interpolated RGB
colorLine :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> String
colorLine r1 g1 b1 r2 g2 b2 total idx line =
  let t = fromIntegral idx / fromIntegral (total - 1)
      r = interpolate r1 r2 t
      g = interpolate g1 g2 t
      b = interpolate b1 b2 t
  in printf "\ESC[38;2;%d;%d;%dm%s" r g b line

-- Linear interpolation
interpolate :: Int -> Int -> Double -> Int
interpolate a b t = round (fromIntegral a * (1 - t) + fromIntegral b * t)

-- Print with gradient block density
printGradientBlocks :: IO ()
printGradientBlocks = do
  putStrLn "\ESC[96m    ████████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "\ESC[96m    ██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "\ESC[36m    ▓▓▓▓▓▓▓▓ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "\ESC[36m    ╚════▒▒▒  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "\ESC[34m    ░░░░░░░░   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "\ESC[34m    ╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"

waitForEnter :: IO ()
waitForEnter = do
  putStr "Press ENTER for next... "
  hFlush stdout
  _ <- getLine
  putStrLn ""
  return ()
