{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\n=== ⚡ SYNAPSE LIGHTNING BOLT THEMES ⚡ ==="
  putStrLn "Colors that instigate lightning bolt thoughts\n"
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Classic Lightning (White → Electric Blue → Deep Purple)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: ⚡ LIGHTNING STRIKE (White → Electric Blue → Purple)"
  putStrLn ""
  printDiagonalTLBRMulti [(255, 255, 255), (100, 200, 255), (150, 50, 255)]
  putStrLn "\ESC[97m    ⚡ NEURAL DISCHARGE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 2: Electric Arc (Bright Yellow → White → Cyan)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: ⚡ ELECTRIC ARC (Yellow → White → Cyan)"
  putStrLn ""
  printDiagonalTRBLMulti [(255, 255, 100), (255, 255, 255), (0, 255, 255)]
  putStrLn "\ESC[93m    ⚡ HIGH VOLTAGE SYNAPSE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: Thought Flash (Deep Blue → Bright White → Deep Blue)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: ⚡ FLASH OF INSIGHT (Blue → White → Blue)"
  putStrLn ""
  printDiagonalTLBRMulti [(0, 100, 200), (255, 255, 255), (0, 100, 200)]
  putStrLn "\ESC[96m    ⚡ SUDDEN REALIZATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: Plasma Bolt (Magenta → Bright Yellow → Cyan)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: ⚡ PLASMA DISCHARGE (Magenta → Yellow → Cyan)"
  putStrLn ""
  printDiagonalTRBLMulti [(255, 0, 255), (255, 255, 0), (0, 255, 255)]
  putStrLn "\ESC[95m    ⚡ NEURAL PLASMA BURST\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: Tesla Coil (Purple → White → Electric Blue)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: ⚡ TESLA COIL (Purple → White → Electric Blue)"
  putStrLn ""
  printDiagonalTLBRMulti [(200, 0, 255), (255, 255, 255), (0, 150, 255)]
  putStrLn "\ESC[95m    ⚡ ELECTROMAGNETIC THOUGHT\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: Neon Lightning (Bright Cyan → White → Hot Pink)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: ⚡ NEON BOLT (Cyan → White → Hot Pink)"
  putStrLn ""
  printDiagonalTRBLMulti [(0, 255, 255), (255, 255, 255), (255, 20, 147)]
  putStrLn "\ESC[96m    ⚡ NEON NEURAL SPARK\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: Solar Flare (Orange → Bright White → Cyan)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: ⚡ SOLAR FLARE (Orange → White → Cyan)"
  putStrLn ""
  printDiagonalTLBRMulti [(255, 150, 0), (255, 255, 255), (0, 255, 255)]
  putStrLn "\ESC[93m    ⚡ THERMAL IGNITION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 8: X-Pattern Lightning (Center bright white)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 8: ⚡ DUAL LIGHTNING (X-Pattern: Center White)"
  putStrLn ""
  printXPattern (100, 100, 255) (255, 255, 255)
  putStrLn "\ESC[97m    ⚡ NEURAL CROSSFIRE DISCHARGE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 9: Aurora Strike (Lime → White → Magenta)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 9: ⚡ AURORA STRIKE (Lime → White → Magenta)"
  putStrLn ""
  printDiagonalTRBLMulti [(200, 255, 0), (255, 255, 255), (255, 0, 255)]
  putStrLn "\ESC[92m    ⚡ POLAR NEURAL DISCHARGE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 10: Pure Energy (All White with slight color tint)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 10: ⚡ PURE ENERGY (White → Cyan-White → White)"
  putStrLn ""
  printDiagonalTLBRMulti [(255, 255, 255), (230, 255, 255), (255, 255, 255)]
  putStrLn "\ESC[97m    ⚡ MAXIMUM NEURAL EXCITATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- ANIMATION SECTION
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "⚡⚡⚡ ANIMATION: LIGHTNING PULSE ⚡⚡⚡"
  putStrLn ""
  putStrLn "Watch the lightning pulse through the neural network..."
  putStrLn "(Press Ctrl+C to stop)"
  putStrLn ""
  threadDelay 1000000

  -- Animate with pulsing white flash
  animateLightningPulse

-- The SYNAPSE logo
logo :: [String]
logo = [ "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
       , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
       , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
       , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
       , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
       , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
       ]

-- Top-left to bottom-right diagonal
printDiagonalTLBRMulti :: [(Int, Int, Int)] -> IO ()
printDiagonalTLBRMulti colors = do
  let height = length logo
      width = maximum (map length logo)
      maxDist = height + width - 2
  mapM_ (putStrLn . addIndent) $ zipWith (colorLineDiagTLBRMulti colors width maxDist) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

colorLineDiagTLBRMulti :: [(Int, Int, Int)] -> Int -> Int -> Int -> String -> String
colorLineDiagTLBRMulti colors width maxDist row line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar col char =
      let dist = row + col
          t = fromIntegral dist / fromIntegral maxDist
          (r, g, b) = interpolateMultiColor colors t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Top-right to bottom-left diagonal
printDiagonalTRBLMulti :: [(Int, Int, Int)] -> IO ()
printDiagonalTRBLMulti colors = do
  let height = length logo
      width = maximum (map length logo)
      maxDist = height + width - 2
  mapM_ (putStrLn . addIndent) $ zipWith (colorLineDiagTRBLMulti colors width maxDist) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

colorLineDiagTRBLMulti :: [(Int, Int, Int)] -> Int -> Int -> Int -> String -> String
colorLineDiagTRBLMulti colors width maxDist row line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar col char =
      let dist = row + (width - col - 1)
          t = fromIntegral dist / fromIntegral maxDist
          (r, g, b) = interpolateMultiColor colors t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- X-pattern: bright in center
printXPattern :: (Int, Int, Int) -> (Int, Int, Int) -> IO ()
printXPattern colorEdge colorCenter = do
  let height = length logo
      width = maximum (map length logo)
  mapM_ (putStrLn . addIndent) $ zipWith (colorLineXPattern colorEdge colorCenter width height) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

colorLineXPattern :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> Int -> String -> String
colorLineXPattern (r1, g1, b1) (r2, g2, b2) width height row line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar col char =
      let distTLBR = abs (row + col - (height + width - 2) `div` 2)
          distTRBL = abs (row + (width - col - 1) - (height + width - 2) `div` 2)
          minDist = min distTLBR distTRBL
          maxPossible = max height width
          t = fromIntegral minDist / fromIntegral maxPossible
          r = clamp $ interpolate r2 r1 t
          g = clamp $ interpolate g2 g1 t
          b = clamp $ interpolate b2 b1 t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Animate lightning pulse effect
animateLightningPulse :: IO ()
animateLightningPulse = do
  let baseColors = [(100, 150, 255), (0, 200, 255)]
      flashColor = (255, 255, 255)

  forM_ (cycle [0..]) $ \frame -> do
    -- Clear screen and move to top
    putStr "\ESC[2J\ESC[H"

    putStrLn "    ⚡ LIGHTNING PULSE THROUGH NEURAL NETWORK ⚡\n"

    -- Calculate pulse intensity (0 to 1)
    let pulse = (sin (fromIntegral frame / 10.0) + 1) / 2 :: Double
        -- Occasional bright flash
        isFlash = (frame `mod` 60) < 5

    if isFlash
      then do
        -- FLASH: Everything bright white
        printLogoSolid flashColor
        putStrLn "\ESC[97;1m    ⚡⚡⚡ NEURAL SPIKE ⚡⚡⚡\ESC[0m"
      else do
        -- Normal pulse: interpolate between base and flash
        let colors = if pulse > 0.7
                     then [(100, 150, 255), flashColor, (0, 200, 255)]
                     else [(100, 150, 255), (0, 200, 255)]
        printDiagonalTLBRMultiNoIndent colors
        putStrLn $ "\ESC[96m    Pulse intensity: " ++ replicate (round (pulse * 20)) '█' ++ "\ESC[0m"

    putStrLn "\n    ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯"
    putStrLn "\ESC[90m    Synaptic firing rate: " ++ show (round (pulse * 100)) ++ " Hz\ESC[0m"
    putStrLn "\ESC[90m    Press Ctrl+C to stop\ESC[0m"

    threadDelay 80000  -- 80ms (12.5 FPS for smooth pulse)

-- Print logo in solid color
printLogoSolid :: (Int, Int, Int) -> IO ()
printLogoSolid (r, g, b) = do
  mapM_ (putStrLn . colorLineSolid r g b) logo
  putStrLn "\ESC[0m"
  where
    colorLineSolid r g b line =
      "    " ++ printf "\ESC[38;2;%d;%d;%dm%s" r g b line

-- Same as printDiagonalTLBRMulti but without indent function (for animation)
printDiagonalTLBRMultiNoIndent :: [(Int, Int, Int)] -> IO ()
printDiagonalTLBRMultiNoIndent colors = do
  let height = length logo
      width = maximum (map length logo)
      maxDist = height + width - 2
  mapM_ putStrLn $ map ("    " ++) $ zipWith (colorLineDiagTLBRMulti colors width maxDist) [0..] logo
  putStrLn "\ESC[0m"

-- Interpolate through multiple color stops
interpolateMultiColor :: [(Int, Int, Int)] -> Double -> (Int, Int, Int)
interpolateMultiColor colors t
  | null colors = (255, 255, 255)
  | length colors == 1 = head colors
  | otherwise =
      let segments = length colors - 1
          scaledT = t * fromIntegral segments
          segmentIdx = min (segments - 1) (floor scaledT)
          localT = scaledT - fromIntegral segmentIdx
          (r1, g1, b1) = colors !! segmentIdx
          (r2, g2, b2) = colors !! (segmentIdx + 1)
      in ( clamp $ interpolate r1 r2 localT
         , clamp $ interpolate g1 g2 localT
         , clamp $ interpolate b1 b2 localT
         )

-- Linear interpolation
interpolate :: Int -> Int -> Double -> Int
interpolate a b t = round (fromIntegral a * (1 - t) + fromIntegral b * t)

-- Clamp to valid RGB range
clamp :: Int -> Int
clamp = max 0 . min 255

waitForEnter :: IO ()
waitForEnter = do
  putStr "Press ENTER for next... "
  hFlush stdout
  _ <- getLine
  putStrLn ""
  return ()
