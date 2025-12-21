{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout, hSetBuffering, BufferMode(NoBuffering))
import Text.Printf (printf)
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.List (scanl')

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\n=== SYNAPSE Letter-Level Color Flow ==="
  putStrLn "Colors change only at LETTER BOUNDARIES\n"
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Bright Cyan → Magenta
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: Cyan → Magenta (High contrast)"
  putStrLn ""
  printLetterGradient [(0, 255, 255), (255, 0, 255)]
  putStrLn "\ESC[96m    ⚡ ELECTRIC NEURAL FLOW\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 2: Electric Blue → Bright Green
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: Electric Blue → Bright Green"
  putStrLn ""
  printLetterGradient [(100, 200, 255), (0, 255, 100)]
  putStrLn "\ESC[92m    ▸ MATRIX NEURAL PATHWAY\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: Orange → Cyan (warm to cool)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: Orange → Cyan (Thermal gradient)"
  putStrLn ""
  printLetterGradient [(255, 165, 0), (0, 255, 255)]
  putStrLn "\ESC[33m    ⚡ ACTIVATION ENERGY FLOW\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: Bright Yellow → Magenta
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: Yellow → Magenta (High visibility)"
  putStrLn ""
  printLetterGradient [(255, 255, 0), (255, 0, 255)]
  putStrLn "\ESC[93m    ▸ SYNAPTIC SIGNAL PROPAGATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: Lime → Electric Blue
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: Lime → Electric Blue"
  putStrLn ""
  printLetterGradient [(200, 255, 0), (0, 150, 255)]
  putStrLn "\ESC[92m    ⚡ NEURAL EXCITATION CASCADE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: Magenta → Cyan (cool spectrum)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: Magenta → Cyan (Cool spectrum)"
  putStrLn ""
  printLetterGradient [(255, 100, 255), (100, 255, 255)]
  putStrLn "\ESC[95m    ▸ SUBSTRATE NEURAL INTERFACE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: Hot Pink → Lime
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: Hot Pink → Lime (Maximum contrast)"
  putStrLn ""
  printLetterGradient [(255, 50, 150), (150, 255, 50)]
  putStrLn "\ESC[95m    ⚡ PEAK NEURAL ACTIVITY\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 8: Bright Cyan → Yellow → Magenta (3-stop)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 8: Cyan → Yellow → Magenta (3-stop gradient)"
  putStrLn ""
  printLetterGradient [(0, 255, 255), (255, 255, 0), (255, 0, 255)]
  putStrLn "\ESC[96m    ▸ FULL SPECTRUM NEURAL TRANSMISSION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 9: Rainbow (multi-stop)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 9: Rainbow Spectrum (6 stops)"
  putStrLn ""
  printLetterGradient [(255, 0, 0), (255, 165, 0), (255, 255, 0), (0, 255, 0), (0, 150, 255), (255, 0, 255)]
  putStrLn "\ESC[91m    ⚡ CHROMATIC SYNAPTIC CASCADE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 10: Neon (alternating bright colors)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 10: Neon (Cyan/Magenta/Yellow cycle)"
  putStrLn ""
  printLetterGradient [(0, 255, 255), (255, 0, 255), (255, 255, 0), (0, 255, 255)]
  putStrLn "\ESC[96m    ▸ NEON NEURAL NETWORK\ESC[0m"
  putStrLn ""
  waitForEnter

  -- ANIMATION SECTION
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "ANIMATION: Flowing Colors (press Ctrl+C to stop)"
  putStrLn ""
  putStrLn "Watch the colors flow through the synaptic network..."
  putStrLn ""
  threadDelay 1000000

  -- Animate with flowing gradient
  animateFlow [(0, 255, 255), (255, 0, 255), (255, 255, 0), (0, 255, 100)]

-- The SYNAPSE logo
logo :: [String]
logo = [ "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
       , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
       , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
       , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
       , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
       , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
       ]

-- Detect letter boundaries and assign colors only at those points
printLetterGradient :: [(Int, Int, Int)] -> IO ()
printLetterGradient colors = do
  let letterBoundaries = findLetterBoundaries (head logo)
      totalLetters = length letterBoundaries
  mapM_ (putStrLn . addIndent) $ map (colorLineByLetters colors letterBoundaries totalLetters) logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

-- Find column positions where letters start (transition from space to non-space)
findLetterBoundaries :: String -> [Int]
findLetterBoundaries line =
  [i | (i, (prev, curr)) <- zip [0..] (zip (' ':line) line), isSpace prev && not (isSpace curr)]
  where
    isSpace c = c == ' '

-- Color a line by assigning colors to each letter
colorLineByLetters :: [(Int, Int, Int)] -> [Int] -> Int -> String -> String
colorLineByLetters colors boundaries totalLetters line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar idx char =
      let letterIdx = findLetterIndex idx boundaries
          t = fromIntegral letterIdx / fromIntegral (max 1 (totalLetters - 1))
          (r, g, b) = interpolateMultiColor colors t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

    findLetterIndex :: Int -> [Int] -> Int
    findLetterIndex idx bounds =
      length $ takeWhile (<= idx) bounds

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
      in ( interpolate r1 r2 localT
         , interpolate g1 g2 localT
         , interpolate b1 b2 localT
         )

-- Linear interpolation
interpolate :: Int -> Int -> Double -> Int
interpolate a b t = round (fromIntegral a * (1 - t) + fromIntegral b * t)

-- Animate flowing colors
animateFlow :: [(Int, Int, Int)] -> IO ()
animateFlow colors = do
  forM_ [0..] $ \frame -> do
    -- Clear screen and move to top
    putStr "\ESC[2J\ESC[H"

    putStrLn "    ⚡ SYNAPTIC SIGNAL FLOW ⚡\n"

    -- Shift colors based on frame
    let offset = (fromIntegral frame / 20.0) :: Double
        shiftedColors = shiftColorPhase colors offset
        letterBoundaries = findLetterBoundaries (head logo)
        totalLetters = length letterBoundaries

    mapM_ (putStrLn . addIndent) $ map (colorLineByLetters shiftedColors letterBoundaries totalLetters) logo
    putStrLn "\ESC[0m"

    putStrLn "\n    ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯"
    putStrLn "\ESC[90m    Neural activity propagating through substrate...\ESC[0m"
    putStrLn "\ESC[90m    Press Ctrl+C to stop\ESC[0m"

    threadDelay 50000  -- 50ms delay (20 FPS)
  where
    addIndent s = "    " ++ s

-- Shift color palette by phase offset (creates animation effect)
shiftColorPhase :: [(Int, Int, Int)] -> Double -> [(Int, Int, Int)]
shiftColorPhase colors offset =
  let numColors = length colors
      indices = [0 .. numColors - 1]
      shiftedIndices = map (\i -> (fromIntegral i + offset) / fromIntegral numColors) indices
  in map (interpolateMultiColor colors . snd . properFraction) shiftedIndices

waitForEnter :: IO ()
waitForEnter = do
  putStr "Press ENTER for next... "
  hFlush stdout
  _ <- getLine
  putStrLn ""
  return ()
