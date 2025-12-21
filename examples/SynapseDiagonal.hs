{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "\n=== SYNAPSE Diagonal Gradients ==="
  putStrLn "Colors flow diagonally across the logo\n"
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Top-left to bottom-right (Cyan → Magenta)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: ↘ Diagonal (Cyan → Magenta)"
  putStrLn ""
  printDiagonalTLBR (0, 255, 255) (255, 0, 255)
  putStrLn "\ESC[96m    ⚡ TOP-LEFT TO BOTTOM-RIGHT FLOW\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 2: Top-right to bottom-left (Yellow → Blue)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: ↙ Diagonal (Yellow → Electric Blue)"
  putStrLn ""
  printDiagonalTRBL (255, 255, 0) (0, 150, 255)
  putStrLn "\ESC[93m    ⚡ TOP-RIGHT TO BOTTOM-LEFT FLOW\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: TL-BR Orange → Cyan
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: ↘ Diagonal (Orange → Cyan)"
  putStrLn ""
  printDiagonalTLBR (255, 165, 0) (0, 255, 255)
  putStrLn "\ESC[33m    ⚡ THERMAL CASCADE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: TR-BL Lime → Magenta
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: ↙ Diagonal (Lime → Magenta)"
  putStrLn ""
  printDiagonalTRBL (200, 255, 0) (255, 0, 255)
  putStrLn "\ESC[92m    ⚡ NEURAL EXCITATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: TL-BR Hot Pink → Cyan
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: ↘ Diagonal (Hot Pink → Cyan)"
  putStrLn ""
  printDiagonalTLBR (255, 50, 150) (0, 255, 255)
  putStrLn "\ESC[95m    ⚡ SYNAPTIC WAVE\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: TR-BL Bright Green → Electric Blue
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: ↙ Diagonal (Bright Green → Electric Blue)"
  putStrLn ""
  printDiagonalTRBL (0, 255, 100) (100, 150, 255)
  putStrLn "\ESC[92m    ⚡ MATRIX FLOW\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: TL-BR with 3 stops (Cyan → Yellow → Magenta)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: ↘ Diagonal 3-Stop (Cyan → Yellow → Magenta)"
  putStrLn ""
  printDiagonalTLBRMulti [(0, 255, 255), (255, 255, 0), (255, 0, 255)]
  putStrLn "\ESC[96m    ⚡ CHROMATIC DIAGONAL\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 8: TR-BL with 3 stops (Orange → Lime → Cyan)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 8: ↙ Diagonal 3-Stop (Orange → Lime → Cyan)"
  putStrLn ""
  printDiagonalTRBLMulti [(255, 165, 0), (200, 255, 0), (0, 255, 255)]
  putStrLn "\ESC[33m    ⚡ THERMAL SPECTRUM\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 9: TL-BR Rainbow
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 9: ↘ Diagonal Rainbow"
  putStrLn ""
  printDiagonalTLBRMulti [(255, 0, 0), (255, 165, 0), (255, 255, 0), (0, 255, 100), (0, 150, 255), (255, 0, 255)]
  putStrLn "\ESC[91m    ⚡ FULL SPECTRUM DIAGONAL\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 10: X-pattern (center bright, edges dark)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 10: X-Pattern (Dual diagonal from center)"
  putStrLn ""
  printXPattern (50, 50, 150) (0, 255, 255)
  putStrLn "\ESC[96m    ⚡ NEURAL CROSSFIRE\ESC[0m"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nDiagonal gradients create DYNAMIC FLOW!"

-- The SYNAPSE logo
logo :: [String]
logo = [ "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
       , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
       , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
       , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
       , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
       , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
       ]

-- Top-left to bottom-right diagonal (row + col)
printDiagonalTLBR :: (Int, Int, Int) -> (Int, Int, Int) -> IO ()
printDiagonalTLBR color1 color2 = do
  let height = length logo
      width = maximum (map length logo)
      maxDist = height + width - 2
  mapM_ (putStrLn . addIndent) $ zipWith (colorLineDiagTLBR color1 color2 width maxDist) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

colorLineDiagTLBR :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> Int -> String -> String
colorLineDiagTLBR (r1, g1, b1) (r2, g2, b2) width maxDist row line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar col char =
      let dist = row + col
          t = fromIntegral dist / fromIntegral maxDist
          r = clamp $ interpolate r1 r2 t
          g = clamp $ interpolate g1 g2 t
          b = clamp $ interpolate b1 b2 t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Top-right to bottom-left diagonal (row + (width - col))
printDiagonalTRBL :: (Int, Int, Int) -> (Int, Int, Int) -> IO ()
printDiagonalTRBL color1 color2 = do
  let height = length logo
      width = maximum (map length logo)
      maxDist = height + width - 2
  mapM_ (putStrLn . addIndent) $ zipWith (colorLineDiagTRBL color1 color2 width maxDist) [0..] logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

colorLineDiagTRBL :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int -> Int -> String -> String
colorLineDiagTRBL (r1, g1, b1) (r2, g2, b2) width maxDist row line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar col char =
      let dist = row + (width - col - 1)
          t = fromIntegral dist / fromIntegral maxDist
          r = clamp $ interpolate r1 r2 t
          g = clamp $ interpolate g1 g2 t
          b = clamp $ interpolate b1 b2 t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Multi-color diagonal (TL-BR)
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

-- Multi-color diagonal (TR-BL)
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

-- X-pattern: bright in center, dark on edges
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
      let -- Distance from both diagonals
          distTLBR = abs (row + col - (height + width - 2) `div` 2)
          distTRBL = abs (row + (width - col - 1) - (height + width - 2) `div` 2)
          -- Use minimum distance to create X pattern
          minDist = min distTLBR distTRBL
          maxPossible = max height width
          t = fromIntegral minDist / fromIntegral maxPossible
          r = clamp $ interpolate r2 r1 t  -- Reversed: center bright
          g = clamp $ interpolate g2 g1 t
          b = clamp $ interpolate b2 b1 t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

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
