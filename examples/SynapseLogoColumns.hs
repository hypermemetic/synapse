{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Data.List (transpose)

main :: IO ()
main = do
  putStrLn "\n=== SYNAPSE Column Gradient Preview ==="
  putStrLn "Coloring by COLUMN instead of row...\n"
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Cyan → Blue (left to right)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: Column Gradient (Cyan → Electric Blue)"
  putStrLn ""
  printColumnGradient 0 255 255  0 100 255
  putStrLn "\ESC[90m    ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯\ESC[0m"
  putStrLn "    EACH COLUMN FLOWS FROM LEFT TO RIGHT"
  putStrLn ""
  waitForEnter

  -- Option 2: Purple → Cyan (neural synaptic flow)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: Synaptic Flow (Purple → Cyan)"
  putStrLn ""
  printColumnGradient 150 50 255  0 255 255
  putStrLn "\ESC[35m    ▸ Neural signal propagation visualization\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: Ice → Deep Blue (cold precision)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: Ice Flow (White → Deep Blue)"
  putStrLn ""
  printColumnGradient 220 240 255  0 50 150
  putStrLn "\ESC[96m    ⚡ PRECISION GRADIENT CONTROL\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: Rainbow spectrum (full color)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: Rainbow Spectrum (Full color sweep)"
  putStrLn ""
  printRainbowColumns
  putStrLn "\ESC[36m    [ CHROMATIC NEURAL INTERFACE ]\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: Green → Blue (matrix neural)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: Matrix Neural (Green → Cyan → Blue)"
  putStrLn ""
  printColumnGradient 0 255 100  0 100 255
  putStrLn "\ESC[92m    ▸ SUBSTRATE CONNECTION LAYER\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: Fire (red → orange → yellow)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: Fire Flow (Red → Orange → Yellow)"
  putStrLn ""
  printColumnGradient 255 0 0  255 255 0
  putStrLn "\ESC[91m    ⚡ HIGH ENERGY SYNAPTIC ACTIVATION\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: Dual gradient (mirror)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: Mirror Gradient (Edges bright, center dark)"
  putStrLn ""
  printMirrorGradient
  putStrLn "\ESC[96m    ━━ SYMMETRIC NEURAL ARCHITECTURE ━━\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 8: Pulsing columns (alternating intensity)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 8: Wave Pattern (Oscillating brightness)"
  putStrLn ""
  printWavePattern
  putStrLn "\ESC[36m    Synaptic oscillation at rest state\ESC[0m"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nColumn gradients create FLOW and DIRECTION!"

-- The SYNAPSE logo as a 2D array
logo :: [String]
logo = [ "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
       , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
       , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
       , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
       , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
       , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
       ]

-- Print logo with column-based gradient
printColumnGradient :: Int -> Int -> Int -> Int -> Int -> Int -> IO ()
printColumnGradient r1 g1 b1 r2 g2 b2 = do
  let width = maximum (map length logo)
  mapM_ (putStrLn . addIndent) $ map (colorLineByColumn r1 g1 b1 r2 g2 b2 width) logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

-- Color each character in a line based on its column position
colorLineByColumn :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> String -> String
colorLineByColumn r1 g1 b1 r2 g2 b2 width line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar idx char =
      let t = fromIntegral idx / fromIntegral (width - 1)
          r = interpolate r1 r2 t
          g = interpolate g1 g2 t
          b = interpolate b1 b2 t
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Rainbow spectrum across columns
printRainbowColumns :: IO ()
printRainbowColumns = do
  let width = maximum (map length logo)
  mapM_ (putStrLn . addIndent) $ map (rainbowLine width) logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

rainbowLine :: Int -> String -> String
rainbowLine width line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar idx char =
      let t = fromIntegral idx / fromIntegral (width - 1)
          (r, g, b) = hueToRgb (t * 360)
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Convert hue (0-360) to RGB
hueToRgb :: Double -> (Int, Int, Int)
hueToRgb hue =
  let h = hue / 60
      x = 1 - abs (fromIntegral (floor h `mod` 2) - (h - fromIntegral (floor h :: Int)))
      (r', g', b') = case floor h :: Int of
                       0 -> (1, x, 0)
                       1 -> (x, 1, 0)
                       2 -> (0, 1, x)
                       3 -> (0, x, 1)
                       4 -> (x, 0, 1)
                       _ -> (1, 0, x)
  in (round (r' * 255), round (g' * 255), round (b' * 255))

-- Mirror gradient (bright on edges, dark in center)
printMirrorGradient :: IO ()
printMirrorGradient = do
  let width = maximum (map length logo)
  mapM_ (putStrLn . addIndent) $ map (mirrorLine width) logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

mirrorLine :: Int -> String -> String
mirrorLine width line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar idx char =
      let t = fromIntegral idx / fromIntegral (width - 1)
          -- Mirror: 0 at edges, 1 at center
          mirror = 1 - abs (t * 2 - 1)
          -- Bright cyan at edges, dark blue in center
          r = interpolate 0 0 mirror
          g = interpolate 255 50 mirror
          b = interpolate 255 150 mirror
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Wave pattern (sinusoidal brightness)
printWavePattern :: IO ()
printWavePattern = do
  let width = maximum (map length logo)
  mapM_ (putStrLn . addIndent) $ map (waveLine width) logo
  putStrLn "\ESC[0m"
  where
    addIndent s = "    " ++ s

waveLine :: Int -> String -> String
waveLine width line =
  concat $ zipWith colorChar [0..] line
  where
    colorChar idx char =
      let t = fromIntegral idx / fromIntegral (width - 1)
          -- Sine wave for brightness (0-1)
          wave = (sin (t * pi * 4) + 1) / 2
          -- Cyan with varying intensity
          r = 0
          g = round (255 * wave)
          b = round (255 * (0.5 + wave * 0.5))
      in printf "\ESC[38;2;%d;%d;%dm%c" r g b char

-- Linear interpolation
interpolate :: Int -> Int -> Double -> Int
interpolate a b t = round (fromIntegral a * (1 - t) + fromIntegral b * t)

waitForEnter :: IO ()
waitForEnter = do
  putStr "Press ENTER for next... "
  hFlush stdout
  _ <- getLine
  putStrLn ""
  return ()
