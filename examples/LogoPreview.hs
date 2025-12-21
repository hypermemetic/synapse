{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "\n=== SYNAPSE Logo Preview ==="
  putStrLn "Press ENTER to cycle through options...\n"

  -- Option 1: Cyan Gradient
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 1: Cyan Gradient (neural/electric)"
  putStrLn ""
  putStrLn "\ESC[96m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "\ESC[36m███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "\ESC[34m███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 2: Bold Cyan
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 2: Bold Cyan (clean, high contrast)"
  putStrLn ""
  putStrLn "\ESC[96;1m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 3: Magenta
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 3: Magenta (neural/brain vibe)"
  putStrLn ""
  putStrLn "\ESC[95m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 4: Green
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 4: Bright Green (Matrix/tech style)"
  putStrLn ""
  putStrLn "\ESC[92m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 5: Blue
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 5: Blue (calm, professional)"
  putStrLn ""
  putStrLn "\ESC[94m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 6: Purple-Blue gradient
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 6: Purple-Blue Gradient (deep neural)"
  putStrLn ""
  putStrLn "\ESC[95m███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "\ESC[35m███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "\ESC[34m███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝\ESC[0m"
  putStrLn ""
  waitForEnter

  -- Option 7: No color (current)
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "Option 7: No Color (current - classic, universal)"
  putStrLn ""
  putStrLn "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  putStrLn "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  putStrLn "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  putStrLn "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  putStrLn "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  putStrLn "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
  putStrLn ""

  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
  putStrLn "\nWhich one do you like best?"

waitForEnter :: IO ()
waitForEnter = do
  putStr "Press ENTER for next... "
  hFlush stdout
  _ <- getLine
  putStrLn ""
  return ()
