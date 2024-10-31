module Katex(localKatex) where

import System.Process(readProcess)
import System.IO.Unsafe(unsafePerformIO)

import Data.Text
import Text.Pandoc
import Text.Pandoc.Walk(walk)

localKatex :: Pandoc -> Pandoc
localKatex = walk processMath

processMath :: Inline -> Inline
processMath (Math typ txt) =
    let svg = unsafePerformIO $ runKatex typ (unpack txt)
    in RawInline (Format $ pack "html") (pack svg) -- Math typ (pack "gaga")
    where
        runKatex :: MathType -> String -> IO String
        runKatex DisplayMath input = runKatexWithExtraArgs ["-d"] input
        runKatex InlineMath input = runKatexWithExtraArgs [] input

        runKatexWithExtraArgs :: [String] -> String -> IO String
        runKatexWithExtraArgs args input = readProcess "katex" (args ++ ["-T"]) input
processMath b = b