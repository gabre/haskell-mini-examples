module Main (main) where

import Lib
import Data.Map (fromList)

main :: IO ()
main =
    either
        error
        (putStrLn . show)
        (interpret
            (PLBinary BinOpAnd (PLAtomic "a") (PLAtomic "b"))
            (fromList [("a", True), ("b", False)]))
