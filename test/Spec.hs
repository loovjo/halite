{-# LANGUAGE ScopedTypeVariables #-}

import qualified TestTypes
import qualified TestTypeSolver


main :: IO ()
main = sequence_
    [ putStrLn ""
    , TestTypes.testTypes
    , TestTypeSolver.testTypeSolver
    ]

