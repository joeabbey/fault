module Sample (greet, add, Config) where

import Data.List
import qualified Data.Map as Map
import Control.Monad (when, unless)

-- | A configuration type.
data Config = Config
  { name :: String
  , port :: Int
  }

-- | Greet a user.
greet :: String -> String
greet name = "Hello, " ++ name

-- | Add two numbers.
add :: Int -> Int -> Int
add x y = x + y

-- Internal helper.
helper :: String -> IO ()
helper msg = putStrLn msg
