module Main where

import Control.Monad.Eff.Console
import Math (sqrt)
import Prelude
import Data.List
import Data.Maybe

import Control.Plus (empty)

diagonal w h = sqrt (w * w + h * h)

main = print (diagonal 3.0 4.0)
