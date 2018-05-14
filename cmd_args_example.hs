{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
import System.Console.CmdArgs

data Sample = Sample {hello :: String}
              deriving (Show, Data, Typeable)

sample = Sample{hello = def}

main = print =<< cmdArgs sample