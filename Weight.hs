{-# LANGUAGE DeriveDataTypeable #-}
module Weight where
import Data.Data

data Weight 
    = Zero
    | One
    | Omega
    deriving (Show,Eq,Ord,Data)

