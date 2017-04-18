{-# LANGUAGE DeriveDataTypeable #-}
module DataCon where
import {-# SOURCE #-} Type
import Data.Data

data DataCon = MkData {
        dcName :: String,
        dcUnique :: !Int,
        dcOrigArgTys :: [Type],
        --dcFields :: [FieldLabel], --I suspect this is for records
        dcOrigResTy :: Type
    }

    
    deriving(Eq,Ord,Show,Data)

