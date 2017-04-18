module TyCon where
import Data.Data (Data)

data TyCon

instance Data TyCon
instance Show TyCon
instance Eq TyCon
instance Ord TyCon
