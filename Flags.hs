module Flags where
import qualified Data.Set as S
import Control.Monad (msum)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

type Flags = S.Set Flag

data Flag 
    = DumpCore
    | DumpRename
    | DumpSimp
    | DumpLin
    | DumpAtom
    | DumpInline
    deriving (Eq,Ord,Show)

initialFlags :: Flags
initialFlags = S.empty

allFlags = S.fromList $ map snd flags

isSet :: Flags -> Flag -> Bool
isSet = flip S.member

set :: Flags -> Flag -> Flags
set = flip S.insert

unset :: Flags -> Flag -> Flags
unset = flip S.delete

flags :: [(String, Flag)]
flags = 
    [ ("ddump-core"   , DumpCore)
    , ("ddump-rn"     , DumpRename)
    , ("ddump-simp"   , DumpSimp)
    , ("ddump-lin"    , DumpLin)
    , ("ddump-atom"   , DumpAtom)
    , ("ddump-inline" , DumpInline)
    ]

matches :: String -> (String, Flag) -> Maybe Flag
matches s (flagstr, flag)
    | ('-' : flagstr) `isPrefixOf` s = Just flag
    | otherwise = Nothing

-- | Command line switches for flag options
flagOpts :: [String]
flagOpts = fmap fst flags

-- | Lookup the flag from a command line option switch.
flagFor :: String -> Maybe Flags.Flag
flagFor s = msum $ fmap (matches s) flags

stringFrom :: Flag -> String
stringFrom f = fromMaybe (error "No string for flag: " ++ show f) $
    lookup f (map (\(a,b) -> (b,a)) flags)
