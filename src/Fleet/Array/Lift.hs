{-# LANGUAGE UnliftedDatatypes #-}
module Fleet.Array.Lift where

import GHC.Exts
import Data.Kind

type Lift :: UnliftedType -> Type
data Lift a = Lift a
