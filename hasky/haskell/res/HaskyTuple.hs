module HaskyTuple where

import Foreign.Storable

data Tuple2 a b = Tuple2
    { c2fst :: a
    , c2snd :: b
    } deriving (Show, Eq)

t2Size :: (Storable a, Storable b) => Tuple2 a b -> Int
t2Size ct = (sizeOf $ c2fst ct) + (sizeOf $ c2snd ct)

t2Alignment :: (Storable a, Storable b) => Tuple2 a b -> Int
t2Alignment ct = aConstraint + bConstraint
    where aConstraint = alignment $ c2fst ct
          bConstraint = alignment $ c2snd ct

instance (Storable a, Storable b) => Storable (Tuple2 a b) where
    sizeOf    = t2Size
    alignment = t2Alignment
    peek _    = undefined -- Tuples are only valid as return types.
    poke _ _  = undefined -- we never inpoke side-effects ;)

data Tuple3 a b c = Tuple3
    { c3fst :: a
    , c3snd :: b
    , c3trd :: c
    } deriving (Show, Eq)

t3Size :: (Storable a, Storable b, Storable c) => Tuple3 a b c -> Int
t3Size ct = (sizeOf $ c3fst ct)  + (sizeOf $ c3snd ct) + (sizeOf $ c3trd ct)

t3Alignment :: (Storable a, Storable b, Storable c) => Tuple3 a b c -> Int
t3Alignment ct = aConstraint + bConstraint + cConstraint
    where aConstraint = alignment $ c3fst ct
          bConstraint = alignment $ c3snd ct
          cConstraint = alignment $ c3trd ct

instance (Storable a, Storable b, Storable c) => Storable (Tuple3 a b c) where
    sizeOf    = t3Size
    alignment = t3Alignment
    peek _    = undefined
    poke _ _  = undefined
