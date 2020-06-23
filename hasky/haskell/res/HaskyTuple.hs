module HaskyTuple where

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils (new)

type CTuple2 a b = Ptr (Tuple2 a b)

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

newTuple2 :: (Storable a, Storable b) => a -> b -> IO (CTuple2 a b)
newTuple2 x y = new $ Tuple2 x y

goto2Snd :: (Storable a, Storable b) => Ptr (Tuple2 a b) -> a -> Ptr b
goto2Snd ptr x = let align p = alignPtr p $ sizeOf p
                     jump = plusPtr ptr $ sizeOf x
                 in align jump

instance (Storable a, Storable b) => Storable (Tuple2 a b) where
    sizeOf    = t2Size
    alignment = t2Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        b <- peek (goto2Snd ptr a)
        return $ Tuple2 a b
    poke ptr (Tuple2 a b) = do
        poke (castPtr ptr) a
        poke (goto2Snd ptr a) b
{-    peek _    = undefined -- Tuples are only valid as return types.
    poke _ _  = undefined -- we never inpoke side-effects ;)
-}

type CTuple3 a b c = Ptr (Tuple3 a b c)

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

newTuple3 :: (Storable a, Storable b, Storable c) => a -> b -> c -> IO (CTuple3 a b c)
newTuple3 x y z = new $ Tuple3 x y z

instance (Storable a, Storable b, Storable c) => Storable (Tuple3 a b c) where
    sizeOf    = t3Size
    alignment = t3Alignment
    peek _    = undefined
    poke _ _  = undefined
