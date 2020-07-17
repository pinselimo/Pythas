module Foreign.HaskyTuple (
    CTuple2, newTuple2, peekTuple2,
    CTuple3, newTuple3, peekTuple3
) where

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..))

type CTuple2 a b = Ptr (Struct2 a b)

newTuple2 :: (Storable a, Storable b) => (a, b) -> IO (CTuple2 a b)
newTuple2 (x, y) = new $ Struct2 x y

peekTuple2 :: (Storable a, Storable b) => CTuple2 a b -> IO (a,b)
peekTuple2 ct = do
    t <- peek ct
    return (s2fst t, s2snd t)

type CTuple3 a b c = Ptr (Struct3 a b c)

newTuple3 :: (Storable a, Storable b, Storable c) => (a, b, c) -> IO (CTuple3 a b c)
newTuple3 (x, y, z) = new $ Struct3 x y z

peekTuple3 :: (Storable a, Storable b, Storable c) => CTuple3 a b c -> IO (a,b,c)
peekTuple3 ct = do
    t <- peek ct
    return (s3fst t, s3snd t, s3trd t)

