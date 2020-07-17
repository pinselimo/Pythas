module Foreign.HaskyTuple (
    CTuple2, newTuple2, peekTuple2,
    CTuple3, newTuple3, peekTuple3,
    CTuple4, newTuple4, peekTuple4
) where

import Foreign.Storable (Storable, peek)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Utils (new)
import Foreign.C.Structs (Struct2(..), Struct3(..), Struct4(..))

type CTuple2 a b = Ptr (Struct2 a b)

newTuple2 :: (Storable a, Storable b) => (a, b) -> IO (CTuple2 a b)
newTuple2 (x, y) = new $ Struct2 x y

peekTuple2 :: (Storable a, Storable b) => CTuple2 a b -> IO (a,b)
peekTuple2 ct = do
    s <- peek ct
    return (s2fst s, s2snd s)

type CTuple3 a b c = Ptr (Struct3 a b c)

newTuple3 :: (Storable a, Storable b, Storable c) => (a, b, c) -> IO (CTuple3 a b c)
newTuple3 (x, y, z) = new $ Struct3 x y z

peekTuple3 :: (Storable a, Storable b, Storable c) => CTuple3 a b c -> IO (a,b,c)
peekTuple3 ct = do
    s <- peek ct
    return (s3fst s, s3snd s, s3trd s)

type CTuple4 a b c d = Ptr (Struct4 a b c d)

newTuple4 :: (Storable a, Storable b, Storable c, Storable d) => (a, b, c, d) -> IO (CTuple4 a b c d)
newTuple4 (w, x, y, z) = new $ Struct4 w x y z

peekTuple4 :: (Storable a, Storable b, Storable c, Storable d) => CTuple4 a b c d -> IO (a,b,c,d)
peekTuple4 ct = do
    s <- peek ct
    return (s4fst s, s4snd s, s4trd s, s4fth s)

