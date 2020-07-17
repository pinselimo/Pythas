module Foreign.HaskyArray (CArray, newArray, peekArray, withArray, freeArray, fromArray) where

import Foreign.Ptr
import Foreign.C.Types (CInt)
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array as ARR
import System.IO.Unsafe (unsafePerformIO)

type CArray a = Ptr (CArrayStruct a)

data CArrayStruct a = CArrayStruct {
    len :: CInt,
    ptr :: Ptr a
} deriving (Show)

carrSize :: (Storable a) => CArrayStruct a -> Int
carrSize a = max size_len align_ptr + max size_ptr align_len
    where align_ptr = alignment $ ptr a
          align_len = alignment $ len a
          size_ptr  = sizeOf $ ptr a
          size_len  = sizeOf $ len a

carrAlignment :: (Storable a) => CArrayStruct a -> Int
carrAlignment a =  max lenConstraint ptrConstraint
    where ptrConstraint = alignment $ ptr a
          lenConstraint = alignment $ len a

gotoArray :: (Storable a) => CArray a -> Ptr (Ptr a)
gotoArray a = let align p = alignPtr p $ sizeOf p
                  jump = plusPtr  a $ sizeOf (0 :: CInt)
            in align jump

instance (Storable a) => Storable (CArrayStruct a) where
    sizeOf    = carrSize
    alignment = carrAlignment
    peek ptr  = do
        len <- peek (castPtr ptr)
        arr <- peek (gotoArray ptr)
        return CArrayStruct { len = len, ptr = arr }
    poke ptr (CArrayStruct len arr) = do
        poke (castPtr ptr) len
        poke (gotoArray ptr) arr

newArray :: (Storable a) => [a] -> IO (CArray a)
newArray xs = do
    p    <- malloc
    arr  <- ARR.newArray xs
    poke p $ CArrayStruct (fromIntegral $ length xs) arr
    return p

peekArray :: (Storable a) => CArray a -> IO [a]
peekArray ap = do
    array <- peek ap
    let l = len array
    let a = ptr array
    if a == nullPtr
    then return []
    else ARR.peekArray (fromIntegral l) a

fromArray :: (Storable a) => CArray a -> [a]
fromArray = unsafePerformIO . peekArray

withArray :: Storable a => CArray a -> ([a] -> [a]) -> IO (CArray a)
withArray ca f = do
    xs <- peekArray ca
    newArray $ f xs

freeArray :: (Storable a) => CArray a -> IO ()
freeArray ap = do
    array <- peek ap
    free $ ptr array
    free ap
