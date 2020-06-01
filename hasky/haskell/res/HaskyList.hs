module HaskyList (CList, newList, peekList, withList, freeList, fromList) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import System.IO.Unsafe (unsafePerformIO)

type CList a = Ptr (ListElem a)

data ListElem a = ListElem { 
    value :: a,
    next ::  Ptr (ListElem a) 
    } deriving (Show)

elemSize :: (Storable a) => ListElem a -> Int
elemSize l = valueSize + ptrSize
    where ptrSize = sizeOf $ next l
          valueSize = sizeOf $ value l

elemAlignment :: (Storable a) => ListElem a -> Int
elemAlignment l =  valueConstraint + ptrConstraint
    where ptrConstraint = alignment $ next l
          valueConstraint = alignment $ value l

nextListElem :: (Storable a) => CList a -> a -> Ptr (CList a)
nextListElem l = let align p = alignPtr p $ sizeOf p
                     jump  v = plusPtr  l $ sizeOf v
        in align . jump

instance (Storable a) => Storable (ListElem a) where
    sizeOf    = elemSize 
    alignment = elemAlignment
    peek ptr  = do
        ptrValue <- peek (castPtr ptr)
        nextElem <- peek (nextListElem ptr ptrValue)
        return  ListElem { value = ptrValue, next = nextElem }
    poke ptr (ListElem value next) = do
        poke (castPtr ptr) value
        poke (nextListElem ptr value) next

newList :: (Storable a) => [a] -> IO (CList a)
newList [] = return nullPtr
newList (x:xs) = do
    p    <- malloc
    next <- newList xs
    poke p $ ListElem x next
    return p

peekList :: (Storable a) => CList a -> IO [a]
peekList lp = do
    le <- peek lp
    let x = value le
    let n = next le
    if n == nullPtr
    then return [x]
    else do
        li <- peekList n 
        return (x:li)

fromList :: (Storable a) => CList a -> [a]
fromList = unsafePerformIO . peekList

withList :: Storable a => CList a -> ([a] -> [a]) -> IO (CList a)
withList cl f = do
    xs <- peekList cl
    newList $ f xs

freeList :: (Storable a) => CList a -> IO ()
freeList lp = do
    le <- peek lp
    let n = next le
    free lp
    if n == nullPtr
    then return ()
    else freeList n