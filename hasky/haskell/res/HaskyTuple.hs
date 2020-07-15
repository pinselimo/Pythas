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
t2Size ct = max size_a align_b + max size_b align_a
    where align_a = alignment $ c2fst ct
          align_b = alignment $ c2snd ct
          size_a  = sizeOf $ c2fst ct
          size_b  = sizeOf $ c2snd ct

t2Alignment :: (Storable a, Storable b) => Tuple2 a b -> Int
t2Alignment ct = max (alignment $ c2fst ct) (alignment $ c2snd ct)

newTuple2 :: (Storable a, Storable b) => (a, b) -> IO (CTuple2 a b)
newTuple2 (x, y) = new $ Tuple2 x y

peekTuple2 :: (Storable a, Storable b) => CTuple2 a b -> IO (a,b)
peekTuple2 ct = do
    t <- peek ct
    return (c2fst t, c2snd t)

goto2Snd :: (Storable a, Storable b) => Ptr (Tuple2 a b) -> a -> Ptr b
goto2Snd ptr x = let align p = alignPtr p $ alignment p
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

type CTuple3 a b c = Ptr (Tuple3 a b c)

data Tuple3 a b c = Tuple3
    { c3fst :: a
    , c3snd :: b
    , c3trd :: c
    } deriving (Show, Eq)

t3Size :: (Storable a, Storable b, Storable c) => Tuple3 a b c -> Int
t3Size ct
    | align_a >= sum_bc  = 2 * align_a
    | align_b >= size_a
    && align_b >= size_c = 3 * align_b
    | align_c >= sum_ab  = 2 * align_c
    | otherwise          = size_a + size_b + size_c -- Fallback
    where align_a = alignment $ c3fst ct
          align_b = alignment $ c3snd ct
          align_c = alignment $ c3trd ct
          size_a  = sizeOf $ c3fst ct
          size_b  = sizeOf $ c3snd ct
          size_c  = sizeOf $ c3trd ct
          sum_ab  = size_a + size_b
          sum_bc  = size_b + size_c

t3Alignment :: (Storable a, Storable b, Storable c) => Tuple3 a b c -> Int
t3Alignment ct = max aConstraint $ max bConstraint cConstraint
    where aConstraint = alignment $ c3fst ct
          bConstraint = alignment $ c3snd ct
          cConstraint = alignment $ c3trd ct

newTuple3 :: (Storable a, Storable b, Storable c) => (a, b, c) -> IO (CTuple3 a b c)
newTuple3 (x, y, z) = new $ Tuple3 x y z

peekTuple3 :: (Storable a, Storable b, Storable c) => CTuple3 a b c -> IO (a,b,c)
peekTuple3 ct = do
    t <- peek ct
    return (c3fst t, c3snd t, c3trd t)

goto3snd :: (Storable a, Storable b, Storable c) => Ptr (Tuple3 a b c) -> a -> Ptr b
goto3snd ptr x = let align p = alignPtr p $ alignment p
                     jump = plusPtr ptr $ sizeOf x
                 in align jump

goto3trd :: (Storable b, Storable c) => Ptr b -> b -> Ptr c
goto3trd ptr x = let align p = alignPtr p $ alignment p
                     jump = plusPtr ptr $ sizeOf x
                 in align jump

instance (Storable a, Storable b, Storable c) => Storable (Tuple3 a b c) where
    sizeOf    = t3Size
    alignment = t3Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        let ptr_b = goto3snd ptr a
        b <- peek ptr_b
        let ptr_c = goto3trd ptr_b b
        c <- peek ptr_c
        return $ Tuple3 a b c
    poke ptr (Tuple3 a b c) = do
        poke (castPtr ptr) a
        let ptr_b = goto3snd ptr a
        poke ptr_b b
        let ptr_c = goto3trd ptr_b b
        poke ptr_c c

