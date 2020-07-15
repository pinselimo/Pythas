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

next :: (Storable a, Storable b, Storable c) => Ptr a -> b -> Ptr c -> IO (Ptr c)
next ptr x ptr_x = do
    let ptr_y = plusPtr ptr $ sizeOf x
    y <- peek ptr_x
    return $ alignPtr ptr_y $ alignment y

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

instance (Storable a, Storable b) => Storable (Tuple2 a b) where
    sizeOf    = t2Size
    alignment = t2Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        b <- peek =<< alloca (next ptr a)
        return $ Tuple2 a b
    poke ptr (Tuple2 a b) = do
        poke (castPtr ptr) a
        ptr_b <- alloca (next ptr a)
        poke ptr_b b

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

instance (Storable a, Storable b, Storable c) => Storable (Tuple3 a b c) where
    sizeOf    = t3Size
    alignment = t3Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        ptr_b <- alloca (next ptr a)
        b <- peek ptr_b
        ptr_c <- alloca (next ptr_b b)
        c <- peek ptr_c
        return $ Tuple3 a b c
    poke ptr (Tuple3 a b c) = do
        poke (castPtr ptr) a
        ptr_b <- alloca (next ptr a)
        poke ptr_b b
        ptr_c <- alloca (next ptr_b b)
        poke ptr_c c

type CTuple4 a b c d = Ptr (Tuple4 a b c d)

data Tuple4 a b c d = Tuple4
    { c4fst :: a
    , c4snd :: b
    , c4trd :: c
    , c4fth :: d
    } deriving (Show, Eq)

t4Size :: (Storable a, Storable b, Storable c, Storable d) => Tuple4 a b c d -> Int
t4Size ct
    |  sum_abc <= amax
    || sum_bcd <= amax = 2 * amax
    |  sum_ab  <= amax
    && size_d  <= amax = 3 * amax
    |  size_a  <= amax
    && sum_cd  <= amax = 3 * amax
    |  sum_bc  <= amax = 3 * amax
    |  otherwise       = 4 * amax
    where amax    = t4Alignment ct
          size_a  = sizeOf $ c4fst ct
          size_b  = sizeOf $ c4snd ct
          size_c  = sizeOf $ c4trd ct
          size_d  = sizeOf $ c4fth ct
          sum_ab  = size_a + size_b
          sum_bc  = size_b + size_c
          sum_cd  = size_c + size_d
          sum_abc = sum_ab + size_c
          sum_bcd = size_b + sum_cd

t4Alignment :: (Storable a, Storable b, Storable c, Storable d) => Tuple4 a b c d -> Int
t4Alignment ct = foldr max dConstraint [aConstraint, bConstraint, cConstraint]
    where aConstraint = alignment $ c4fst ct
          bConstraint = alignment $ c4snd ct
          cConstraint = alignment $ c4trd ct
          dConstraint = alignment $ c4fth ct

newTuple4 :: (Storable a, Storable b, Storable c, Storable d) => (a, b, c, d) -> IO (CTuple4 a b c d)
newTuple4 (w, x, y, z) = new $ Tuple4 w x y z

peekTuple4 :: (Storable a, Storable b, Storable c, Storable d) => CTuple4 a b c d -> IO (a,b,c,d)
peekTuple4 ct = do
    t <- peek ct
    return (c4fst t, c4snd t, c4trd t, c4fth t)

instance (Storable a, Storable b, Storable c, Storable d) => Storable (Tuple4 a b c d) where
    sizeOf    = t4Size
    alignment = t4Alignment
    peek ptr  = do
        a <- peek (castPtr ptr)
        ptr_b <- alloca (next ptr a)
        b <- peek ptr_b
        ptr_c <- alloca (next ptr_b b)
        c <- peek ptr_c
        ptr_d <- alloca (next ptr_c c)
        d <- peek ptr_d
        return $ Tuple4 a b c d
    poke ptr (Tuple4 a b c d) = do
        poke (castPtr ptr) a
        ptr_b <- alloca (next ptr a)
        poke ptr_b b
        ptr_c <- alloca (next ptr_b b)
        poke ptr_c c
        ptr_d <- alloca (next ptr_c c)
        poke ptr_d d

