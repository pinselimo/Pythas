module Foreign.HaskyString (CWString, newCWString, peekCWString, freeCWString) where

import qualified Foreign.C.String as STR
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

type CWString = Ptr STR.CWString

newCWString :: String -> IO CWString
newCWString s = do
 p <- malloc
 s <- STR.newCWString s
 poke p s
 return p

peekCWString :: CWString -> IO String
peekCWString cws = peek cws >>= STR.peekCWString

freeCWString :: CWString -> IO ()
freeCWString cws = peek cws >>= free >> free cws
