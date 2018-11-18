module AppKitUtil where

import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (Ptr, nullPtr)
import qualified Data.Vector as Vector

type NSArray a = Ptr ObjC_NSArray
data ObjC_NSArray

type NSRunningApplication = Ptr ObjC_NSRunningApplication
data ObjC_NSRunningApplication

type NSEnumerator a = Ptr ObjC_NSEnumerator
data ObjC_NSEnumerator

foreign import ccall nsarrayLength :: NSArray a -> IO Int

foreign import ccall nsarrayEnumerator :: NSArray a -> IO (NSEnumerator a)

foreign import ccall enumeratorNextObject :: NSEnumerator (Ptr a) -> IO (Ptr a)

listFromNSArray :: NSArray (Ptr a) -> IO [Ptr a]
listFromNSArray arr = do
  e <- nsarrayEnumerator arr
  Vector.toList <$> loop e Vector.empty
  where
  loop e acc = (maybePtr <$> enumeratorNextObject e) >>= \case
    Just a -> loop e $ acc `Vector.snoc` a
    Nothing -> pure acc

foreign import ccall "getRunningApps"
  _getRunningApps :: IO (NSArray NSRunningApplication)

getRunningApps :: IO [NSRunningApplication]
getRunningApps = _getRunningApps >>= listFromNSArray 

foreign import ccall "getAppName"
  _getAppName :: NSRunningApplication -> IO CString

getAppName :: NSRunningApplication -> IO (Maybe String)
getAppName app = _getAppName app >>= fromCString

foreign import ccall "getActiveAppName"
  _getActiveAppName :: IO CString

-- | Get the active application name.
getActiveAppName :: IO (Maybe String)
getActiveAppName = _getActiveAppName >>= fromCString

foreign import ccall focusApp :: NSRunningApplication -> IO Bool

-- | Guards against null pointers.
maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr ptr = if ptr == nullPtr then Nothing else Just ptr

-- | Convert a char* to a [Char]
fromCString :: CString -> IO (Maybe String)
fromCString cstr = traverse peekCString $ maybePtr cstr
