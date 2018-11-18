module AppKitUtil where

import Conduit (liftIO)
import Data.Conduit (ConduitT, runConduit, yield, (.|))
import Data.Conduit.Combinators (sinkList)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (Ptr, nullPtr)

type NSArray a = Ptr ObjC_NSArray
data ObjC_NSArray

type NSRunningApplication = Ptr ObjC_NSRunningApplication
data ObjC_NSRunningApplication

type NSEnumerator a = Ptr ObjC_NSEnumerator
data ObjC_NSEnumerator

foreign import ccall nsarrayLength :: NSArray a -> IO Int

foreign import ccall nsarrayEnumerator :: NSArray a -> IO (NSEnumerator a)

foreign import ccall enumeratorNextObject :: NSEnumerator (Ptr a) -> IO (Ptr a)

-- | Create a ConduitT source to stream pointers from an NSArray.
sourceFromNSArray :: forall a. NSArray (Ptr a) -> ConduitT () (Ptr a) IO ()
sourceFromNSArray arr = do
  loop =<< liftIO (nsarrayEnumerator arr)
  where
  loop :: NSEnumerator (Ptr a) -> ConduitT () (Ptr a) IO ()
  loop e = (maybePtr <$> liftIO (enumeratorNextObject e)) >>= \case
    Just a -> yield a >> loop e
    Nothing -> pure ()

-- | Convert an NSArray to a Haskell list.
listFromNSArray :: NSArray (Ptr a) -> IO [Ptr a]
listFromNSArray arr = runConduit $ sourceFromNSArray arr .| sinkList

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
