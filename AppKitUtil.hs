{-# LANGUAGE ForeignFunctionInterface #-}
module AppKitUtil where

import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "getActiveAppName"
  _getActiveAppName :: IO CString

-- | Get the active application name.
getActiveAppName :: IO (Maybe String)
getActiveAppName = _getActiveAppName >>= fromCString

foreign import ccall "focusAppByName"
  _focusAppByName :: CString -> IO ()

focusAppByName :: String -> IO ()
focusAppByName appName = withCString appName _focusAppByName

-- | Guards against null pointers.
maybePtr :: Ptr a -> Maybe (Ptr a)
maybePtr ptr = if ptr == nullPtr then Nothing else Just ptr

-- | Convert a char* to a [Char]
fromCString :: CString -> IO (Maybe String)
fromCString cstr = traverse peekCString $ maybePtr cstr
