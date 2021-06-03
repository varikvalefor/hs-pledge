{-# LANGUAGE ForeignFunctionInterface #-}

module System.Pledge (Promise(..), pledge) where

import Data.List (intersperse, nub)
import Data.Char (toLower)
import Foreign (Ptr, nullPtr)
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (CString, withCString)
import System.FilePath (FilePath)

foreign import ccall "unistd.h pledge" c_pledge :: CString -> Ptr [CString] -> IO Int

-- | Promises is an alias of [Promise].
type Promises = [Promise]

-- Allowed promises. See OpenBSD's pledge(2) for documentation.
data Promise = Rpath    | Wpath     | Cpath  | Stdio | Tmppath | Dns     | Inet   | Flock
             | Unix     | Id        | Ioctl  | Getpw | Proc    | Settime | Fattr  | Protexec
             | Tty      | Sendfd    | Recvfd | Exec  | Route   | Mcast   | Vminfo | Ps
             | Coredump | Disklabel | Pf     | None
             deriving (Eq, Show)

-- | For all Promises k, for all [FilePath] g, pledge k g is roughly equivalent
-- to the C pledge(prmises k, g).  However, the whitelisting of filepaths is
-- currently unsupported.
pledge :: Promises -> [FilePath] -> IO ()
-- special case for completely empty pledge. Useful? Maybe not.
pledge [] _ = throwErrnoIfMinus1_ "pledge" $ c_pledge nullPtr nullPtr
-- Generic case, but we don't support giving whilelist of paths yet
pledge proms [] =
  withCString (promises proms) $ \c_proms ->
      let c_paths = nullPtr in
            throwErrnoIfMinus1_ "pledge" $ c_pledge c_proms c_paths
pledge _ _ = error "Pledge does not support [FilePath]."

-- | For all Promise k, promise k equals a lowercase String representation
-- of k.
promise :: Promise -> String
promise = map toLower . show

-- | For all Promises k, promises k equals a String representation of k which
-- is compatible with C's pledge.
promises :: Promises -> String
promises [] = ""
promises proms 
  | None `elem` proms = ""
  | otherwise = unwords $ map promise $ nub proms
