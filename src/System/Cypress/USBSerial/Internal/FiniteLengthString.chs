{-|
Module      : System.Cypress.USBSerial.Internal.FiniteLengthString
Description : A marshalled finite length string
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : portable

`FiniteLengthString` is a dependent type that carries a string length information that is used for marshalling.
-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.Cypress.USBSerial.Internal.FiniteLengthString (
    FiniteLengthString(..),
    toString,
    fromString,
    show
) where

import           Foreign.Storable
import           GHC.TypeNats
import           Foreign.C.String
import           Foreign.Marshal
import           Foreign.Ptr
import           Data.String (IsString(..))
import Debug.Trace (trace)

import Control.Monad (forM_)
import Data.Word


#include "CyUSBSerial.h"

-- |A string of given finite length.
data FiniteLengthString (n::Nat) = FiniteLengthString String deriving (Eq)

instance (KnownNat n) => Show (FiniteLengthString n) where
    show (FiniteLengthString chars) = show chars

instance (KnownNat n) => IsString (FiniteLengthString n) where
    fromString = FiniteLengthString

instance (KnownNat n) => Storable (FiniteLengthString n) where
    sizeOf x = trace "size=" $ fromIntegral $ natVal x
    alignment _ = {#alignof UCHAR#}
    poke p x@(FiniteLengthString chars) = withCString (take l chars) $ \pChars -> 
        copyBytes (castPtr p) (castPtr pChars) (l+1)
        where n = fromIntegral $ natVal x
              l = min (n-1) (length chars) -- a string of length l needs l+1 bytes
    peek p = do
      s <- peekCAString (castPtr p)  --peekCString (castPtr p)
      return $ FiniteLengthString s

-- |Convert a `FiniteLengthString` to a `String`, i.e., basically, unwrap the `FiniteLengthString`.
toString :: (KnownNat n) => (FiniteLengthString n) -> String
toString (FiniteLengthString chars) = chars