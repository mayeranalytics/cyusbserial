{-|
Module      : System.Cypress.Safe.USBSerial.Internal.Utils
Description : Some error handling functions for System.Cypress.Safe.USBSerial
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

Here we have the "safe" version of various functions that are not part of the original API.

- Reading and writing of the config flash.

__Note:__ `progConfigFlash` reconfigures the device, use this function with great care! 
__It is very possible that you brick your device!__ So make sure you only program known good config sets.
-}

module System.Cypress.Safe.USBSerial.Internal.Utils where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.Internal.USBSerial (ReturnStatus(..))

-- |Convert a `USB ReturnStatus` to an `USB (Either ReturnStatus a)`, for example for use with
-- `try`. The same applies to `USBWithHandle`.
throwUnlessSuccess :: Monad m => ExceptT ReturnStatus m ReturnStatus -> ExceptT ReturnStatus m (Either ReturnStatus ())
throwUnlessSuccess a = a >>= \status -> return (if status == Success then Right () else Left status)
