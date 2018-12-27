{-|
Module      : System.Cypress.Safe.USBSerial.Extras
Description : Some functions that are not part of the original API.
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

module System.Cypress.Safe.USBSerial.Extras (
    -- * Config flash reading and writing - use with care!
    readConfigFlash,
    progConfigFlash
) where

import           Control.Monad.Reader
import           Data.Char (ord)
import           Data.Int
import           Data.Word
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString, useAsCStringLen, packCStringLen)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import           Data.Bits  (shift, (.&.), (.|.))
import           System.Cypress.USBSerial.Extras hiding (readConfigFlash, progConfigFlash)
import qualified System.Cypress.USBSerial.Extras as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import           Control.Monad.Except

------------------------------------------------  CyReadConfigFlash  ---------------------------------------------------
-- |Read the config flash. This is a "safe" operation in the sense that it won't brick your device.
--
-- Returns a `ByteString` of length 512 containing the config data, or
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `progConfigFlash`
--
readConfigFlash :: Word32 -- ^ Timeout value of the API
                -> USBWithHandle (Either ReturnStatus ByteString)  -- ^ Failed `ReturnStatus` or `DataBuffer`
readConfigFlash t = do
    h <- lift ask
    liftIO $ Cy.readConfigFlash h t

------------------------------------------------  CyProgConfigFlash  ------------------------------------------------
-- |Write the config flash. The `ByteString` must have length 512 and __must contain a good device configuration set__.
--
-- __Use with extreme caution!__ It is very possible that you brick your device with an inappropriate config set!
--
-- Again: __Be careful or you will brick your device!__
--
-- Returns `Success` or:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
-- * `ErrorOther` when the `ByteString` does not have length 512
--
-- See Also
--
-- * `readConfigFlash`
--
progConfigFlash :: ByteString -- ^ `ByteString` of length 512 containing the config data
                -> Word32 -- ^ Timeout value of the API
                -> USBWithHandle (Either ReturnStatus ())
progConfigFlash bs t = do
    h <- lift ask
    throwUnlessSuccess $ liftIO $ Cy.progConfigFlash h bs t

