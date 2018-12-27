{-|
Module      : System.Cypress.Safe.USBSerial.PHDC
Description : PHDC part of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

PHDC is the __p__ersonal __h__ealthcare __d__evice __c__lass for devices such as pulse monitors, glucose meters, etc.
It is supported by [CY7C65211](http://www.cypress.com/documentation/datasheets/cy7c65211-usb-serial-single-channel-uarti2cspi-bridge-capsense-and-bcd)
and [CY7C65215](http://www.cypress.com/documentation/datasheets/cy7c65215-usb-serial-dual-channel-uarti2cspi-bridge-capsense-and-bcd).

Typically, PHDC devices are connected to a computer via USB for configuration, live monitoring, and playback.

This is the `safe` version of `System.Cypress.USBSerial.PHDC.PHDC`.

See section 10 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.Safe.USBSerial.PHDC (
    -- * Feature set and clear
    setFeature,
    clrFeature,
    -- * Status
    getStatus
) where

import           Control.Monad.Reader (lift, ask, liftIO)
import           Control.Error.Safe (tryRight)
import           Control.Error.Util hiding (tryIO)
import           System.Cypress.USBSerial.PHDC hiding (setFeature, clrFeature, getStatus)
import qualified System.Cypress.USBSerial.PHDC as Cy
import           System.Cypress.Safe.USBSerial
import           System.Cypress.Safe.USBSerial.Internal.Utils
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Char8 (ByteString)
import           Data.Word
    
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  PHDC API  ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------  10.1 CyPhdcSetFeature  ------------------------------------------------
-- |This API sends a PHDC set feature command.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle was invalid.
-- * `ErrorIoTimeout` if request timed out.
-- * `ErrorRequestFailed` if request was failed by device.
--
-- See Also
--
-- * `clrFeature`
-- * `getStatus`
--
-- 10.1 CyPhdcSetFeature
setFeature :: USBWithHandle ReturnStatus
setFeature = do
    h <- lift ask
    liftIO $ Cy.setFeature h

-----------------------------------------------  10.2 CyPhdcClrFeature  ------------------------------------------------
-- |This API sends a PHDC clear feature command.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle was invalid.
-- * `ErrorIoTimeout` if request timed out.
-- * `ErrorRequestFailed` if request was failed by device.
--
-- See Also
--
-- * `setFeature`
-- * `getStatus`
--
-- 10.2 CyPhdcClrFeature
clrFeature :: USBWithHandle ReturnStatus
clrFeature = do
    h <- lift ask
    liftIO $ Cy.clrFeature h

------------------------------------------------  10.3 CyPhdcGetStatus  ------------------------------------------------
-- |Retrieve the endpoint status of PHDC transaction.
--
-- `getStatus` returns 2 bytes of data pending bit map as per the PHDC specification.
--
-- Returns `Word16` with data pending bit map, or on failure
--
-- * `ErrorInvalidHandle` if handle was invalid.
-- * `ErrorIoTimeout` if request timed out.
-- * `ErrorRequestFailed` if request was failed by device.
--
-- See Also
--
-- * `clrFeature`
-- * `setFeature`
--
-- 10.3 CyPhdcGetStatus
getStatus :: USBWithHandle (Either ReturnStatus Word16) -- Failed `ReturnStatus` or status 
getStatus = do
    h <- lift ask
    liftIO $ Cy.getStatus h

