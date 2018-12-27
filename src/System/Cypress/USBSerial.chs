{-|
Module      : System.Cypress.USBSerial
Description : Constants, data types, USB Initialization and Common API of the Cypress USBSeral library
Copyright   : (c) Markus Mayer
License     : BSD3
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

Constants, data types, USB Initialization and Common API of the Cypress USBSeral library.

* Library initialisation
* Device enumeration
* Opening and closing
* Version information and signature
* Events
* GPIO getting and setting

        * GPIO pins (CY'11: 10x, CY'13: 8x, CY'15: 17x)
        * See `getGpioValue`, `setGpioValue`

* Recycling and resetting
* User flash reading and writing

    * 512-byte flash for storing user parameters
    * See `readUserFlash`, `progUserFlash`

See section 2.-5. of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial (
    -- * Version information
    version,
    versionString,
    versionMajor,
    versionMinor,
    versionPatch,
    versionBuild,
    -- * Types
    Handle,
    ReturnStatus(..),
    errorString,
    DeviceClass(..),
    DeviceInfo(..),
    DeviceID, InterfaceID,
    ManufacturerName,
    ProductName,
    SerialNum,
    DeviceFriendlyName,
    DeviceSerialBlock(..),
    DeviceType(..),
    VidPid(..), Vid, Pid,
    LibraryVersion(..),
    FirmwareVersion(..),
    CallbackEvents(..),
    EventNotification,
    -- * Library initialisation
    libraryInit,
    libraryExit,
    withLibraryInit,
    -- * Device enumeration
    getListOfDevices,
    getDeviceInfo,
    getDeviceInfoVidPid,
    -- * Opening and closing
    open,
    close,
    -- * Version information and signature
    getLibraryVersion,
    getFirmwareVersion,
    getSignature,
    Signature(..),
    -- * Events
    withEventNotification,
    abortEventNotification,
    -- * GPIO getting and setting
    getGpioValue,
    setGpioValue,
    -- * Recycling and resetting
#ifndef linux_HOST_OS
    cyclePort,
#endif
    resetDevice,
    -- * User flash reading and writing
    readUserFlash,
    progUserFlash
) where

import           Foreign
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.C.String
import           Control.Monad (liftM, when)
import           Data.Char (ord)
import           Data.Int
import           Data.List (intercalate)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString, useAsCStringLen, packCStringLen)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import           Data.Bits  (shift, (.&.), (.|.))
import           System.Cypress.USBSerial.Internal.FiniteLengthString
import           System.Cypress.USBSerial.Internal.USBSerial
import           Control.Monad.Except


#include "CyUSBSerial.h"

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------  USB Initialization API  -----------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------  4.1 CyLibraryInit  --------------------------------------------------
-- |Initialize the library.
--
-- Call this when the application is being started.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorDriverInitFailed` on failure (Failure could be because of not calling `libraryExit` previously)
--
-- See Also
--
-- * `open`
-- * `libraryExit`
--
-- From: 4.1 CyLibraryInit
{#fun unsafe CyLibraryInit as libraryInit
  {}
  -> `ReturnStatus' toReturnStatus
#}

-- |Check `ReturnStatus` and fail the computation in the `ExceptT` monad if not `Success`
failUnlessSuccess :: IO ReturnStatus -> ExceptT ReturnStatus IO ()
failUnlessSuccess status' = do
    status <- liftIO status'
    if status == Success then return () else throwError status

-- |Run computation act bracketed by `libraryInit` and `libraryExit`.
-- Normally, the result or `ReturnStatus` of the computation @act@ will be returned.
-- Only when `libraryExit` fails it is its `ReturnStatus` that will be returned.
withLibraryInit :: ExceptT ReturnStatus IO a -> IO (Either ReturnStatus a)
withLibraryInit act = runExceptT $ do
    failUnlessSuccess libraryInit
    a <- act
    exitSuccess <- liftIO libraryExit
    if exitSuccess == Success
    then return a
    else throwError exitSuccess


-------------------------------------------------  4.2 CyLibraryExit  --------------------------------------------------
-- |Free the library.
--
-- Call this when exiting the application.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorRequestFailed` on failure
--
-- See Also
--
-- * `open`
-- * `close`
-- * `libraryInit`
--
-- From: 4.2 CyLibraryExit
{#fun unsafe CyLibraryExit as libraryExit
  {}
  -> `ReturnStatus' toReturnStatus
#}

-----------------------------------------------  4.3 CyGetListofDevices  -----------------------------------------------
-- |Retrieve the number of USB devices connected to the host.
--
-- In Windows family of operating systems the API retrieves only the number of devices that are attached to 
-- CyUSB3.SYS driver. For other operating systems, it retrieves the total number of USB devices present on the 
-- bus. It includes both USB Serial device as well as other devices.
--
-- Returns number of devices connecte, or on failure one of:
--
-- * `ErrorRequestFailed` on failure
-- * `ErrorDeviceNotFound` if there are no devices attached.
-- * `ErrorRequestFailed` if library was not initialized.
--
-- See Also
--
-- * `getDeviceInfo`
-- * `getDeviceInfoVidPid`
-- * `open`
-- * `close`
--
-- From: 4.3 CyGetListofDevices
getListOfDevices :: IO (Either ReturnStatus Word8)  -- ^ Failed `ReturnStatus` or number of connected devices
getListOfDevices = toEither <$> _cyGetListofDevices

{#fun unsafe CyGetListofDevices as _cyGetListofDevices
  { alloca- `Word8' peekCUChar* -- ^ numDevices: Number of Devices connected
  }
  -> `ReturnStatus' toReturnStatus
#}

peekCUChar :: Ptr CUChar -> IO Word8
peekCUChar p = fromIntegral <$> peek p

------------------------------------------------  4.4 CyGetDeviceInfo  -------------------------------------------------
-- |Retrieve the device information of a USB device.
--
-- In order to get the device information on
-- particular device user needs to provide the device number. To identify the device of interest, the application
-- needs to loop through all devices connected and obtain the information.
--
-- Returns `DeviceInfo`, or on failure one of:
--
-- * `ErrorRequestFailed` if library is not initialized (Only for Linux and Mac).
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `DeviceInfo`
-- * `DeviceType`
-- * `DeviceClass`
-- * `getListOfDevices`
-- * `getDeviceInfoVidPid`
-- * `open`
-- * `close`
--
-- From: 4.4 CyGetDeviceInfo
getDeviceInfo :: Word8  -- ^ Device number
              -> IO (Either ReturnStatus DeviceInfo) -- ^ Failed `ReturnStatus` or `DeviceInfo`
getDeviceInfo d = toEither <$> _cyGetDeviceInfo d

{#fun unsafe CyGetDeviceInfo as _cyGetDeviceInfo
  { `Word8'                               -- ^ Device number of the device of interest
  , allocaDeviceInfo- `DeviceInfo' peekDeviceInfo*  -- ^ Info of device returned
  }
  -> `ReturnStatus' toReturnStatus
#}

peekDeviceInfo :: Ptr () -> IO DeviceInfo
peekDeviceInfo = peek . castPtr

allocaDeviceInfo :: (Ptr () -> IO a) -> IO a
allocaDeviceInfo f = alloca $ \(p :: Ptr DeviceInfo) -> f (castPtr p)

---------------------------------------------  4.5 CyGetDeviceInfoVidPid  ----------------------------------------------
-- |Retrieve the information of all devices with specified Vendor ID and Product ID.
--
-- The deviceIdList contains the device numbers of all the devices with specified VID and PID. 
-- The device of interest can be identified from the information in `DeviceInfo`, which includes interface number,
-- string descriptor, deviceType and deviceClass.
--
-- Returns a list of device ID, `DeviceInfo` tuples, or on failure one of:
--
-- * `ErrorRequestFailed` on if library is not initialized (Only for Linux and Mac)
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDeviceInfoFetchFailed` if failed to fetch device information.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `DeviceInfo`
-- * `DeviceClass`
-- * `DeviceType`
-- * `getListOfDevices`
-- * `getDeviceInfo`
-- * `open`
-- * `close`
--
-- From: 4.5 CyGetDeviceInfoVidPid
getDeviceInfoVidPid :: VidPid -- ^ vidPid: VID and PID of device of interest
                    -> Word8 -- ^ Total length of the deviceInfoList allocated (Size of deviceInfoList array)
                    -> IO (Either ReturnStatus [(DeviceID, DeviceInfo)]) -- ^ Failed `ReturnStatus` or list of device ID and `DeviceInfo`
getDeviceInfoVidPid vidpid l = 
    allocaArray (fromIntegral l) $ \(pis :: Ptr Word8) ->
    allocaArray (fromIntegral l) $ \(pds :: Ptr DeviceInfo) ->
    alloca $ \(pl :: Ptr Word8) -> do
        vidpid' <- vidPidToWord32 vidpid
        status <- toReturnStatus <$> _cyGetDeviceInfoVidPid'_ vidpid' (castPtr pis) (castPtr pds) (castPtr pl) (fromIntegral l)
        returnIfSuccess status $ do
            count <- peek pl
            pis' <- peekArray (fromIntegral count) pis
            pds' <- peekArray (fromIntegral count) pds
            return (zip pis' pds')

type DeviceID = Word8 -- ^ Device ID ('number') as returned by `getDeviceInfoVidPid` and needed by `open`
type InterfaceID = Word8 -- ^ Interface ID ('number') and required by `open`


foreign import ccall unsafe "CyUSBSerial.h CyGetDeviceInfoVidPid"
  _cyGetDeviceInfoVidPid'_ :: Word32  -- ^ VID and PID of device of interest
                           -> Ptr CUChar  -- ^ Array of device ID's returned
                           -> Ptr ()  -- ^ Array of pointers to device info list
                           -> Ptr CUChar -- ^ Count of devices with specified VID PID
                           -> CUChar  -- ^ infoListLength: Total length of the deviceInfoList allocated (Size of deviceInfoList array)
                           -> IO CInt   -- ^ ReturnStatus

-- |Cast `VidPid`, which is two `Word16` to `Word32`
vidPidToWord32 :: VidPid -> IO Word32
vidPidToWord32 vp = with vp $ \p -> peek (castPtr p) :: IO Word32

-----------------------------------------------------  4.6 CyOpen  -----------------------------------------------------
-- |Open the USB Serial device.
--
-- Use device number and interface number to identify the device.
--
-- Returns handle, or on failure one of:
--
-- * `ErrorRequestFailed` on if library is not initialized (Only for Linux and Mac)
-- * `ErrorInvalidParameter` if the input parameters are invalid.
-- * `ErrorDriverOpenFailed` if open was unsuccessful.
-- * `ErrorAccessDenied` if access is denied by operating system.
-- * `ErrorAllocationFailed` if memory allocation was failed.
-- * `ErrorDeviceNotFound` if specified device number is invalid.
--
-- See Also
--
-- * `getListOfDevices`
-- * `getDeviceInfoVidPid`
-- * `getDeviceInfo`
-- * `close`
--
-- From: 4.6 CyOpen
open :: DeviceID      -- ^ Device number
     -> InterfaceID   -- ^ Interface Number
     -> IO (Either ReturnStatus Handle)  -- ^ Failed `ReturnStatus` or `Handle`
open d i = toEither <$> _cyOpen d i

{#fun unsafe CyOpen as _cyOpen
  { `Word8'  -- ^ deviceNumber: Device number of device that needs to be opened
  , `Word8'  -- ^ interfaceNum: Interface Number
  , alloca- `Handle' peek* -- ^ handle: Handle returned by the API
  }
  -> `ReturnStatus' toReturnStatus
#}

----------------------------------------------------  4.7 CyClose  -----------------------------------------------------
-- |Close the specified device handle and releases all resources associated with it.
--
-- Invoke this with a valid device handle and after a successful `open`.
--
-- Returns
--
-- * `Success` on success.
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if handle is invalid in case of Windows.
-- * `ErrorRequestFailed` on error in case of library being not initialized (Only for Linux and Mac).
--
-- See Also
--
-- * `open`
--
-- From: 4.7 CyClose
{#fun unsafe CyClose as close
  { id `Handle' -- ^ Handle of the device
  }
  -> `ReturnStatus' toReturnStatus
#}

#ifndef linux_HOST_OS 
--------------------------------------------------  4.8 CyCyclePort  ---------------------------------------------------
-- |Power-cycle the host port.
--
-- It will reenumerate the device after the power cycle.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid in case of Linux/Mac.
-- * `ErrorInvalidParameter` if handle is invalid in case of Windows.
-- * `ErrorRequestFailed` on error if request was failed by driver.
--
-- See Also
-- 
-- * `resetDevice`
--
-- From: 4.8 CyCyclePort
{#fun unsafe CyCyclePort as cyclePort
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}
#endif

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------  Common APIs  -----------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

----------------------------------------------  5.1 CyGetLibraryVersion  -----------------------------------------------
-- |Retrieve the version of the USB Serial library.
--
-- See Also
--
-- * `getFirmwareVersion`
--
-- From: 5.1 CyGetLibraryVersion
getLibraryVersion :: Handle -- ^ Valid device handle
                  -> IO LibraryVersion 
getLibraryVersion h = do
    (status, v) <- _cyGetLibraryVersion h
    when (status /= Success) $ error "getLibraryVersion: Internal error" -- this should not happen
    return v

{#fun unsafe CyGetLibraryVersion as _cyGetLibraryVersion
  { id `Handle' -- ^ Valid device handle
  , allocaLibraryVersion- `LibraryVersion' peekLibraryVersion* -- ^ PCY_LIBRARY_VERSION version: Library version of the current library
  }
  -> `ReturnStatus' toReturnStatus
#}

peekLibraryVersion :: Ptr () -> IO LibraryVersion
peekLibraryVersion = peek . castPtr

allocaLibraryVersion :: (Ptr () -> IO a) -> IO a
allocaLibraryVersion f = alloca $ \(p :: Ptr LibraryVersion) -> f (castPtr p)


----------------------------------------------  5.2 CyGetFirmwareVersion  ----------------------------------------------
-- |Retrieve the firmware version of the USB Serial device.
--
-- Returns `FirmwareVersion`, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `getLibraryVersion`
--
-- From: 5.2 CyGetFirmwareVersion
getFirmwareVersion :: Handle -- ^ Valid device handle
                   -> IO (Either ReturnStatus FirmwareVersion) -- ^ Failed `ReturnStatus` or `FirmwareVersion`
getFirmwareVersion h = toEither <$> _cyGetFirmwareVersion h

{#fun unsafe CyGetFirmwareVersion as _cyGetFirmwareVersion
  { id `Handle' -- ^ Valid device handle
  , allocaFirmwareVersion- `FirmwareVersion' peekFirmwareVersion* -- ^ PCY_FIRMWARE_VERSION firmwareVersion: Firmware version.
  }
  -> `ReturnStatus' toReturnStatus
#}

peekFirmwareVersion :: Ptr () -> IO FirmwareVersion
peekFirmwareVersion = peek . castPtr

allocaFirmwareVersion :: (Ptr () -> IO a) -> IO a
allocaFirmwareVersion f = alloca $ \(p :: Ptr FirmwareVersion) -> f (castPtr p)

-------------------------------------------------  5.3 CyGetSignature  -------------------------------------------------
-- |Retrieve the `Signature` of the device firmware. It is a `String` like "CYUS".
--
-- Returns the `Signature` (a `String`), or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- 5.3 CyGetSignature
getSignature :: Handle -- ^ Valide device handle
             -> IO (Either ReturnStatus Signature) -- ^ Failed `ReturnStatus` or `Signature`
getSignature h = toEither <$> _cyGetSignature h


-- |A `Signature` is just a `String`
type Signature = String

{#fun unsafe CyGetSignature as _cyGetSignature
  { id `Handle'                    -- ^ Valid device handle
  , allocaCUString- `String' peekCUString* -- ^ Signature returned
  }
  -> `ReturnStatus' toReturnStatus
#}

peekCUString :: Ptr CUChar -> IO String
peekCUString = peekCAString . castPtr

allocaCUString :: (Ptr CUChar -> IO a) -> IO a
allocaCUString = allocaBytes 256 

---------------------------------------------  5.4 CySetEventNotification  ---------------------------------------------
-- |Register a callback for error/event notifications during UART/SPI data transfers.
--
-- A callback will be issued based on the error/events sent by the device.
--
-- Returns good value, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorStatusMonitorExist` if notification callback is already registered.
--
-- See Also
-- 
-- * `CallbackEvents`
-- * `EventNotificationCallback`
-- * `abortEventNotification`
--
-- From: 5.4 CySetEventNotification
withEventNotification :: EventNotification
                      -> Handle   -- ^ Valid device handle
                      -> (Handle -> IO a) -- ^ action to execute
                      -> IO (Either ReturnStatus a) -- ^ Failed `ReturnStatus` or good value
withEventNotification e h act = do
    cbPtr <- createEventNotification e
    status <- toReturnStatus <$> _cySetEventNotification'_ h cbPtr
    retval <- if status == Success 
        then Right <$> act h 
        else (return $ Left status)
    _ <- abortEventNotification h
    freeHaskellFunPtr cbPtr
    return retval

foreign import ccall safe "CyUSBSSerial.h CySetEventNotification"
    _cySetEventNotification'_ :: Ptr ()   -- ^ Valid device handle
                              -> FunPtr (CUShort -> IO ()) -- ^ CY_EVENT_NOTIFICATION_CB_FN notificationCbFn: Callback function pointer
                              -> IO CInt

foreign import ccall "wrapper" createEventNotification :: EventNotification -> IO (FunPtr (CUShort -> IO ()))
                             
--------------------------------------------  5.5 CyAbortEventNotification  --------------------------------------------
-- |Unregister the event callback.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorRequestFailed` if API is called before registering callback.
--
-- See Also
--
-- * `setEventNotification`
--
-- From: 5.5 CyAbortEventNotification
{#fun unsafe CyAbortEventNotification as abortEventNotification
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  5.6 CyGetGpioValue  -------------------------------------------------
-- |Retrieve the value of a GPIO.
--
-- Returns current state of the GPIO, or on failure one of:
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `setGpioValue`
--
-- From: 5.6 CyGetGpioValue
getGpioValue :: Handle -- ^ Valide device handle
             -> Word8  -- ^ GPIO number
             -> IO (Either ReturnStatus Word8) -- ^ Failed `ReturnStatus` or current state of the GPIO
getGpioValue h n = toEither <$> _cyGetGpioValue h n

{#fun unsafe CyGetGpioValue as _cyGetGpioValue
  { id `Handle' -- ^ Valid device handle
  , `Word8'     -- ^ GPIO number
  , alloca- `Word8' peekCUChar* -- ^ Current state of the GPIO
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  5.7 CySetGpioValue  -------------------------------------------------
-- |Set the value of a GPIO.
--
-- The GPIO must be configured as an output.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` on error when request is failed by USB Serial device.
--
-- See Also
--
-- * `getGpioValue`
--
-- From: 5.7 CySetGpioValue
{#fun unsafe CySetGpioValue as setGpioValue
  { id `Handle' -- ^ Valid device handle
  , `Word8'     -- ^ GPIO number
  , `Word8'     -- ^ Value that needs to be set
  }
  -> `ReturnStatus' toReturnStatus
#}

-------------------------------------------------  5.8 CyResetDevice  --------------------------------------------------
-- |Reset the device by sending a vendor request.
--
-- The device will be re-enumerated.
--
-- Returns
--
-- * `Success` on success
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `cyclePort`
--
-- From: 5.8 CyResetDevice
{#fun unsafe CyResetDevice as resetDevice
  { id `Handle' -- ^ Valid device handle
  }
  -> `ReturnStatus' toReturnStatus
#}

------------------------------------------------  5.9 CyReadUserFlash  -------------------------------------------------
-- |Read from the flash address specified.
--
-- The total space available is 512 bytes. The flash area address offset is from 0x0000 to 0x00200
-- and should be read page wise (page size is 128 bytes).
--
-- Returns data buffer containing data or 
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `progUserFlash`
--
-- From: 5.9 CyReadUserFlash
readUserFlash :: Handle -- ^ Valid device handle
              -> Word32 -- ^ flashAddress: Address from which the data is read
              -> Word32 -- ^ length to read
              -> Word32 -- ^ Timeout value of the API
              -> IO (Either ReturnStatus ByteString)  -- ^ Failed `ReturnStatus` or `DataBuffer`
readUserFlash h a l t = allocaDataBuffer l $ \p -> do
    status <- toReturnStatus <$> _cyReadUserFlash'_ h (castPtr p) (fromIntegral a) (fromIntegral t)
    returnIfSuccess status $ toByteString p

foreign import ccall unsafe "CyUSBSerial.h CyReadUserFlash"
    _cyReadUserFlash'_ :: Ptr ()    -- ^ Valid device handle
                       -> Ptr ()    -- ^ data buffer containing buffer address, length to read
                       -> CUInt     -- ^ Address from which the data is read
                       -> CUInt     -- ^ Timeout value of the API
                       -> IO CInt


------------------------------------------------  5.10 CyProgUserFlash  ------------------------------------------------
-- |Write to the user flash area on the USB Serial device.
--
-- The total space available is 512 bytes. The flash area address offset is from 0x0000
-- to 0x00200 and should be written page wise (page size is 128 bytes). 
--
-- Returns number of bytes written or
--
-- * `ErrorInvalidHandle` if handle is invalid.
-- * `ErrorInvalidParameter` if specified parameters are invalid or out of range.
-- * `ErrorIoTimeout` if the request is timed out.
-- * `ErrorRequestFailed` when request is failed by USB Serial device.
--
-- See Also
--
-- * `readUserFlash`
--
-- From: 5.10 CyProgUserFlash
progUserFlash :: Handle      -- ^ Valid device handle
               -> Word32     -- ^ Address towhich the data is written
               -> ByteString -- ^ Data to write
               -> Word32     -- ^ Timeout value of the API
               -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes written
progUserFlash h a bs t = withByteString bs $ \p -> do
    status <- toReturnStatus <$> _cyProgUserFlash'_ h (castPtr p) (fromIntegral a) (fromIntegral t)
    returnIfSuccess status $ do
        len <- fromIntegral <$> getDataBufferLength p
        return len

foreign import ccall unsafe "CyUSBSSerial.h CyProgUserFlash"
    _cyProgUserFlash'_ :: Ptr () -> Ptr () -> CUInt -> CUInt -> IO CInt
