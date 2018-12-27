{-|
Module      : System.Cypress.USBSerial.Internal.USBSeerial
Description : Constants, data types and helper functions
Copyright   : (c) Markus Mayer
License     : LGPL-2.1
Maintainer  : mmayer@mayeranalytics.com
Stability   : experimental
Portability : non-portable (needs dll/so/dylib)

See section 2,3 of the Cypress USB Serial Library API Guide, Rev 1.0.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE DataKinds                #-}

module System.Cypress.USBSerial.Internal.USBSerial (
    version,
    versionString,
    versionMajor,
    versionMinor,
    versionPatch,
    versionBuild,
    maxDeviceInterface,
    stringDescriptorSize,
    Handle(..),
    ReturnStatus(..),
    errorString,
    DeviceClass(..),
    DeviceInfo(..),
    ManufacturerName(..),
    ProductName(..),
    SerialNum(..),
    DeviceFriendlyName(..),
    DeviceSerialBlock(..),
    DeviceType(..),
    VidPid(..), Vid, Pid,
    DataBuffer(..),
    allocaDataBuffer,
    setDataBufferLength,
    getDataBufferLength,
    setDataBufferTransferCount,
    getDataBufferTransferCount,
    withByteString,
    toByteString,
    LibraryVersion(..),
    FirmwareVersion(..),
    CallbackEvents(..),
    EventNotification(..),
    callbackEventsBitmapToSet,
    toEither,
    returnIfSuccess,
    toReturnStatus,
    genericRead,
    genericWrite,
    charToBool,
    boolToChar
) where

import           Foreign
import           Foreign.C.Types
import           Foreign.Storable
import           Foreign.C.String
import           Control.Monad (liftM, when)
import           Data.Char (ord)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Int
import           Data.List (intercalate)
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString, useAsCStringLen, packCStringLen)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen, unsafePackCStringLen)
import           Data.Bits  (shift, (.&.), (.|.))
import           System.Cypress.USBSerial.Internal.FiniteLengthString

#include "CyUSBSerial.h"

{#typedef BOOL Bool#}

------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------  Constants  ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------  2.1 CY_US_VERSION  --------------------------------------------------
-- |Version number for the device.
--
-- LSB has major, next byte has minor, next byte has patch number
--
-- From: 2.1 CY_US_VERSION
version :: Int
version = versionMajor .|. (versionMinor `shift` 8) .|. (versionPatch `shift` 16)

-- |Version as string a.b.c.d
versionString :: String
versionString = intercalate "." (show <$> [versionMajor, versionMinor, versionPatch, versionBuild])

----------------------------------------------  2.2 CY_US_VERSION_MAJOR  -----------------------------------------------
-- |Major version number for library.
--
-- From: 2.2 CY_US_VERSION_MAJOR
versionMajor :: Int
versionMajor = {#const CY_US_VERSION_MAJOR#}

----------------------------------------------  2.3 CY_US_VERSION_MINOR  -----------------------------------------------
-- |Minor version number for library.
--
-- From: 2.3 CY_US_VERSION_MINOR
versionMinor :: Int
versionMinor = {#const CY_US_VERSION_MINOR#}

----------------------------------------------  2.4 CY_US_VERSION_PATCH  -----------------------------------------------
-- |Patch version number for library.
--
-- From: 2.4 CY_US_VERSION_PATCH
versionPatch :: Int
versionPatch = {#const CY_US_VERSION_PATCH#}

----------------------------------------------  2.5 CY_US_VERSION_BUILD  -----------------------------------------------
-- |Library build number.
--
-- From: 2.5 CY_US_VERSION_BUILD
versionBuild :: Int
versionBuild = {#const CY_US_VERSION_BUILD#}

--------------------------------------------  2.6 CY_MAX_DEVICE_INTERFACE  ---------------------------------------------
-- |Maximum number of interfaces
--
-- From: 2.6 CY_MAX_DEVICE_INTERFACE
maxDeviceInterface :: Int
maxDeviceInterface = {#const CY_MAX_DEVICE_INTERFACE#}

-------------------------------------------  2.7 CY_STRING_DESCRIPTOR_SIZE  --------------------------------------------
-- |String descriptor size
--
-- From: 2.7 CY_STRING_DESCRIPTOR_SIZE
stringDescriptorSize :: Int
stringDescriptorSize = {#const CY_STRING_DESCRIPTOR_SIZE#}


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------  Data Types  -----------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------  3.1 CY_HANDLE  ----------------------------------------------------
-- |CyUSB Device Handle.
--
-- The handle is used by application to communicate with USB serial device. The handle is obtained by calling `System.Cypress.USBSerial.USB.open`.
--
-- See Also
--
-- * `System.Cypress.USBSerial.USB.open`
--
-- From: 3.1 CY_HANDLE
type Handle = Ptr ()

------------------------------------------------  3.2 CY_RETURN_STATUS  ------------------------------------------------
-- |Enumeration defining return status of @all@ library functions.
--
-- `ErrorOther` is not part of the Cypress USB-Serial API. It is added to allow custom return status 
-- in the `System.Cypress.USBSerial.Safe` functions.
--
-- From: 3.2 CY_RETURN_STATUS
data ReturnStatus = Success -- ^ API returned successfully without any errors.
                  | ErrorAccessDenied   -- ^ Access of the API is denied for the application
                  | ErrorDriverInitFailed -- ^ Driver initialisation failed
                  | ErrorDeviceInfoFetchFailed  -- ^ Device information fetch failed
                  | ErrorDriverOpenFailed   -- ^ Failed to open a device in the library
                  | ErrorInvalidParameter   -- ^ One or more parameters sent to the API was invalid
                  | ErrorRequestFailed  -- ^ Request sent to USB Serial device failed
                  | ErrorDownloadFailed -- ^ Firmware download to the device failed
                  | ErrorFirmwareInvalidSignature   -- ^ Invalid Firmware signature in firmware file
                  | ErrorInvalidFirmware    -- ^ Invalid firmware
                  | ErrorDeviceNotFound -- ^ Device disconnected
                  | ErrorIoTimeout  -- ^ Timed out while processing a user request
                  | ErrorPipeHalted -- ^ Pipe halted while trying to transfer data
                  | ErrorBufferOverflow -- ^ OverFlow of buffer while trying to read/write data
                  | ErrorInvalidHandle  -- ^ Device handle is invalid
                  | ErrorAllocationFailed   -- ^ Error in Allocation of the resource inside the library
                  | ErrorI2cDeviceBusy  -- ^ I2C device busy
                  | ErrorI2cNakError    -- ^ I2C device NAK
                  | ErrorI2cArbitrationError    -- ^ I2C bus arbitration error
                  | ErrorI2cBusError    -- ^ I2C bus error
                  | ErrorI2cBusBusy -- ^ I2C bus is busy
                  | ErrorI2cStopBitSet  -- ^ I2C master has sent a stop bit during a transaction
                  | ErrorStatusMonitorExist -- ^ API Failed because the SPI/UART status monitor thread already exists
                  | ErrorOther String   -- ^ Other error produced by some of the `Safe` functions
    deriving (Eq,Show)

-- `ReturnStatus` is broken out, instead of using c2hs, because the `ErrorOther` has to be added.

instance Enum ReturnStatus where
    succ Success = ErrorAccessDenied
    succ ErrorAccessDenied = ErrorDriverInitFailed
    succ ErrorDriverInitFailed = ErrorDeviceInfoFetchFailed
    succ ErrorDeviceInfoFetchFailed = ErrorDriverOpenFailed
    succ ErrorDriverOpenFailed = ErrorInvalidParameter
    succ ErrorInvalidParameter = ErrorRequestFailed
    succ ErrorRequestFailed = ErrorDownloadFailed
    succ ErrorDownloadFailed = ErrorFirmwareInvalidSignature
    succ ErrorFirmwareInvalidSignature = ErrorInvalidFirmware
    succ ErrorInvalidFirmware = ErrorDeviceNotFound
    succ ErrorDeviceNotFound = ErrorIoTimeout
    succ ErrorIoTimeout = ErrorPipeHalted
    succ ErrorPipeHalted = ErrorBufferOverflow
    succ ErrorBufferOverflow = ErrorInvalidHandle
    succ ErrorInvalidHandle = ErrorAllocationFailed
    succ ErrorAllocationFailed = ErrorI2cDeviceBusy
    succ ErrorI2cDeviceBusy = ErrorI2cNakError
    succ ErrorI2cNakError = ErrorI2cArbitrationError
    succ ErrorI2cArbitrationError = ErrorI2cBusError
    succ ErrorI2cBusError = ErrorI2cBusBusy
    succ ErrorI2cBusBusy = ErrorI2cStopBitSet
    succ ErrorI2cStopBitSet = ErrorStatusMonitorExist
    succ ErrorStatusMonitorExist = ErrorOther ""
    succ _ = error "ReturnStatus.succ: ErrorOther has no successor"

    pred Success = error "ReturnStatus.pred: Success has no predecessor"
    pred ErrorAccessDenied = Success
    pred ErrorDriverInitFailed = ErrorAccessDenied
    pred ErrorDeviceInfoFetchFailed = ErrorDriverInitFailed
    pred ErrorDriverOpenFailed = ErrorDeviceInfoFetchFailed
    pred ErrorInvalidParameter = ErrorDriverOpenFailed
    pred ErrorRequestFailed = ErrorInvalidParameter
    pred ErrorDownloadFailed = ErrorRequestFailed
    pred ErrorFirmwareInvalidSignature = ErrorDownloadFailed
    pred ErrorInvalidFirmware = ErrorFirmwareInvalidSignature
    pred ErrorDeviceNotFound = ErrorInvalidFirmware
    pred ErrorIoTimeout = ErrorDeviceNotFound
    pred ErrorPipeHalted = ErrorIoTimeout
    pred ErrorBufferOverflow = ErrorPipeHalted
    pred ErrorInvalidHandle = ErrorBufferOverflow
    pred ErrorAllocationFailed = ErrorInvalidHandle
    pred ErrorI2cDeviceBusy = ErrorAllocationFailed
    pred ErrorI2cNakError = ErrorI2cDeviceBusy
    pred ErrorI2cArbitrationError = ErrorI2cNakError
    pred ErrorI2cBusError = ErrorI2cArbitrationError
    pred ErrorI2cBusBusy = ErrorI2cBusError
    pred ErrorI2cStopBitSet = ErrorI2cBusBusy
    pred ErrorStatusMonitorExist = ErrorI2cStopBitSet
    pred _ = ErrorStatusMonitorExist  -- this only matches ErrorOther!

    enumFromTo from to = go from where
        end = fromEnum to
        go v = case compare (fromEnum v) end of
                    LT -> v : go (succ v)
                    EQ -> [v]
                    GT -> []

    enumFrom from = enumFromTo from ErrorStatusMonitorExist

    fromEnum Success = 0
    fromEnum ErrorAccessDenied = 1
    fromEnum ErrorDriverInitFailed = 2
    fromEnum ErrorDeviceInfoFetchFailed = 3
    fromEnum ErrorDriverOpenFailed = 4
    fromEnum ErrorInvalidParameter = 5
    fromEnum ErrorRequestFailed = 6
    fromEnum ErrorDownloadFailed = 7
    fromEnum ErrorFirmwareInvalidSignature = 8
    fromEnum ErrorInvalidFirmware = 9
    fromEnum ErrorDeviceNotFound = 10
    fromEnum ErrorIoTimeout = 11
    fromEnum ErrorPipeHalted = 12
    fromEnum ErrorBufferOverflow = 13
    fromEnum ErrorInvalidHandle = 14
    fromEnum ErrorAllocationFailed = 15
    fromEnum ErrorI2cDeviceBusy = 16
    fromEnum ErrorI2cNakError = 17
    fromEnum ErrorI2cArbitrationError = 18
    fromEnum ErrorI2cBusError = 19
    fromEnum ErrorI2cBusBusy = 20
    fromEnum ErrorI2cStopBitSet = 21
    fromEnum ErrorStatusMonitorExist = 22
    fromEnum _ = error "ReturnStatus.fromEnum: OtherError has no number"

    toEnum 0 = Success
    toEnum 1 = ErrorAccessDenied
    toEnum 2 = ErrorDriverInitFailed
    toEnum 3 = ErrorDeviceInfoFetchFailed
    toEnum 4 = ErrorDriverOpenFailed
    toEnum 5 = ErrorInvalidParameter
    toEnum 6 = ErrorRequestFailed
    toEnum 7 = ErrorDownloadFailed
    toEnum 8 = ErrorFirmwareInvalidSignature
    toEnum 9 = ErrorInvalidFirmware
    toEnum 10 = ErrorDeviceNotFound
    toEnum 11 = ErrorIoTimeout
    toEnum 12 = ErrorPipeHalted
    toEnum 13 = ErrorBufferOverflow
    toEnum 14 = ErrorInvalidHandle
    toEnum 15 = ErrorAllocationFailed
    toEnum 16 = ErrorI2cDeviceBusy
    toEnum 17 = ErrorI2cNakError
    toEnum 18 = ErrorI2cArbitrationError
    toEnum 19 = ErrorI2cBusError
    toEnum 20 = ErrorI2cBusBusy
    toEnum 21 = ErrorI2cStopBitSet
    toEnum 22 = ErrorStatusMonitorExist
    toEnum unmatched = error ("ReturnStatus.toEnum: Cannot match " ++ show unmatched)

-- |Get the readable error description string from each `ReturnStatus`.
errorString :: ReturnStatus -> String
errorString Success = "API returned successfully without any errors."
errorString ErrorAccessDenied = "Access of the API is denied for the application"
errorString ErrorDriverInitFailed = "Driver initialisation failed"
errorString ErrorDeviceInfoFetchFailed = "Device information fetch failed"
errorString ErrorDriverOpenFailed = "Failed to open a device in the library"
errorString ErrorInvalidParameter = "One or more parameters sent to the API was invalid"
errorString ErrorRequestFailed = "Request sent to USB Serial device failed"
errorString ErrorDownloadFailed = "Firmware download to the device failed"
errorString ErrorFirmwareInvalidSignature = "Invalid Firmware signature in firmware file"
errorString ErrorInvalidFirmware = "Invalid firmware"
errorString ErrorDeviceNotFound = "Device disconnected"
errorString ErrorIoTimeout = "Timed out while processing a user request"
errorString ErrorPipeHalted = "Pipe halted while trying to transfer data"
errorString ErrorBufferOverflow = "OverFlow of buffer while trying to read/write data"
errorString ErrorInvalidHandle = "Device handle is invalid"
errorString ErrorAllocationFailed = "Error in Allocation of the resource inside the library"
errorString ErrorI2cDeviceBusy = "I2C device busy"
errorString ErrorI2cNakError = "I2C device NAK"
errorString ErrorI2cArbitrationError = "I2C bus arbitration error"
errorString ErrorI2cBusError = "I2C bus error"
errorString ErrorI2cBusBusy = "I2C bus is busy"
errorString ErrorI2cStopBitSet = "I2C master has sent a stop bit during a transaction"
errorString ErrorStatusMonitorExist = "API Failed because the SPI/UART status monitor thread already exists"
errorString (ErrorOther s) = "Other error: " ++ s


------------------------------------------------  3.3 CY_DEVICE_CLASS  -------------------------------------------------
-- |Enumeration defining list of USB device classes supported by USB Serial device.
--
-- See Also
--
-- * `DeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
--
-- From: 3.3 CY_DEVICE_CLASS
data DeviceClass = DeviceClass'Disabled -- ^ None or the interface is disabled
                 | DeviceClass'Cdc      -- ^ CDC ACM class
                 | DeviceClass'Phdc     -- ^ PHDC class
                 | DeviceClass'Vendor   -- ^ VENDOR specific class
                 | DeviceClass'NotSupported -- ^ all other device classes are not supported (thie is not part of the original API)
    deriving (Eq,Show)

-- hand-craft this one because the `DeviceClass'NotSupported` has to be added as the default class.

instance Enum DeviceClass where
  succ DeviceClass'Disabled = DeviceClass'Cdc
  succ DeviceClass'Cdc = DeviceClass'Phdc
  succ DeviceClass'Phdc = DeviceClass'Vendor
  succ DeviceClass'Vendor = DeviceClass'NotSupported
  succ DeviceClass'NotSupported = error "DeviceClass.succ: DeviceClass'NotSupported has no successor"

  pred DeviceClass'Cdc = DeviceClass'Disabled
  pred DeviceClass'Phdc = DeviceClass'Cdc
  pred DeviceClass'Vendor = DeviceClass'Phdc
  pred DeviceClass'NotSupported = DeviceClass'Vendor
  pred DeviceClass'Disabled = error "DeviceClass.pred: DeviceClass'Disabled has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from DeviceClass'NotSupported

  fromEnum DeviceClass'Disabled = 0
  fromEnum DeviceClass'Cdc = 0x02
  fromEnum DeviceClass'Phdc = 0x0F
  fromEnum DeviceClass'Vendor = 0xFF
  fromEnum DeviceClass'NotSupported = error "DeviceClass.fromEnum: Don't use DeviceClass'NotSupported"

  toEnum 0 = DeviceClass'Disabled
  toEnum 0x02 = DeviceClass'Cdc
  toEnum 0x0F = DeviceClass'Phdc
  toEnum 0xFF = DeviceClass'Vendor
  toEnum _ = DeviceClass'NotSupported   -- default !


-------------------------------------------------  3.4 CY_DEVICE_INFO  -------------------------------------------------
-- |This structure is used to hold information of the device connected to host.
--
-- The information can be obtained by using `System.Cypress.USBSerial.USB.getDeviceInfo `and `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
-- APIs.The information includes VID, PID, number of interfaces, string descriptors, device type and
-- device class supported by each interface. Device type is valid only if the interface is `DeviceClass'Vendor`.
--
-- See Also
--
-- * `VidPid`
-- * `DeviceClass`
-- * `DeviceType`
-- * `System.Cypress.USBSerial.USB.getDeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
--
-- From: 3.4 CY_DEVICE_INFO
data DeviceInfo = DeviceInfo
    { deviceInfo'vidPid :: VidPid -- ^ VID and PID
    , deviceInfo'numInterfaces :: Word8 -- ^ Number of interfaces supported
    , deviceInfo'manufacturerName :: ManufacturerName -- ^ Manufacturer name
    , deviceInfo'productName :: ProductName -- ^ Product name
    , deviceInfo'serialNum :: SerialNum -- ^ Serial number
    , deviceInfo'deviceFriendlyName :: DeviceFriendlyName -- ^ Device friendly name : Windows only
    , deviceInfo'deviceType :: [DeviceType] -- ^ Type of the device each interface has (Valid only for USB Serial Device) and interface in vendor class
    , deviceInfo'deviceClass :: [DeviceClass] -- ^ Interface class of each interface
#ifdef mingw32_HOST_OS    
    , deviceInfo'deviceBlock :: DeviceSerialBlock -- ^ On Windows, each USB Serial device interface is associated with a separate driver instance. This           variable represents the present driver interface instance that is associated with a serial block.
#endif
    } deriving (Show, Eq)

{#pointer *CY_DEVICE_INFO as DeviceInfoPtr -> DeviceInfo#}

-- |`FiniteLengthString` with manufacturer name, used in `DeviceInfo`.
type ManufacturerName = FiniteLengthString {#const CY_STRING_DESCRIPTOR_SIZE#}
-- |`FiniteLengthString` with product name, used in `DeviceInfo`.
type ProductName = FiniteLengthString {#const CY_STRING_DESCRIPTOR_SIZE#}
-- |`FiniteLengthString` with serial number, used in `DeviceInfo`.
type SerialNum = FiniteLengthString {#const CY_STRING_DESCRIPTOR_SIZE#}
-- |`FiniteLengthString` with friendly name, used in `DeviceInfo`.
type DeviceFriendlyName = FiniteLengthString {#const CY_STRING_DESCRIPTOR_SIZE#}

instance Storable DeviceInfo where
    sizeOf _  = {#sizeof  CY_DEVICE_INFO #}
    alignment _ = {#alignof CY_DEVICE_INFO #}
    peek p = do
        numInterfaces <- peekByteOff p {#offsetof CY_DEVICE_INFO->numInterfaces #} :: IO Word8
        let n = fromIntegral numInterfaces :: Int
        deviceTypes' <- peekArray n (plusPtr p {#offsetof CY_DEVICE_INFO->deviceType #}) :: IO [Int32] -- assuming sizeof enum == 4 !
        deviceClasses' <- peekArray n (plusPtr p {#offsetof CY_DEVICE_INFO->deviceClass #}) :: IO [Int32]
        DeviceInfo
           <$> peekByteOff p {#offsetof CY_DEVICE_INFO->vidPid #}
           <*> return numInterfaces
           <*> peekByteOff p {#offsetof CY_DEVICE_INFO->manufacturerName #}
           <*> peekByteOff p {#offsetof CY_DEVICE_INFO->productName #}
           <*> peekByteOff p {#offsetof CY_DEVICE_INFO->serialNum #}
           <*> peekByteOff p {#offsetof CY_DEVICE_INFO->deviceFriendlyName #}
           <*> return (toEnum.fromEnum <$> deviceTypes')
           <*> return (toEnum.fromEnum <$> deviceClasses')
#ifdef mingw32_HOST_OS
           <*> peekByteOff p {#offsetof CY_DEVICE_INFO->deviceBlock #}
#endif
    poke p x = do
        let deviceTypes = toEnum.fromEnum <$> deviceInfo'deviceType x :: [Int32]    -- assuming sizeof enum == 4 !
            deviceClasses = toEnum.fromEnum <$> deviceInfo'deviceClass x :: [Int32]
            ldt = length $ deviceInfo'deviceType x
            ldc = length $ deviceInfo'deviceClass x
        when (ldt /= ldc) $ error "length deviceInfo'deviceType != length deviceInfo'deviceClass"
        if ldt > maxDeviceInterface
        then error ("deviceInfo'deviceType > " ++ show maxDeviceInterface)
        else do    
            pokeByteOff p {#offsetof CY_DEVICE_INFO.vidPid #} (deviceInfo'vidPid x)
            {#set CY_DEVICE_INFO.numInterfaces #} p (fromIntegral $ deviceInfo'numInterfaces x)
            pokeByteOff p {#offsetof CY_DEVICE_INFO.manufacturerName #} (deviceInfo'manufacturerName x)
            pokeByteOff p {#offsetof CY_DEVICE_INFO.productName #} (deviceInfo'productName x)
            pokeByteOff p {#offsetof CY_DEVICE_INFO.serialNum #} (deviceInfo'serialNum x)
            pokeByteOff p {#offsetof CY_DEVICE_INFO.deviceFriendlyName #} (deviceInfo'deviceFriendlyName x)
            pokeArray (plusPtr p {#offsetof CY_DEVICE_INFO.deviceType #}) deviceTypes
            pokeArray (plusPtr p {#offsetof CY_DEVICE_INFO.deviceClass #}) deviceClasses
#ifdef mingw32_HOST_OS
            {#set CY_DEVICE_INFO.deviceBlock #} p (deviceInfo'deviceBlock x)
#endif


---------------------------------------------  3.6 CY_DEVICE_SERIAL_BLOCK  ---------------------------------------------
-- |This enumeration type defines the available device serial blocks for *Windows*.
--
-- A USB Serial device can have up to two configurable serial blocks. UART, SPI, I2C or JTAG functionality can be configured
-- and used in these serial blocks. The Windows driver binds to a serial block rather than the entire device. So it is
-- essential to find out which serial block to which current communications are directed. These enumeration structure
-- provides the possible SERIAL BLOCK Options. 
-- 
-- This data type information *doesn't apply for non-windows operating system*.
--
-- See Also
--
-- * `DeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
--
-- From: 3.6 CY_DEVICE_SERIAL_BLOCK
{#enum CY_DEVICE_SERIAL_BLOCK as DeviceSerialBlock {upcaseFirstLetter} with prefix = "SerialBlock_" add prefix = "SerialBlock'" deriving (Eq, Show)#}


-------------------------------------------------  3.7 CY_DEVICE_TYPE  -------------------------------------------------
-- |Enumeration defining list of device types supported by the USB Serial device in each interface.
--
-- This applies when the interface is configured as `DeviceClass'Vendor`.
-- The interface type can be queried from the device by using `getDeviceInfo`
-- and `getDeviceInfoVidPid`. The `DeviceInfo` structure contains the interface type.
--
-- See Also
--
-- * `DeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
--
-- From: 3.7 CY_DEVICE_TYPE
data DeviceType = DeviceType'DISABLED
                | DeviceType'UART
                | DeviceType'SPI
                | DeviceType'I2C
                | DeviceType'JTAG
                | DeviceType'MFG
                | DeviceType'NotSupported   -- ^ This `DeviceType` is not part of the original API.
  deriving (Eq,Show)

-- hand-craft this one because the `DeviceClass'NotSupported` has to be added as the default class.

instance Enum DeviceType where
  succ DeviceType'DISABLED = DeviceType'UART
  succ DeviceType'UART = DeviceType'SPI
  succ DeviceType'SPI = DeviceType'I2C
  succ DeviceType'I2C = DeviceType'JTAG
  succ DeviceType'JTAG = DeviceType'MFG
  succ DeviceType'MFG = DeviceType'NotSupported
  succ DeviceType'NotSupported = error "DeviceType.succ: DeviceType'MFG has no successor"

  pred DeviceType'UART = DeviceType'DISABLED
  pred DeviceType'SPI = DeviceType'UART
  pred DeviceType'I2C = DeviceType'SPI
  pred DeviceType'JTAG = DeviceType'I2C
  pred DeviceType'MFG = DeviceType'JTAG
  pred DeviceType'NotSupported = DeviceType'MFG
  pred DeviceType'DISABLED = error "DeviceType.pred: DeviceType'DISABLED has no predecessor"

  enumFromTo from to = go from
    where
      end = fromEnum to
      go v = case compare (fromEnum v) end of
                 LT -> v : go (succ v)
                 EQ -> [v]
                 GT -> []

  enumFrom from = enumFromTo from DeviceType'MFG

  fromEnum DeviceType'DISABLED = 0
  fromEnum DeviceType'UART = 1
  fromEnum DeviceType'SPI = 2
  fromEnum DeviceType'I2C = 3
  fromEnum DeviceType'JTAG = 4
  fromEnum DeviceType'MFG = 5
  fromEnum DeviceType'NotSupported = error "DeviceClass.fromEnum: Don't use DeviceType'NotSupported"

  toEnum 0 = DeviceType'DISABLED
  toEnum 1 = DeviceType'UART
  toEnum 2 = DeviceType'SPI
  toEnum 3 = DeviceType'I2C
  toEnum 4 = DeviceType'JTAG
  toEnum 5 = DeviceType'MFG
  toEnum _ = DeviceType'NotSupported -- default !


---------------------------------------------------  3.8 CY_VID_PID  ---------------------------------------------------
-- |This structure is used to hold VID and PID of USB device
--
-- This Structure holds the VID and PID of a USB device.
--
-- See Also
--
-- * `DeviceInfo`
-- * `System.Cypress.USBSerial.USB.getDeviceInfoVidPid`
--
-- From: 3.8 CY_VID_PID
data VidPid = VidPid
    { vidPid'vid :: Vid -- ^ Holds the VID of the device
    , vidPid'pid :: Pid -- ^ Holds the PID of the device
    } deriving (Show, Eq)

type Vid = Word16   -- ^ Vendor ID
type Pid = Word16   -- ^ Product ID

instance Storable VidPid where
    sizeOf _  = {#sizeof  CY_VID_PID#}
    alignment _ = {#alignof CY_VID_PID#}
    peek p = VidPid
           <$> liftM fromIntegral ({#get CY_VID_PID->vid #} p) -- UINT16
           <*> liftM fromIntegral ({#get CY_VID_PID->pid #} p) -- UINT16
    poke p x = do
        {#set CY_VID_PID.vid #} p (fromIntegral $ vidPid'vid x) -- UINT16
        {#set CY_VID_PID.pid #} p (fromIntegral $ vidPid'pid x) -- UINT16


------------------------------------------------  3.10 CY_DATA_BUFFER  -------------------------------------------------
-- |This structure is used to hold data buffer information.
--
-- This structure is used by all the data transaction APIs in the library to perform read, write operations. Before
-- using a variable of this structre users need to initialize various members appropriately.
--
-- See Also
--
-- * `System.Cypress.USBSerial.Uart.read`
-- * `System.Cypress.USBSerial.Uart.write`
-- * `System.Cypress.USBSerial.I2C.read`
-- * `System.Cypress.USBSerial.I2C.write`
-- * `System.Cypress.USBSerial.SPI.readWrite`
-- * `System.Cypress.USBSerial.JTAG.read`
-- * `System.Cypress.USBSerial.JTAG.write`
--
-- From: 3.10 CY_DATA_BUFFER
data DataBuffer = DataBuffer
    { dataBuffer'buffer :: Ptr ()  -- ^ Pointer to the buffer from where the data is read/written
    , dataBuffer'length :: Word32 -- ^ Length of the buffer
    , dataBuffer'transferCount :: Word32 -- ^ Number of bytes actually read/written
    } deriving (Show, Eq)

instance Storable DataBuffer where
    sizeOf _  = {#sizeof  CY_DATA_BUFFER#}
    alignment _ = {#alignof CY_DATA_BUFFER#}
    peek p = DataBuffer
        <$> (peekByteOff p {#offsetof CY_DATA_BUFFER->buffer #} :: IO (Ptr ()))
        <*> liftM fromIntegral ({#get CY_DATA_BUFFER->length #} p)
        <*> liftM fromIntegral ({#get CY_DATA_BUFFER->transferCount #} p)
    poke p x = do
        pokeByteOff p {#offsetof CY_DATA_BUFFER->buffer #} (dataBuffer'buffer x)
        setDataBufferLength p (fromIntegral $ dataBuffer'length x)
        setDataBufferTransferCount p (fromIntegral $ dataBuffer'transferCount x)

-- | Allocate and initialize a `DataBuffer` of length @len@, then execute computation `f` 
-- (like `alloca`, but for `DataBuffer`).
--
-- Used in:
--
-- * `System.Cypress.USBSerial.Uart.read`
-- * `System.Cypress.USBSerial.I2C.read`
-- * `System.Cypress.USBSerial.SPI.readWrite`
-- * `System.Cypress.USBSerial.JTAG.read`
allocaDataBuffer :: Word32 -> (Ptr DataBuffer -> IO b) -> IO b
allocaDataBuffer len f = allocaBytes (fromIntegral len) $ \buf -> alloca $ \(p :: Ptr DataBuffer) -> do
    pokeByteOff p {#offsetof CY_DATA_BUFFER->buffer #} buf
    setDataBufferLength p (fromIntegral len)
    setDataBufferTransferCount p 0
    f p

-- |Set buffer length of `DataBuffer` given by `Ptr DataBuffer`
setDataBufferLength :: Ptr DataBuffer   -- ^ `Ptr` to `DataBuffer`
                    -> Word32           -- ^ length to set
                    -> IO ()
setDataBufferLength p l = {#set CY_DATA_BUFFER.length #} p (fromIntegral l)

-- |Get buffer length of `DataBuffer` given by `Ptr DataBuffer`
getDataBufferLength :: Ptr DataBuffer   -- ^ `Ptr` to `DataBuffer`
                    -> IO Word32        -- ^ transfer count
getDataBufferLength p = fromIntegral <$> {#get CY_DATA_BUFFER->length #} p

-- |Set buffer transfer count of `DataBuffer` given by `Ptr DataBuffer`
setDataBufferTransferCount :: Ptr DataBuffer   -- ^ `Ptr` to `DataBuffer`
                           -> Word32           -- ^ length to set
                           -> IO ()
setDataBufferTransferCount p l = {#set CY_DATA_BUFFER.transferCount #} p (fromIntegral l)

-- |Get buffer transfer count of `DataBuffer` given by `Ptr DataBuffer`
getDataBufferTransferCount :: Ptr DataBuffer    -- ^ `Ptr` to `DataBuffer`
                           -> IO Word32         -- ^ transfer count
getDataBufferTransferCount p = fromIntegral <$> {#get CY_DATA_BUFFER->transferCount #} p

-- |Execute the computation @f@, passing as argument a pointer to a temporarily allocated and initialized `DataBuffer`  
-- (like `with`, but for `DataBuffer` constructed from a `Data.ByteString.Char8.ByteString`).
--
-- Note that this uses `unsafeUseAsCStringLen`.
--
-- Used in:
--
-- * `System.Cypress.USBSerial.I2C.write`
-- * `System.Cypress.USBSerial.JTAG.write`
-- * `System.Cypress.USBSerial.SPI.readWrite`
-- * `System.Cypress.USBSerial.Uart.write`
withByteString :: ByteString -> (Ptr DataBuffer -> IO b) -> IO b
withByteString bs f = 
    unsafeUseAsCStringLen bs $ \(pBs, len) -> 
    allocaBytes (fromIntegral len) $ \buf -> 
    alloca $ \(p :: Ptr DataBuffer) -> do
        pokeByteOff p {#offsetof CY_DATA_BUFFER->buffer #} pBs
        setDataBufferLength p (fromIntegral len)
        setDataBufferTransferCount p 0
        f p

-- |Extract `ByteString` from `DataBuffer` given by `Ptr` to `DataBuffer`. 
-- The length is determined by transferCount
toByteString :: Ptr DataBuffer -> IO ByteString
toByteString p = do
    buf <- peekByteOff p {#offsetof CY_DATA_BUFFER->buffer #} :: IO (Ptr ())
    l <- getDataBufferLength p
    packCStringLen (castPtr buf, fromIntegral l)


----------------------------------------------  3.12 CY_LIBRARY_VERSION  -----------------------------------------------
-- |This structure is used to hold version information of the library.
--
-- See Also
--
-- * `System.Cypress.Safe.USBSerial.getLibraryVersion`
-- * `System.Cypress.USBSerial.getLibraryVersion`
--
-- From: 3.12 CY_LIBRARY_VERSION
data LibraryVersion = LibraryVersion
    { libraryVersion'majorVersion :: Word8 -- ^ The major version of the library
    , libraryVersion'minorVersion :: Word8 -- ^ The minor version of the library
    , libraryVersion'patch :: Word16 -- ^ The patch number of the library
    , libraryVersion'buildNumber :: Word8 -- ^ The build number of the library
    } deriving (Show, Eq)

instance Storable LibraryVersion where
    sizeOf _  = {#sizeof  CY_LIBRARY_VERSION#}
    alignment _ = {#alignof CY_LIBRARY_VERSION#}
    peek p = LibraryVersion
           <$> liftM fromIntegral ({#get CY_LIBRARY_VERSION->majorVersion #} p) -- UINT8
           <*> liftM fromIntegral ({#get CY_LIBRARY_VERSION->minorVersion #} p) -- UINT8
           <*> liftM fromIntegral ({#get CY_LIBRARY_VERSION->patch #} p) -- UINT16
           <*> liftM fromIntegral ({#get CY_LIBRARY_VERSION->buildNumber #} p) -- UINT8
    poke p x = do
        {#set CY_LIBRARY_VERSION.majorVersion #} p (fromIntegral $ libraryVersion'majorVersion x) -- UINT8
        {#set CY_LIBRARY_VERSION.minorVersion #} p (fromIntegral $ libraryVersion'minorVersion x) -- UINT8
        {#set CY_LIBRARY_VERSION.patch #} p (fromIntegral $ libraryVersion'patch x) -- UINT16
        {#set CY_LIBRARY_VERSION.buildNumber #} p (fromIntegral $ libraryVersion'buildNumber x) -- UINT8


----------------------------------------------  3.14 CY_FIRMWARE_VERSION  ----------------------------------------------
-- |This structure is used to hold firmware version of the USB Serial device.
--
-- See Also
--
-- * `System.Cypress.USBSerial.USB.getFirmwareVersion`
--
-- From: 3.14 CY_FIRMWARE_VERSION
data FirmwareVersion = FirmwareVersion
    { firmwareVersion'majorVersion :: Word8 -- ^ Major version of the Firmware
    , firmwareVersion'minorVersion :: Word8 -- ^ Minor version of the Firmware
    , firmwareVersion'patchNumber :: Word16 -- ^ Patch Number of the Firmware
    , firmwareVersion'buildNumber :: Word32 -- ^ Build Number of the Firmware
    } deriving (Show, Eq)

instance Storable FirmwareVersion where
    sizeOf _  = {#sizeof  CY_FIRMWARE_VERSION#}
    alignment _ = {#alignof CY_FIRMWARE_VERSION#}
    peek p = FirmwareVersion
           <$> liftM fromIntegral ({#get CY_FIRMWARE_VERSION->majorVersion #} p) -- UINT8
           <*> liftM fromIntegral ({#get CY_FIRMWARE_VERSION->minorVersion #} p) -- UINT8
           <*> liftM fromIntegral ({#get CY_FIRMWARE_VERSION->patchNumber #} p) -- UINT16
           <*> liftM fromIntegral ({#get CY_FIRMWARE_VERSION->buildNumber #} p) -- UINT32
    poke p x = do
        {#set CY_FIRMWARE_VERSION.majorVersion #} p (fromIntegral $ firmwareVersion'majorVersion x) -- UINT8
        {#set CY_FIRMWARE_VERSION.minorVersion #} p (fromIntegral $ firmwareVersion'minorVersion x) -- UINT8
        {#set CY_FIRMWARE_VERSION.patchNumber #} p (fromIntegral $ firmwareVersion'patchNumber x) -- UINT16
        {#set CY_FIRMWARE_VERSION.buildNumber #} p (fromIntegral $ firmwareVersion'buildNumber x) -- UINT32


----------------------------------------------  3.16 CY_CALLBACK_EVENTS  -----------------------------------------------
-- |Enumeration defining UART/SPI transfer error or status bit maps.
--
-- See Also
--
-- * `withEventNotification`
--
-- From: 3.16 CY_CALLBACK_EVENTS
{#enum CY_CALLBACK_EVENTS as CallbackEvents {underscoreToCase} with prefix = "Cy" add prefix = "Event'" deriving (Eq, Show, Ord)#}


------------------------------------------  3.17 CY_EVENT_NOTIFICATION_CB_FN  ------------------------------------------
-- |Function pointer for getting async error/success notification on UART/SPI
--
-- This function pointer that will be passed to `CySetEventNotification `and get a
-- callback with a 2 byte value bit map that reports error/events triggered during UART/SPI transaction. The bit map
-- is defined inCY_CALLBACK_EVENTS`.`
--
-- See Also
--
-- * `CallbackEvents`
--
-- From: 3.17 CY_EVENT_NOTIFICATION_CB_FN
type EventNotification = Word16 -- ^ Bitmap of `CallbackEvents`. Use `callbackEventsBitmapToSet` to get a `Set` of `CallbackEvents`.
                       -> IO ()

-- |Convert a bitmap of CY_CALLBACK_EVENTS to a `Set` of `CallbackEvents`. Use this in `EventNotification`.
callbackEventsBitmapToSet :: Word16 -> Set CallbackEvents
callbackEventsBitmapToSet b = Set.fromList events'
    where events = [Event'UartCtsBit, Event'UartDsrBit, Event'UartBreakBit, Event'UartRingSignalBit, Event'UartFrameErrorBit, Event'UartParityErrorBit, Event'UartDataOverrunBit, Event'UartDcdBit, Event'SpiTxUnderflowBit, Event'SpiBusErrorBit, Event'ErrorEventFailedBit]
          masks = (fromIntegral.fromEnum) <$> events :: [Word16]
          events' = map snd $ filter (\(i,e)->b .&. i==1) (zip masks events)

          
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------  Helpers  -------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

-- |Transform the tuples returned by c-function wrappers generated by @c2hs@ into an `Either`.
toEither :: (ReturnStatus, a)      -- ^ tuple as returned by the c-function wrappers
         -> Either ReturnStatus a  -- ^ Failed `ReturnStatus` or good value
toEither (s,a) = if s==Success then Right a else Left s

-- |Wrap the `ReturnStatus` returned by a library call into an `Either` `ReturnStatus` @a@, where @a@ is obtained 
-- by running the @action@.
returnIfSuccess :: ReturnStatus -> IO a -> IO (Either ReturnStatus a)
returnIfSuccess status action = 
    if status == Success then Right <$> action
    else return $ Left status

-- |Convert return value of library call to `ReturnStatus`
toReturnStatus :: CInt -> ReturnStatus
toReturnStatus = toEnum . fromIntegral

-- |Generic read
--
-- Used in:
--
-- * `System.Cypress.USBSerial.JTAG.read`
-- * `System.Cypress.USBSerial.UART.read`
genericRead :: Handle      -- ^ Valid device handle
            -> Word32      -- ^ read length
            -> Word32      -- ^ API timeout value
            -> (Ptr () -> Ptr () -> CUInt -> IO CInt) -- ^ C function call
            -> IO (Either ReturnStatus ByteString)  -- ^ Failed `ReturnStatus` or `ByteString` with read data
genericRead h l t cCall = 
    allocaDataBuffer (fromIntegral l) $ \pDat -> do
        setDataBufferLength pDat l
        status <- toReturnStatus <$> cCall (castPtr h) (castPtr pDat) (fromIntegral t)
        returnIfSuccess status $ toByteString pDat

-- |Generic write
--
-- Used in
--
-- * `System.Cypress.USBSerial.JTAG.write`
-- * `System.Cypress.USBSerial.UART.write`
genericWrite :: Handle      -- ^ Valid device handle
             -> ByteString  -- ^ data to write
             -> Word32      -- ^ API timeout value
             -> (Ptr () -> Ptr () -> CUInt -> IO CInt) -- ^ C function call
             -> IO (Either ReturnStatus Word32) -- ^ Failed `ReturnStatus` or number of bytes read
genericWrite h bs t cCall = 
    withByteString bs $ \pDat -> do
        status <- toReturnStatus <$> cCall (castPtr h) (castPtr pDat) (fromIntegral t)
        returnIfSuccess status $ getDataBufferTransferCount pDat

-- |Convert C char to Bool. Needed in the marshalling functions for data types with `Bool`.
charToBool :: CUChar -> Bool
charToBool i = i /= 0

-- |Convert Bool to C char. Needed in the marshalling functions for data types with `Bool`.
boolToChar :: Bool -> CUChar
boolToChar b = if b then 1 else 0
        