#include <stdio.h>
#include <stdlib.h>
#include <libusb-1.0/libusb.h>
#include "extras.h"

/* 
 *         config flash layout
 *      0   1   2   3   4   5   6   7 
 *  0   C   Y   U   S  --- version ---
 *  8  -- checksum --- ...............
 * 12  ...............................
 */

uint32_t calcChkSum(CONFIG_FLASH* config) {
    uint32_t s = 0, *p = (uint32_t*)config;
    for(int i=3; i<CONFIG_FLASH_SIZE / 4; s += p[i++]);
    return s;
}

uint8_t chkSum(CONFIG_FLASH* config) {
    return calcChkSum(config) == config->checkSum;
}

CYWINEXPORT CY_RETURN_STATUS WINCALLCONVEN CyReadConfigFlash (
    CY_HANDLE handle,                       /*Valid device handle*/
    CONFIG_FLASH* config,                   /*ptr to pre-allocated CONFIG_FLASH*/
    UINT32 ioTimeout                        /*Timeout value of the API*/
    )
{
    UINT16 wValue, wIndex, wLength;
    UINT8 bmRequestType, bmRequest;
    int rStatus;
    CY_DEVICE *device;
    libusb_device_handle *devHandle;
    
    if (handle == NULL)
        return CY_ERROR_INVALID_HANDLE;
    if (config == NULL)
        return CY_ERROR_INVALID_PARAMETER;

    device = (CY_DEVICE *)handle;
    devHandle = device->devHandle;

    rStatus = libusb_control_transfer (devHandle, 0x40, 0xe2, 0xA6BC, 0xB1B0, NULL, 0, 10);
    if (rStatus == LIBUSB_ERROR_TIMEOUT) {
        CY_DEBUG_PRINT_ERROR ("CY:Time out error..function is %s \n", __func__);
        return CY_ERROR_IO_TIMEOUT;
    } else if (rStatus < 0) {
        CY_DEBUG_PRINT_ERROR ("CY:Time out error..function is %s \n", __func__);
        return CY_ERROR_REQUEST_FAILED;
    }
    
    bmRequestType = CY_VENDOR_REQUEST_DEVICE_TO_HOST;
    bmRequest = 0xb5; 
    wValue = 0;
    wIndex = 0;
    wLength = CONFIG_FLASH_SIZE;

    CY_DEBUG_PRINT_INFO ("CY:The Length is %d , Value is %d and index is %d\n", wLength, wValue, wIndex);
    rStatus = libusb_control_transfer (devHandle, bmRequestType, bmRequest,
            wValue, wIndex, (void*)config, wLength, ioTimeout);
    if (rStatus > 0) {
        return CY_SUCCESS;
    }
    else if (rStatus == LIBUSB_ERROR_TIMEOUT) {
        CY_DEBUG_PRINT_ERROR ("CY:Time out error ..function is %s \n", __func__);
        return CY_ERROR_IO_TIMEOUT;
    }
    else {
        CY_DEBUG_PRINT_ERROR ("CY: Error in function %s ...libusb error is %d!\n", __func__, rStatus);
        return CY_ERROR_REQUEST_FAILED;
    }
}

CYWINEXPORT CY_RETURN_STATUS WINCALLCONVEN CyProgConfigFlash (
    CY_HANDLE handle,                       /*Valid device handle*/
    CONFIG_FLASH* config,                   /*ptr to pre-allocated CONFIG_FLASH*/
    UINT32 ioTimeout                          /*Timeout value of the API*/
    )
{
    UINT16 wValue, wIndex, wLength;
    UINT8 bmRequestType, bmRequest;
    int rStatus;
    CY_DEVICE *device;
    libusb_device_handle *devHandle;
    
    if (handle == NULL)
        return CY_ERROR_INVALID_HANDLE;
    if (config == NULL)
        return CY_ERROR_INVALID_PARAMETER;
    if(!chkSum(config))
        return CY_ERROR_INVALID_PARAMETER;

    device = (CY_DEVICE *)handle;
    devHandle = device->devHandle;
    
    bmRequestType = CY_VENDOR_REQUEST_HOST_TO_DEVICE;
    bmRequest = 0xb6; 
    wValue = 0;
    wIndex = 0;
    wLength = CONFIG_FLASH_SIZE;

    CY_DEBUG_PRINT_INFO ("CY:The Length is %d , Value is %d and index is %d\n", wLength, wValue, wIndex);
    rStatus = libusb_control_transfer (devHandle, bmRequestType, bmRequest,
            wValue, wIndex, (void*)config, wLength, ioTimeout);
    if (rStatus > 0) {
        return CY_SUCCESS;
    }
     else if (rStatus == LIBUSB_ERROR_TIMEOUT) {
        CY_DEBUG_PRINT_ERROR ("CY:Time out error ..function is %s \n", __func__);
        return CY_ERROR_IO_TIMEOUT;
    }
    else {
        CY_DEBUG_PRINT_ERROR ("CY: Error in function %s ...libusb error is %d!\n", __func__, rStatus);
        return CY_ERROR_REQUEST_FAILED;
    }
}
