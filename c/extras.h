#ifndef __EXTRAS_H__
#define __EXTRAS_H__ __EXTRAS_H__

#include <stdint.h>
#include "CyUSBSerial.h"
#include "CyUSBCommon.h"

#define CONFIG_FLASH_SIZE 512
#define CONFIG_CHKSUM_OFFSET 8

/* 
 *         config flash layout
 *      0   1   2   3   4   5   6   7 
 *  0   C   Y   U   S  --- version ---
 *  8  -- checksum --- ...............
 * 12  ...............................
 */

typedef struct _CONFIG_FLASH {
    char cyus[4];
    uint32_t version;
    uint32_t checkSum;
    uint8_t data[0x200 - 4*3];
} CONFIG_FLASH;

uint32_t calcChkSum(CONFIG_FLASH* config);

uint8_t chkSum(CONFIG_FLASH* config);

CYWINEXPORT CY_RETURN_STATUS  WINCALLCONVEN CyReadConfigFlash (
    CY_HANDLE handle,                       /*Valid device handle*/
    CONFIG_FLASH* config,                   /*ptr to pre-allocated CONFIG_FLASH*/
    UINT32 ioTimeout                        /*Timeout value of the API*/
    );

CYWINEXPORT CY_RETURN_STATUS CyProgConfigFlash (
    CY_HANDLE handle,                       /*Valid device handle*/
    CONFIG_FLASH* config,                   /*ptr to CONFIG_FLASH*/
    UINT32 timeout                          /*Timeout value of the API*/
    );

#endif //ifndef __EXTRAS_H__