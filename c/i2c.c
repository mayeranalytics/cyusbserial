#include <stdio.h>
#include <stdlib.h>
#include "CyUSBSerial.h"

int
main()
{
    CY_RETURN_STATUS status;
    if(sizeof(CY_DEVICE_TYPE) != sizeof(int)) {
        fprintf(stderr, "sizeof(CY_DEVICE_TYPE) != sizeof(int)\n");
        exit(1);
    }
    status = CyLibraryInit();
    if(status != CY_SUCCESS) {
        printf("FAIL %d\n", status);
        exit(1);
    }

    CY_HANDLE handle = NULL;
    status = CyOpen(11,0,&handle);
    if(status != CY_SUCCESS) {
        printf("FAIL CyOpen %d\n", status);
    } else {
        CY_I2C_DATA_CONFIG config = {0x40, false, true};
        char buf[256];
        CY_DATA_BUFFER databuf = {buf, 1, 0};
        databuf.buffer[0] = 0;
        status = CyI2cWrite(handle, &config, &databuf, 0);
        if(status != CY_SUCCESS) {
            printf("FAIL CyI2cWrite %d\n", status);
        } else {
            printf("success write %d\n", databuf.transferCount);
            databuf.transferCount = 0;
            databuf.length = 1;
            databuf.buffer[0] = 0;
            status = CyI2cRead(handle, &config, &databuf, 1000);
            if(status != CY_SUCCESS) {
                printf("FAIL CyI2cRead %d\n", status);
            } else {
                printf("success read %d: 0x%x\n",databuf.transferCount, (int)(databuf.buffer[0]));
            }
        }
        CyClose(handle);
    }


    CyLibraryExit();
    return 0;
}
