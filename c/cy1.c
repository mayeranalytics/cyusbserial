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
    UINT8 numDevices;
    status = CyGetListofDevices(&numDevices);
    if(status == CY_SUCCESS) {
        printf("I have %d devices\n\n", numDevices);
        for(int i=0; i<numDevices; i++) {
            printf("----- %d -----\n", i);
            CY_DEVICE_INFO info;
            status = CyGetDeviceInfo(i, &info);
            if(status != CY_SUCCESS) {
                printf("FAIL with status %d\n", status);
                continue;
            }
            printf("vidpid %x\n", info.vidPid);
            printf("%d interfaces\n", info.numInterfaces);
            printf("manuf %s\n", info.manufacturerName);
            printf("prod  %s\n", info.productName);
            printf("ser   %s\n", info.serialNum);
            printf("fname %s\n", info.deviceFriendlyName);
            printf("type class\n");
            for(int j = 0; j<info.numInterfaces; j++) {
                printf("  %d %d\n", info.deviceType[j], info.deviceClass[j]);
            }
            printf("\n");
        }
    } else {
        printf("CyGetListofDevices failed with code %d\n", status);
    }

    CyLibraryExit();
    return 0;
}
