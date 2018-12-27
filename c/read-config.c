#include "extras.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <libusb-1.0/libusb.h>

CY_HANDLE findDevice(uint16_t vid, uint16_t pid, bool verbose) {
    CY_VID_PID vidPid = {vid, pid};
    CY_RETURN_STATUS status;
    UINT8 numDevices;
    status = CyGetListofDevices(&numDevices);
    if(status == CY_SUCCESS) {
        CY_VID_PID vidpid = {0x04b4, 0x0002};
        UINT8 deviceCount = 0;
        UINT8 deviceIdList[255];
        CY_DEVICE_INFO deviceInfoList[255];
        status = CyGetDeviceInfoVidPid(vidpid, deviceIdList, deviceInfoList, &deviceCount, 255);
        CY_HANDLE handle;
        if(status == CY_SUCCESS) {
            if(verbose)
                printf("deviceCount = %d\n", deviceCount);
            for(int i=0; i<deviceCount; i++) {
                if(verbose) {
                    printf("  id = %d\n", deviceIdList[i]);
                    printf("  numIfaces = %d\n", deviceInfoList[i].numInterfaces);
                }
                for(int j=0; j<deviceInfoList[i].numInterfaces; j++) {
                    if(verbose) {
                        printf("  iface = %d\n", j);
                        printf("    class = %d\n", deviceInfoList[i].deviceClass[j]);
                        printf("    type = %d\n", deviceInfoList[i].deviceType[j]);
                    }
                    if(deviceInfoList[i].deviceClass[j] == 255 && deviceInfoList[i].deviceType[j]==5) {
                        status = CyOpen(deviceIdList[i], j, &handle);
                        if(status != CY_SUCCESS) {  
                            if(verbose)    
                                printf("    open id=%d, iface=%d failed %d\n", deviceIdList[i], j, status);
                            return NULL;
                        } else {
                            return handle;
                        }
                    }
                }
            }
        } else if(status == CY_ERROR_DEVICE_NOT_FOUND) {
            if(verbose)    
                printf("No device with vid:pid=%04x:%04x found\n", vid, pid);
        } else 
            fprintf(stderr, "CyGetDeviceInfoVidPid failed with code %d\n", status);

    } else {
        fprintf(stderr, "CyGetListofDevices failed with code %d\n", status);
    }
    return NULL;
}

int
main(int argc, char* argv[])
{
    CY_RETURN_STATUS status;

    if(argc < 2) {
        fprintf(stderr, "I need a output filename\n");
        exit(1);
    }

    status = CyLibraryInit();
    if(status != CY_SUCCESS) {
        printf("FAIL %d\n", status);
        exit(1);
    }

    CY_HANDLE handle = NULL;
    bool flag = false;
    while(!(handle=findDevice(0x04b4, 0x0002, false))) {
        if(!flag) { printf("insert device!\n"); flag = true; }
        sleep(1);
    }

    if(handle) {
        CONFIG_FLASH config;
        status = CyReadConfigFlash(handle, &config, 10);
        if(status != CY_SUCCESS) {  
            printf("      failed read %d\n", status);
        } else {
            FILE *ptr;
            ptr = fopen(argv[1],"wb");
            fwrite(&config,CONFIG_FLASH_SIZE,1,ptr);
            fclose(ptr);
            printf("Wrote 512 bytes to %s\n", argv[1]);
        }
        CyClose(handle);
    }

    CyLibraryExit();
    return 0;
}
