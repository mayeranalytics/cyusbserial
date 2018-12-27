#include <stdio.h>
#include <stdlib.h>
#include "CyUSBSerial.h"

int
main()
{
    CY_DEVICE_INFO info;
    printf("vipi = %lld\n", ((long long int)&(info.vidPid)) - (long long int)(&info));
    printf("ifac = %lld\n", ((long long int)&(info.numInterfaces)) - (long long int)(&info));
    printf("name = %lld\n", ((long long int)&(info.manufacturerName)) - (long long int)(&info));
    printf("prod = %lld\n", ((long long int)&(info.productName)) - (long long int)(&info));
    printf("derN = %lld\n", ((long long int)&(info.serialNum)) - (long long int)(&info));
    printf("fNam = %lld\n", ((long long int)&(info.deviceFriendlyName)) - (long long int)(&info));
    printf("type = %lld\n", ((long long int)&(info.deviceType)) - (long long int)(&info));
    printf("clas = %lld\n", ((long long int)&(info.deviceClass)) - (long long int)(&info));
    printf("length = %lu\n", sizeof(CY_DEVICE_INFO));
    return 0;
}
