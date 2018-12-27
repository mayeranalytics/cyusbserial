#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "extras.h"

int
main(int argc, char* argv[])
{
    if(argc < 2) {
        fprintf(stderr, "I need one or more filenames\n");
        exit(1);
    }
    CONFIG_FLASH config;
    FILE *ptr;
    while(argc>1) {
        argc--;
        ptr = fopen(argv[argc],"rb");
        if(!ptr) {
            fprintf(stderr, "No such file: %s\n", argv[argc]);
            exit(1);
        }
        size_t r = fread((void*)&config, CONFIG_FLASH_SIZE, 1, ptr);
        fclose(ptr);
        if(r != 1) {
            fprintf(stderr, "Failed to read %s\n", argv[argc]);
            perror("Error: ");
        } else {
            printf("%s %s\n", chkSum(&config) ? "OK" : "FAIL", argv[argc]);
        }
    }
    return 0;
}
