========================================================================================
                      Cypress Semiconductor Corporation
                            CyUSB Serial library.
=========================================================================================
Pre-requisite:
1. libusb-1.0.9 is required for compilation and functioning of the APIs in the library.
2. GNU Make and the GCC tool-chain are used for the build process.

Installation steps:

1.cd to the main directory where library source files are extracted and type
	'sudo make'

2.This will generate shared library libcyusbserial.so.1 and its soft link libcyusbserial.so.
  Both of them will be copied/installed to /usr/local/lib.

3.Application can link and start using the library.

Changing USB device node permission:

**Note:
By default the USB device node created in linux do not have read/write permission
for normal user. In order to make the USB device node accessable, copy 90-cyusb.rules
which is inside the library folder of release package to /etc/udev/rules.d folder (Default VID mentioned is Cypress VID
which is 0x4B4, if VID is changed then update it in ATTR{idVendor} section of rules file).




