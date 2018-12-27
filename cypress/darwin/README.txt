========================================================================================
                      Cypress Semiconductor Corporation
                            CyUSB Serial library.
=========================================================================================
Pre-requisite:
1. libusb-1.0.9 is required for compilation and functioning of the APIs in the library.

Installation steps:

1.cd to the main directory where library source files are extracted and type
	'sudo make'

2.This will generate shared library libcyusbserial.0.1.dylib and its soft link libcyusbserial.dylib
  Both of them will be copied/installed to /usr/local/lib.

3.Application can link and start using the library.

