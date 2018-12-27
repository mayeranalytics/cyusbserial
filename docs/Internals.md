# Internals

The interface generator [ch2s](https://wiki.haskell.org/C2hs) is used for creating the Haskell wrapper.

### Project structure

```bash
src/System/Cypress/
├── USBSerial
│   ├── PHDC.chs
│   ├── I2C.chs
│   ├── JTAG.chs
│   ├── SPI.chs
│   ├── UART.chs
│   └── USB.chs
└── USBSerial.chs
```

#### `System.Cypress.USBSerial`



#### `System.Cypress.USBSerial.USB`

has the USB Initialization API

#### Haskell FFI docs:

- [GHC Threading And FFI](http://www.vex.net/~trebla/haskell/ghc-conc-ffi.xhtml)
- [Glasgow Haskell Compiler User's Guide: 13. Foreign function interface](https://downloads.haskell.org/~ghc/8.4.4/docs/html/users_guide/index.html)

