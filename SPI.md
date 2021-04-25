SPI words for sys11-forth
=========================

We decide to use the word names provided by AMForth:

* !@spi ( n1 – n2 ) SPI exchange of 2 bytes, high byte first
* c!@spi ( txbyte – rxbyte) SPI exchange of 1 byte
* n@spi ( addr len – ) read len bytes from SPI to addr
* n!spi ( addr len – ) read len bytes from SPI to addr
* n!@spi ( addr len – ) exchange len bytes with SPI to/from addr

Moreover, the sys11 mainboard has 8 SPI Chip Select lines, that can be
activated using the special following word:
* selspi ( line -- ) select spi line, or deselect all lines if negative.

The SPI device in the 68HC11 MCU can also be used as a slave (or peripheral).
This is not supported for now.
