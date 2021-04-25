;*******************************************************************************
                    #ROM
;*******************************************************************************

RESERVED            equ       0xFFFF
noit                rti

;*******************************************************************************
                    #VECTORS  $FFC0
;*******************************************************************************

_vectors            dw        RESERVED            ; FFC0
                    dw        RESERVED            ; FFC2
                    dw        RESERVED            ; FFC4
                    dw        RESERVED            ; FFC6
                    dw        RESERVED            ; FFC8
                    dw        RESERVED            ; FFCA
                    dw        RESERVED            ; FFCC
                    dw        RESERVED            ; FFCE
                    dw        RESERVED            ; FFD0
                    dw        RESERVED            ; FFD2
                    dw        RESERVED            ; FFD4

                    dw        noit                ; FFD6 SCI
                    dw        noit                ; FFD8 SPI
                    dw        noit                ; FFDA PAI edge
                    dw        noit                ; FFDC PA overflow
                    dw        noit                ; FFDE Timer overflow
                    dw        noit                ; FFE0 OC5
                    dw        noit                ; FFE2 OC4
                    dw        noit                ; FFE4 OC3
                    dw        noit                ; FFE6 OC2
                    dw        noit                ; FFE8 OC1
                    dw        noit                ; FFEA IC3
                    dw        noit                ; FFEC IC2
                    dw        noit                ; FFEE IC1
                    dw        noit                ; FFF0 Real Time Int
                    dw        noit                ; FFF2 IRQ
                    dw        noit                ; FFF4 XIRQ
                    dw        noit                ; FFF6 SWI
                    dw        noit                ; FFF8 Illegal Opcode
                    dw        noit                ; FFFA COP Fail
                    dw        noit                ; FFFC Clock Monitor
                    dw        _start              ; FFFE RESET
