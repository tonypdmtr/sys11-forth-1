;*******************************************************************************
; Mini forth interpreter based on eForth with simplifications
;
; Overview
; ========
; This is a forth interpreter inspired by eForth, with simplifications.
; Its goal is to be as compact as possible so it's possible to assemble itself from Forth.
; The following simplifications have be made with respect to the usual eForth implementations:
; Memory map simplifications: just a 1K param stack, and a RAM zone with dic and data
; growing from the low addresses and the return stack growing from the high addresses
; No USER pointer, only one user is supported
; Only a single word list, no vocabularies for each user.
; Favor F2012 words and semantics over the older standards if different.
;
; Builtin words are stored in RODATA. They cant be changed, but can be
; overriden by user entries.
;
; Hardware Registers
; ------------------
;
; The forth interpreter uses registers for its internal use.
; SP is used as parameter stack pointer.
; Y  is used as return stack pointer.
; X and D can be used for calculations.
;
; Data structures
; ===============
;
; Interpreter variables
; ---------------------
;
; The state of the interpreter is described in several variables, stored in the internal HC11 RAM.
; Here is a description of what each variable does.
; IP   - The instruction pointer. Points to the next instruction to execute.
; HEREP - pointer to the next free byte in the Data Space
; LASTP - pointer to the last definition. Initialized to the last builtin word definition.
; STATP - pointer containing 0x0000 when interpreting and 0xFFFF when compiling.
; BASE - Contains the value of the current number conversion base
;
; Dictionary and Data space
; -------------------------
;
; The data space is used to store new word definitions and user data structures.
; It grows from low addresses (0100h) to upper addresses in the external memory.
;
; User definitions are allocated in the data space. The definitions are added
; one after the other, linking the new one to the previous one. The pointer to
; the last definition is maintained in LAST. This pointer is initialized to the
; last entry in the builtin dictionary.
;
; Dictionary entry
; ----------------
;
; Each entry has the following structure:
; 2 bytes: pointer to previous entry
; N bytes: the word itself, NULL terminated. Only ASCII values 32-127 are allowed.
; 1 byte:  flags. It seems that we need 2 flags:
; - immediate: word has to be executed even while compiling.
; - compile only: word cannot be executed (control structures?).
; (NOTE: to save one byte per word, the flags can be set in the terminator itself
; if the name is terminated by a byte having the most significant bit set instead of zero.)
; The 3 previous bytes form the word header. It is only inspected when searching for words.
; 2 bytes: code pointer (ITC) - address of native code that implements this routine. Special value ENTER
; is used to handle compiled forth definitions.
; A forth word pointer, as used in user definitions, is a pointer to the code pointer for the word.
;
; Parameter stack
; ---------------
; The parameter stack is used to store temporary data items. It shoud not grow
; to an extremely large value, so it size is fixed when the interpreter is
; initialized.
; The stack grows down, it starts at the end of the RAM, and the storage
; PUSH is post-decrement, push LSB first.
; POP  is pre-increment, pull MSB first.
; The stack pointer always point at the next free location in the stack.
; For the moment underflow and overflows are not detected.
;
; The parameter stack is also used using push/rts to simplify the inner interpreter.
; This means that we need one more item to jump to the next forth opcode. This is important:
; if the forth stack underflows, we still need the stack to work for one more item.
; This will not work if a word produces a double underflow.
;
; Return stack
; ------------
; The return stack is used to push return addresses when nesting word
; executions. It is also used to manage control flow structures. Since this
; space is used a lot by complex programs, it is expected that limiting its
; size might prevent large programs from executing. Its size is only limited
; by the growth of the data space. It is initialized just below the maximum
; span of the parameter stack (7C00h), and it can grows down until the return stack
; pointer hits HERE.
;
; To push to the return stack, we store at 0,y then dey dey (post-decrement)
; To pop from return stack, we increment Y by 2 with ldab #2 aby then load at 0,y (pre-increment)
; Top of stack can be peeked by index addressing using an offset of 2.
; For the moment underflow and overflows are not detected.
;
; Interpreter
; -----------
; The intepreter receives chars from the input device until an end of line is
; reached. The compiler is then executed, which translates input to an unnamed
; word. The unnamed word is then executed.
;
; Compilation
; -----------
; Each word is recognized and replaced by the address of its code pointer.
; Note this is not the address of the definition but the code pointer itself.
; The last code pointer entry of a compiled word is a RETURN, which pops an
; address from the return stack and use it as new instruction pointer.
;
; The compiler uses all the CPU registers, so the previous contents of the
; forth registers is saved before compilation and restored after.
                    #ExtraOn
                    #CaseOn
                    #SEG0     $E000
                    #ROM

; System definitions
INIT                equ       0x3D
; Serial definitions
BAUD                equ       0x2B
SCCR1               equ       0x2C
SCCR2               equ       0x2D
SCSR                equ       0x2E
SCDR                equ       0x2F
SCSR_RDRF           equ       0x20                ; Receive buffer full
SCSR_TDRE           equ       0x80                ; Transmit buffer empty

; Forth memory map
SP_ZERO             equ       0x8000-5            ; End of RAM (address of first free byte)
RP_ZERO             equ       0x7C00-2            ; 1K before param stack (address of first free word)
HERE_ZERO           equ       0x0100              ; Start of user data space

; Forth config
TIB_LEN             equ       80
USE_RTS             equ       1                   ; slightly smaller code using RTS to opcode jmp
USE_MUL             equ       1                   ; use HC11 multiplier instead of eforth UM* routine
USE_DIV             equ       1                   ; use HC11 divider instead of eforth UM/MOD routine

USE_SPI             equ       1                   ; Enable words for SPI master transactions (specific to sys11)
USE_BLOCK           equ       0                   ; Enable words for SPI master transactions (specific to sys11)

; Word flags - added to length, so word length is encoded on 6 bits
WORD_IMMEDIATE      equ       0x80
WORD_COMPILEONLY    equ       0x40
WORD_LENMASK        equ       0x3F

; Define variables in internal HC11 RAM
;*******************************************************************************
                    #RAM                          ;.data
;*******************************************************************************

IP                  dw        0                   ; Instruction pointer
HEREP               dw        0                   ; Pointer to HERE, the address of next free byte in dic/data space
LASTP               dw        0                   ; Pointer to the last defined word entry
TXVEC               dw        0                   ; Pointer to the word implementing EMIT
RXVEC               dw        0                   ; Pointer to the word implementing ?KEY
BASEP               dw        0                   ; Value of the base used for number parsing
HOLDP               dw        0                   ; Pointer used for numeric output
STATP               dw        0                   ; Pointer to word implementing the current behaviour: compile/interpret
HANDP               dw        0                   ; Exception handler pointer
CURDP               dw        0                   ; Pointer to the word currently being defined. Stored in word :
LSTCRP              dw        0                   ; Pointer to the last created action word (used by DOES>)
CSPP                dw        0                   ; Storage for stack pointer value used for checks

; Input text buffering

pTEMP               rmb       6                   ; Temp variable used in LPARSE, UM* and UM/MOD
TOINP               dw        0                   ; Parse position in the input buffer
NTIBP               dw        0                   ; Number of received characters in current line
STIBP               dw        0                   ; Size of the tib
TIBP                dw        0                   ; Address of the actual input buffer, to allow switching to other buffers
TIBBUF              rmb       TIB_LEN             ; Space for Default Input buffer

; ===========================================================================
; Structure of a compiled word: We have a suite of code pointers. Each code pointer has to be executed in turn.
; The last code pointer of the list has to be "exit", which returns to the caller, or a loop.
; +---+------------+------+------+------+-----+------+
; |HDR| code_ENTER | PTR1 | PTR2 | PTR3 | ... | EXIT |
; +---+------------+------+------+------+-----+------+
;                     ^      ^
;                     IP    nxtIP=IP+2
; IP ONLY POINTS AT WORD ADDRESSES! Never to asm code addresses. Thats why
; words implemented in assembly are only made of a code pointer.

                    #ROM
; ===========================================================================
; Startup code
; ===========================================================================

_start

; Map registers in zero page
                    clra
                    staa      INIT+0x1000

; Setup the runtime environment
                    lds       #SP_ZERO            ; Parameter stack at end of RAM. HC11 pushes byte per byte.
                    ldy       #RP_ZERO            ; Return stack 1K before end of RAM. We push word per word.
                    ldx       #BOOT               ; load pointer to startup code, skipping the native ENTER pointer!
                    bra       NEXT2               ; Start execution

; ===========================================================================
; Core routines for word execution
; ===========================================================================

; ---------------------------------------------------------------------------
; Execute the next word. IP is incremented, stored back, and the cell pointed
; by IP is loaded. It contains a code address, which is jumped at.
; This is not a forth word.

                    #ROM

PUSHD               pshd                          ; We can use this instead of next to push a result before ending a word
NEXT                ldx       IP                  ; Get the instruction pointer
NEXT2                                             ; We can call here if X already has the value for IP
                    inx:2                         ; Increment IP to look at next word
                    stx       IP                  ; Save IP for next execution round
                    dex:2                         ; Redecrement, because we need the original IP
                                                  ; Now X contains pointer to pointer to code (aka IP, pointer to forth opcode)
                    ldx       ,x                  ; Now X contains pointer to code (forth opcode == address of first cell in any word)
doEXECUTE
                    ldd       ,x                  ; Now D contains the code address to execute opcode (a code_THING value)
                    #ifdef    USE_RTS
                    pshd
                    rts                           ; X contains new IP, D contains code_ address, pushed on stack.

                    #else
                    xgdx
                    jmp       0,x                 ; D contains new IP, X contains code_ address

                    #endif

; ---------------------------------------------------------------------------
; Starts execution of a compiled word. The current IP is pushed on the return stack, then we jump
; This function is always called by the execution of NEXT.
code_ENTER
; This is called with the address of instruction being run (aka forth opcode) in D
                    #ifdef    USE_RTS
; Preserve X that contains new IP
                    ldd       IP
                    std       0,y                 ; Push the next IP to be executed after return from this thread
                    #else
; Preserve D that contains new IP
                    ldx       IP
                    stx       0,y                 ; Push the next IP to be executed after return from this thread
                    #endif
                    dey                           ; Post-Decrement Y by 2 to push
                    dey
                    #ifndef   USE_RTS
; IP was in D, transfer in X
                    xgdx
                    #endif
                    inx                           ; Increment, now X is the address of the first word in the list
                    inx
                    bra       NEXT2               ; Manage next opcode address

; ---------------------------------------------------------------------------
; CORE 6.1.1380 EXIT ( -- ) End the execution of a word. The previous IP is on the return stack, so we pull it.
                    #DATA
word_RETURN
                    dw        0
                    fcb       4
                    fcs       "EXIT"
RETURN
                    dw        code_RETURN
                    #ROM
code_RETURN
                    ldab      #2
                    aby                           ; Pre-Increment Y by 2 to pop
                    ldx       0,y                 ; Pop previous value for IP from top of return stack
                    bra       NEXT2

; ===========================================================================
; Internal words
; These words have no header because they cannot be executed by the user.
; However, they are used to implement compiled routines.

; ===========================================================================

; ---------------------------------------------------------------------------
; Do litteral: Next cell in thread is an immediate litteral value to be pushed.
                    #DATA
IMM
                    dw        code_IMM
                    #ROM
code_IMM
                    ldx       IP                  ; Load next word in D
                    ldd       0,x
                    inx                           ; Increment IP to look at next word
                    inx
                    stx       IP                  ; IP+1->IP Save IP for next execution
                    bra       PUSHD

; ---------------------------------------------------------------------------
; Load next word in IP
                    #DATA
BRANCH
                    dw        code_BRANCH
                    #ROM
code_BRANCH
                    ldx       IP                  ; Load next word in X
                    ldx       0,x
                    bra       NEXT2

; ---------------------------------------------------------------------------
; Pull a value. If zero, load next word in IP
                    #DATA
BRANCHZ
                    dw        code_BRANCHZ
                    #ROM
code_BRANCHZ
                    ldx       IP                  ; Load next word in D
                    ldd       0,x                 ; D contains branch destination
                    inx
                    inx
                    stx       IP                  ; Make IP look at next word after branch address

                    pulx                          ; Get flag
                    cpx       #0x0000             ; TODO make it more efficient - not certain if possible
                    beq       qbranch1
                    bra       NEXT                ; Not zero

qbranch1                                          ; Pulled value was zero, do the branch
                    xgdx                          ; store branch destination (D) in X, then execute at this point
                    bra       NEXT2

; ---------------------------------------------------------------------------
; Pull a value from R stack. If zero, skip, else decrement and jump to inline target
                    #DATA
JNZD
                    dw        code_JNZD
                    #ROM
code_JNZD
                    ldd       2,y                 ; get counter on return stack
                    beq       .Lbranch            ; index is zero -> loop is complete
                    subd      #1                  ; no, bump the counter
                    std       2,y                 ; and replace on stack
                    bra       code_BRANCH         ; branch to target using existing code that load target from next word

.Lbranch
                    ldab      #2                  ; Remove counter from return stack. code efficiency similar to aby,aby
                    aby
                    ldx       IP                  ; get the IP
                    inx
                    inx                           ; and get addr past branch target
                    bra       NEXT2               ; and go do next word

; ===========================================================================
; Native words
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY ( -- ) init HC11 SCI
; The first time we use the dic section, make sure this section is loaded
                    #SEG0                         ;"a",@progbits

word_IOINIT         dw        word_RETURN
                    fcb       6
                    fcs       "IOINIT"
IOINIT              dw        code_IOINIT

                    #ROM

code_IOINIT         ldaa      #0x30               ; 9600 bauds at 8 MHz
                    staa      BAUD
                    ldaa      #0x0C
                    staa      SCCR2
                    bra       NEXT

; ---------------------------------------------------------------------------
; PROPRIETARY ( txbyte -- ) - Transmit byte on HC11 SCI
                    #SEG0
word_IOTX
                    dw        word_IOINIT
                    fcb       5
                    fcs       "IOTX!"
IOTX
                    dw        code_IOTX
                    #ROM
code_IOTX
                    pula
                    pulb
.Ltx
                    brclr     SCSR,#SCSR_TDRE,.Ltx
                    stab      SCDR
                    bra       NEXT

; ---------------------------------------------------------------------------
; PROPRIETARY ( -- rxbyte TRUE | FALSE ) - Receive byte from HC11 SCI
                    #SEG0
word_IORX
                    dw        word_IOTX
                    fcb       5
                    fcs       "?IORX"
IORX
                    dw        code_IORX
                    #ROM
code_IORX
                    clra
                    brclr     SCSR,#SCSR_RDRF,norx  ; RX buffer not full: skip to return FALSE
                    ldab      SCDR
                    pshd
; Push the TRUE flag
                    coma                          ; Turn 00 into FF in just one byte!
                    tab                           ; copy FF from A to B, now we have FFFF in D, which is TRUE
                    jmp       PUSHD

norx
                    clrb                          ; Finish FALSE cell
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.1370 (... ca -- ...) Execute the word whose address is on the stack
                    #SEG0
word_EXECUTE
                    dw        word_IORX
                    fcb       7
                    fcs       "EXECUTE"
EXECUTE
                    dw        code_EXECUTE

                    #ROM
code_EXECUTE
                    pulx                          ; Retrieve a word address from stack. This address contains a code pointer
                    bra       doEXECUTE

; ---------------------------------------------------------------------------
; CORE 6.1.0010 (d a -- ) Store a cell at address
                    #SEG0
word_STORE
                    dw        word_EXECUTE
                    fcb       1
                    fcs       "!"
STORE
                    dw        code_STORE

                    #ROM
code_STORE
                    pulx                          ; TOS contains address
                    pula                          ; PREV contains data
                    pulb
                    std       0,x
                    jmp       NEXT

; ---------------------------------------------------------------------------
; CORE 6.1.0850 (c a -- ) Store a char at address
                    #SEG0
word_CSTORE
                    dw        word_STORE
                    fcb       2
                    fcs       "C!"
CSTORE
                    dw        code_CSTORE

                    #ROM
code_CSTORE
                    pulx                          ; TOS contains address
                    pula                          ; PREV contains data, A = MSB, discarded
                    pulb                          ; B = LSB
                    stab      0,x
                    jmp       NEXT

; ---------------------------------------------------------------------------
; CORE 6.1.0650 (a -- d) Load a cell at given address
                    #SEG0
word_LOAD
                    dw        word_CSTORE
                    fcb       1
                    fcs       "@"
LOAD
                    dw        code_LOAD

                    #ROM
code_LOAD
                    pulx
                    ldd       0,x
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.0870 (a -- d) Load a cell at given address
                    #SEG0
word_CLOAD
                    dw        word_LOAD
                    fcb       2
                    fcs       "C@"
CLOAD
                    dw        code_CLOAD

                    #ROM
code_CLOAD
                    pulx
                    clra
                    ldab      0,x
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.2060 R> ( -- x ) ( R: x -- )
                    #SEG0
word_RFROM
                    dw        word_CLOAD
                    fcb       2                   ; + WORD_COMPILEONLY
                    fcs       "R>"
RFROM
                    dw        code_RFROM

                    #ROM
code_RFROM
                    ldab      #2                  ; Preinc Y to pull from Return Stack
                    aby
                    ldd       0,y
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.0580 >R ( -- x ) ( R: x -- )
                    #SEG0
word_TOR
                    dw        word_RFROM
                    fcb       2                   ; + WORD_COMPILEONLY
                    fcs       ">R"
TOR
                    dw        code_TOR

                    #ROM
code_TOR
                    pula
                    pulb
                    std       0,y
                    dey                           ; Postdec Y to push on Return Stack
                    dey
                    jmp       NEXT

; ---------------------------------------------------------------------------
; CORE 6.1.2070 R@ ( -- x ) ( R: x -- x )
                    #SEG0
word_RLOAD
                    dw        word_TOR
                    fcb       2
                    fcs       "R@"
RLOAD
                    dw        code_RLOAD

                    #ROM
code_RLOAD
                    ldd       2,y
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; PROPRIETARY
                    #SEG0
word_SPLOAD
                    dw        word_RLOAD
                    fcb       3
                    fcs       "SP@"
SPLOAD
                    dw        code_SPLOAD
                    #ROM
code_SPLOAD
                    tsx
                    dex
                    pshx
                    jmp       NEXT

; ---------------------------------------------------------------------------
                    #SEG0
word_SPSTORE
                    dw        word_SPLOAD
                    fcb       3
                    fcs       "SP!"
SPSTORE
                    dw        code_SPSTORE
                    #ROM
code_SPSTORE
                    pulx
                    inx
                    txs
                    jmp       NEXT

; ---------------------------------------------------------------------------
                    #SEG0
word_RPLOAD
                    dw        word_SPSTORE
                    fcb       3
                    fcs       "RP@"
RPLOAD
                    dw        code_RPLOAD
                    #ROM
code_RPLOAD
                    pshy
                    jmp       NEXT

; ---------------------------------------------------------------------------
                    #SEG0
word_RPSTORE
                    dw        word_RPLOAD
                    fcb       3                   ; + WORD_COMPILEONLY
                    fcs       "RP!"
RPSTORE
                    dw        code_RPSTORE
                    #ROM
code_RPSTORE
                    puly
                    jmp       NEXT

; ---------------------------------------------------------------------------
; CORE 6.1.1290 DUP ( u -- u u )
                    #SEG0
word_DUP
                    dw        word_RPSTORE
                    fcb       3
                    fcs       "DUP"
DUP
                    dw        code_DUP

                    #ROM
code_DUP
                    tsx                           ; Get stack pointer +1 in X
                    ldd       0,x                 ; Load top of stack in D
                    jmp       PUSHD               ; This will push top of stack again

; ---------------------------------------------------------------------------
; CORE 6.1.1990 OVER ( u1 u2 -- u1 u2 u1 )
                    #SEG0
word_OVER
                    dw        word_DUP
                    fcb       4
                    fcs       "OVER"
OVER
                    dw        code_OVER

                    #ROM
code_OVER
                    tsx                           ; Get stack pointer +1 in X
                    ldd       2,x                 ; Load value before top of stack
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.2260 SWAP ( u v -- v u )
                    #SEG0
word_SWAP
                    dw        word_OVER
                    fcb       4
                    fcs       "SWAP"
SWAP
                    dw        code_SWAP

                    #ROM
code_SWAP
                    pulx
                    pula
                    pulb
                    pshx
                    jmp       PUSHD               ; This will push top of stack again

; ---------------------------------------------------------------------------
; CORE 6.1.1260 DROP ( u -- )
                    #SEG0
word_DROP
                    dw        word_SWAP
                    fcb       4
                    fcs       "DROP"
DROP
                    dw        code_DROP

                    #ROM
code_DROP
                    pulx                          ; Get a parameter and discard it
                    jmp       NEXT                ; This will push top of stack again

; ===========================================================================
; Math
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY UM+ ( u v -- u+v cy ) - Add two cells, return sum and carry flag
                    #SEG0
word_UPLUS
                    dw        word_DROP
                    fcb       3
                    fcs       "UM+"
UPLUS
                    dw        code_UPLUS

                    #ROM
code_UPLUS
                    pula
                    pulb                          ; pull TOS
                    tsx
                    addd      0,x                 ; add to new TOS, sets N,Z,C,V
                    std       0,x                 ; Replace TOS, does not affect carry, clears V, changes N and Z
                    ldab      #0                  ; CLRB clears carry, LDAB leaves it.
                    rolb                          ; Get carry flag in B0 (D.LSB)
                    clra                          ; Clear A (D.MSB)
                    jmp       PUSHD               ; Push second return item and do next word

; ---------------------------------------------------------------------------
; CORE 6.1.0120 + ( u v -- u+v ) - Add two cells
; We do a native version for speed
                    #SEG0
word_PLUS
                    dw        word_UPLUS
                    fcb       1
                    fcs       "+"
PLUS
                    dw        code_PLUS

                    #ROM
code_PLUS
                    pula
                    pulb
                    tsx
                    addd      0,x
                    std       0,x
                    jmp       NEXT

; ---------------------------------------------------------------------------
; CORE 6.1.2490 XOR ( u v -- u^v )
                    #SEG0
word_XOR
                    dw        word_PLUS
                    fcb       3
                    fcs       "XOR"
XOR
                    dw        code_XOR

                    #ROM
code_XOR
                    pula
                    pulb
                    tsx
                    eora      0,x
                    eorb      1,x
                    pulx
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.0720 AND ( u v -- u&v )
                    #SEG0
word_AND
                    dw        word_XOR
                    fcb       3
                    fcs       "AND"
AND
                    dw        code_AND

                    #ROM
code_AND
                    pula
                    pulb
                    tsx
                    anda      0,x
                    andb      1,x
                    pulx
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.1980 OR ( u v -- u|v )
                    #SEG0
word_OR
                    dw        word_AND
                    fcb       2
                    fcs       "OR"
OR
                    dw        code_OR

                    #ROM
code_OR
                    pula
                    pulb
                    tsx
                    oraa      0,x
                    orab      1,x
                    pulx
                    jmp       PUSHD

; ---------------------------------------------------------------------------
; CORE 6.1.0250 0< ( u -- u<0 ) push true if pull negative
                    #SEG0
word_ZLESS
                    dw        word_OR
                    fcb       2
                    fcs       "0<"
ZLESS
                    dw        code_ZLESS

                    #ROM
code_ZLESS
                    pula
                    pulb
                    tsta                          ; check high bit of MSB
                    bmi       .Ltrue              ; branch if negative
                    ldd       #0x0000
                    jmp       PUSHD

.Ltrue
                    ldd       #0xFFFF
                    jmp       PUSHD

; ===========================================================================
; Other forth words implemented in forth.
; These words are pre-compiled lists, they are all executed by code_ENTER.
; The following words can only be pointers to cells containing references to
; other words. Direct pointers to cells containing code addresses are not
; possible.

; ===========================================================================

; ===========================================================================
; Basic ops
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY ( wordptr -- ) Execute the forth word whose address is stored in the passed pointer
                    #SEG0
word_LOADEXEC
                    dw        word_ZLESS
                    fcb       5
                    fcs       "@EXEC"
LOADEXEC
                    dw        code_ENTER          ; ptr
                    dw        LOAD                ; word
                    dw        DUP                 ; word word
                    dw        BRANCHZ,noexec      ; word, exit if null
                    dw        EXECUTE             ; Execute the loaded forth word.
noexec
                    dw        RETURN              ; Nothing is stored. just return.

; ---------------------------------------------------------------------------
; CORE 6.1.1320 EMIT ( c -- ) - Write char on output device
                    #SEG0
word_EMIT
                    dw        word_LOADEXEC
                    fcb       4
                    fcs       "EMIT"
EMIT
                    dw        code_ENTER
                    dw        IMM,TXVEC
                    dw        LOADEXEC
                    dw        RETURN

; ---------------------------------------------------------------------------
; FACILITY 10.6.1.1755 ?KEY ( -- c t | f ) - return input character and true, or a false if no input.
                    #SEG0
word_QKEY
                    dw        word_EMIT
                    fcb       4
                    fcs       "?KEY"
QKEY
                    dw        code_ENTER
                    dw        IMM,RXVEC
                    dw        LOADEXEC
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1750 KEY ( -- c ) - wait for a character on input device and return it
                    #SEG0
word_KEY
                    dw        word_QKEY
                    fcb       3
                    fcs       "KEY"
KEY
                    dw        code_ENTER
key1
                    dw        QKEY
                    dw        BRANCHZ,key1
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY nuf? ( -- t ) - return false if no input, else pause and if cr return true.
                    #SEG0
word_NUFQ
                    dw        word_KEY
                    fcb       4
                    fcs       "NUF?"
NUFQ
                    dw        code_ENTER
                    dw        QKEY
                    dw        DUP
                    dw        BRANCHZ,nufq1
                    dw        DDROP
                    dw        KEY
                    dw        IMM,0x0D
                    dw        EQUAL
nufq1
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY SP0 ( -- a) - initial value of parameter stack pointer
                    #SEG0
word_SPZERO
                    dw        word_NUFQ
                    fcb       3
                    fcs       "SP0"
SPZERO
                    dw        code_ENTER
                    dw        IMM,SP_ZERO
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY SP0 ( -- a) - initial value of return stack pointer
                    #SEG0
word_RPZERO
                    dw        word_SPZERO
                    fcb       3
                    fcs       "RP0"
RPZERO
                    dw        code_ENTER
                    dw        IMM,RP_ZERO
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1200 DEPTH ( -- n )  return the depth of the data stack.
                    #SEG0
word_DEPTH
                    dw        word_RPZERO
                    fcb       5
                    fcs       "DEPTH"
DEPTH
                    dw        code_ENTER
                    dw        SPLOAD,SPZERO,SWAP,SUB
                    dw        IMM,2,SLASH
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2030 PICK ( ... +n -- ... w ) - copy the nth stack item to tos.
                    #SEG0
word_PICK
                    dw        word_DEPTH
                    fcb       4
                    fcs       "PICK"
PICK
                    dw        code_ENTER
                    dw        INC,CELLS
                    dw        INC
                    dw        SPLOAD,PLUS,LOAD
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0380 2DUP ( u1 u2 -- u1 u2 u1 u2 )
                    #SEG0
word_DDUP
                    dw        word_PICK
                    fcb       4
                    fcs       "2DUP"
DDUP
                    dw        code_ENTER
                    dw        OVER
                    dw        OVER
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0370 2DROP ( x x -- )
                    #SEG0
word_DDROP
                    dw        word_DDUP
                    fcb       5
                    fcs       "2DROP"
DDROP
                    dw        code_ENTER
                    dw        DROP
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0630 ?DUP ( u -- u u if u NOT zero )
                    #SEG0
word_DUPNZ
                    dw        word_DDROP
                    fcb       4
                    fcs       "?DUP"
DUPNZ
                    dw        code_ENTER
                    dw        DUP                 ; Dup first, to allow testing
                    dw        BRANCHZ,DUPNZ2      ; if zero, no dup happens (this consumes the first dupe)
                    dw        DUP                 ; Not zero: Dup the value and leave on stack
DUPNZ2
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2160 ROT ( a b c -- b c a )
                    #SEG0
word_ROT
                    dw        word_DUPNZ
                    fcb       3
                    fcs       "ROT"
ROT
                    dw        code_ENTER
                    dw        TOR
                    dw        SWAP
                    dw        RFROM
                    dw        SWAP
                    dw        RETURN

; ===========================================================================
; Math and logical
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.1720 INVERT ( n -- n ) - invert all bits
                    #SEG0
word_NOT
                    dw        word_ROT
                    fcb       6
                    fcs       "INVERT"
NOT
                    dw        code_ENTER
                    dw        IMM,0xFFFF
                    dw        XOR
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1910 NEGATE ( n -- -n ) - Twos complement
                    #SEG0
word_NEGATE
                    dw        word_NOT
                    fcb       6
                    fcs       "NEGATE"
NEGATE
                    dw        code_ENTER
                    dw        NOT
                    dw        IMM,1
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; DOUBLE 8.6.1.1230 DNEGATE ( d -- -d ) - Twos complement
                    #SEG0
word_DNEGATE
                    dw        word_NEGATE
                    fcb       7
                    fcs       "DNEGATE"
DNEGATE
                    dw        code_ENTER
                    dw        NOT,TOR,NOT
                    dw        IMM,1,UPLUS
                    dw        RFROM,PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0160 - ( a b -- a-b ) - could be assembly to improve performance a bit
                    #SEG0
word_SUB
                    dw        word_DNEGATE
                    fcb       1
                    fcs       "-"
SUB
                    dw        code_ENTER
                    dw        NEGATE
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0690 ABS ( n -- |n| ) - absolute value
                    #SEG0
word_ABS
                    dw        word_SUB
                    fcb       3
                    fcs       "ABS"
ABS
                    dw        code_ENTER
                    dw        DUP
                    dw        ZLESS
                    dw        BRANCHZ,abspos
                    dw        NEGATE
abspos
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2360 UM* ( u v -- uvlo uvhi ) - long 16x32 multiplication
                    #SEG0
word_UMSTAR
                    dw        word_ABS
                    fcb       3
                    fcs       "UM*"
UMSTAR
          #ifdef    USE_MUL
                    dw        code_UMSTAR
                    #ROM
code_UMSTAR
; Multiplication input: 2 16-bit cells on TOS
                    tsx
; At this point the operands are at 0,x and 2,x
; These can be accessed bytewise in A and B to be used immediately in MUL.
; The result for MUL is in D. Accumulations are needed, we use a 4 byte pTEMP
; Data layout:
; @X +0 +1 +2 +3
; AH AL BH BL
; Mul algorithm:
; AH AL
; x      BH BL
; ------------
; AL BL
; AH BL
; AL BH
; AH BH
; ------------
; hi(AHBH) , lo(AHBH) + hi(ALBH) + hi(AHBL) , lo(ALBH) + lo(AHBL) + hi(ALBL) , L(ALBL)

; $1234 x $5678
; 12 34
; 56 78

; 34 x 78 = 0x00001860
; 34 x 56 = 0x00117800
; --------------------
; sum       0x00119060
; 12 x 78 = 0x00087000
; --------------------
; sum       0x001a0060
; 12 x 56 = 0x060c0000
; --------------------
; sum       0x06260060

; We will compute right to left.

; pre-clear the zone that will only be accessed by additions
                    clra
                    clrb
                    std       pTEMP

; low bytes
                    ldaa      1,x                 ; AL
                    ldab      3,x                 ; BL
                    mul                           ; ALBL in D
                    std       pTEMP+2

; first middle pair
                    ldaa      0,x                 ; AH
                    ldab      3,x                 ; BL
                    mul                           ; AHBL in D
                    addd      pTEMP+1
                    std       pTEMP+1
                    bcc       step3
; carry set -> propagate
                    inc       pTEMP
step3
; second middle pair
                    ldaa      1,x                 ; AL
                    ldab      2,x                 ; BH
                    mul                           ; ALBH in D
                    addd      pTEMP+1
                    std       pTEMP+1
                    bcc       step4
; carry set -> propagate
                    inc       pTEMP
step4
; high pair
                    ldaa      0,x                 ; AH
                    ldab      2,x                 ; BH
                    mul                           ; AHBH in D
                    addd      pTEMP
                    std       pTEMP
; done, store result as a dual cell value, high word pushed first.
; We just replace the two cells at TOS
                    ldd       pTEMP
                    std       0,x
                    ldd       pTEMP+2
                    std       2,x

                    jmp       NEXT

          #else
                    dw        code_ENTER
                    dw        IMM,0,SWAP,IMM,15,TOR
umst1               dw        DUP,UPLUS,TOR,TOR
                    dw        DUP,UPLUS,RFROM,PLUS,RFROM
                    dw        BRANCHZ,umst2
                    dw        TOR,OVER,UPLUS,RFROM,PLUS
umst2               dw        JNZD,umst1
                    dw        ROT,DROP
                    dw        RETURN
          #endif

; ---------------------------------------------------------------------------
; CORE 6.1.0090 * ( u v -- u*v ) - short 16x16 multiplication with result same size as operands. we just drop half of the result bits
                    #SEG0
word_STAR
                    dw        word_UMSTAR
                    fcb       1
                    fcs       "*"
STAR
                    dw        code_ENTER
                    dw        UMSTAR
                    dw        DROP                ; Forget the Most Significant word
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1810 M* ( n n -- d ) signed multiply. return double product.
word_MSTAR
                    dw        word_STAR
                    fcb       2
                    fcs       "M*"
MSTAR
                    dw        code_ENTER
                    dw        DDUP,XOR,ZLESS,TOR
                    dw        ABS,SWAP,ABS,UMSTAR
                    dw        RFROM
                    dw        BRANCHZ,msta1
                    dw        DNEGATE
msta1
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2370 UM/MOD ( ul0 uh0 un -- ur uq ) - 32/16 division and modulo
; TODO native implementation since hc11 has a divider
                    #SEG0
word_UMMOD
                    dw        word_MSTAR
                    fcb       6
                    fcs       "UM/MOD"
UMMOD
                    #ifdef    USE_DIV
; Native implementation of UM/MOD using HC11 divider.
; HC11 divides 16 by 16 to produce a 16-bit quotient and remainder (IDIV) or 16-bit left-shifted 16 by 16 (FDIV).
; We need to extend that to a 32/16 divide.

; Example in base 10: divide 19 by 8
; 19 | 8        To divide 19 by 8 we first divide 10 by 8
; 10 |---       This yields 1 and remains 2 (FDIV)
; 2 | 1        But we divided 10 instead of 19
; 9 |          So we add the last digit to the remainder. Total is 11.
; 11 |          This is larger than the base 10, there was a carry.
; 10 | 1        So we have to divide again. Using hig digit we divide 10 by 8 and get 1 remains 2. (FDIV)
; 2 |          But we divided 10 instead of 11.
; 3 |          So we add the last digit to the remainder (IDIV). This is 3.
; Now the remainder is smaller than the divisor, process is complete.

; With Forth cells, the process is the same but we divide in base 65536.
; The first steps of the division are done with FDIV which divides D<<65536/X
; The last step is a HC11 IDIV since we divide a single digit with a single digit.

; 0x12345678 = 0x5678 * 0x35E5 + 0x2520
; 305419896 =  22136 *  13797 +   9504

; Divide 0x12340000 by 0x5678 using FDIV, quotient is 0x35E4, remainder 0x2520

; Now add low digit: 0x5678 + 0x2520 = 0x7B98 without carry generated
; So the second FDIV step can be skipped.

; Final division
; 0x7B98 / 0x5678 = 0x5678 * 0x0001 + 0x2520

; Summary: Remainder is 0x2520, quotient is 0x35E4 + 1 = 0x35E5

; 1 - udh(D) FDIV u(X), result is Q0 (X), R(D)
; 2 - add R(D) with udl, result in D
; 3 - if no carry goto 6
; 4 - D FDIV u(X), result is Q1 (X), R(D)
; 5 - add R(D) with udl, result in D
; 6 - D FDIV u(X), result is Q2 (X), R(D)
; 7 - final remainder is D, quotient is Q0+Q1+Q2

; udh is used once, it is used to accumulate the quotient

; To improve code performance we dont load D and X from stack directly (would need expensive opcodes involving Y index)
; We just pull the operands into pTEMP and access them from here in direct addressing mode.

                    dw        code_UMMOD
                    #ROM
code_UMMOD
                    pulx
                    stx       pTEMP               ; n
                    pulx
                    stx       pTEMP+2             ; h0
                    pulx
                    stx       pTEMP+4             ; l0

                    ldd       pTEMP+2             ; load h0 word
                    ldx       pTEMP               ; load n

                    fdiv

                    addd      pTEMP+4             ; add udl to remainder
                    std       pTEMP+4             ; save in back for later, keeps C
                    stx       pTEMP+2             ; overwrite udh with quotient accumulator, keeps C
                    bcc       skipdiv2            ; no overflow so second fdiv is not required

                    ldd       #0x0001             ; load H1, which is always one here.
                    ldx       pTEMP               ; load divisor, D is still alive, contains remainder+udl

                    fdiv

                    addd      pTEMP+4             ; add remainder to L1
                    xgdx                          ; now D contains new quotient, X gets L1+R1
                    addd      pTEMP+2             ; acc quotient
                    std       pTEMP+2             ; store sum quotient
                    xgdx                          ; now D contains R1+L1, X is quotient sum

skipdiv2
                    ldx       pTEMP               ; Final divisor load
                    idiv                          ; D contains final remainder
                    xgdx                          ; Remainder in X, D contains quotient to accumulate
                    pshx                          ; We're done with the remainder.
                    addd      pTEMP+2             ; Acc the quotient
                    jmp       PUSHD               ; Use common code to push it

                    #else
                    dw        code_ENTER
                    dw        DDUP
                    dw        ULESS
                    dw        BRANCHZ,umm4
                    dw        NEGATE
                    dw        IMM,15
                    dw        TOR
umm1
                    dw        TOR
                    dw        DUP
                    dw        UPLUS
                    dw        TOR,TOR,DUP,UPLUS
                    dw        RFROM,PLUS,DUP
                    dw        RFROM,RLOAD,SWAP,TOR
                    dw        UPLUS,RFROM,OR
                    dw        BRANCHZ,umm2
                    dw        TOR,DROP,INC,RFROM
                    dw        BRANCH,umm3
umm2
                    dw        DROP
umm3
                    dw        RFROM
                    dw        JNZD,umm1
                    dw        DROP,SWAP,RETURN
umm4
                    dw        DROP,DDROP
                    dw        IMM,-1,DUP          ; overflow, return max
                    dw        RETURN
                    #endif

; ---------------------------------------------------------------------------
; CORE 6.1.1561 FM/MOD ( d n -- r q ) - signed floored divide of double by single. return mod and quotient.
                    #SEG0
word_FMSMOD
                    dw        word_UMMOD
                    fcb       6
                    fcs       "FM/MOD"
FMSMOD
                    dw        code_ENTER
                    dw        DUP,ZLESS,DUP,TOR
                    dw        BRANCHZ,mmod1
                    dw        NEGATE,TOR,DNEGATE,RFROM

mmod1
                    dw        TOR,DUP,ZLESS
                    dw        BRANCHZ,mmod2
                    dw        RLOAD,PLUS
mmod2
                    dw        RFROM,UMMOD,RFROM
                    dw        BRANCHZ,mmod3
                    dw        SWAP,NEGATE,SWAP
mmod3
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0240 /MOD ( n n -- r q ) signed divide. return mod and quotient.
                    #SEG0
word_SLMOD
                    dw        word_FMSMOD
                    fcb       4
                    fcs       "/MOD"
SLMOD
                    dw        code_ENTER
                    dw        OVER
                    dw        ZLESS
                    dw        SWAP
                    dw        FMSMOD
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1890 MOD ( n n -- r ) signed divide. return mod only.
                    #SEG0
word_MOD
                    dw        word_SLMOD
                    fcb       3
                    fcs       "MOD"
MOD
                    dw        code_ENTER
                    dw        SLMOD
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0230 / ( n n -- q ) signed divide. return quotient only.
                    #SEG0
word_SLASH
                    dw        word_MOD
                    fcb       1
                    fcs       "/"
SLASH
                    dw        code_ENTER
                    dw        SLMOD
                    dw        SWAP
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0880 CELL+ ( u -- u+2 )
                    #SEG0
word_CELLP
                    dw        word_SLASH
                    fcb       5
                    fcs       "CELL+"
CELLP
                    dw        code_ENTER
                    dw        IMM,2
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0897 CHAR+ ( u -- u+1 )
                    #SEG0
word_CHARP
                    dw        word_CELLP
                    fcb       5
                    fcs       "CHAR+"
CHARP
                    dw        code_ENTER
                    dw        IMM,1
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0290 1+ (n -- n)
; This is similar to CHARP. It would be more complex to define an alias
; mechanism than to duplicate the implementation

                    #SEG0
word_INC
                    dw        word_CHARP
                    fcb       2
                    fcs       "1+"
INC
                    dw        code_ENTER
                    dw        IMM,1
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0300 1- (n -- n)
                    #SEG0
word_DEC
                    dw        word_INC
                    fcb       2
                    fcs       "1-"
DEC
                    dw        code_ENTER
                    dw        IMM,-1
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0890 CELLS ( u -- u*2 ) - compute bytes required to store u cells
                    #SEG0
word_CELLS
                    dw        word_DEC
                    fcb       5
                    fcs       "CELLS"
CELLS
                    dw        code_ENTER
                    dw        DUP
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0130 +! (val adr -- ) add val to the contents of adr
                    #SEG0
word_PLUS_STORE
                    dw        word_CELLS
                    fcb       2
                    fcs       "+!"
PLUS_STORE
                    dw        code_ENTER
                    dw        SWAP                ; adr val
                    dw        OVER                ; adr val adr
                    dw        LOAD                ; adr val *adr
                    dw        PLUS                ; adr val+*adr
                    dw        SWAP                ; val+*adr adr
                    dw        STORE               ; empty
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2340 U< ( u v -- u<v ) unsigned compare of top two items.
                    #SEG0
word_ULESS
                    dw        word_PLUS_STORE
                    fcb       2
                    fcs       "U<"
ULESS
                    dw        code_ENTER
                    dw        DDUP                ; (u) (v) (u) (v)
                    dw        XOR                 ; (u) (v) (u^v)
                    dw        ZLESS               ; (u) (v) ((u^v)<0)
                    dw        BRANCHZ,ULESS1
                    dw        SWAP                ; (v) (u)
                    dw        DROP                ; (v)
                    dw        ZLESS               ; (v<0)
                    dw        RETURN
ULESS1
                    dw        SUB                 ; (u-v)
                    dw        ZLESS               ; ((u-v)<0)
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0480 < ( n1 n2 -- t ) - signed compare of top two items.
                    #SEG0
word_LESS
                    dw        word_ULESS
                    fcb       1
                    fcs       "<"
LESS
                    dw        code_ENTER
                    dw        DDUP
                    dw        XOR
                    dw        ZLESS
                    dw        BRANCHZ,less1
                    dw        DROP
                    dw        ZLESS
                    dw        RETURN
less1
                    dw        SUB
                    dw        ZLESS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1870 MAX ( n n -- n ) - return the greater of two top stack items.
word_MAX
                    dw        word_LESS
                    fcb       3
                    fcs       "MAX"
MAX
                    dw        code_ENTER
                    dw        DDUP
                    dw        LESS
                    dw        BRANCHZ,max1
                    dw        SWAP
max1
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1880 MIN ( n n -- n ) - return the smaller of two top stack items.
word_MIN
                    dw        word_MAX
                    fcb       3
                    fcs       "MIN"
MIN
                    dw        code_ENTER
                    dw        DDUP
                    dw        SWAP
                    dw        LESS
                    dw        BRANCHZ,min1
                    dw        SWAP
min1
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2440 WITHIN ( u ul uh -- ul <= u < uh )
                    #SEG0
word_WITHIN
                    dw        word_MIN
                    fcb       6
                    fcs       "WITHIN"
WITHIN
                    dw        code_ENTER
                    dw        OVER                ; u ul uh ul
                    dw        SUB                 ; u ul (uh-ul)
                    dw        TOR                 ; u ul R: (uh-ul)
                    dw        SUB                 ; (u-ul) R: (uh-ul)
                    dw        RFROM               ; (u-ul) (uh-ul)
                    dw        ULESS               ; ((u-ul) < (uh-ul))
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0530 = ( w w -- t ) equality flag FFFF if both value are the same (xor would return zero for equality)
word_EQUAL
                    dw        word_WITHIN
                    fcb       1
                    fcs       "="
EQUAL
                    dw        code_ENTER
                    dw        XOR
                    dw        BRANCHZ,equtrue
                    dw        IMM,0
                    dw        RETURN
equtrue
                    dw        IMM,0xFFFF          ; True is -1 !
                    dw        RETURN

; ===========================================================================
; Strings
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.0980 COUNT ( cstradr -- bufadr len ) Return the buf addr and len of a pointed counted string
                    #SEG0
word_COUNT
                    dw        word_EQUAL
                    fcb       5
                    fcs       "COUNT"
COUNT
                    dw        code_ENTER
                    dw        DUP                 ; cstradr cstradr
                    dw        CHARP               ; cstradr cstradr+1
                    dw        SWAP                ; bufadr cstradr
                    dw        CLOAD               ; bufadr len
                    dw        RETURN

; ---------------------------------------------------------------------------
; STRING 17.6.1.0910 CMOVE (src dest count --) - memcpy
                    #SEG0
word_CMOVE
                    dw        word_COUNT
                    fcb       5
                    fcs       "CMOVE"
CMOVE
                    dw        code_ENTER
                    dw        TOR                 ; src dest | R:count
                    dw        BRANCH,cmov2
cmov1
                    dw        TOR                 ; src | R:count dest
                    dw        DUP                 ; src src | R: count dest
                    dw        CLOAD               ; src data | R: count dest
                    dw        RLOAD               ; src data dest | R: count dest
                    dw        CSTORE              ; src | R: count dest
                    dw        CHARP               ; src+1 | R: count dest
                    dw        RFROM               ; src+1 dest | R: count
                    dw        CHARP               ; src+1->src dest+1->dest | R: count
cmov2
                    dw        JNZD,cmov1          ; src dest | if count>0, count--, goto cmov1
                    dw        DDROP               ; -- R: --
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY (buf len dest -- dest) Create a counted string in dest from len chars at buf
                    #SEG0
word_PACKS
                    dw        word_CMOVE
                    fcb       5
                    fcs       "PACK$"
PACKS
                    dw        code_ENTER
; Save count
                    dw        DUP                 ; buf len dest dest
                    dw        TOR                 ; buf len dest | R: dest
                    dw        DDUP                ; buf len dest len dest | R: dest
                    dw        CSTORE              ; buf len dest | R: dest
; Copy string after count
                    dw        CHARP               ; buf len (dest+1) | R:dest
                    dw        SWAP                ; buf (dest+1) len | R:dest
                    dw        CMOVE               ; R:dest
                    dw        RFROM               ; dest
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY csame (ptra ptrb len -- flag ) - compare strings on len bytes
                    #SEG0
word_CSAME
                    dw        word_PACKS
                    fcb       6
                    fcs       "CSAME?"
CSAME
                    dw        code_ENTER
                    dw        TOR                 ; ptra ptrb | R:len
                    dw        BRANCH,csame2
csame0
                    dw        OVER                ; ptra ptrb ptra | R:len
                    dw        CLOAD               ; ptra ptrb chra | R:len
                    dw        OVER                ; ptra ptrb chra ptrb | R:len
                    dw        CLOAD               ; ptra ptrb chra chrb | R:len
                    dw        SUB                 ; ptra ptrb chrdiff | R:len
                    dw        DUP                 ; ptra ptrb chrdiff chrdiff | R:len
                    dw        BRANCHZ,csame1      ; ptra ptrb chrdiff | R:len
; chars are different, we're done
                    dw        RFROM               ; ptra ptrb chrdiff len
                    dw        DROP                ; ptra ptrb chrdiff
                    dw        TOR                 ; ptra ptrb | R: chrdiff
                    dw        DDROP               ; R: chrdiff
                    dw        RFROM               ; chrdiff
                    dw        RETURN

csame1
; both chars are similar. Increment pointers and loop - ptra ptrb chrdiff | R:len
                    dw        DROP                ; ptra ptrb | R:len
                    dw        CHARP               ; ptra+1 | R: len
                    dw        SWAP
                    dw        CHARP               ; ptra+1 ptrb+1 | R: len
                    dw        SWAP

csame2
                    dw        JNZD,csame0         ; ptra+1 ptrb+1 | R: len if not null, else --
; If we reached this point then both strings are same
                    dw        DDROP
                    dw        IMM,0
                    dw        RETURN

; ---------------------------------------------------------------------------
; internal_ccompare (cstra cstrb lena lenb -- flag ) - return 0 if match
; Internal comparison code for both CCOMPARE and NAMECOMPARE.
; TODO rewrite the full chain of words FINDONE,FIND,TOKEN,WORD to
; work with direct (buf,len) pairs instead of counted strings. The goal is to avoid
; the useless copy made by PACK$
                    #SEG0
internal_compare
                    dw        code_ENTER
; Compare lengths. not equal? not same strings.
                    dw        OVER                ; cstra cstrb lena lenb lena
                    dw        SUB                 ; cstra cstrb lena (lenb-lena)
                    dw        SWAP                ; cstra cstrb lendiff lena
                    dw        TOR                 ; cstra cstrb lendiff | R:lena
                    dw        BRANCHZ,ccoeq       ; cstra cstrb
; Different lengths
                    dw        DDROP
                    dw        RFROM               ; lena if not zero serves as difference marker
                    dw        RETURN
ccoeq
; Length match. Compare chars
                    dw        CHARP
                    dw        SWAP
                    dw        CHARP
                    dw        RFROM               ; bufb bufa len
                    dw        CSAME               ; result
                    dw        RETURN

; ---------------------------------------------------------------------------
; ccompare (cstr cstr -- flag ) - return 0 if match
; Generic version of string compare that does not mask the length bits
                    #SEG0
word_CCOMPARE
                    dw        word_CSAME
                    fcb       8
                    fcs       "CCOMPARE"
CCOMPARE
                    dw        code_ENTER
                    dw        OVER                ; cstra cstrb cstra
                    dw        CLOAD               ; cstra cstrb lena
                    dw        OVER                ; cstra cstrb lena cstrb
                    dw        CLOAD               ; cstra cstrb lena lenb
                    dw        internal_compare
                    dw        RETURN

; ---------------------------------------------------------------------------
; namecompare (cstr cstr -- flag ) - return 0 if match
; Specific version of string compare that masks the length bits so the IMM and COMP bits are discarded
                    #SEG0
word_NAMECOMPARE
                    dw        word_CCOMPARE
                    fcb       11
                    fcs       "NAMECOMPARE"
NAMECOMPARE
                    dw        code_ENTER
                    dw        OVER                ; cstra cstrb cstra
                    dw        CLOAD               ; cstra cstrb lena
                    dw        IMM,WORD_LENMASK
                    dw        AND
                    dw        OVER                ; cstra cstrb lena cstrb
                    dw        CLOAD               ; cstra cstrb lena lenb
                    dw        IMM,WORD_LENMASK
                    dw        AND
                    dw        internal_compare
                    dw        RETURN

; ---------------------------------------------------------------------------
; STRING 17.6.1.0935 COMPARE ( buf1 len1 buf2 len2 -- flag )
; compare strings up to the length of the shorter string. zero if match
; TODO

; ---------------------------------------------------------------------------
; INTERNAL DOSTR Common code from inline string extraction. MUST BE used by another word,
; since the string is loaded from the previous-previous entry
                    #SEG0
; NO NAME
DOSTR
                    dw        code_ENTER
                    dw        RFROM
                    dw        RLOAD
                    dw        RFROM
                    dw        COUNT
                    dw        PLUS
                    dw        TOR
                    dw        SWAP
                    dw        TOR
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL IMMSTR ( -- adr )
; This is a runtime-only routine. It is compiled by S" but not accessible otherwise.

                    #SEG0
IMMSTR
                    dw        code_ENTER
                    dw        DOSTR
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL SHOWSTR
; This is a runtime-only routine. It is compiled by ." but not accessible otherwise.

                    #SEG0
SHOWSTR
                    dw        code_ENTER
                    dw        DOSTR
                    dw        COUNT
                    dw        TYPE
                    dw        RETURN

; ===========================================================================
; Numeric output
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2000 PAD ( -- a ) - return the address of a temporary buffer.
; Note: this returns the END of a 80 byte buffer right after the current colon definition.
; The buffer is filled in reverse using a div/mod by base algorithm.
; No overflow because numeric output is never overlapping compilation. PAD is always used
; in the context defined by <# and #>
                    #SEG0
word_PAD
                    dw        word_NAMECOMPARE
                    fcb       3
                    fcs       "PAD"
PAD
                    dw        code_ENTER
                    dw        HERE
                    dw        IMM,80
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0490 <# ( -- )
                    #SEG0
word_BDIGS
                    dw        word_PAD
                    fcb       2
                    fcs       "<#"
BDIGS
                    dw        code_ENTER
                    dw        PAD
                    dw        IMM,HOLDP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0040 #> ( w -- b u ) - prepare the output string to be type'd.
                    #SEG0
word_EDIGS
                    dw        word_BDIGS
                    fcb       2
                    fcs       "#>"
EDIGS
                    dw        code_ENTER
                    dw        DROP
                    dw        IMM,HOLDP
                    dw        LOAD
                    dw        PAD
                    dw        OVER
                    dw        SUB
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1670 HOLD ( c -- ) - insert a character into the numeric output string. Storage is predecremented.
                    #SEG0
word_HOLD
                    dw        word_EDIGS
                    fcb       4
                    fcs       "HOLD"
HOLD
                    dw        code_ENTER
                    dw        IMM,HOLDP
                    dw        LOAD
                    dw        DEC
                    dw        DUP
                    dw        IMM,HOLDP
                    dw        STORE
                    dw        CSTORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY DIGIT ( u -- c ) - convert digit u to a character.
                    #SEG0
word_DIGIT
                    dw        word_HOLD
                    fcb       5
                    fcs       "DIGIT"
DIGIT
                    dw        code_ENTER
                    dw        IMM,9
                    dw        OVER
                    dw        LESS
                    dw        IMM,7
                    dw        AND
                    dw        PLUS
                    dw        IMM,'0'
                    dw        PLUS
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY EXTRACT ( n base -- n c ) - extract the least significant digit from n.
                    #SEG0
word_EXTRACT
                    dw        word_DIGIT
                    fcb       7
                    fcs       "EXTRACT"
EXTRACT
                    dw        code_ENTER
                    dw        IMM,0
                    dw        SWAP
                    dw        UMMOD
                    dw        SWAP
                    dw        DIGIT
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0030 # ( u -- u ) - extract one digit from u and append the digit to output string.
                    #SEG0
word_DIG
                    dw        word_EXTRACT
                    fcb       1
                    fcs       "#"
DIG
                    dw        code_ENTER
                    dw        BASE
                    dw        LOAD
                    dw        EXTRACT
                    dw        HOLD
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0050 #S ( u -- 0 ) - convert u until all digits are added to the output string.
                    #SEG0
word_DIGS
                    dw        word_DIG
                    fcb       2
                    fcs       "#S"
DIGS
                    dw        code_ENTER
digs1
                    dw        DIG
                    dw        DUP
                    dw        BRANCHZ,digs2
                    dw        BRANCH,digs1
digs2
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2210 SIGN ( n -- ) - add a minus sign to the numeric output string.
                    #SEG0
word_SIGN
                    dw        word_DIGS
                    fcb       4
                    fcs       "SIGN"
SIGN
                    dw        code_ENTER
                    dw        ZLESS
                    dw        BRANCHZ,sign1
                    dw        IMM,'-'
                    dw        HOLD
sign1
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY STR       ( n -- b u ) - convert a signed integer to a numeric string.
                    #SEG0
word_STR
                    dw        word_SIGN
                    fcb       3
                    fcs       "STR"
STR
                    dw        code_ENTER
                    dw        DUP                 ; n n
                    dw        TOR                 ; n | R: n
                    dw        ABS                 ; absn | R:n
                    dw        BDIGS               ; absn | R:n
                    dw        DIGS
                    dw        RFROM
                    dw        SIGN
                    dw        EDIGS
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2330 U.R ( u +n -- ) - display an unsigned integer in n column, right justified.
                    #SEG0
word_UDOTR
                    dw        word_STR
                    fcb       3
                    fcs       "U.R"
UDOTR
                    dw        code_ENTER
                    dw        TOR,BDIGS,DIGS,EDIGS
                    dw        RFROM,OVER,SUB
                    dw        SPACES,TYPE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.2.0210 .R ( n +n -- ) - display an integer in a field of n columns, right justified.
                    #SEG0
word_DOTR
                    dw        word_UDOTR
                    fcb       2
                    fcs       ".R"
DOTR
                    dw        code_ENTER
                    dw        TOR,STR,RFROM,OVER,SUB
                    dw        SPACES,TYPE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2320 U. ( u -- ) - display an unsigned integer in free format.
                    #SEG0
word_UDOT
                    dw        word_DOTR
                    fcb       2
                    fcs       "U."
UDOT
                    dw        code_ENTER
                    dw        SPACE
                    dw        BDIGS
                    dw        DIGS
                    dw        EDIGS
                    dw        TYPE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0180 . ( w -- ) display an integer in free format, preceeded by a space.
                    #SEG0
word_DOT
                    dw        word_UDOT
                    fcb       1
                    fcs       "."
DOT
                    dw        code_ENTER
                    dw        BASE
                    dw        LOAD
                    dw        IMM,10
                    dw        XOR
                    dw        BRANCHZ,dot1
; Not decimal: display unsigned
                    dw        UDOT
                    dw        RETURN
dot1
; Decimal: display signed
                    dw        SPACE
                    dw        STR
                    dw        TYPE
                    dw        RETURN

; ===========================================================================
; Numeric input
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.0750 BASE ( -- a ) - push address of current numeric base
                    #SEG0
word_BASE
                    dw        word_DOT
                    fcb       4
                    fcs       "BASE"
BASE
                    dw        code_ENTER
                    dw        IMM,BASEP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.1660 HEX ( -- )
                    #SEG0
word_HEX
                    dw        word_BASE
                    fcb       3
                    fcs       "HEX"
HEX
                    dw        code_ENTER
                    dw        IMM,16
                    dw        BASE
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1170 DECIMAL ( -- )
                    #SEG0
word_DECIMAL
                    dw        word_HEX
                    fcb       7
                    fcs       "DECIMAL"
DECIMAL
                    dw        code_ENTER
                    dw        IMM,10
                    dw        BASE
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL DIGIT? ( c base -- u t ) convert ascii to digit with success flag
                    #SEG0
word_DIGITQ
                    dw        word_DECIMAL
                    fcb       6
                    fcs       "DIGIT?"
DIGITQ
                    dw        code_ENTER
                    dw        TOR
                    dw        IMM,'0'
                    dw        SUB

                    dw        IMM,9
                    dw        OVER
                    dw        LESS

                    dw        BRANCHZ,dgtq1

                    dw        IMM,7
                    dw        SUB

                    dw        DUP
                    dw        IMM,10
                    dw        LESS
                    dw        OR

dgtq1
                    dw        DUP
                    dw        RFROM
                    dw        ULESS
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY NUMBER? ( cstr -- n t | a f ) - convert a number counted string to integer. push a flag on tos.
; F2012 instead requires CORE 6.1.0570 >NUMBER (ud1 c-addr u1 -- ud2 c-addr2 u2)
                    #SEG0
word_NUMBERQ
                    dw        word_DIGITQ
                    fcb       7
                    fcs       "NUMBER?"
NUMBERQ
                    dw        code_ENTER
                    dw        BASE                ; cstr &base
                    dw        LOAD                ; cstr base
                    dw        TOR                 ; cstr | R:base
                    dw        IMM,0               ; cstr 0 | R:base
                    dw        OVER                ; cstr 0 cstr | R:base
                    dw        COUNT               ; cstr 0 strptr strlen | R:base

                    dw        OVER                ; cstr 0 strptr strlen strptr | R:base
                    dw        CLOAD               ; cstr 0 strptr strlen strptr[0] | R:base
                    dw        IMM,'$'             ; cstr 0 strptr strlen strptr[0] $ | R:base
                    dw        EQUAL               ; cstr 0 strptr strlen (1 if strptr[0]==$) | R:base
                    dw        BRANCHZ,numq1       ; cstr 0 strptr strlen | R: base, jump if first buffer char is not $

; Equal returns 0 for FALSE (not equal). here we deal with $123 hex strings.
                    dw        HEX                 ; cstr 0 strptr strlen | R:base
                    dw        SWAP                ; cstr 0 strlen strptr | R:base
                    dw        CHARP               ; cstr 0 strlen strptr+1 | R:base
                    dw        SWAP                ; cstr 0 strptr+1 strlen | R:base
                    dw        DEC                 ; cstr 0 strptr+1 strlen-1 | R:base

numq1                                             ; Buffer doesnt start with a $ sign. Check for initial minus sign.
                    dw        OVER                ; cstr 0 strptr strlen strbuf | R: base
                    dw        CLOAD               ; cstr 0 strptr strlen strchar | R:base
                    dw        IMM,'-'             ; cstr 0 strptr strlen strchar '-' | R:base
                    dw        EQUAL               ; cstr 0 strptr strlen strchar=='-'
                    dw        TOR                 ; cstr 0 strptr strlen | R:base -1_if_negative

                    dw        SWAP                ; cstr 0 strlen strbuf | R:base -1_if_negative
                    dw        RLOAD               ; cstr 0 strlen strbuf -1_if_neg | R:base -1_if_negative
                    dw        SUB                 ; cstr 0 strlen strbuf+1 | R:base -1_if_negative
                    dw        SWAP                ; cstr 0 strbuf+1 strlen | R:base -1_if_negative
                    dw        RLOAD               ; cstr 0 strbuf+1 strlen -1_if_neg | R:base -1_if_negative
                    dw        PLUS                ; cstr 0 strbuf+1 strlen-1 | R:base -1_if_negative
                    dw        DUPNZ               ; cstr 0 strbuf+1 strlen-1 [strlen-1 if not zero] | R:base -1_if_negative
                    dw        BRANCHZ,numq6       ; jump to end if new len is zero

                    dw        DEC                 ; cstr 0 strptr strlen-1 (for JNZD) | R:base -1_if_negative
                    dw        TOR                 ; cstr 0 strptr | R:base -1_if_negative strlen-1

numq2
                    dw        DUP                 ; cstr 0 strptr strptr | R:base -1_if_negative strlen-1
                    dw        TOR                 ; cstr 0 strptr | R:base -1_if_negative strlen-1 strptr
                    dw        CLOAD               ; cstr 0 strchar | R:base -1_if_negative strlen-1 strptr
                    dw        BASE                ; cstr 0 strchar &base | R:base -1_if_negative strlen-1 strptr
                    dw        LOAD                ; cstr 0 strchar base | R:base -1_if_negative strlen-1 strptr
                    dw        DIGITQ              ; cstr 0 digit flag | R:base -1_if_negative strlen-1 strptr
                    dw        BRANCHZ,numq4       ; cstr 0 digit - if failed (false) goto numq4 | R:base -1_if_negative strlen-1 strptr

                    dw        SWAP
                    dw        BASE
                    dw        LOAD
                    dw        STAR
                    dw        PLUS
                    dw        RFROM
                    dw        INC
                    dw        JNZD,numq2

                    dw        RLOAD
                    dw        SWAP
                    dw        DROP
                    dw        BRANCHZ,numq3

                    dw        NEGATE

numq3
                    dw        SWAP
                    dw        BRANCH,numq5

numq4                                             ; invalid digit cstr 0 digit | R:base -1_if_negative strlen-1 strptr
                    dw        RFROM               ; cstr 0 digit strptr | R:base -1_if_negative strlen-1
                    dw        RFROM               ; cstr 0 digit strptr strlen-1 | R:base -1_if_negative
                    dw        DDROP               ; cstr 0 digit | R:base -1_if_negative
                    dw        DDROP               ; cstr | R:base -1_if_negative
                    dw        IMM,0               ; cstr 0 | R:base -1_if_negative
numq5
                    dw        DUP                 ; cstr 0 0 |R:base is_negative
numq6                                             ; Process String End cstr 0 strbuf | R:base is_negative
                    dw        RFROM               ; cstr 0 strbuf is_negative | R:base
                    dw        DDROP               ; cstr 0 | R:base
                    dw        RFROM               ; cstr 0 base
                    dw        BASE                ; cstr 0 base &BASE
                    dw        STORE               ; cstr 0
                    dw        RETURN

; ===========================================================================
; Memory management
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.1650 HERE ( -- a) Push the address of the next free byte
word_HERE
                    dw        word_NUMBERQ
                    fcb       4
                    fcs       "HERE"
HERE
                    dw        code_ENTER
                    dw        IMM,HEREP           ; (HEREP=&HERE)
                    dw        LOAD                ; (HERE)
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2395 UNUSED ( -- u )
; Return the amount of free bytes in the data space
word_UNUSED
                    dw        word_HERE
                    fcb       6
                    fcs       "UNUSED"
UNUSED
                    dw        code_ENTER
                    dw        RPLOAD
                    dw        HERE
                    dw        SUB
                    dw        RETURN

; ===========================================================================
; Terminal
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY BS ( -- 8 )
                    #SEG0
word_BS
                    dw        word_UNUSED
                    fcb       2
                    fcs       "BS"
BS
                    dw        code_ENTER
                    dw        IMM,8
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0770 BL ( -- 32 )
                    #SEG0
word_BL
                    dw        word_BS
                    fcb       2
                    fcs       "BL"
BL
                    dw        code_ENTER
                    dw        IMM,32
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2220 SPACE ( -- ) Emit a blank char
                    #SEG0
word_SPACE
                    dw        word_BL
                    fcb       5
                    fcs       "SPACE"
SPACE
                    dw        code_ENTER
                    dw        BL
                    dw        EMIT
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2230 SPACES ( n -- ) Emit blank chars
                    #SEG0
word_SPACES
                    dw        word_SPACE
                    fcb       6
                    fcs       "SPACES"
SPACES
                    dw        code_ENTER
                    dw        IMM,0
                    dw        MAX
                    dw        TOR
                    dw        BRANCH,spcend
spcloop
                    dw        SPACE
spcend
                    dw        JNZD,spcloop
spcdone
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0990 CR ( -- ) Emit a carriage return
                    #SEG0
word_CR
                    dw        word_SPACES
                    fcb       2
                    fcs       "CR"
CR
                    dw        code_ENTER
                    dw        IMM,13
                    dw        EMIT
                    dw        IMM,10
                    dw        EMIT
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY ( buf bufend ptr -- buf bufend ptr  )  if ptr == buf
; ( buf bufend ptr -- buf bufend ptr-1)  if ptr  > buf
; Do a backspace: if not a bufstart, remove char from buf, then back, space, back
                    #SEG0
word_BKSP
                    dw        word_CR
                    fcb       4
                    fcs       "BKSP"
BKSP
                    dw        code_ENTER
; check beginning of buffer
                    dw        TOR                 ; buf bufend R: ptr
                    dw        OVER                ; buf bufend buf R: ptr
                    dw        RFROM               ; buf bufend buf ptr
                    dw        SWAP                ; buf bufend ptr buf
                    dw        OVER                ; buf bufend ptr buf ptr
                    dw        XOR                 ; buf bufend ptr (buf == ptr)
                    dw        BRANCHZ,bksp1       ; buf bufend ptr

; Remove char from buf
                    dw        IMM,1               ; buf bufend ptr 1
                    dw        SUB                 ; buf bufend (ptr-1)

; Send chars to erase output
                    dw        BS,EMIT
                    dw        BL,EMIT             ; should replace emit by vectorable echo
                    dw        BS,EMIT
bksp1
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY TAP ( buf bufend ptr c -- buf bufend (ptr+1) ) accumulate character in buffer - no bounds checking
                    #SEG0
word_TAP
                    dw        word_BKSP
                    fcb       3
                    fcs       "TAP"
TAP
                    dw        code_ENTER
                    dw        DUP                 ; buf bufend ptr c c
                    dw        EMIT                ; buf bufend ptr c | shoud be vectored to allow disable echo
                    dw        OVER                ; buf bufend ptr c ptr
                    dw        CSTORE              ; buf bufend ptr
                    dw        CHARP               ; buf bufend (ptr+1)
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY TTAP (buf bufend ptr c -- buf bufend ptr)
                    #SEG0
word_TTAP
                    dw        word_TAP
                    fcb       4
                    fcs       "TTAP"
TTAP
                    dw        code_ENTER
                    dw        DUP                 ; buf bufend ptr c c
                    dw        IMM,13              ; buf bufend ptr c c 13
                    dw        XOR                 ; buf bufend ptr c (c==13)
                    dw        BRANCHZ,ktap2       ; buf bufend ptr c | manage end of buf
                    dw        BS                  ; buf bufend ptr c 8
                    dw        XOR                 ; buf bufend ptr (c==8)
                    dw        BRANCHZ,ktap1       ; buf bufend ptr | manage backspace
                    dw        BL                  ; buf bufend ptr 32 | replace other non-printable by spaces
                    dw        TAP                 ; buf bufend ptr
                    dw        RETURN
ktap1               dw        BKSP                ; buf bufend ptr
                    dw        RETURN
ktap2               dw        DROP                ; buf bufend ptr
                    dw        SWAP                ; buf ptr bufend
                    dw        DROP                ; buf ptr
                    dw        DUP                 ; buf ptr ptr
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0695 ACCEPT ( buf len -- count) Read up to len or EOL into buf.
; Returns char count
                    #SEG0
word_ACCEPT
                    dw        word_TTAP
                    fcb       6
                    fcs       "ACCEPT"
ACCEPT
                    dw        code_ENTER
                    dw        OVER                ; buf len buf
                    dw        PLUS                ; buf bufend
                    dw        OVER                ; buf bufend bufcur , setup start, end, cur
ACCEPT1
                    dw        DDUP                ; buf bufend bufcur bufend bufcur
                    dw        XOR                 ; buf bufend bufcur (bufend==bufcur)
                    dw        BRANCHZ,ACCEPT4     ; buf bufend bufcur if buf reached bufend, finish word
                    dw        KEY                 ; buf bufend bufcur key
                    dw        DUP                 ; buf bufend bufcur key key
                    dw        BL                  ; buf bufend bufcur key key 32
                    dw        IMM,127             ; buf bufend bufcur key key 32 127
                    dw        WITHIN              ; buf bufend bufcur key (key is printable?)
                    dw        BRANCHZ,ACCEPT2     ; buf bufend bufcur key , if not printable do ttap and loop again
                    dw        TAP                 ; buf bufend bufcur , print and save printable key
                    dw        BRANCH,ACCEPT1      ; buf bufend bufcur , again
ACCEPT2
                    dw        TTAP                ; buf bufend bufcur , manage non printable key
                    dw        BRANCH,ACCEPT1      ; buf bufend bufcur , again
ACCEPT4
                    dw        DROP                ; buf bufend - bufend has been replaced by bufcur in TTAP
                    dw        SWAP                ; bufend buf
                    dw        SUB                 ; len
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2310 TYPE ( buf len -- ) Emit len chars starting at buf.
                    #SEG0
word_TYPE
                    dw        word_ACCEPT
                    fcb       4
                    fcs       "TYPE"
TYPE
                    dw        code_ENTER
                    dw        TOR                 ; buf | R: len
                    dw        BRANCH,type2
type1
                    dw        DUP                 ; buf buf | R: len
                    dw        CLOAD               ; buf char | R: len
                    dw        EMIT                ; buf
                    dw        CHARP               ; buf+1
type2               dw        JNZD,type1          ; if @R (==len) > 0 then manage next char
                    dw        DROP                ; remove buf from stack
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY >CHAR ( char -- char ) - filter non printable chars
word_TCHAR
                    dw        word_TYPE
                    fcb       5
                    fcs       ">CHAR"
TCHAR
                    dw        code_ENTER
                    dw        IMM,0x7F,AND,DUP    ; mask msb
                    dw        IMM,127,BL,WITHIN   ; check for printable
                    dw        BRANCHZ,tcha1       ; branch if printable
                    dw        DROP,IMM,'_'        ; literal underscore
tcha1
                    dw        RETURN

; ===========================================================================
; Parsing
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY (buf buflen delim -- buf len deltabuf) skip spaces, find word that ends at delim
                    #SEG0
word_LPARSE
                    dw        word_TCHAR
                    fcb       5
                    fcs       "parse"
LPARSE
                    dw        code_ENTER
                    dw        IMM,pTEMP           ; buf buflen delim &TEMP
                    dw        STORE               ; buf buflen - TEMP contains delim

                    dw        OVER                ; buf buflen bufinit
                    dw        TOR                 ; buf buflen | R: bufinit
                    dw        DUP                 ; buf buflen buflen | R: bufinit
                    dw        BRANCHZ,pars8       ; buf buflen | R:bufinit if(buflen==0) goto pars8

; Buflen not zero
                    dw        DEC                 ; buf len-1 | R:bufinit
                    dw        IMM,pTEMP           ; buf len-1 &TEMP | R:bufinit
                    dw        LOAD                ; buf len-1 delim | R:bufinit
                    dw        BL                  ; buf len-1 delim blank | R:bufinit
                    dw        EQUAL               ; buf len-1 (delim==blank) | R:bufinit could it be simple xor?
                    dw        BRANCHZ,pars3       ; buf len-1 jump to pars3 if delim is blank, else (delim not blank): continue
                    dw        TOR                 ; buf | R: bufinit len-1
pars1
; skip leading blanks only
                    dw        BL                  ; buf blank | R: bufinit len-1
                    dw        OVER                ; buf blank buf | R: bufinit len-1
                    dw        CLOAD               ; buf blank curchar | R: bufinit len-1
                    dw        SUB                 ; buf curchar-bl | R: bufinit len-1
                    dw        ZLESS               ; buf (curchar<blank) | R: bufinit len-1
                    dw        NOT                 ; buf (curchar>=blank) | R: bufinit len-1
                    dw        BRANCHZ,pars2       ; buf | R: bufinit len-1

; curchar is below blank ->not printable, try next
                    dw        IMM,1               ; buf 1 | R: bufinit len-1
                    dw        PLUS                ; buf+1->buf | R: bufinit len-1
                    dw        JNZD,pars1          ; buf | R: bufinit len-2 and goto pars1 or buf | R: bufinit continue if len-1 is null
; all chars parsed
                    dw        RFROM               ; buf len-1
                    dw        DROP                ; buf
                    dw        IMM,0               ; buf 0
                    dw        DUP                 ; buf 0 0
                    dw        RETURN              ; all delim

pars2                                             ; Curchar >=delim
                    dw        RFROM               ; buf len-1

pars3                                             ; Initial situation, delim is blank
                    dw        OVER                ; buf len-1 buf
                    dw        SWAP                ; buf buf len-1
                    dw        TOR                 ; buf buf | R:len-1

pars4                                             ; scan for delimiter, beginning of a for loop
                    dw        IMM,pTEMP           ; buf buf &TEMP
                    dw        LOAD                ; buf buf delim
                    dw        OVER                ; buf buf delim buf
                    dw        CLOAD               ; buf buf delim curchar
                    dw        SUB                 ; buf buf (delim-curchar)

                    dw        IMM,pTEMP           ; buf buf (delim-curchar) &TEMP
                    dw        LOAD                ; buf buf (delim-curchar) delim
                    dw        BL                  ; buf buf (delim-curchar) delim blank
                    dw        EQUAL               ; buf buf (delim-curchar) (delim==blank)
                    dw        BRANCHZ,pars5       ; buf buf (delim-curchar) if(delim==blank) goto pars5
                    dw        ZLESS               ; buf buf (delim<curchar)

pars5                                             ; delim is blank
                    dw        BRANCHZ,pars6       ; buf buf if(delim<curchar) then goto par6
                    dw        CHARP               ; buf (buf+1)
                    dw        JNZD,pars4          ; buf (buf+1) and loop to pars4 if (len-1)>0
                    dw        DUP                 ; buf (buf+1) (buf+1)
                    dw        TOR                 ; buf (buf+1) | R:(buf+1)
                    dw        BRANCH,pars7

pars6                                             ; delim<curchar
                    dw        RFROM
                    dw        DROP
                    dw        DUP
                    dw        CHARP
                    dw        TOR

pars7
                    dw        OVER
                    dw        SUB
                    dw        RFROM
                    dw        RFROM
                    dw        SUB
                    dw        RETURN

pars8                                             ; Empty buffer case ;buf 0 | R:bufinit
                    dw        OVER                ; buf 0 buf | R:bufinit
                    dw        RFROM               ; buf 0 buf bufinit
                    dw        SUB                 ; buf 0 0
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2008 PARSE (delim "ccc<delim>" -- buf len) parse TIB at current pos and return delim spaced word
                    #SEG0
word_PARSE
                    dw        word_LPARSE
                    fcb       5
                    fcs       "PARSE"
PARSE
                    dw        code_ENTER
; Compute current input buffer pointer
                    dw        TOR                 ; -- | R: delim
                    dw        IMM,TIBP            ; &tib
                    dw        LOAD                ; tib
                    dw        TOIN                ; tib &done_count
                    dw        LOAD                ; tib done_count
                    dw        PLUS                ; buf
; Compute remaining count
                    dw        IMM,NTIBP           ; buf &ntib
                    dw        LOAD                ; buf ntib
                    dw        TOIN                ; buf ntib &done_count
                    dw        LOAD                ; buf ntib done_count
                    dw        SUB                 ; buf remaining_count
                    dw        RFROM               ; buf remaining_count delim
; Call low level word
                    dw        LPARSE              ; buf wordlen delta
                    dw        TOIN                ; buf wordlen delta &done_count
                    dw        PLUS_STORE          ; buf wordlen
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2020 PARSE-NAME ( "<spaces>name<space>" -- c-addr u ) TODO
; Goal is to avoid the use of PACKS that unnecessarily copies the string.
; As a consequence, this method also
; - avoids polluting HERE to save the compiled string
; - avoids the need for NAMECOMPARE and internal_compare
; However this is usually not a problem since the PACKed string is put in the
; right place to create new colon definitions.

; WORD and TOKEN. Create a counted string at HERE, which
; is used as temp memory. HERE pointer is not modified so each parsed word
; is stored at the same address (in unused data space). If an executed or
; compiled word manipulates HERE, then it is no problem: the user data will
; overwrite the word that was parsed and the next word will be stored a bit
; farther. It does not matter since this buffer is only used to FIND the code
; pointer for this word, usually. Another advantage of storing the word at
; HERE is that it helps compiling new word definitions!

; ---------------------------------------------------------------------------
; CORE 6.1.2450 WORD (delim "<delims>ccc,delim>" -- cs)
; Exceptional F2012 incompatibility: 6.1.2450 originally skip initial DELIMITERS
; while this implementation only skips initial SPACES only.
                    #SEG0
word_WORD
                    dw        word_PARSE
                    fcb       4
                    fcs       "WORD"
WORD
                    dw        code_ENTER
                    dw        PARSE               ; buf len
                    dw        HERE                ; buf len dest
                    dw        PACKS               ; dest
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY TOKEN ( -- cs)
                    #SEG0
word_TOKEN
                    dw        word_WORD
                    fcb       5
                    fcs       "TOKEN"
TOKEN
                    dw        code_ENTER
                    dw        BL
                    dw        WORD
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY (Parse-word')' -- ) Display a string while compiling. Could be removed to save mem.
                    #SEG0
word_DOTPAR
                    dw        word_TOKEN
                    fcb       2                   ; + WORD_IMMEDIATE
                    fcs       ".("
DOTPAR
                    dw        code_ENTER
                    dw        IMM,')'
                    dw        PARSE
                    dw        TYPE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0080 ( ("ccc)" -- ) Inline comment, nop
                    #SEG0
word_PAR
                    dw        word_DOTPAR
                    fcb       1                   ; + WORD_IMMEDIATE
                    fcs       "("
PAR
                    dw        code_ENTER
                    dw        IMM,')'
                    dw        PARSE
                    dw        DDROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2535 \ ( "ccc<eol>" -- ) Line comment , discard the rest of the input buffer
; BLOCK 7.6.2.2535 \ ( "ccc<eol>" -- )
                    #SEG0
word_BSLASH
                    dw        word_PAR
                    fcb       1                   ; + WORD_IMMEDIATE
                    fcs       "\\"
BSLASH
                    dw        code_ENTER
                    dw        IMM,NTIBP
                    dw        LOAD
                    dw        TOIN
                    dw        STORE
                    dw        RETURN

; ===========================================================================
; Dic search
; ===========================================================================

; ---------------------------------------------------------------------------
; INTERNAL TRWL_FIND ( req 0 ] cur -- flag )
; callback for TRAVERSE-WORDLIST that finds words.
; req : name that is searched for
; cur : word currently TRAVERSEd
; Strategy: TRAVERSE-WORDLIST does not put items on the stack before calling
; its callback, so the previous items are available to the callback.
; FIND pushes a zero before browsing the list.
; if a word is found, TRWL_FIND will replace this zero with the code pointer
; of the found word and stop the search.
; After the list if traversed, we can inspect this item and determine if a
; word was found. The FIND word itself with then load the flags and determine
; immediate or not.

                    #SEG0
TRWL_FIND
                    dw        code_ENTER
; compare cstr to current name stored at voc
; In compilation mode, we do not have to avoid compile-only words
                    dw        STATE               ; req 0 cur 0[interpret]/-1[compile]
                    dw        NOT                 ; req 0 cur -1[interpret]/0[compile]
                    dw        BRANCHZ,compiling   ; req 0 cur if compile then dont check CONLY flag

; We are in interpretation mode. we have to check the compile only flag
; Check flags within name. If word is compile only, skip it without even comparing name
                    dw        DUP                 ; req 0 cur cur
                    dw        CLOAD               ; req 0 cur namelen+flags
                    dw        IMM,WORD_COMPILEONLY  ; req 0 cur namelen+flags COMPILEONLY
                    dw        AND                 ; req 0 cur NZ_IF_COMPILE_ONLY
                    dw        BRANCHZ,compiling   ; req 0 cur , if not compile only then compare names
; word is compile only : finish iteration
                    dw        RETURN              ; req 0 cur -> not zero so try again with next word

compiling
                    dw        ROT                 ; 0 cur req
                    dw        DDUP                ; 0 cur req cur req
                    dw        NAMECOMPARE         ; 0 cur req equal_flag
                    dw        BRANCHZ,found       ; 0 cur req jump_if_equal

; Strings are different / word is compile only, look at next word
                    dw        ROT                 ; cur req 0
                    dw        ROT                 ; req 0 cur
                    dw        RETURN              ; req 0 cur -> not zero so try agn with next word

found
; Push a one if immediate, -1 if not immediate. now req is useless
                    dw        DROP                ; 0 cur
                    dw        DUP                 ; 0 cur cur
                    dw        CLOAD               ; 0 cur namelen+FLAGS
                    dw        DUP                 ; 0 cur namelen+FLAGS namelen+FLAGS
                    dw        IMM,WORD_LENMASK;0  ; cur namelen+flags namelen+FLAGS 0x3F
                    dw        AND                 ; 0 cur namelen+flags namelen
                    dw        CHARP               ; 0 cur namelen+flags namelen+1
                    dw        ROT                 ; 0 namelen+flags namelen+1 cur
                    dw        PLUS                ; 0 namelen+flags codeptr
                    dw        SWAP                ; 0 codeptr namelen+flags
                    dw        ROT                 ; codeptr namelen+flags 0
                    dw        DROP                ; codeptr namelen+flags
                    dw        IMM,WORD_IMMEDIATE
                    dw        AND                 ; codeptr NZ_IF_IMMEDIATE
                    dw        IMM,-1              ; codeptr NZ_IF_IMM -1 (not imm by default)
                    dw        SWAP                ; codeptr -1 NZ_IF_IMM
                    dw        BRANCHZ,fnotimm     ; codeptr -1 jump if name is not imm - will return -1
                    dw        NEGATE              ; codeptr 1 will return 1 for immediate
fnotimm
                    dw        IMM,0               ; codeptr +-1 0 Flag to terminate the traversal
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1550 FIND ( cstr -- codeaddr 1 [immediate] | codeaddr -1 [normal] | cstr 0 ) ?
; Check ALL vocabularies for a matching word and return code and name address, else same cstr and zero

                    #SEG0
word_FIND
                    dw        word_BSLASH
                    fcb       4
                    fcs       "FIND"
FIND
                    dw        code_ENTER          ; req - is a cstr
                    dw        IMM,0               ; req 0 - This setups the return state if nothing is found
                    dw        IMM,TRWL_FIND       ; req 0 cb
                    dw        IMM,LASTP           ; req 0 cb [pointer containing the address of the last word]
                    dw        LOAD                ; req 0 cb voc[address of last word entry]
                    dw        TRWL                ; Browse all words. The search stops when the required word is found
; Search complete. What do we find on the stack?
; If no word was found: the zero previously pushed will be found
; req 0 [if nothing found] codeptr +-1 [if found]
                    dw        RETURN

; ===========================================================================
; Error handling
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY HANDLER ( -- a ) Return address of current exception handler
                    #SEG0
word_HANDLER
                    dw        word_FIND
                    fcb       7
                    fcs       "HANDLER"
HANDLER
                    dw        code_ENTER
                    dw        IMM,HANDP
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY CATCH ( xt -- err#\0 ) setup frame to handle errors thrown while executing xt
; TODO make it use int codes instead of strings to comply with EXCEPTION 9.6.1.0875
                    #SEG0
word_CATCH
                    dw        word_HANDLER
                    fcb       5
                    fcs       "CATCH"
CATCH
                    dw        code_ENTER
; save error frame
                    dw        SPLOAD
                    dw        TOR
                    dw        HANDLER
                    dw        LOAD
                    dw        TOR
; Execute
                    dw        RPLOAD
                    dw        HANDLER
                    dw        STORE
                    dw        EXECUTE
; Restore error frame
                    dw        RFROM
                    dw        HANDLER
                    dw        STORE
; No error
                    dw        RFROM
                    dw        DROP
                    dw        IMM,0
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY THROW  ( err# -- err# ) return from the encapsulating catch with an error code
; TODO make it use int codes to comply with EXCEPTION 9.6.1.2275
                    #SEG0
word_THROW
                    dw        word_CATCH
                    fcb       5
                    fcs       "THROW"
THROW
                    dw        code_ENTER
; restore return stack
                    dw        HANDLER
                    dw        LOAD
                    dw        RPSTORE
; restore handler frame
                    dw        RFROM
                    dw        HANDLER
                    dw        STORE
; restore data stack
                    dw        RFROM
                    dw        SWAP
                    dw        TOR
                    dw        SPSTORE
                    dw        DROP
                    dw        RFROM
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0670 ABORT ( -- ) Jump to quit
; TODO use int codes to comply with EXCEPTION_EXT 9.6.2.0670
                    #SEG0
word_ABORT
                    dw        word_THROW
                    fcb       5
                    fcs       "ABORT"
ABORT
                    dw        code_ENTER
                    dw        IMMSTR
                    fcb       7
                    fcs       " abort!"
                    dw        THROW

; ---------------------------------------------------------------------------
; PROPRIETARY ABORT" ( f -- ) run time routine of abort" . abort with a message.
                    #SEG0
word_ABORTNZ
                    dw        word_ABORT
                    fcb       6                   ; + WORD_COMPILEONLY
                    fcs       'abort"'
ABORTNZ
                    dw        code_ENTER
                    dw        BRANCHZ,abor1
                    dw        DOSTR
                    dw        THROW
abor1                                             ; Cancel abort if TOS was zero
                    dw        DOSTR
                    dw        DROP
                    dw        RETURN

; ===========================================================================
; System state (interpret/compile)
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.2250 STATE ( -- state ) - Return 0 if interpreting and -1 if compiling
word_STATE
                    dw        word_ABORTNZ
                    fcb       5
                    fcs       "STATE"
STATE
                    dw        code_ENTER
                    dw        IMM,STATP
                    dw        LOAD
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY Set the system state to interpretation
; TODO use STATE instead and comply with CORE 6.1.2500
                    #SEG0
word_INTERP
                    dw        word_STATE
                    fcb       1                   ; + WORD_IMMEDIATE
                    fcs       "["
INTERP
                    dw        code_ENTER
                    dw        IMM,0
                    dw        IMM,STATP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; Set the system state to compilation
; TODO use STATE instead and comply with CORE 6.1.2540
                    #SEG0
word_COMPIL
                    dw        word_INTERP
                    fcb       1                   ; + WORD_IMMEDIATE
                    fcs       "]"
COMPIL
                    dw        code_ENTER
                    dw        IMM,0xFFFF
                    dw        IMM,STATP
                    dw        STORE
                    dw        RETURN

; ===========================================================================
; Compiler
; ===========================================================================

; ---------------------------------------------------------------------------
; CORE 6.1.0070 ' ( "<spaces>name" -- ca ) - search context vocabularies for the next word in input stream.
                    #SEG0
word_TICK
                    dw        word_COMPIL
                    fcb       1
                    fcs       "'"
TICK
                    dw        code_ENTER
                    dw        TOKEN
                    dw        FIND
                    dw        BRANCHZ,tick1
                    dw        RETURN
tick1
                    dw        THROW

; ---------------------------------------------------------------------------
; CORE 6.1.0710 ALLOT ( n -- ) - allocate n bytes to the code dictionary.
                    #SEG0
word_ALLOT
                    dw        word_TICK
                    fcb       5
                    fcs       "ALLOT"
ALLOT
                    dw        code_ENTER
                    dw        IMM,HEREP
                    dw        PLUS_STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0150 , (u -- ) Pop a word and save it HERE, then make HERE point to the next cell
                    #SEG0
word_COMMA
                    dw        word_ALLOT
                    fcb       1
                    fcs       ","
COMMA
                    dw        code_ENTER
                    dw        HERE                ; (VALUE) (HERE)
                    dw        DUP                 ; (VALUE) (HERE) (HERE)
                    dw        CELLP               ; (VALUE) (HERE) (HERE+2)
                    dw        IMM,HEREP           ; (VALUE) (HERE) (HERE+2) (HEREP=&HERE)
                    dw        STORE               ; (VALUE) (HERE)
                    dw        STORE               ; Empty
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0860 C, (u -- ) Pop a word and the LSB char it HERE, then make HERE point to the next char
                    #SEG0
word_CCOMMA
                    dw        word_COMMA
                    fcb       2
                    fcs       "C,"
CCOMMA
                    dw        code_ENTER
                    dw        HERE                ; (VALUE) (HERE)
                    dw        DUP                 ; (VALUE) (HERE) (HERE)
                    dw        CHARP               ; (VALUE) (HERE) (HERE+1)
                    dw        IMM,HEREP           ; (VALUE) (HERE) (HERE+1) (HP=&HERE)
                    dw        STORE               ; (VALUE) (HERE)
                    dw        CSTORE              ; Empty
                    dw        RETURN

; ---------------------------------------------------------------------------
; POSTPONE ( "<spaces>ccc<space>" -- ) - compile the next immediate word into code dictionary.
                    #SEG0
word_POSTPONE
                    dw        word_CCOMMA
                    fcb       8                   ; + WORD_IMMEDIATE
                    fcs       "POSTPONE"
POSTPONE
                    dw        code_ENTER
                    dw        TICK
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL compile ( -- ) - compile the next address in colon list to code dictionary.
; This is a short hand for IMM,VALUE,COMMA. Only goal is to save ROM space (one word saved per use wrt to direct IMM).
; TODO rename COMPILE_IMM
COMPILE_IMM
                    dw        code_ENTER
                    dw        RFROM
                    dw        DUP
                    dw        LOAD
                    dw        COMMA
                    dw        CELLP
                    dw        TOR
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1780 LITERAL ( w -- ) - compile tos to code dictionary as an integer literal.

                    #SEG0
word_LITERAL
                    dw        word_POSTPONE
                    fcb       7                   ; + WORD_IMMEDIATE
                    fcs       "LITERAL"
LITERAL
                    dw        code_ENTER
                    dw        COMPILE_IMM,IMM
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY $,"       ( -- ) - compile a literal string up to next "
word_SCOMPQ
                    dw        word_LITERAL
                    fcb       3
                    fcs       '$,"'
SCOMPQ
                    dw        code_ENTER
                    dw        IMM,'"'
                    dw        WORD                ; Clever! Use HERE as storage temporary, cstring is already put at the right place!
; Compute the new value for HERE
                    dw        CLOAD
                    dw        CHARP
                    dw        HERE
                    dw        PLUS
                    dw        IMM,HEREP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY for ( -- a ) - start a for-next loop structure in a colon deND:nition.
; This word pushes the current address on the data stack for later jump back
                    #SEG0
word_FOR
                    dw        word_SCOMPQ
                    fcb       3                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "FOR"
FOR
                    dw        code_ENTER
                    dw        COMPILE_IMM,TOR
                    dw        HERE
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY next ( a -- ) - terminate a for-next loop structure.
; This word USES the loop-start address that was pushed on the stack by FOR
                    #SEG0
word_NEXT
                    dw        word_FOR
                    fcb       4                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "NEXT"
NXT
                    dw        code_ENTER
                    dw        COMPILE_IMM,JNZD
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY AFT ( a -- a a ) - jump to then in a for-aft-then-next loop the first time through.
                    #SEG0
word_AFT
                    dw        word_NEXT
                    fcb       3                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "AFT"
AFT
                    dw        code_ENTER
                    dw        DROP
                    dw        AHEAD
                    dw        BEGIN
                    dw        SWAP
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0760 BEGIN ( -- a ) - start an infinite or indefinite loop structure.
; This word pushes the current address on the data stack for later jump back

                    #SEG0
word_BEGIN
                    dw        word_AFT
                    fcb       5                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "BEGIN"
BEGIN
                    dw        code_ENTER
                    dw        HERE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2390 UNTIL ( a -- ) - terminate a begin-until indefinite loop structure.
; This word USES the loop-start address that was pushed on the stack by BEGIN
                    #SEG0
word_UNTIL
                    dw        word_BEGIN
                    fcb       5                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "UNTIL"
UNTIL
                    dw        code_ENTER
                    dw        COMPILE_IMM,BRANCHZ
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.0700 AGAIN ( a -- ) - terminate a begin-again infinite loop structure.
; This word USES the loop-start address that was pushed on the stack by BEGIN
                    #SEG0
word_AGAIN
                    dw        word_UNTIL
                    fcb       5                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "AGAIN"
AGAIN
                    dw        code_ENTER
                    dw        COMPILE_IMM,BRANCH
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1700 IF ( -- a ) - begin a conditional branch structure.
; This word pushes the address where the forward jump address will have to be stored by THEN or ELSE
                    #SEG0
word_IF
                    dw        word_AGAIN
                    fcb       2                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "IF"
IF
                    dw        code_ENTER
                    dw        COMPILE_IMM,BRANCHZ
                    dw        HERE                ; Push the address of the forward ref on the stack
                    dw        IMM,0,COMMA         ; Reserve a cell to subsequent definition by THEN
                    dw        RETURN

; ---------------------------------------------------------------------------
; TOOLS 15.6.2.0702 AHEAD ( -- a ) - compile a forward branch instruction.
                    #SEG0
word_AHEAD
                    dw        word_IF
                    fcb       5                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "AHEAD"
AHEAD
                    dw        code_ENTER
                    dw        COMPILE_IMM,BRANCH
                    dw        HERE
                    dw        IMM,0
                    dw        COMMA
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2140 REPEAT ( a a -- ) - terminate a begin-while-repeat indefinite loop.
                    #SEG0
word_REPEAT
                    dw        word_AHEAD
                    fcb       6                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "REPEAT"
REPEAT
                    dw        code_ENTER
                    dw        AGAIN
                    dw        HERE
                    dw        SWAP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2270 THEN ( a -- ) - terminate a conditional branch structure.
                    #SEG0
word_THEN
                    dw        word_REPEAT
                    fcb       4                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "THEN"
THEN
                    dw        code_ENTER
                    dw        HERE
                    dw        SWAP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1310 ELSE ( a -- a ) - start the false clause in an if-else-then structure.
                    #SEG0
word_ELSE
                    dw        word_THEN
                    fcb       4                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "ELSE"
ELSE
                    dw        code_ENTER
                    dw        AHEAD
                    dw        SWAP
                    dw        THEN
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2430 WHILE ( a -- a a ) - conditional branch out of a begin-while-repeat loop.
                    #SEG0
word_WHILE
                    dw        word_ELSE
                    fcb       5                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "WHILE"
WHILE
                    dw        code_ENTER
                    dw        IF
                    dw        SWAP
                    dw        RETURN

; ---------------------------------------------------------------------------
; ABORT"
; Words that need a string literal after them have an immediate word for compilation and a runtime word that is actually compiled.
; TODO make compliant with CORE 6.1.0680 and EXCEPTION_EXT 9.6.2.0680
                    #SEG0
word_ABORTQ
                    dw        word_WHILE
                    fcb       6                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       'ABORT"'
ABORTQ
                    dw        code_ENTER
                    dw        COMPILE_IMM,ABORTNZ
                    dw        SCOMPQ
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2165 S" - compile an inline string literal.
; COMPILE TIME: ( "ccc<quote>" -- ) -> STRQ, executes common string litteral definition SCOMPQ
; RUN TIME:     ( -- c-addr u )     -> IMMSTR, reads following string litteral defined by SCOMPQ

; Words that need a string literal after them have an immediate word for compilation and a runtime word that is actually compiled.
                    #SEG0
word_STRQ
                    dw        word_ABORTQ
                    fcb       2                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       'S"'
STRQ
                    dw        code_ENTER
                    dw        COMPILE_IMM,IMMSTR
                    dw        SCOMPQ
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0190 ." - compile an inline string literal to be typed out at run time.
; COMPILE TIME: ( "ccc<quote>" -- ) -> DOTQ, executes common string litteral definition SCOMPQ
; RUN TIME:     ( -- )              -> SHOWSTR, reads following string litteral defined by SCOMPQ

; Words that need a string literal after them have an immediate word for compilation and a runtime word that is actually compiled.
word_DOTQ
                    dw        word_STRQ
                    fcb       2                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       '."'
DOTQ
                    dw        code_ENTER
                    dw        COMPILE_IMM,SHOWSTR
                    dw        SCOMPQ
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY $,N ( na -- ) - build a new dictionary name using the string at na.
                    #SEG0
word_SNAME
                    dw        word_DOTQ
                    fcb       3
                    fcs       "$,N"
SNAME
                    dw        code_ENTER
; Store a pointer to the current definition
                    dw        HERE
                    dw        IMM,CURDP
                    dw        STORE
; Store the pointer to the previous definition in the prev link
                    dw        IMM,LASTP
                    dw        LOAD                ; Load pointer to last name
                    dw        COMMA               ; save prev address
; Parse word name and store at HERE
                    dw        TOKEN               ; cstr | save name string at HERE
                    dw        DUP                 ; cstr cstr
                    dw        FIND                ; cstr code name | cstr name 0
                    dw        BRANCHZ,sn2         ; cstr code | cstr name
; not zero: name exists, warn user
                    dw        SHOWSTR
                    fcb       7
                    fcs       "  redef"
sn2
; goto end of token
                    dw        DROP                ; cstr
                    dw        CLOAD               ; len
                    dw        CHARP               ; len+1
                    dw        HERE                ; len+1 here
                    dw        PLUS                ; here_after_str
                    dw        IMM,HEREP           ; here_after_str herep
                    dw        STORE               ; -- Update HERE after the word name
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0450 : ( "<spaces>ccc<space>" -- ) - start a new colon definition using next word as its name.
                    #SEG0
word_COLON
                    dw        word_SNAME
                    fcb       1
                    fcs       ":"
COLON
                    dw        code_ENTER
                    dw        SNAME
                    dw        COMPILE_IMM,code_ENTER  ; save codeptr to execute the definition
                    dw        COMPIL              ; Enter compilation mode
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY OVERT ( -- ) Link last word in current vocabulary
word_OVERT
                    dw        word_COLON
                    fcb       5
                    fcs       "OVERT"
OVERT
                    dw        code_ENTER
; Update LAST to the current def
                    dw        IMM,CURDP
                    dw        LOAD
                    dw        IMM,LASTP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.0460 ; ( -- ) - terminate a colon definition.
                    #SEG0
word_SEMICOL
                    dw        word_OVERT
                    fcb       1                   ; + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       ";"
SEMICOL
                    dw        code_ENTER
                    dw        COMPILE_IMM,RETURN  ; Write the final RETURN
                    dw        OVERT               ; Save new LAST
                    dw        INTERP              ; Back to interpreter mode
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1710 IMMEDIATE ( -- )
                    #SEG0
word_IMMEDIATE
                    dw        word_SEMICOL
                    fcb       9
                    fcs       "IMMEDIATE"
IMMEDIATE
                    dw        code_ENTER
                    dw        IMM,CURDP           ; curdp = &cur_def_ptr
                    dw        LOAD                ; cur_def_ptr
                    dw        CELLP               ; cstr
                    dw        DUP                 ; cstr cstr
                    dw        CLOAD               ; cstr name_len_and_flags
                    dw        IMM,WORD_IMMEDIATE  ; cstr name_len_and_flags IMM
                    dw        OR                  ; cstr name_len_and_flags|IMM
                    dw        SWAP                ; name_len_and_flags|IMM cstr
                    dw        CSTORE              ; --
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL DOVAR ( -- a ) - run time routine for variable and create.
; Before DOVAR is called the return stack receives the address right after
; the dovar word itself. This value is popped from the return stack, so when
; returning from DOVAR with RETURN, execution is transferred not to the word
; that called dovar, but to its parent. This means that the word that uses dovar
; is interrupted. The parent continues to execute with the address of the word
; that follows dovar on the stack.
                    #SEG0
DOVAR
                    dw        code_ENTER
                    dw        RFROM
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1000 CREATE ( "<spaces>ccc<space>" -- ) - compile a new array entry without allocating code space.
; CREATE returns the address of the byte just after the DOVAR word. Using
; ALLOT after CREATE is a method to reserve memory, and the name created by CREATE would
; push the address of this reserved memory on the stack, simulating a buffer or variable.
; But it is interesting to replace DOVAR by some other word address, which is used to
; implement DOES> (later)
                    #SEG0
word_CREATE
                    dw        word_IMMEDIATE
                    fcb       6
                    fcs       "CREATE"
CREATE
                    dw        code_ENTER
                    dw        SNAME
                    dw        OVERT
                    dw        IMM,code_ENTER      ; save code to execute the definition
                    dw        COMMA
; Store the current address so DOES can replace the DOVAR if called.
                    dw        HERE
                    dw        IMM,LSTCRP
                    dw        STORE
; Now emit the DOVAR, this word can be replaced later by DOES>
                    dw        COMPILE_IMM,DOVAR
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2410 VARIABLE  ( -- ; <string> ) - compile a new variable initialized to 0.
; VARIABLE uses CREATE and follows this by a call to COMMA that stores zero
; in the cell right after DOVAR, then increments HERE by a cell. This has the
; effect to return the address of this cell when the name is invoked.
                    #SEG0
word_VARIABLE
                    dw        word_CREATE
                    fcb       8
                    fcs       "VARIABLE"
VARIABLE
                    dw        code_ENTER
                    dw        CREATE              ; parse the name that follows in the input stream and link in in the dict
                    dw        IMM,0
                    dw        COMMA               ; Store a zero after create's DOVAR and increment HERE by a cell
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1250 DOES>
; [when a definition calls DOES>]
; COMPILE TIME:   ( -- ) Execute the run time semantics (immediate word)
; RUN TIME:       ( -- ) Replaces the action of the last CREATEd definition by the code that follows DOES>.
; [when the definition that called DOES> is executed]
; INITIATION TIME:( ... -- ... a ) Place the address of the CREATEd definition data field on the stack.
; EXECUTION TIME: ( ... -- ... ) Execute the action that is described by the words that follow DOES>.

; Change the behaviour of CREATE with custom code
; When a word is created with CREATE NAME, the structure assembled at HERE is:
; dw  LAST
; fcb NAME_LENGTH
; fcs NAME
; dw  code_ENTER
; dw  DOVAR

; CREATE in interpretation mode

; When executed, this word RFROM the return address of DOVAR towards the created name.
; This is in fact the address of the word that follows DOVAR, which is the address of
; the datafield for the word. DOVAR then executes a RETURN, that will not end the
; execution of DOVAR (since the return address was pulled) but will instead end the
; execution of the CREATEd word.
; When CREATE is executed, HERE also points to the address after the DOVAR containing cell.

; An allocation word that increments HERE can then be used to create a data field after the
; DOVAR word, that can be used for data storage.

; CREATE in compilation mode

; It is possible to compile a colon definition that uses CREATE. When such a definition is
; executed, it will parse the next TOKEN from the input buffer and use that for CREATE. This
; is what is used by VARIABLE:
; : VARIABLE CREATE 0 , ;

; When used like this:
; VARIABLE LEN
; The execution of variable will call CREATE, create will parse LEN and create a definition.
; After this, zero is pushed on the stack and stored at HERE, then HERE in incremented by a cell.
; This is a defining word: It creates a new word.
; When LEN is executed, the DOVAR word of CREATE will be executed, and that will push the address
; of the datafield on the stack. LEN can then be used with @ and ! to hold a value.

; CREATE in compilation mode with appended DOES>

; At execution of the colon definition that contains create/does,
; 1 - CREATE will parse the input buffer and create a definition, then execute any allocation word that follows.
; 2 - DOES> will start compiling a new unnamed executable word (by compiling a code_ENTER) and replace the DOVAR word in the newly CREATEd definition by the address of the code_ENTER that was just stored.

; This is easy because one can just save the address of the DOVAR word before leaving create.
; The following words are compiled right next after DOES>, so this creates an unnamed word that will be executed by the newly CREATEd definition.

; Summary
; DOES> is an immediate word. when found in a colon definition (compilation state) the following happens:
; -compiles a STORE of the next next address
; -compile a RETURN
; (this is the address that was stored in create's DOVAR
; -prepare a new unnamed word (compile code_ENTER)
; -compile the words that follow DOES>

; when a colon definition that contains DOES is executed
; -create and the allocation words up to DOES are executed
; -STORE of the unnamed code that follows is stored in created DOVAR
; -done

; when the word defined by create and modified by DOES is executed
; -code that was installed by DOES> is executed
; -data field of created definition is pushed
; -user words after DOES are executed

; Example for assembler:
; : INHERENT          ( Defines the name of the class)
; CREATE          ( this will create an instance)
; C,          ( store the parameter for each instance)
; DOES>           ( this is the class' common action)
; C@          ( get each instance's parameter)
; C,          ( the assembly action, as above)
; ;               ( End of definition)

; HEX
; 12  INHERENT NOP,   ( Defines an instance NOP, of class INHERENT, with parameter 12H.)

; sys11 forth ver 1.00
; : INH CREATE C, DOES> C@ C, ;  ok
; 42 INH NOP  ok
; $100 64 DUMP
; 100  F2 9C  3 49 4E 48 E0 2B EF FD ED D1 E1 57  1 1A  r__INH`+o}mQaW__
; 110  E1 57  0 54 E1 9B E1 8E E1 55 E0 2B E1 A9 E1 A2  aW_Ta_a_aU`+a)a"
; 120  ED D1 E1 55  1  0  3 4E 4F 50 E0 2B  1 1A 26  4  mQaU___NOP`+__&_
; 130  44 55 4D 50 50 4D 41 4C  1 2D  4 50 4C 4F 50 E0  DUMPPMAL_-_PLOP`
; 140  2B EF EE  5 41  0 2E 31 34 31  0 33 31 2E 30 30  +on_A_.141_31.00  ok

; 100    F29C    PREV
; 102    3       LEN
; 103    INH     NAME
; 106    E02B    code_ENTER
; 108    EFFD    CREATE
; 10A    EDD1    C,
; 10C    E157    IMM
; 10E    011A
; 110    E157    IMM
; 112    0054    LASTCRP
; 114    E19B    LOAD
; 116    E18E    STORE
; 118    E155    RETURN
; ------------------------------- start of action
; 11A    E02B    code_ENTER
; 11C    E1A9    RFROM
; 11E    E1A2    C@
; 120    EDD1    C,
; 122    E155    RETURN
; ------------------------------- end of action
; 124    0100
; 126    3
; 127    NOP
; 12A    E02B    code_ENTER
; 12C    011A    action_for_INH
; 12E    26      data zone for NOP

word_DOES           dw        word_VARIABLE
                    fcb       5 + WORD_COMPILEONLY + WORD_IMMEDIATE
                    fcs       "DOES>"
DOES                dw        code_ENTER
; When called, we are compiling a word.
; Now, we have to define a new unnamed word. The address
; of the unnamed word is HERE. this value must be stored in place of
; the DOVAR that was defined by the previous CREATE
                    dw        COMPILE_IMM,IMM
                    dw        HERE
                    dw        IMM,6,CELLS,PLUS    ; Compute the address of the code that replaces DOVAR in the created def
                    dw        COMMA
                    dw        COMPILE_IMM,IMM
                    dw        COMPILE_IMM,LSTCRP  ; The cell pointed by this address contains the address of the DOVAR for the last CREATE
                    dw        COMPILE_IMM,LOAD    ; Get the address that contains DOVAR
                    dw        COMPILE_IMM,STORE   ; This actually stores the SAME code pointer (after the definition end) in each created instance.
                                                  ; The code executed by the definition stops here. Next compiled words will be the DOES action.
                    dw        COMPILE_IMM,RETURN

;Start the code that will be executed by the CREATEd definition
                    dw        COMPILE_IMM,code_ENTER
                    dw        COMPILE_IMM,RFROM   ; Just before executing the DOES action, we compile code that acts like the original DOVAR to get the CREATEd data field
                    dw        RETURN              ; DOES> has finished preparing the mem. next compiled words are added.

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.1850 MARKER ( <spaces>name<spaces> -- )
; Create a definition that, when executed, will delete itself and all words
; that were defined later. We do that by saving/restoring HERE and LAST
; Example:
; sys11 forth ver 1.00
; MARKER base  ok
; $100 31 DUMP
; 100  F2 A2  4 62 61 73 65 E0 21 E1 BE F2 A2 E1 BE  0  r"_base`!a>r"a>_
; 110  44 E2  1 E1 BE  1  0 E1 BE  0 42 E2  1 E1 BC  4  Db_a>__a>_Bb_a<_  ok

; 100    F2A2    PREV
; 102    4 str -> base
; 107    E021    code_ENTER
; 109    E1BE    IMM
; 10B    F2A2    prev
; 10D    E1BE    IMM
; 10F    0044    LASTP
; 111    E201    STORE

; 113    E1BE    IMM
; 115    0100    HERE_START
; 117    E1BE    IMM
; 119    0042    HEREP
; 11B    E201    STORE
; 11D    E1BC    RETURN

                    #SEG0
word_MARKER
                    dw        word_DOES
                    fcb       6
                    fcs       "MARKER"
MARKER
                    dw        code_ENTER
                    dw        HERE                ; here_before | Save HERE - this is the restore point
                    dw        CREATE              ; here_before | Create a DOVAR definition, eating the next token
                    dw        IMM,LSTCRP          ; here_before &lstptr
                    dw        LOAD                ; here_before listptr
                    dw        IMM,HEREP           ; here_before listptr &here Rewind HERE to overwrite the DOVAR with COMMA
                    dw        STORE               ; here_before --
; Now TOS contains the pointer to this code's word.
; We can replace DOVAR by some code that will restore HERE and LASTP.
; After create, loading at here_before will retrieve the ptr to the prev word
                    dw        DUP                 ; here_before here_before

                    dw        LOAD                ; here_before prev_before

                    dw        LITERAL             ; here_before Generate code to push prev_before
                    dw        IMM,LASTP           ; here_before LASTP
                    dw        LITERAL             ; here_before Generate code to push LASTP
                    dw        COMPILE_IMM,STORE;Generate  ; code to store prev_before into LAST

                    dw        LITERAL             ; Generate code to push here_before
                    dw        IMM,HEREP           ; HEREP
                    dw        LITERAL             ; Generate code to push the address of here
                    dw        COMPILE_IMM,STORE

                    dw        COMPILE_IMM,RETURN
                    dw        RETURN

; ===========================================================================
; Shell
; ===========================================================================

; ---------------------------------------------------------------------------
                    #SEG0
word_TOIN
                    dw        word_MARKER
                    fcb       3
                    fcs       ">IN"
TOIN
                    dw        code_ENTER
                    dw        IMM,TOINP
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY PROMPT ( -- )
                    #SEG0
word_PROMPT
                    dw        word_TOIN
                    fcb       6
                    fcs       "PROMPT"
PROMPT
; Test if we are in compilation mode
                    dw        code_ENTER
                    dw        STATE
                    dw        NOT
                    dw        BRANCHZ,compilmode  ; state not zero: jump to compilation behaviour (just cr)
; No: interpretation: show OK
                    dw        SHOWSTR
                    fcb       4
                    fcs       "  ok"
compilmode
                    dw        CR
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY ?STACK ( -- ) abort if the data stack underflows.
                    #SEG0
word_QSTACK
                    dw        word_PROMPT
                    fcb       6
                    fcs       "?STACK"
QSTACK
                    dw        code_ENTER
                    dw        DEPTH
                    dw        ZLESS
                    dw        ABORTNZ
                    fcb       10
                    fcs       "underflow!"
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL eval ( -- )
; Used by EVALUATE nd QUIT. Evaluate all words in input buffer.
; Each word is interpreted or compiled according to current behaviour

                    #SEG0
EVAL
                    dw        code_ENTER
eval1
                    dw        TOKEN               ; tokcstr
                    dw        DUP                 ; tokcstr tokcstr
                    dw        CLOAD               ; tokcstr toklen | input stream empty?
                    dw        BRANCHZ,evalfinish  ; tokcstr Could not parse: finish execution of buffer

                    dw        FIND                ; code TRUE || codeimm 1 || cstr false
                    dw        DUPNZ               ; Duplicate only if 1 or -1
                    dw        BRANCHZ,evnum       ; code +-1 | cstr if not name then evnum

; Name is found. Execute in all cases (immediate or not)
                    dw        STATE               ; code +-1 state
                    dw        BRANCHZ,evinterp    ; code +-1

; In compilation mode. Check if word is immediate
                    dw        IMM,1               ; code +-1 1
                    dw        XOR                 ; code zero_if_imm
                    dw        BRANCHZ,evinterp2   ; code

; Word is not immediate -> compile
                    dw        COMMA
                    dw        BRANCH,evnext

evinterp
                    dw        DROP                ; code
evinterp2
; Interpret word - because interpreting or compiling an immediate word.
                    dw        EXECUTE
                    dw        BRANCH,evnext       ; Manage next token

evnum
                    dw        NUMBERQ
                    dw        BRANCHZ,notfound    ; consume the OK flag and leaves the number on the stack for later use
; Valid number
                    dw        STATE               ; tokstr state
                    dw        BRANCHZ,evnext      ; If interpreting, the number just stays on the stack and look at next token
                    dw        LITERAL             ; If compiling, the number left on stack is instead compiled as an IMM
                    dw        BRANCH,evnext       ; Manage next token

notfound
                    dw        THROW               ; Throw the failed name as exception, to be caught in QUIT

evnext
                    dw        QSTACK              ; TODO Check stack underflow
                    dw        BRANCH,eval1        ; Do next token

evalfinish
                    dw        DROP                ; --
                    dw        PROMPT
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY HAND ( -- ) Reset the input buffer to its default storage and size.
word_HAND
                    dw        word_QSTACK
                    fcb       4
                    fcs       "HAND"
HAND
                    dw        code_ENTER
                    dw        IMM,TIB_LEN
                    dw        IMM,STIBP
                    dw        STORE
                    dw        IMM,TIBBUF
                    dw        IMM,TIBP
                    dw        STORE
                    dw        IMM,0
                    dw        IMM,NTIBP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2216 SOURCE ( -- buf len )
; Return the current input buffer address and length
word_SOURCE
                    dw        word_HAND
                    fcb       6
                    fcs       "SOURCE"
SOURCE
                    dw        code_ENTER
                    dw        IMM,TIBP
                    dw        LOAD
                    dw        IMM,STIBP
                    dw        LOAD
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2148 ( -- STIB NTIB IN TIB 4 )
word_SAVEINPUT
                    dw        word_SOURCE
                    fcb       10
                    fcs       "SAVE-INPUT"
SAVEINPUT
                    dw        code_ENTER
                    dw        IMM,STIBP           ; buf len &tib_size
                    dw        LOAD                ; buf
                    dw        IMM,NTIBP           ; buf len &tib_received
                    dw        LOAD                ; buf
                    dw        TOIN                ; >IN
                    dw        LOAD                ; --
                    dw        IMM,TIBP            ; buf &tib
                    dw        LOAD                ; --
                    dw        IMM,4
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE_EXT 6.2.2148 ( STIB NTIB IN TIB 4 -- flag )
word_RESTOREINPUT
                    dw        word_SAVEINPUT
                    fcb       13
                    fcs       "RESTORE-INPUT"
RESTOREINPUT
                    dw        code_ENTER
                    dw        IMM,4
                    dw        XOR
                    dw        BRANCHZ,restore
                    dw        IMM,1               ; cannot restore - stack problem
                    dw        RETURN
restore
                    dw        IMM,TIBP            ; buf &tib
                    dw        STORE               ; --
                    dw        TOIN                ; >IN
                    dw        STORE               ; --
                    dw        IMM,NTIBP           ; buf len &tib_received
                    dw        STORE               ; buf
                    dw        IMM,STIBP           ; buf len &tib_size
                    dw        STORE               ; buf
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.1360 EVALUATE ( ... buf len -- ... )
; BLOCK 7.6.1.1360 EVALUATE
; Make the buffer pointed by buf and len the TIB, evaluate, then restore the
; original console TIB.
word_EVALUATE
                    dw        word_RESTOREINPUT
                    fcb       8
                    fcs       "EVALUATE"
EVALUATE
                    dw        code_ENTER
                    dw        SAVEINPUT
                    dw        TOR,TOR,TOR,TOR,TOR
                    dw        DUP
                    dw        IMM,STIBP           ; buf len &tib_size
                    dw        STORE               ; buf
                    dw        IMM,NTIBP           ; buf len &tib_received
                    dw        STORE               ; buf
                    dw        IMM,TIBP            ; buf &tib
                    dw        STORE               ; --
                    dw        IMM,0               ; 0
                    dw        TOIN                ; >IN
                    dw        STORE               ; --
                    dw        EVAL                ; Evaluate input
                    dw        RFROM,RFROM,RFROM,RFROM,RFROM
                    dw        RESTOREINPUT        ; Restore input source
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY CONSOLE ( -- ) Make IO vectors point at the default serial implementations
word_CONSOLE
                    dw        word_EVALUATE
                    fcb       7
                    fcs       "CONSOLE"
CONSOLE
                    dw        code_ENTER
                    dw        IMM,IOTX
                    dw        IMM,TXVEC
                    dw        STORE
                    dw        IMM,IORX
                    dw        IMM,RXVEC
                    dw        STORE
                    dw        HAND
                    dw        RETURN

; ---------------------------------------------------------------------------
; CORE 6.1.2050 QUIT ( -- ) Main forth interactive interpreter loop
; TODO check compliance with F2012
                    #SEG0
word_QUIT
                    dw        word_CONSOLE
                    fcb       4
                    fcs       "QUIT"
QUIT
                    dw        code_ENTER
QUIT0
                    dw        INTERP              ; Force interpretation mode in case catch aborts during compilation

QUIT1
; Load the terminal input buffer
                    dw        IMM,TIBP
                    dw        LOAD
                    dw        IMM,STIBP
                    dw        LOAD
                    dw        ACCEPT

; Save the length of the received buffer
                    dw        IMM,NTIBP
                    dw        STORE

; Reset input buffer pointer to start of buffer
                    dw        IMM,0
                    dw        TOIN
                    dw        STORE

; Execute the line
                    dw        IMM,EVAL            ; Function to be caught
                    dw        CATCH               ; returns zero if no error
                    dw        DUPNZ               ; Does nothing if no error
                    dw        BRANCHZ,QUIT1       ; Consume catch return code. If thats zero, no error, loop again

; CATCH caught an error, DUPNZ left the error that was thrown on the stack
                    dw        CONSOLE
                    dw        SHOWSTR
                    fcb       7
                    fcs       "  err: "
                    dw        COUNT,TYPE          ; Display error message from throw
                    dw        CR
; TODO PRESET reinit data stack to top
                    dw        BRANCH,QUIT0        ; Interpret again

; ===========================================================================
; Tools
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY _type ( b u -- ) - display a string. filter non-printing characters.
                    #SEG0
word_UTYPE
                    dw        word_QUIT
                    fcb       5
                    fcs       "_TYPE"
UTYPE
                    dw        code_ENTER
                    dw        TOR                 ; start count down loop
                    dw        BRANCH,utyp2        ; skip first pass
utyp1
                    dw        DUP,CLOAD,TCHAR,EMIT  ; display only printable
                    dw        CHARP               ; increment address
utyp2
                    dw        JNZD,utyp1          ; loop till done
                    dw        DROP
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY dm+ ( a u -- a ) - dump u bytes from , leaving a+u on the stack.
                    #SEG0
word_DMP
                    dw        word_UTYPE
                    fcb       3
                    fcs       "dm+"
DMP
                    dw        code_ENTER
                    dw        OVER,IMM,4,UDOTR    ; display address
                    dw        SPACE,TOR           ; start count down loop
                    dw        BRANCH,pdum2        ; skip first pass
pdum1
                    dw        DUP,CLOAD,IMM,3,UDOTR  ; display numeric data
                    dw        INC                 ; increment address
pdum2
                    dw        JNZD,pdum1          ; loop till done
                    dw        RETURN

; ---------------------------------------------------------------------------
; TOOLS 15.6.1.1280 DUMP ( a u -- ) - dump u bytes from a, in a formatted manner.
                    #SEG0
word_DUMP
                    dw        word_DMP
                    fcb       4
                    fcs       "DUMP"
DUMP
                    dw        code_ENTER
                    dw        BASE,LOAD,TOR,HEX   ; save radix, set hex
                    dw        IMM,16,SLASH        ; change count to lines
                    dw        TOR                 ; start count down loop
dump1
                    dw        CR,IMM,16,DDUP,DMP
                    dw        ROT,ROT
                    dw        SPACE,SPACE,UTYPE   ; display printable characters
                    dw        NUFQ,NOT            ; user control
                    dw        BRANCHZ,dump2
                    dw        JNZD,dump1          ; loop till done
                    dw        BRANCH,dump3
dump2
                    dw        RFROM,DROP          ; cleanup loop stack, early exit
dump3
                    dw        DROP,RFROM,BASE,STORE  ; restore radix
                    dw        RETURN

; ---------------------------------------------------------------------------
; TOOLS 15.6.1.0220 .S ( -- ) - display the contents of the data stack.
word_DOTS
                    dw        word_DUMP
                    fcb       2
                    fcs       ".S"
DOTS
                    dw        code_ENTER
                    dw        CR,DEPTH            ; stack depth
                    dw        TOR                 ; start count down loop
                    dw        BRANCH,dots2        ; skip first pass
dots1
                    dw        RLOAD,PICK,DOT
dots2
                    dw        JNZD,dots1          ; loop till done
                    dw        SHOWSTR
                    fcb       4
                    fcs       " <sp"
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY !csp ( -- ) - save stack pointer in csp for error checking.
word_CSPSTORE
                    dw        word_DOTS
                    fcb       4
                    fcs       "!CSP"
CSPSTORE
                    dw        code_ENTER
                    dw        SPLOAD
                    dw        IMM,CSPP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY ?csp ( -- ) - abort if stack pointer differs from that saved in csp.
word_CSPCHECK
                    dw        word_CSPSTORE
                    fcb       4
                    fcs       "?CSP"
CSPCHECK
                    dw        code_ENTER
                    dw        SPLOAD
                    dw        IMM,CSPP
                    dw        LOAD
                    dw        XOR
                    dw        ABORTNZ
                    fcb       6
                    fcs       "stack!"
                    dw        RETURN

; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; TOOLS_EXT 15.6.2.2297 TRAVERSE-WORDLIST ( ... codeptr voc -- ... )
; TRAVERSE-WORDLIST-CALLBACK ( ... namecstr -- ... flag )
; Execute codeptr for each word in voc passing name ptr, stop when flag returns zero.
; When callback is executed the stack does not have anything,
; so previous items can be accessed.

word_TRWL
                    dw        word_CSPCHECK
                    fcb       17
                    fcs       "TRAVERSE-WORDLIST"
TRWL
                    dw        code_ENTER
wldo
                    dw        DUP                 ; cbcodeptr voc voc
                    dw        LOAD                ; cbcodeptr voc prev
                    dw        TOR                 ; cbcodeptr voc | R:prev
                    dw        OVER                ; cbcodeptr voc cbcodeptr | R:prev
                    dw        TOR                 ; cbcodeptr voc | R: prev cbcodeptr
                    dw        CELLP               ; cbcodeptr nameptr | R:prev cbcodeptr
                    dw        SWAP                ; nameptr cbcodeptr | R:prev cbcodeptr
                    dw        EXECUTE             ; flag | R:prev cbcodeptr
                    dw        BRANCHZ,wlabrt      ; jump if callback has returned false | R:prev cbcodeptr
                    dw        RFROM               ; cbcodeptr
                    dw        RFROM               ; cbcodeptr prev
                    dw        DUPNZ               ; cbcodeptr prev prev | cbcodeptr 0
                    dw        BRANCHZ,wlend       ; cbcodeptr prev | cbcodeptr, jmp if prev null

; previous is not null, look at prev word
                    dw        BRANCH,wldo         ; cbcodeptr prev
wlabrt
                    dw        RFROM               ; cbcodeptr |R:prev
                    dw        RFROM               ; cbcodeptr prev
                    dw        DROP                ; cbcodeptr
wlend
                    dw        DROP                ; --
                    dw        RETURN

; ---------------------------------------------------------------------------
; INTERNAL routine to print each word enumerated by TRAVERSE-WORDLIST
; (namecstr -- 0 [abort] | !0 [continue])
TRWL_TYPE
                    dw        code_ENTER
                    dw        DUP                 ; namecstr nameptr - keep a nameptr on list to continue enum
                    dw        COUNT               ; namecstr nameptr+1 len+flags
                    dw        IMM,WORD_LENMASK;   ; namecstr nameptr namelen+flags 0x3F
                    dw        AND                 ; namecstr nameptr namelen
                    dw        SPACE
                    dw        TYPE                ; namecstr - this is a true flag to continue enum
                    dw        RETURN

; ---------------------------------------------------------------------------
; TOOLS 15.6.1.2465 WORDS ( -- )
; Display the names in the context vocabulary.
word_WORDS
                    dw        word_TRWL
                    fcb       5
                    fcs       "WORDS"
WORDS
                    dw        code_ENTER
                    dw        CR
                    dw        IMM,TRWL_TYPE
                    dw        IMM,LASTP           ; cstr [pointer containing the address of the last word]
                    dw        LOAD                ; cstr voc[address of last word entry]
                    dw        TRWL                ; Run this word on all words of the list
                    dw        RETURN

; ===========================================================================
; Boot
; ===========================================================================

; ---------------------------------------------------------------------------
; PROPRIETARY CLEAR ( -- )
; ERASE in F2012 is a memclr
; Delete all added definitions.
word_CLEAR
                    dw        word_WORDS
                    fcb       5
                    fcs       "CLEAR"
CLEAR
                    dw        code_ENTER
                    dw        IMM,HERE_ZERO
                    dw        IMM,HEREP
                    dw        STORE
                    #ifdef    USE_SPI
                    dw        IMM,word_SPITRAN
                    #else
                    dw        IMM,word_hi
                    #endif
                    dw        IMM,LASTP
                    dw        STORE
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY VER ( -- u )
                    #SEG0
word_VER
                    dw        word_CLEAR
                    fcb       3
                    fcs       "VER"
VER
                    dw        code_ENTER
                    dw        IMM,0x100
                    dw        RETURN

; ---------------------------------------------------------------------------
; PROPRIETARY hi ( -- )
                    #SEG0
word_hi
                    dw        word_VER
                    fcb       2
                    fcs       "hi"
hi
                    dw        code_ENTER
                    dw        CR
                    dw        SHOWSTR
                    fcb       16
                    fcs       "sys11 forth ver "
                    dw        BASE,LOAD,TOR
                    dw        HEX,VER,BDIGS,DIG,DIG,IMM,'.',HOLD,DIG,EDIGS,TYPE
                    dw        RFROM,BASE,STORE
                    dw        CR
                    dw        RETURN

                    #ifdef    USE_SPI
; ===========================================================================
; SPI Bus
; ===========================================================================

; ---------------------------------------------------------------------------
; ( -- )
word_SPIINIT
                    dw        word_hi
                    fcb       7
                    fcs       "SPIINIT"
SPIINIT
                    dw        code_SPIINIT
                    #ROM
code_SPIINIT
                    jmp       NEXT

; ---------------------------------------------------------------------------
; ( n -- )
                    #SEG0
word_SPISEL
                    dw        word_SPIINIT
                    fcb       6
                    fcs       "SPISEL"
SPISEL
                    dw        code_SPISEL
                    #ROM
code_SPISEL
                    jmp       NEXT

; ---------------------------------------------------------------------------
; ( tx -- rx )
                    #SEG0
word_SPIEXCH
                    dw        word_SPISEL
                    fcb       7
                    fcs       "SPIEXCH"
SPIEXCH
                    dw        code_SPIEXCH
                    #ROM
code_SPIEXCH
                    jmp       NEXT

; ---------------------------------------------------------------------------
; ( n adrtx adrrx -- )
                    #SEG0
word_SPITXRX
                    dw        word_SPIEXCH
                    fcb       7
                    fcs       "SPITXRX"
SPITXRX
                    dw        code_SPITXRX
                    #ROM
code_SPITXRX
                    jmp       NEXT

; ---------------------------------------------------------------------------
; ( n adr --) 0 SPITXRX
                    #SEG0
word_SPISEND
                    dw        word_SPITXRX
                    fcb       7
                    fcs       "SPISEND"
SPISEND
                    dw        code_ENTER
                    dw        IMM,0
                    dw        SPITXRX
                    dw        RETURN

; ---------------------------------------------------------------------------
; ( n adr --) 0 SWAP SPITXRX
                    #SEG0
word_SPIRECV
                    dw        word_SPISEND
                    fcb       7
                    fcs       "SPIRECV"
SPIRECV
                    dw        code_ENTER
                    dw        IMM,0
                    dw        SWAP
                    dw        SPITXRX
                    dw        RETURN

; ---------------------------------------------------------------------------
; ( n adr --) DUP SPITXRX
                    #SEG0
word_SPITRAN
                    dw        word_SPIRECV
                    fcb       7
                    fcs       "SPITRAN"
SPITRAN
                    dw        code_ENTER
                    dw        DUP
                    dw        SPITXRX
                    dw        RETURN
                    #endif

                    #ifdef    USE_BLOCK
; ===========================================================================
; F2012 BLOCK word set
; This extension uses an SPI EEPROM through the SPI words
; ===========================================================================

; ---------------------------------------------------------------------------
; BLOCK 7.6.1.0790 BLK ( -- addr )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.0800 BLOCK ( u -- addr )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.0820 BUFFER ( u -- addr )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.1559 FLUSH ( -- )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.1790 LOAD ( ... u -- ... )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.2180 SAVE-BUFFERS ( -- )
; ---------------------------------------------------------------------------
; BLOCK 7.6.1.2400 UPDATE ( -- )
; ---------------------------------------------------------------------------
; BLOCK 7.6.2.1330 EMPTY-BUFFERS ( -- )
; ---------------------------------------------------------------------------
; BLOCK 7.6.2.1770 LIST ( u -- )
; ---------------------------------------------------------------------------
; BLOCK 7.6.2.2125 REFILL ( -- flag )
; ---------------------------------------------------------------------------
; BLOCK 7.6.2.2190 SCR ( -- addr )
; ---------------------------------------------------------------------------
; BLOCK 7.6.2.2280 THRU ( ... first last -- ... )
; ---------------------------------------------------------------------------
                    #endif

; ---------------------------------------------------------------------------
; Main forth interactive interpreter loop
; There is no header and no code pointer. This is not really a valid word.
                    #DATA
BOOT
                    dw        IOINIT              ; Setup HC11 uart
                    dw        CONSOLE             ; Setup IO vectors
                    dw        DECIMAL             ; Setup environment
                    dw        CLEAR               ; Setup HERE and LAST
                    dw        hi                  ; Show a startup banner

                    dw        QUIT

                    #Uses     vectors.inc
