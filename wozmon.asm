; Ported to  Bare-ROM z80 systems By Dr. Peker 2025
; Monitor can run on standalone z80 systems. All CP/M depencencies removed.
; BUFFER structure changed to run in flexible memory locations.
; Backspace BUG corrected.
; uses 8251 for serial I/O but also 6551 or 6850 can be used with minor changes.
; 8251 init code added and must be called once before using the monitor to run as
; standalone system.
; Wozmon for Z80 By Dr.Peker 2025
; inspired code from  Christian Welzel 2023
; Christian Welzel 2023 - www.welzel-online.ch
; 
; Code can be compiled in Zasm assembler by following parameters:
;./ zasm -uwy --target=ram -x wozmon.asm -o wozmon.hex

; Optional "Wozmon" greeting at start-up added. (can be removed to save space)
;               

; Keys and Characters
;
BS		.EQU 	$08					; Backspace
LF		.EQU 	$0A					; Line Feed
CR		.EQU	$0D					; Carriage Return
ESC		.EQU 	$1B					; Escape
FF		.EQU 	$0C					; Form feed 
SPC		.EQU 	' '					; Space
DOT		.EQU 	'.'					; Period
COLON	.EQU 	':'					; Colon  
BACKSL	.EQU 	$5C					; back slash 
CTRLQ   .EQU 	$11					; Ctrl-Q to enter SYMON monitor

stack_top .EQU $FFFF

QUIT .EQU  $0072                  ; QUIT address for Efex prompt
;Change Quit adress as needed. 

XAML	.EQU 	$b400				; start at free space in upper RAM
XAMH	.EQU 	XAML + 1
STL		.EQU 	XAMH + 1
STH		.EQU 	STL + 1
L		.EQU 	STH + 1
H		.EQU 	L + 1
YSAV	.EQU 	H + 1
MODE	.EQU 	YSAV + 23
COUNTR	.EQU 	MODE + 3
SADDR   .EQU 	COUNTR + 3  

INBUF    .EQU  $B000                  ; Input buffer

; I/O port addresses
               
                org $A000
                
                jp      RESET

;
; input buffer
;


RESET
                ld		sp, stack_top   ; Init Stack
                ld      iy, $B000
                ;ld      a, $00          ; Reset A
                ld a, $0c    ; Form feed
                call OUTCH
                ld a, $0a    ; Form feed
                call OUTCH
                ld a, $07    ; BELL
                call OUTCH
                ld a, 'W'    ; Form feed
                call OUTCH
                ld a, 'o'    ; Form feed
                call OUTCH
                ld a, 'z'    ; Form feed
                call OUTCH
                ld a, 'm'   ; Form feed
                call OUTCH
                ld a, 'o'    ; Form feed
                call OUTCH
                ld a, 'n'   ; Form feed
                call OUTCH
                 ld a, $0a    ; Form feed
                call OUTCH
                ld a, $0d    ; Form feed
                call OUTCH
                

                ld      a, 00          ; Clear MODE (XAM mode).


NOTCR
                cp      $08             ; Backspace key?
                jr      z, BACKSPACE
                cp      $1B             ; ESC?
                jr      z, ESCAPE       ; Yes.
                ;inc     iy              ; Advance text index.

                ld      b, iyl          ; Auto ESC if line longer than 127.
                bit     7, b            ;   ...
                jr      z, NEXTCHAR     ;     jp if bit 7 is not set (plus)

ESCAPE          ld      a, 	$FF			; clear screen
     		    call 	ECHO
                ld      a, $5C          ; "\".
                call    ECHO            ; Output it.

GETLIN
                ld      a, $0D          ; Send CR
                call    ECHO
                ld      a, $0A          ; Send LF
                call    ECHO
                ld      iy, INBUF + 1    ; Initialize text index.


BACKSPACE       dec     iy              ; Back up text index.
                ld      b, iyl          ; Beyond start of line, reinitialize.
                bit     7, b            ;   ...
                jr      nz, GETLIN     ;     jp if bit 7 is set (minus)
                ld      a, SPC           ; Send space (overwrite)
                call    ECHO
                ld      a, BS           ; and Backspace again.
                call    ECHO

NEXTCHAR
                CALL GETCH   ; Read a char from "virtual" UART
                cp      $FF             ; Char? ($FF from UART = no char)
                jr      z, NEXTCHAR     ; No, loop until valid character
                cp      $08             ; Backspace key?
                call      z, BACKPACK
                inc     iy 
                ld      (iy), a      ; Add to text buffer.
                call    ECHO            ; Display character.
                cp      $0D             ; CR?
                jr      nz, NOTCR       ; No.

                ld      iy, INBUF        ; Reset text index. (DEFAULT ınbuf)
                ld      a, $00          ; For XAM mode.
                ld      ixl, a          ; TAX  ; X=0.
SETBLOCK
                sla     a
SETSTOR
                sla     a               ; Leaves $7B if setting STOR mode.
                ld      (MODE), a       ; $00 = XAM, $74 = STOR, $B8 = BLOK XAM.
BLSKIP
                inc     iy              ; Advance text index.
NEXTITEM
                ld      a, (iy)      ; Get character.
                cp      $0D             ; CR?
                jr      z, GETLIN      ; Yes, done this line.
                cp      $2E             ; "."?
                jr      c, BLSKIP       ; Skip delimiter.
                jr      z, SETBLOCK     ; Set BLOCK XAM mode.
                cp      $3A             ; ":"?
                jr      z, SETSTOR      ; Yes, set STOR mode.
                cp      $52             ; "R"?
                jr      z, RUN          ; Yes, run user program.
                cp      'Q'           ; "Q"?
                jp      z, QUIT            ; Yes, quit to user program.
                ld      hl, $0000       ; $00 -> L and H.
                ld      (YSAV), iy      ; Save Y for comparison

NEXTHEX
                ld      a, (iy)      ; Get character for hex test.
                xor     $30             ; Map digits to $0-9.
                cp      $0A             ; Digit?
                jr      c, DIG          ; Yes.
                adc     a, $89          ; Map letter "A"-"F" to $FA-FF.
                cp      $F9             ; Hex letter?
                jr      c, NOTHEX       ; No, character not hex.
DIG
                sla     a
                sla     a               ; Hex digit to MSD of A.
                sla     a
                sla     a

                ld      b, $04          ; Shift count.
HEXSHIFT
                rla                     ; Hex digit left, MSB to carry.
                rl      l               ; Rotate into LSD.
                rl      h               ; Rotate into MSD's.
                djnz    HEXSHIFT        ; Done 4 shifts?
                                        ; No, loop.
                inc     iy              ; Advance text index.
                jr      NEXTHEX         ; Always taken. Check next character for hex.

NOTHEX
                ld      b, iyl          ; Check if L, H empty (no hex digits). 
                ld      a, (YSAV)       ;   ...
                cp      b               ;     ...
                jp      z, ESCAPE       ; Yes, generate ESC sequence.

                ld      a, (MODE)       ; Load MODE to A
                bit     6, a            ; Test MODE byte: "overflow" bit
                jr      z, NOTSTOR      ; B6=1 is STOR, 0 is XAM and BLOCK XAM.

                ld      a, l            ; LSD's of hex data.
                ld      (de), a         ;   ...
                inc     de              ; Increment store index.

TONEXTITEM     jr      NEXTITEM        ; Get next command item.

RUN
                jp      (ix)            ; Run at current XAM index.

NOTSTOR
                bit     7, a
                jr      nz, XAMNEXT     ; B7 = 0 for XAM, 1 for BLOCK XAM.
SETADR         ld      de, hl          ; Copy hex data to 'store index'.
                push    hl              ; And to 'XAM index'.
                pop     ix              ;   ...
                cp      a               ; Set zero flag

NXTPRNT
                jp      nz, PRDATA      ; NE means no address to print.
                ld      a, $0D          ; CR.
                call    ECHO            ; Output it.
                ld      a, $0A          ; LF.
                call    ECHO            ; Output it.
                ld      a, ixh          ; 'Examine index' high-order byte.
                call    PRBYTE          ; Output it in hex format.
                ld      a, ixl          ; Low-order 'examine index' byte.
                call    PRBYTE          ; Output it in hex format.
                ld      a, $3A          ; ":".
                call    ECHO            ; Output it.

PRDATA
                ld      a, $20          ; Blank.
                call    ECHO            ; Output it.
                ld      a, (ix)         ; Get data byte at 'examine index'.
                call    PRBYTE          ; Output it in hex format.
XAMNEXT        ld      a, $00
                ld      (MODE), a       ; 0 -> MODE (XAM mode).
                ld      a, ixl
                cp      l               ; Compare 'examine index' to hex data.
                ld      a, ixh
                sbc     a, h
                jr      nc, TONEXTITEM  ; Not less, so no more data to output.

                inc     ix              ; Increment 'examine index'.

MOD8CHK
                ld      a, ixl          ; Check low-order 'examine index' byte
                and     a, $07          ; For MOD 8 = 0
                jr      NXTPRNT         ; Always taken.

PRBYTE
                push    af              ; Save A for LSD.
                srl     a
                srl     a
                srl     a               ; MSD to LSD position.
                srl     a
                call    PRHEX           ; Output hex digit.
                pop     af              ; Restore A.

PRHEX
                and     $0F             ; Mask LSD for hex print.
                or      $30             ; Add "0".
                cp      $3A             ; Digit?
                jr      c, ECHO         ; Yes, output it.
                adc     $07             ; Add offset for letter.

ECHO
                CALL OUTCH             ; Output character in A.
                ret                     ; Return.
;Backspace bug removed with this code below
BACKPACK        DEC    iy              ; Back up text index.
                ld      a, SPC           ; Send space (overwrite)
                call    ECHO
                ld      a, BS           ; and Backspace again.
                call    ECHO
                RET


INIT8251	

        LD A,040H            
        OUT (001H),A  
        NOP
	    NOP  
        LD A,040H            
        OUT (001H),A
	    NOP
	    NOP  
                  
        LD A,040H            
        OUT (001H),A  ; WORST CASE INIT
	    NOP
	    NOP
	
	
	
        LD A,040H            
        OUT (001H),A  		; RESET 8251
        NOP
	    NOP

        LD A,04EH            
        OUT (001H),A      	 ; 8-N-1  CLK/1  4EH FOR /16, 4D FOR /1  (MODE )
        NOP
	    NOP    
        LD A,037H            
        OUT (001H),A      	 ; RESET ALL ERROR FLAGS AND ENABLE RXRDY,TXRDY (COMMAND)

        
;-----------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------
	
;TXD ROUTINE sends contents of A REGISTER  to serial out pin 
;19200 BAUD, 8-N-1


OUTCH    PUSH AF
LOPTX 	IN A,(01H)
	AND 001H     ;TXRDY?
	JP Z, LOPTX
        POP AF
	OUT (00H),A
        RET
	
;----------------------------------------------------------------------------------
;RXD ROUTINE receives 1 bayt from serial  to A REGISTER 
;19200 BAUD, 8-N-1

GETCH    
	LD A,037H
	OUT (01),A ;ENABLE TX AND RX AND CLEAR ERR BITS
	
LOPTR 	IN A,(01H)
	AND 002H     ;TXRDY?
	JP Z, LOPTR
        IN A,(00H)   ;RECEIVE CHAR
	RET

    DB 0,0,0,0,0,0,0,0
	

	END
