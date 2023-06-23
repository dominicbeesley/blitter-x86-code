[map all]


	; TODO: Norton utilities get stuck reading io 3DAh - VGA?!?/CGA control/status - make it toggle bits in
	;	fb_cpu80188.vhd to fool programs that wait for retrace?


%ifdef BOARD_18x
	cpu 186
	CLOCK_SPEED	equ	16000000
	%include "hardware_18x.inc"
%elifdef BOARD_386ex
	cpu 386
	CLOCK_SPEED	equ	32000000
	%include "hardware_386ex.inc"
%else
	%error "No board config detected"
%endif


%macro	DBG_C	1
		pushf
		push	AX
		mov	AL,%1
		call	deice_print_char
		pop	AX
		popf
%endmacro

%macro	DBG_STR 1
		call	deice_printstr
		db	%1
		db	0
%endmacro

%macro	M_EOI 0
	%ifdef BOARD_18x
		mov	AX,08000h
		mov	DX,io_PCB_EOI
		out	DX,AL			; EOI
	%elifdef BOARD_386ex
		mov	AL,VAL_EOI
		mov	DX,OCW2M
		out	(DX),AL
	%endif
%endmacro

%macro	M_EOI2 0
	%ifdef BOARD_18x
		mov	AX,08000h
		mov	DX,io_PCB_EOI
		out	DX,AL			; EOI
	%elifdef BOARD_386ex
		mov	AL,VAL_EOI
		mov	DX,OCW2S
		mov	DX,OCW2M
		out	(DX),AL
	%endif
%endmacro

%include "hardware_beeb.inc"


		
		SEG_ROMBASE 	equ 	0xFC00
		SEG_RESET 	equ 	0xFFFF
		SEG_SCREEN	equ	0xF300

		; boot time stack
		SEG_INIT_STACK	equ	0x30
		INIT_STACK_TOS	equ	0x80

BOOT_LOCN	equ		7C00H

		section interrupts nobits start=0
		SEG_INT		equ	0x00
INT_DIV0:	resd		1
INT_RESV:	resd		1
NMI_PTR: 	resd		1
INT3_PTR:	resd		1
INTO_PTR:	resd		1
INT_BOUND:
INT5_PTR:	resd		1
INT_OPCODE:	resd		1
INT_WAIT:	resd		1
INT_DOUBLE:
INT_PTR: 	resd		1
		resd		7
VIDEO_INT:	resd		1
		resd		7
BASIC_PTR:	resd		1		; cassette BASIC entry point
		resd		4
PARM_PTR:	resd		1		; video parameters pointer
DISK_POINTER:	resd		1
EXT_PTR:	resd		1		; extension routine pointer
		resd		1
IO_ROM_INIT:	resw		1		
IO_ROM_SEG:	resw		1		; option ROM SEG
	

		section biosdata nobits start=0 absolute=0x40
		SEG_BIOS_DATA	equ	0x40
;----------------------------------------
;	 ROM BIOS DATA AREAS		:
;----------------------------------------
RS232_BASE:	resw	4		; ADDRESSES OF RS232 ADAPTERS
PRINTER_BASE:	resw	4		; ADDRESSES OF PRINTERS
EQUIP_FLAG:	resw	1		; INSTALLED HARDWARE
MFG_TST: 	resb	1		; INITIALIZATION FLAG
MEMORY_SIZE:	resw	1		; MEMORY SIZE IN K BYTES
IO_RAM_SIZE:	resw	1		; MEMORY IN I/O CHANNEL
;----------------------------------------
;	   KEYBOARD DATA AREAS		:
;----------------------------------------
KB_FLAG: 	resb	1		; toggles when a shift key is depressed
KB_FLAG_1:	resb	1		; reflects if a "shift" key is depressed

;----- SHIFT FLAG EQUATES WITHIN KB_FLAG/1

INS_STATE	EQU	80H		; INSERT STATE IS ACTIVE
CAPS_STATE	EQU	40H		; CAPS LOCK STATE HAS BEEN TOGGLED
NUM_STATE	EQU	20H		; NUM LOCK STATE HAS BEEN TOGGLED
SCROLL_STATE	EQU	10H		; SCROLL LOCK STATE HAS BEEN TOGGLED
ALT_SHIFT	EQU	08H		; ALTERNATE SHIFT KEY DEPRESSED
CTL_SHIFT	EQU	04H		; CONTROL SHIFT KEY DEPRESSED
LEFT_SHIFT	EQU	02H		; LEFT SHIFT KEY DEPRESSED
RIGHT_SHIFT	EQU	01H		; RIGHT SHIFT KEY DEPRESSED

ALT_INPUT:	resb	1		; STORAGE FOR ALTERNATE KEYPAD ENTRY
BUFFER_HEAD:	resw	1		; POINTER TO HEAD OF KEYBOARD BUFFER
BUFFER_TAIL:	resw	1		; POINTER TO TAIL OF KEYBOARD BUFFER
KB_BUFFER:	resw	16		; ROOM FOR 15 ENTRIES
KB_BUFFER_END:

;----- HEAD = TAIL INDICATES THAT THE BUFFER IS EMPTY

NUM_KEY 	EQU	69		; SCAN CODE FOR NUMBER LOCK
SCROLL_KEY	EQU	70		; SCROLL LOCK KEY
ALT_KEY 	EQU	56		; ALTERNATE SHIFT KEY SCAN CODE
CTL_KEY 	EQU	29		; SCAN CODE FOR CONTROL KEY
CAPS_KEY	EQU	58		; SCAN CODE FOR SHIFT LOCK
LEFT_KEY	EQU	42		; SCAN CODE FOR LEFT SHIFT
RIGHT_KEY	EQU	54		; SCAN CODE FOR RIGHT SHIFT
INS_KEY 	EQU	82		; SCAN CODE FOR INSERT KEY
DEL_KEY 	EQU	83		; SCAN CODE FOR DELETE KEY

;----------------------------------------
;	DISKETTE DATA AREAS		:
;----------------------------------------
SEEK_STATUS:	resb	1		; drive recalibration status
					; BIT 1-0 = if bits 1/0 set then B/A needs a recal
					; BIT 7 is an interrupt occurrence flag - not used by us!?
		resb	1		; - motor status on NEC, not used by us
		resb	1		; - motor tick count, not used by us
MOTOR_WAIT	EQU	37		; TWO SECONDS OF COUNTS FOR MOTOR TURN OFF


DISKETTE_STATUS:resb	1		; last command return code
TIME_OUT	EQU	80H		; disk controller failed to respond
BAD_SEEK	EQU	40H		; seek operation failed
BAD_NEC 	EQU	20H		; controller has failed
BAD_CRC 	EQU	10H		; bad crc on diskette read
DMA_BOUNDARY	EQU	09H		; attempt to dma across 64k boundary
BAD_DMA 	EQU	08H		; dma overrun on operation
RECORD_NOT_FND	EQU	04H		; sector not found
WRITE_PROTECT	EQU	03H		; write protect
BAD_ADDR_MARK	EQU	02H		; address mark not found - not sure how this differs to sector not found on 1770?
BAD_CMD 	EQU	01H		; bad command passed to diskette i/o

		; the next 7 bytes are different to PC
		; the 1770 doesn't remember the track for each drive
		; so we need to check if the track register is set up correctly 
		; and restore it ourselves
DSK_PREV_DRV	resb	1		; drive of previous command, if different need to restore track register
DSK_TRACK_TBL	resb	2		; drive A, B track number

		resb	4		;absolute	49H
;----------------------------------------
;	VIDEO DISPLAY DATA AREA 	:
;----------------------------------------
CRT_MODE	resb	1		; CURRENT CRT MODE
CRT_COLS	resw	1		; NUMBER OF COLUMNS ON SCREEN
CRT_LEN 	resw	1		; LENGTH OF REGEN IN BYTES
CRT_START	resw	1		; STARTING ADDRESS IN REGEN BUFFER
CURSOR_POSN	resw	8		; CURSOR FOR EACH OF UP TO 8 PAGES
CURSOR_MODE	resw	1		; CURRENT CURSOR MODE SETTING
ACTIVE_PAGE	resb	1		; CURRENT PAGE BEING DISPLAYED
NA_ADDR_6845	resw	1		; BASE ADDRESS FOR ACTIVE DISPLAY CARD -- NOT USED!
CRT_MODE_SET	resb	1		; CURRENT SETTING OF THE 3X8 REGISTER
CRT_PALETTE	resb	1		; CURRENT PALETTE SETTING COLOR CARD

		resb	5		;absolute	6Ch
;----------------------------------------
;	    TIMER DATA AREA		:
;----------------------------------------
TIMER_LOW	resw	1		; LOW WORD OF TIMER COUNT
TIMER_HIGH	resw	1		; HIGH WORD OF TIMER COUNT
TIMER_OFL	resb	1		; TIMER HAS ROLLED OVER SINCE LAST READ







		resb	15		; absolute	80H	
;----------------------------------------
;	EXTRA KEYBOARD DATA AREA	:
;----------------------------------------
BUFFER_START	resw	1
BUFFER_END	resw	1


		resb	2		; absolute	86H	; BBC KEYBOARD stuff uses PCjr slots
KB_BBC_ROLL	resb	2		; 2 key roll over buffer


		section .text
font:
		incbin	"font.bin"


	%ifdef BOARD_386ex	
word_tab:	

		DW	UCSADL, 0FF80h	; set memory to manual READY 0 wait states

		DW	0FFFFh		; end marker

%macro		M_BYT 2
		DW	%1
		DB	%2
%endmacro

byte_tab:	M_BYT	INTCFG, 0

		M_BYT	ICW1M, 	010h	; edge sensitive
		M_BYT	ICW1S, 	010h	; edge sensitive

		M_BYT	ICW2M, 	020h	; base interrupt #
		M_BYT	ICW2S, 	070h	; base interrupt #

		M_BYT	ICW3M, 	004h	; slave cascade
		M_BYT	ICW3S, 	002h	; slave ID

		M_BYT	ICW4M, 	001h	; ?
		M_BYT	ICW4S, 	001h	; ?

		M_BYT	OCW1M, 	0FFh	; Mask all interrupts!
		M_BYT	OCW1S, 	0FFh	; Mask all interrupts!

		DW	0FFFFh		; end marker

	%endif

_start:
handle_res:	

		cli
		cld

%ifdef BOARD_386ex

		; enable extended I/O with DOS reflection

		in	AL,REMAPCFGH	; reset the state machine (state A)
        	mov	AX,0x8000
        	out	REMAPCFGH,AL	; move to (state B)
        	xchg	AH,AL
        	out	REMAPCFGL,AL	; move to (state C)
        	out	REMAPCFG,AX	; move to (state D), sets ESE bit

		mov	AX,CS
		mov	DS,AX		; point DS:SI at table
		mov	SI,word_tab

.initwlp:	lodsw			; get port #
		mov	DX,AX
		inc	AX
		jz	.initwsk
		lodsw
		out	DX,AX
		jmp	.initwlp
.initwsk:

		mov	SI,byte_tab

.initblp:	lodsw			; get port #
		mov	DX,AX
		inc	AX
		jz	.initbsk
		lodsb
		out	DX,AL
		jmp	.initblp
.initbsk:


%endif		

		; set up stack and data

		mov	AX,SEG_INIT_STACK		; Get stack segment
		mov	SS,AX	
		mov	SP,INIT_STACK_TOS		; put SP at TOS


		; initialise DEICE
		call	deice_init

		DBG_STR	`DeIce started...\n`

		; set up sys via

		mov	AL,0FH
		mov	DX,io_SHEILA_SYSVIA_DDRB
		out	DX,AL

		; set latches 6..0 to 1
		mov	DX,io_SHEILA_SYSVIA_ORB
.llp:		dec	AL
		out	DX,AL
		call	snd_wait_8
		cmp	AL,9
		jnc	.llp

		; set DDRA of user via to all outputs
		mov	DX,io_SHEILA_USRVIA_DDRA
		mov	AL,0FFh
		out	DX,AL

		; disable VIA interrupts and clear flags
		mov	AL,07Fh

		mov	DX,io_SHEILA_USRVIA_IER
		out	DX,AL
		dec	DX
		out	DX,AL

		mov	DX,io_SHEILA_SYSVIA_IER
		out	DX,AL
		dec	DX
		out	DX,AL

		inc	DX
		mov	AL,0F2h				; reenable interrupts for T1,T2,CB1,CA1
		out	DX,AL
        	
        	mov	AL,004h
        	mov	DX,io_SHEILA_SYSVIA_PCR		
        	out	DX,AL				; CB2=in-neg, CB1=neg, CA2=in-pos, CA1=neg

        	mov	AL,060h
        	mov	DX,io_SHEILA_SYSVIA_ACR		
        	out	DX,AL				; T1I=cont, T2=pulse count, SR=disabled, PA/B latch=disabled

        	mov	AL,00Eh
        	mov	DX,io_SHEILA_SYSVIA_T1LL
        	out	DX,AL



        	; TODO 
;        	sta     sheila_USRVIA_pcr               ; DA94 8D 6C FE                 .l.
;        	sta     LFEC0                           ; DA97 8D C0 FE                 ...
;        	cmp     sheila_USRVIA_pcr               ; DA9A CD 6C FE                 .l.
;        	beq     LDAA2                           ; DA9D F0 03                    ..
;        	inc     sysvar_USERVIA_IRQ_MASK_CPY     ; DA9F EE 77 02                 .w.
;LDAA2:

	  	mov	AL,027h
        	mov	DX,io_SHEILA_SYSVIA_T1LH
        	out	DX,AL				; Timer1 = 270E i.e. 100Hz
        	sub	DX,2
        	out	DX,AL				; and out to counter L

        	call    snd_init                        ; DAAA 20 60 EC                  `.


        	; TODO serial
;        	lda     sysvar_SERPROC_CTL_CPY          ; DAAD AD 82 02                 ...
;        	and     #$7F                            ; DAB0 29 7F                    ).
;        	jsr     setSerULA                       ; DAB2 20 A7 E6                  ..


		; TODO: self test


		; set up BIOS interrupt vectors to dummy interrupt [l=595]
		sub	DI,DI
		mov	DS,DI				; point DS at 0
		mov	ES,DI				; point ES at 0

		mov	CX,32
		cld
D3:		mov	AX,DUMMY_RETURN
		stosw
		mov	AX,CS
		stosw
		loop	D3

		; TODO: check this is right
		; zero rest of first 64k ram
		mov	cx,08000h-64
		xor	ax,ax
		rep	stosw


		;TODO: setup NMI, PRINT_SCREN, Cassette BASIC?

		;TODO: spurious interrupts checking (l=636)


		;Setup BIOS subroutines from VECTOR_TABLE, note all are in current CS
		push	DS
		mov	DI,VIDEO_INT
		push	CS
		pop	DS
		mov	SI,VECTOR_TABLE+10H
		mov	CX,16
E1A:		movsw
		inc	DI		;CS part of pointers already init'd to D11
		inc	DI
		loop	E1A
		pop	DS

		;TODO: check mode etc

		mov	AH,0
		mov	AL,3
		int	10H		;force 80x25 mode


		;Setup rest of INT table (l=1034)

		sub	AX,AX
		mov	ES,AX
		mov	CX,8			
		push	DS			
		push	CS			
		pop	DS
		mov	SI,VECTOR_TABLE
		mov	DI,INT_PTR
F7A:		movsw
		inc	DI		;CS part of pointers already init'd to D11
		inc	DI
		loop	F7A

		; hard-coded other interrupts
		; TODO:!

		;TODO: this is a bodge/test
		mov	word [ES:7*4],INT7_HANDLE

		mov	word [ES:NMI_PTR],DEICE_NMI_INT	; NMI INTERRUPT - debug button !
		;MOV	ES:INT5_PTR,OFFSET PRINT_SCREEN	; PRINT SCREEN

		; TODO: not got cassette basic yet so just print error message
		mov	word [ES:BASIC_PTR],BOOT_FAIL_PRINT
		mov	word [ES:BASIC_PTR+2],CS	; SEGMENT FOR CASSETTE BASIC


		pop	DS

		;TODO: option rom stuff?


		;TODO disk check here


		;TODO: bodge equipment flag
		call	DDS
		mov	word [EQUIP_FLAG], 004Dh


		mov	SI,KB_BUFFER
		mov	[BUFFER_HEAD],SI		; SETUP KEYBOARD PARAMETERS
		mov	[BUFFER_TAIL],SI
		mov	[BUFFER_START],SI
		add	SI,KB_BUFFER_END-KB_BUFFER
		mov	[BUFFER_END],SI

	%ifdef BOARD_18x
		; Test Timer 0+2 interrupt

TIMER_PRESCALE	equ	4
TIMER_DIVIDER   equ	(CLOCK_SPEED*10)/(4*TIMER_PRESCALE*182)

		; make timer 2 continuous 4 cycles loop, no interrupt
		mov	DX,io_PCB_T2CNT
		mov	AX,00000h
		out	DX,AL
		mov	DX,io_PCB_T2CMPA
		mov	AX,TIMER_PRESCALE
		out	DX,AL


		mov	DX,io_PCB_T2CON
		mov	AX,0C001h
		out	DX,AL


		; make timer 0 continuous 54925 cycles loop, with interrupt and prescaler
		mov	DX,io_PCB_T0CNT
		mov	AX,00000h
		out	DX,AL
		mov	DX,io_PCB_T0CMPA
		mov	AX,TIMER_DIVIDER
		out	DX,AL

		mov	DX,io_PCB_TCUCON
		mov	AX,00000h
		out	DX,AL

		mov	DX,io_PCB_PRIMSK
		mov	AX,00000h
		out	DX,AL

		mov	DX,io_PCB_IMASK
		mov	AX,00000h
		out	DX,AL

		mov	DX,io_PCB_T0CON
		mov	AX,0E009h
		out	DX,AL
	%endif


;=============================================================================
;		80C188 is running enter a test program
;=============================================================================

		sti
		cld

		mov	BL,1
		call	BEEP


		mov	AX,CS
		mov	DS,AX
		mov	SI,boot_string

		mov	CX,boot_string_len

.lp		lodsb
		mov	AH,00Eh
		int	10h
		loop	.lp

;; Attributes test
		mov	BL,0
		mov	BH,0
		mov	CX,100h
.lpA:		push	CX

		; explicit position
		mov	AL,BL
		and	AL,0Fh
		mov	AH,4
		mul	byte AH
		inc	AL
		inc	AL
		mov	DL,AL
		mov	DH,BL
		mov	CL,4
		shr	DH,CL
		mov	AH,02h		;set CPOS
		int	10h


		mov	AX,0923h	; write CH+ATTR ' '
		mov	CX,3
		int	10h
		mov	AL,BL
		call	XPC_BYTE

		pop	CX		
		inc	BL
		test	BL,0Fh
		jnz	.skA
		call	tst_crlf
.skA:		loop	.lpA
		call	tst_crlf


		; setup deice interrupts
		xor	AX,AX
		mov	ES,AX
		mov	AX,DEICE_INT1
		mov	[ES:0004h], AX	; trace interrupt

		mov	AX,DEICE_INT3
		mov	[ES:000Ch], AX	; trace interrupt

		; enter deice monitor

		mov	AX,0DEADh
		mov	ES,AX
		mov	AX,0BEEFh
		mov	DS,AX

		mov	AX,SP
		mov	BX,SS
		mov	CX,09ABCh
		mov	DX,0DEF0h
		mov	BP,0B99Bh
		mov	SI,05115h
		mov	DI,0D11Dh

		int	19h


tst_crlf:
		mov	BH,0
		mov	AX,0E0Dh
		int	10h
		mov	AX,0E0Ah
		int	10h
		ret


boot_string:	db	"Ishbel BIOS 3/19/2022",13,10,13,10
boot_string_len equ	$-boot_string

;------------------------------------------------------------------------
;FDC1770_RESET								:
;CORRUPTS	AL,DX							:
;------------------------------------------------------------------------
FDC1770_RESET:	mov	AL,0
		jmp	short	FDC_CTL_WRITE

;------------------------------------------------------------------------
;FDC1770_DRVSEL								:
;INPUT		AL							:
;	Sets drive to low bit of DL A=0/B=1				:
;CORRUPTS	AL,DX							:
;------------------------------------------------------------------------
FDC1770_DRVSEL:	
		mov	AL,DL
		inc	AL
		and	AL,03h
		test	DH,1
		jz	.sk
		or	AL,04h		; side
.sk		or	AL,20h


;------------------------------------------------------------------------
;FDC1770_CTL_WRITE							:
;INPUT		AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_CTL_WRITE:
		push	DX
		mov	DX,io_SHEILA_1770_CTL
		out	DX,AL
		pop	DX
		ret

;------------------------------------------------------------------------
;FDC1770_CMD_WRITE							:
;INPUT		AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_CMD_WRITE:
		push	DX
		mov	DX,io_SHEILA_1770_CMD
FDC_WRITEANDWAIT:
		out	DX,AL

		call	snd_wait_8

		pop	DX
		ret
		

;------------------------------------------------------------------------
;FDC1770_SEC_WRITE							:
;INPUT		AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_SEC_WRITE:
		push	DX
		mov	dx,io_SHEILA_1770_SEC
		jmp	short	FDC_WRITEANDWAIT

;------------------------------------------------------------------------
;FDC1770_TRK_WRITE							:
;INPUT		AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_TRK_WRITE:
		push	DX
		mov	dx,io_SHEILA_1770_TRK
		jmp	short	FDC_WRITEANDWAIT


;------------------------------------------------------------------------
;FDC1770_DAT_WRITE							:
;INPUT		AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_DAT_WRITE:
		push	DX
		mov	dx,io_SHEILA_1770_DAT
		jmp	short	FDC_WRITEANDWAIT

;------------------------------------------------------------------------
;FDC1770_DAT_READ							:
;INPUT		NONE							:
;OUTPUTS	AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_DAT_READ:
		push	DX
		mov	dx,io_SHEILA_1770_DAT
FDC_in_DX:
		in	AL,DX

		call	snd_wait_8

		pop	DX
		ret

FDC_TRK_READ:
		push	DX
		mov	DX,io_SHEILA_1770_TRK
		jmp	short FDC_in_DX

FDC_SEC_READ:
		push	DX
		mov	DX,io_SHEILA_1770_SEC
		jmp	short FDC_in_DX


;------------------------------------------------------------------------
;FDC1770_STAT_READ							:
;INPUT		NONE							:
;OUTPUTS	AL							:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_STAT_READ:
		push	DX
		mov	DX,io_SHEILA_1770_CMD
		jmp	short FDC_in_DX


;------------------------------------------------------------------------
;FDC1770_CMD_EXEC							:
;	Execute command in AL and wait for BUSY to status to go low	:
;INPUT		AL = command						:
;OUTPUT		AL = status						:
;CORRUPTS	DX							:
;------------------------------------------------------------------------
FDC_CMD_EXEC:

		call	snd_wait_8

		call	FDC_CMD_WRITE


.blp0:		call	FDC_STAT_READ
		test	AL,1
		jz	.blp0


.blp:		call	FDC_STAT_READ
		test	AL,1
		jnz	.blp

		ret


snd_init:
		; attenuations
		mov	AL,0FFh
		call	snd_poke
		mov	AL,0DFh
		call	snd_poke
		mov	AL,0BFh
		call	snd_poke
		mov	AL,09Fh
		call	snd_poke

		; pitches
		mov	AL,080h
		call	snd_poke
		mov	AL,000h
		call	snd_poke

		mov	AL,0A0h
		call	snd_poke
		mov	AL,000h
		call	snd_poke

		mov	AL,0C0h
		call	snd_poke
		mov	AL,000h
		call	snd_poke

		mov	AL,0E0h
		call	snd_poke
		mov	AL,000h
		call	snd_poke

		ret


snd_poke:	pushf
		push	DX
		push	AX
		cli
		mov	DX,io_SHEILA_SYSVIA_DDRA
		mov	AL,0FFh
		out	DX,AL
		call	snd_wait_8
		pop	AX
		mov	DX,io_SHEILA_SYSVIA_ORA_NH
		out	DX,AL
		call	snd_wait_8
		mov	AL,000h
		mov	DX,io_SHEILA_SYSVIA_ORB
		out	DX,AL
		call	snd_wait_8
		mov	AL,008h
		mov	DX,io_SHEILA_SYSVIA_ORB
		out	DX,AL
		call	snd_wait_8

		pop	DX
		popf
		ret


; TODO: find more exact delay 
; TODO: this also used in FDC code
snd_wait_8:	
		push	AX
		push	DX
		mov	AH,8
		mov	DX,io_SHEILA_SYSVIA_DDRA
.lp:		in	AL,DX			; read a port to force a 1MHz cycle
		loop	.lp
		pop	CX
		pop	AX
		ret
;------------------------------------------------------------------------
; send_set_pitch							:
;INPUT	CL = channel							:
;	AX = 10 bits of data						:
;PRESERVES all regs							:
;------------------------------------------------------------------------
snd_set_pitch:	push	DX
		push	CX
		push	AX

		shl	CL,5
		and	CL,060h
		or	CL,080h

		push	AX
		and	AL,00Fh
		or	AL,CL
		call	snd_poke
		pop	AX
		shl	AX,4
		mov	AL,AH
		and	AL,03Fh
		call	snd_poke

		pop	AX
		pop	CX
		pop	DX
		ret

;------------------------------------------------------------------------
; snd_set_vol								:
;INPUT	CL = channel							:
;	AL=4 bits volume 0=off						:
;PRESERVES all regs							:
;------------------------------------------------------------------------
snd_set_vol:	push	DX
		push	CX
		push	AX

		shl	CL,5
		and	CL,060h
		or	CL,090h

		and	AL,00Fh		
		or	AL,CL
		xor	AL,00Fh
		call	snd_poke

		pop	AX
		pop	CX
		pop	DX
		ret

		%include "video.asm"


;--- INT 12 -------------------------------------------------------------
; MEMORY_SIZE_DET							:
;	THIS ROUTINE DETERMINES THE AMOUNT OF MEMORY IN THE SYSTEM	:
;	AS REPRESENTED BY THE SWITCHES ON THE PLANAR. NOTE THAT THE	:
;	SYSTEM MAY NOT BE ABLE TO USE I/O MEMORY UNLESS THERE IS A FULL :
;	COMPLEMENT OF 64K BYTES ON THE PLANAR.				:
; INPUT 								:
;	NO REGISTERS							:
;	THE MEMORY_SIZE VARIABLE IS SET DURING POWER ON DIAGNOSTICS	:
;	 ACCORDING TO THE FOLLOWING HARDWARE ASSUMPTIONS:		:
;	PORT 60 BITS 3,2 = 00 - 16K BASE RAM				:
;			   01 - 32K BASE RAM				:
;			   10 - 48K BASE RAM				:
;			   11 - 64K BASE RAM				:
;	PORT 62 BITS 3-0 INDICATE AMOUNT OF I/O RAM IN 32K INCREMENTS	:
;		E.G., 0000 - NO RAM IN I/O CHANNEL			:
;		      0010 - 64K RAM IN I/O CHANNEL, ETC.		:
; OUTPUT								:
;	(AX) = NUMBER OF CONTIGUOUS 1K BLOCKS OF MEMORY 		:
;------------------------------------------------------------------------
MEMORY_SIZE_DET:
	STI				; INTERRUPTS BACK ON
	PUSH	DS			; SAVE SEGMENT
	CALL	DDS
	;TODO: not sure!
	MOV	AX,640
	POP	DS			; RECOVER SEGMENT
	IRET				; RETURN TO CALLER

;--- INT 11 -----------------------------------------------------
; EQUIPMENT DETERMINATION					:
;	THIS ROUTINE ATTEMPTS TO DETERMINE WHAT OPTIONAL	:
;	DEVICES ARE ATTACHED TO THE SYSTEM.			:
; INPUT 							:
;	NO REGISTERS						:
;	THE EQUIP_FLAG VARIABLE IS SET DURING THE POWER ON	:
;	DIAGNOSTICS USING THE FOLLOWING HARDWARE ASSUMPTIONS:	:
;	PORT 60  = LOW ORDER BYTE OF EQUIPMENT			:
;	PORT 3FA = INTERRUPT ID REGISTER OF 8250		:
;		BITS 7-3 ARE ALWAYS 0				:
;	PORT 378 = OUTPUT PORT OF PRINTER -- 8255 PORT THAT	:
;		CAN BE READ AS WELL AS WRITTEN			:
; OUTPUT							:
;	(AX) IS SET, BIT SIGNIFICANT, TO INDICATE ATTACHED I/O	:
;	BIT 15,14 = NUMBER OF PRINTERS ATTACHED 		:
;	BIT 13 NOT USED 					:
;	BIT 12 = GAME I/O ATTACHED				:
;	BIT 11,10,9 = NUMBER OF RS232 CARDS ATTACHED		:
;	BIT 8 UNUSED						:
;	BIT 7,6 = NUMBER OF DISKETTE DRIVES			:
;		00=1, 01=2, 10-3, 11=4 ONLY IF BIT 0 = 1	:
;	BIT 5,4 = INITIAL VIDEO MODE				:
;			00 - UNUSED				:
;			01 - 40X25 BW USING COLOR CARD		:
;			10 - 80X25 BW USING COLOR CARD		:
;			11 - 80X25 BW USING BW CARD		:
;	BIT 3,2 = PLANAR RAM SIZE (00=16K,01=32K,10=48K,11=64K) :
;	BIT 1 NOT USED						:
;	BIT 0 = IPL FROM DISKETTE -- THIS BIT INDICATES THAT	:
;		THERE ARE DISKETTE DRIVES ON THE SYSTEM 	:
;								:
;	NO OTHER REGISTERS AFFECTED				:
;----------------------------------------------------------------
EQUIPMENT:
	STI			; INTERRUPTS BACK ON
	PUSH	DS		; SAVE SEGMENT REGISTER
	CALL	DDS
	MOV	AX,[EQUIP_FLAG]	; GET THE CURRENT SETTINGS
	POP	DS		; RECOVER SEGMENT
	IRET			; RETURN TO CALLER



BEEP_FACTOR	equ	3 ; how many times more to delay than XT
BEEP_489_PITCH  equ	4000000/(32*1000)
BEEP_VOLUME	equ	2

BEEP:	
	mov	CL,0			; sound channel
	mov	AX,BEEP_489_PITCH	; 1kHz
	call	snd_set_pitch
	mov	AL,BEEP_VOLUME
	call	snd_set_vol

	mov	AL,BL
	mov	BL,BEEP_FACTOR
	mul	BL	
.dllp	xor	CX,CX
.dllp2	loop	.dllp2
	dec	AL
	jnz	.dllp

	mov	CL,0
	mov	AL,0
	call	snd_set_vol

	ret



;------------------------------------------------
; CONVERT AND PRINT ASCII CODE			:
;	AL MUST CONTAIN NUMBER TO BE CONVERTED. :
;	AX AND BX DESTROYED.			:
;------------------------------------------------
XPC_BYTE:	; TODO: rename
	push	AX			; RESAVE FOR LOW NYBBLE DISPLAY
	mov	CL,4			; SHIFT COUNT
	shr	AL,CL			; NIBBLE SWAP
	call	XLAT_PR 		; DO THE HIGH NIBBLE DISPLAY
	pop	AX			; RECOVER THE NIBBLE
	and	AL,0FH			; ISOLATE TO LOW NIBBLE
					; FALL INTO NIBBLE CONVERSION
XLAT_PR:				; CONVERT 00-0F TO ASCII CHARACTER
	add	AL,090H 		; ADD FIRST CONVERSION FACTOR
	daa				; ADJUST FOR NUMERIC AND ALPHA RANGE
	adc	AL,040H 		; ADD CONVERSION AND ADJUST LOW NIBBLE
	daa				; ADJUST HI NIBBLE TO ASCII RANGE
PRT_HEX:
	mov	AH,14			; DISPLAY CHAR. IN AL
	mov	BH,0
	int	10H			; CALL VIDEO_IO
	ret


;--- INT 1A ---------------------------------------------
; TIME_OF_DAY						:
;  THIS ROUTINE ALLOWS THE CLOCK TO BE SET/READ 	:
;							:
; INPUT 						:
;   (AH) = 0	READ THE CURRENT CLOCK SETTING		:
;		RETURNS CX = HIGH PORTION OF COUNT	:
;			DX = LOW PORTION OF COUNT	:
;			AL = 0 IF TIMER HAS NOT PASSED	:
;			 24 HOURS SINCE LAST READ	:
;			   <>0 IF ON ANOTHER DAY	:
;   (AH) = 1	SET THE CURRENT CLOCK			:
;	CX = HIGH PORTION OF COUNT			:
;	DX = LOW PORTION OF COUNT			:
; NOTE: COUNTS OCCUR AT THE RATE OF			:
;	 1193180/65536 COUNTS/SEC			:
;	(OR ABOUT 18.2 PER SECOND -- SEE EQUATES BELOW) :
;--------------------------------------------------------
TIME_OF_DAY:
	sti				
	push	DS			
	call	DDS
	or	AH,AH			
	jz	T2			
	dec	AH			
	jz	T3			
	; fall through for unknown function codes
T1:					
	sti				; reenable interrupts and return
	pop	DS			
	iret				
T2:					
	cli				; disable interrupts whilst reading 
	mov	AL,[TIMER_OFL]
	mov	byte [TIMER_OFL],0	; GET OVERFLOW, AND RESET THE FLAG
	mov	CX,[TIMER_HIGH]
	mov	DX,[TIMER_LOW]
	jmp	short T1		; TOD_RETURN
T3:					; SET_TIME
	cli				; NO INTERRUPTS WHILE WRITING
	mov	[TIMER_LOW],DX
	mov	[TIMER_HIGH],CX		; SET THE TIME
	mov	byte [TIMER_OFL],0	; RESET OVERFLOW
	jmp	T1			; TOD_RETURN

;TODO: rewrite comment, maybe tweak rollover rate?
;--------------------------------------------------------
; THIS ROUTINE HANDLES THE TIMER INTERRUPT FROM 	:
;  CHANNEL 0 OF THE 8253 TIMER. INPUT FREQUENCY 	:
;  IS 1.19318 MHZ AND THE DIVISOR IS 65536, RESULTING	:
;  IN APPROX. 18.2 INTERRUPTS EVERY SECOND.		:
;							:
; THE INTERRUPT HANDLER MAINTAINS A COUNT OF INTERRUPTS :
;  SINCE POWER ON TIME, WHICH MAY BE USED TO ESTABLISH	:
;  TIME OF DAY. 					:
; THE INTERRUPT HANDLER ALSO DECREMENTS THE MOTOR	:
;  CONTROL COUNT OF THE DISKETTE, AND WHEN IT EXPIRES,	:
;  WILL TURN OFF THE DISKETTE MOTOR, AND RESET THE	:
;  MOTOR RUNNING FLAGS. 				:
; THE INTERRUPT HANDLER WILL ALSO INVOKE A USER ROUTINE :
;  THROUGH INTERRUPT 1CH AT EVERY TIME TICK.  THE USER	:
;  MUST CODE A ROUTINE AND PLACE THE CORRECT ADDRESS IN :
;  THE VECTOR TABLE.					:
;--------------------------------------------------------
TIMER_INT:
	sti				; INTERRUPTS BACK ON
	push	DS
	push	AX
	push	DX			; SAVE MACHINE STATE
	call	DDS
	inc	word [TIMER_LOW]	; INCREMENT TIME
	jnz	.T4			; TEST_DAY
	inc	word [TIMER_HIGH]	; INCREMENT HIGH WORD OF TIME
.T4:					; TEST_DAY
	cmp	word [TIMER_HIGH],018H 	; TEST FOR COUNT EQUALLING 24 HOURS
	jnz	.T5			; DISKETTE_CTL
	cmp	word [TIMER_LOW],0B0H
	jnz	.T5			; DISKETTE_CTL

;----- TIMER HAS GONE 24 HOURS

	sub	AX,AX
	mov	word [TIMER_HIGH],AX
	mov	word [TIMER_LOW],AX
	mov	byte [TIMER_OFL],1

;----- TEST FOR DISKETTE TIME OUT

.T5:
; TODO: reset 1770 here?
;	DEC	MOTOR_COUNT
;	JNZ	T6			; RETURN IF COUNT NOT OUT
;	AND	MOTOR_STATUS,0F0H	; TURN OFF MOTOR RUNNING BITS
;	MOV	AL,0CH
;	MOV	DX,03F2H		; FDC_CTL_PORT
;	OUT	DX,AL			; TURN OFF THE MOTOR
;T6:					; TIMER_RET:
	int	1CH			; TRANSFER CONTROL TO A USER ROUTINE

	call	dom_keyb_tick

	M_EOI

	pop	DX
	pop	AX
	pop	DS			; RESET MACHINE STATE
	iret				; RETURN FROM INTERRUPT




VECTOR_TABLE:			; Initial vector values, note: only the low words, segments 
				; are all to CS when copying to actual vectors
	DW	TIMER_INT	;	TIMER_INT		; int 08H	80c18x = Timer 0
	DW	D_EOI		;	KB_INT			; int 09H	
	DW	D_EOI						; int 0AH	80x18x = DMA0
	DW	D_EOI						; int 0BH	80x18x = DMA1
	DW	D_EOI						; int 0CH	80x18x = INT0
	DW	D_EOI						; int 0DH	80x18x = INT1
	DW	DUMMY_RETURN	;	DISK_INT 		; int 0EH	80x18x = INT2
	DW	D_EOI						; int 0FH	80x18x = INT3
	DW	VIDEO_IO 					; int 010H
	DW	EQUIPMENT					; int 011H
	DW	MEMORY_SIZE_DET					; int 012H	80x18x = Timer1
	DW	DISKETTE_IO					; int 013H	80x18x = Timer2
	DW	RS232_IO 					; int 014H
	DW	CASSETTE_IO					; int 015H
	DW	KEYBOARD_IO					; int 016H
	DW	PRINTER_IO					; int 017H

	DW	00000H						; INTERRUPT 18H

	DW	BOOT_STRAP					; int 019H
	DW	TIME_OF_DAY					; int 01AH -- TIME OF DAY
	DW	DUMMY_RETURN					; int 01BH -- KEYBOARD BREAK ADDR
	DW	DUMMY_RETURN					; int 01CH -- TIMER BREAK ADDR
	DW	VIDEO_PARMS					; int 01DH -- VIDEO PARAMETERS
	DW	DISK_BASE					; int 01EH -- DISK PARMS
	DW	0						; int 01FH -- POINTER TO VIDEO EXT


D_EOI:
	push	AX
	push	DX
	
	M_EOI

	pop	DX
	pop	AX
	IRET



DDS:
		push	AX			; SAVE AX
		mov	AX,SEG_BIOS_DATA
		mov	DS,AX			; SET DATA SEGMENT
		pop	AX			; RESTORE AX
		ret


		; dummy interrupt - TODO: make this do something?
DUMMY_RETURN:
	IRET





;--- INT 19 -----------------------------------------------------
; bootstrap							:
;	Load first sector from disk A at 7C00 and jump to it	:
;----------------------------------------------------------------


	;TODO: check for disk present

;----- IPL WAS SUCCESSFUL

BOOT_STRAP:

		nop
		nop

		sti				; interrupts back on

		nop
		nop

		DBG_STR	`\nINT 19h starting...`

		mov	SI,B_STR
		call	print_str0

		sub	AX,AX
		mov	DS,AX

;TODO:
;;;----- RESET DISKETTE PARAMETER TABLE VECTOR
;;
	MOV	WORD [DISK_POINTER],DISK_BASE
	MOV	WORD [DISK_POINTER+2],CS
;;	MOV	AX,DATA_WORD[OFFSET EQUIP_FLAG] ; GET THE EQUIPMENT SWITCHES
;;	TEST	AL,1			; ISOLATE IPL SENSE SWITCH
;;	JZ	H3			; GO TO CASSETTE BASIC ENTRY POINT


;TODO: try both disks?

		mov	CX,4			; retry 4 times
.rlp:					
		push	CX			
		xor	AX,AX			
		int	13H			; reset disk system
		jc	.sk			; error try again
		mov	AX,0201h 		; op=2, #sectors=1
		sub	DX,DX			; drive =0
		mov	ES,DX			; seg = 0
		mov	BX,BOOT_LOCN		; BX = 7c00
		mov	CX,1			; sector = 1, track = 0
		int	13H			; read single sector #1, track =0, drive =0
.sk:		pop	CX			; pop retry count
		jnc	.sk_go			; Cy set if unsuccessful call 

		call	DDS
		mov	AL,[DISKETTE_STATUS]
		call	deice_HEX2

		loop	.rlp			; retry

		int	18H			; Cassette BASIC (or in our case a simple message)
.sk_go:		mov	SI,B_GO_STR
		call	print_str0
		jmp	0:BOOT_LOCN


;--- Int 18h ---------------------------------------------------:
; Dummy instead of cassette basic, just print a message for 	:
; boot failure							:
;----------------------------------------------------------------
BOOT_FAIL_PRINT:
		sti
		mov	SI,BF_STR
		call	print_str0
.lp:		sti
		hlt
		jmp	.lp


BF_STR:		db	"Boot failed",13,10,0
B_STR:		db	"Boot start...",13,10,0
B_GO_STR:	db	"Sector Loaded...jumping 7c00",13,10,0


print_str0:	push	AX
		push	DS
		cld
		mov	AX,CS
		mov	DS,AX
.lp:		lodsb
		or	al,al
		jz	.r
		mov	ah,0Eh
		int	10h
		jmp	.lp
.r:		pop	DS
		pop	AX
		ret



;-- INT 13 --------------------------------------------------------------
; DISKETTE I/O								:
;	THIS INTERFACE PROVIDES ACCESS TO THE 5 1/4" DISKETTE DRIVES    :
; INPUT 								:
;	(AH)=0	RESET DISKETTE SYSTEM					:
;		HARD RESET TO NEC, PREPARE COMMAND, RECAL REQUIRED	:
;		ON ALL DRIVES						:
;	(AH)=1	READ THE STATUS OF THE SYSTEM INTO (AL) 		:
;		DISKETTE_STATUS FROM LAST OPERATION IS USED		:
;									:
; REGISTERS FOR READ/WRITE/VERIFY/FORMAT				:
;	(DL) - DRIVE NUMBER (0-3 ALLOWED, VALUE CHECKED)		:
;	(DH) - HEAD NUMBER (0-1 ALLOWED, NOT VALUE CHECKED)		:
;	(CH) - TRACK NUMBER (0-39, NOT VALUE CHECKED)			:
;	(CL) - SECTOR NUMBER (1-8, NOT VALUE CHECKED,			:
;				 NOT USED FOR FORMAT)			:
;	(AL) - NUMBER OF SECTORS ( MAX = 8, NOT VALUE CHECKED, NOT USED :
;					FOR FORMAT)			:
;	(ES:BX) - ADDRESS OF BUFFER ( NOT REQUIRED FOR VERIFY)		:
;									:
;	(AH)=2	READ THE DESIRED SECTORS INTO MEMORY			:
;	(AH)=3	WRITE THE DESIRED SECTORS FROM MEMORY			:
;	(AH)=4	VERIFY THE DESIRED SECTORS				:
;	(AH)=5	FORMAT THE DESIRED TRACK				:
;		FOR THE FORMAT OPERATION, THE BUFFER POINTER (ES,BX)	:
;		MUST POINT TO THE COLLECTION OF DESIRED ADDRESS FIELDS	:
;		FOR THE TRACK.	EACH FIELD IS COMPOSED OF 4 BYTES,	:
;		(C,H,R,N), WHERE C = TRACK NUMBER, H=HEAD NUMBER,	:
;		R = SECTOR NUMBER, N= NUMBER OF BYTES PER SECTOR	:
;		(00=128, 01=256, 02=512, 03=1024).  THERE MUST BE ONE	:
;		ENTRY FOR EVERY SECTOR ON THE TRACK.  THIS INFORMATION	:
;		IS USED TO FIND THE REQUESTED SECTOR DURING READ/WRITE	:
;		ACCESS. 						:
;									:
; DATA VARIABLE -- DISK_POINTER 					:
;	DOUBLE WORD POINTER TO THE CURRENT SET OF DISKETTE PARAMETERS	:
; OUTPUT								:
;	AH = STATUS OF OPERATION					:
;		STATUS BITS ARE DEFINED IN THE EQUATES FOR		:
;		DISKETTE_STATUS VARIABLE IN THE DATA SEGMENT OF THIS	:
;		MODULE. 						:
;	CY = 0	SUCCESSFUL OPERATION (AH=0 ON RETURN)			:
;	CY = 1	FAILED OPERATION (AH HAS ERROR REASON)			:
;	FOR READ/WRITE/VERIFY						:
;		DS,BX,DX,CH,CL PRESERVED				:
;		AL = NUMBER OF SECTORS ACTUALLY READ			:
;		***** AL MAY NOT BE CORRECT IF TIME OUT ERROR OCCURS	:
;	NOTE:	IF AN ERROR IS REPORTED BY THE DISKETTE CODE, THE	:
;		APPROPRIATE ACTION IS TO RESET THE DISKETTE, THEN RETRY :
;		THE OPERATION. ON READ ACCESSES, NO MOTOR START DELAY	:
;		IS TAKEN, SO THAT THREE RETRIES ARE REQUIRED ON READS	:
;		TO ENSURE THAT THE PROBLEM IS NOT DUE TO MOTOR		:
;		START-UP.						:
;------------------------------------------------------------------------
DISKETTE_IO:		
		sti
		
		DBG_STR	`INT13h=====\n`
		call	DBG_DUMPREGS_I

		push	BX
		push	CX
		push	DS
		push	SI
		push	DI
		push	BP
		push	DX
		mov	BP,SP			; BP to save params


		call	DDS
		call	disk_io_int		; call the internal functions

		DBG_STR	`=====INT13h\n`

		; TODO: - think we can ignore all this stuff 1770 does motor control
		mov	AH,[DISKETTE_STATUS]	; get return value
		cmp	AH,1			
		cmc				; set Cy if AH<>0
		pop	DX			
		pop	BP
		pop	DI
		pop	SI
		pop	DS
		pop	CX
		pop	BX			
		retf	2			; don't preserve flags!


disk_io_tbl:
		dw	disk_io_reset
		dw	disk_io_status
		dw	disk_io_read
disk_io_tbl_len	equ	$-disk_io_tbl


disk_io_int:	

		; get function from table and jump to it or return an error if out of bounds
		push	AX
		mov	AL,AH
		xor	AH,AH
		sal	AX,1
		mov	SI,AX
		cmp	AX,disk_io_tbl_len
		pop	AX
		jge	disk_ret_bad_cmd
		jmp	word [CS:SI+disk_io_tbl]

disk_ret_bad_cmd:
		DBG_STR `\nBADDSK`
		push	AX
		mov	AL,AH
		call	deice_HEX2
		pop	AX
		call	deice_CRLF

		mov	AH,BAD_CMD
		mov	byte [DISKETTE_STATUS],AH
		stc
		ret


disk_io_reset:	xor	AX,AX
		mov	[SEEK_STATUS],AL
		mov	[DISKETTE_STATUS],AL

		call	FDC_CTL_WRITE		; put controller into reset

		dec	AL
		mov	[DSK_PREV_DRV],AL
		mov	[DSK_TRACK_TBL],AL
		mov	[DSK_TRACK_TBL+1],AL
		ret

disk_io_status:	mov	AL,[DISKETTE_STATUS]
		ret



disk_io_read:	push	ES
		push	DX
		push	AX
		

		call	disk_io_restore
		jc	.r

		call	disk_io_seek
		jc	.r

	%ifdef BOARD_18x
		
		; set up DMA controller

		mov	DX,io_PCB_D0CON
		mov	AX,00004h				; set "change start" bit to enable start
		out	DX,AL

		mov	DX,io_PCB_D0SRCL
		mov	AX,io_SHEILA_1770_DAT			; source is the 1770 data register
		out	DX,AL
		inc	DX
		inc	DX			;io_PCB_D0SRCH
		mov	AX,0
		out	DX,AL					; not bothered but 0 anyway
		inc	DX
		inc	DX			;io_PCB_D0DSTL

		; need to convert ES:BX to linear address here 
		mov	AX,ES
		shl	AX,4
		add	AX,BX
		pushf
		out	DX,AL					; buffer address low
		inc	DX
		inc	DX			;io_PCB_D0DSTH
		mov	AX,ES
		shr	AX,12
		popf
		adc	AX,0
		out	DX,AL					; buffer address high

		xor	AH,AH					; zero count of actual sectors read

.next_sector:
		pop	AX
		dec	AL					; any sectors left to read?
		push	AX
		js	.r

		mov	DX,io_PCB_D0TC
		mov	AX,00200h				; a single sector	: TODO: multiple sectors!
		out	DX,AL
		inc	DX
		inc	DX			;io_PCB_D0CON
		mov	AX,0A2A6h				; DMEM+DINC+TC+DESTSYN+PRIORITY+CHG+START
		out	DX,AL	

	%else
		;--- 386ex ---
		; set up DMA controller


		

		DBG_STR  `SECRD:`

.next_sector:
		pop	AX
		dec	AL					; any sectors left to read?
		push	AX
		js	.r

		DBG_C   '$'

		mov	DX,DMACMD1
		mov	AL,04h
		out	(DX),AL					; disable DMA

		mov	DX,DMACMD2
		mov	AL,0Bh
		out	(DX),AL					; sample EOP/DRQ synchronous and high pri

		mov	DX,DMACFG
		mov	AL,088h					; external peripheral, mask DAK
		out	(DX),AL

		mov	DX,DMAOVFE
		mov	AL,0Fh
		out	(DX),AL					; all bits of address increment

		mov	DX,DMAMOD1
		mov	AL,44h
		out	(DX),AL					; Channel 0, increment target, single, R->T

		mov	DX,DMAMOD2
		mov	AL,0D0h
		out	(DX),AL					; Channel 0, 2 cycle, req I/O, tar mem, req hold

		mov	DX,DMAMSK
		mov	AL,0h
		out	(DX),AL					; Unmask channel 0

		mov	DX,DMABSR
		mov	AL,90h
		out	(DX),AL					; RBS/TBS 8 bit

		mov	DX,DMACHR
		mov	AL,0h
		out	(DX),AL					; no chaining

		mov	DX,DMACLRBP
		out	(DX),AL					; reset BP

		mov	DX,DMA0REQ
		mov	AL,io_SHEILA_1770_DAT & 0FFh		; source is the 1770 data register
		out	DX,AL
		mov	AL,io_SHEILA_1770_DAT >> 8		; source is the 1770 data register
		out	DX,AL

		; need to convert ES:BX to linear address here 
		mov	AX,ES
		shl	AX,4
		add	AX,BX
		pushf
		mov	DX,DMA0TAR
		out	DX,AL					; buffer address low
		mov	AL,AH
		out	DX,AL

		mov	AX,ES
		shr	AX,12
		popf
		adc	AX,0

		mov	DX,DMA0TAR2
		out	DX,AL					; buffer address high

		mov	DX,DMA0TAR3
		mov	AL,AH
		out	DX,AL		

		mov	AX,ES
		add	AX,32					; move on read addr
		mov	ES,AX

		mov	DX,DMA0BYC
		mov	AX,00h
		out	DX,AL
		mov	AX,02h
		out	DX,AL					; a single sector	: TODO: multiple sectors!

		mov	DX,DMA0BYC2
		xor	AL,AL
		out	DX,AL

		mov	DX,DMASTS
		in	AL,DX					; clear the transfer complete flag


		mov	DX,DMACMD1
		mov	AL,00h
		out	DX,AL					; enable DMA (both channels?!)	
	%endif

		; TODO: check sector number overflow against drive type somehow?
		mov	AL,CL
		call	FDC_SEC_WRITE

		DBG_C	'T'
		call	FDC_TRK_READ
		call	deice_HEX2
		DBG_C	'S'
		call	FDC_SEC_READ
		call	deice_HEX2


		DBG_C	'e'

		mov	AL,FDC_II_CMD_RDSEC+FDC_II_CMD_BITS_E
		or	AH,AH
		jz	.skH
		or	AL,FDC_II_CMD_BITS_H			; if not 1st set H, remove E
		and	AL,~FDC_II_CMD_BITS_E
.skH:		call	FDC_CMD_EXEC

;;		DBG_C	'$'
;;		call	deice_HEX2
;;		DBG_C	'='	

		call	disk_io_type_II_err_rd

;;		pushf
;;		mov	AL,[DISKETTE_STATUS]
;;		call	deice_HEX2
;;		DBG_C	'~'
;;		mov	DX,io_PCB_D0DSTH
;;		in	AX,DX
;;		and	AL,0Fh
;;		call	deice_HEX2
;;		mov	DX,io_PCB_D0DSTL
;;		in	AX,DX
;;		push	AX
;;		mov	AL,AH
;;		call	deice_HEX2
;;		pop	AX
;;		call	deice_HEX2
;;		popf
		jc	.r

		; check on DMA status should be off with TC=0
	%ifdef BOARD_18x
		mov	DX,io_PCB_D0TC
		in	AX,DX
		or	AX,AX
		jz	.skokdma
.baddma		stc
		mov	byte [DISKETTE_STATUS],BAD_DMA
.skokdma:	mov	DX,io_PCB_D0CON
		in	AX,DX
		test	AL,2
		jnz	.baddma
	%else

		mov	DX,DMACMD1
		mov	AL,04h
		out	DX,AL					; disable DMA (both channels?!)	

		; check for DMA finished

		mov	DX,DMASTS
		in	AL,DX
		test	AL,0
		jz	.skokdma
		mov	byte [DISKETTE_STATUS],BAD_DMA
.skokdma:
	%endif

		pop	AX
		inc	AH
		push	AX

		inc	CL	;TODO: check for overrun track?

		jmp	.next_sector


.r:
		
		pop	AX
		pop	DX
		pop	ES
		
		mov	AL,AH				; count of sectors actually read

		DBG_C	'}'
		call	deice_CRLF

		ret


disk_io_seek:	; seek to track in CH
		call	FDC_TRK_READ
		cmp	CH,AL
		jz	.r
		mov	AL,CH
		call	FDC_DAT_WRITE
;;		DBG_C	'T'
;;		mov	AL,CH
;;		call	deice_HEX2
		mov	AL,FDC_I_CMD_SEEK+FDC_CMD_BITS_SEEKSPEED+FDC_I_CMD_BITS_V
		call	FDC_CMD_EXEC
		call	disk_io_type_I_err
		jc	.r
		mov	AL,CL
		call	disk_io_save_track
.r:		ret




		; check if the drive in DL needs a recal/restore
		; if not check to see if it was the last drive used, if not restore the track register
		; as saved by the last command
		; the FDC control register is setup
disk_io_restore:
		push	DX
		push	AX
		mov	byte [DISKETTE_STATUS],0
		call	FDC1770_DRVSEL					; AL now contains drv+1 in bottom 2 bits
		and	AL,003h
		test	[SEEK_STATUS],AL
		jnz	.no_recal

		mov	AL,FDC_I_CMD_REST+FDC_CMD_BITS_SEEKSPEED	; no verify!
		call	FDC_CMD_EXEC
		call	disk_io_type_I_err
		jc	.r
		call	FDC1770_DRVSEL					; TODO - lazy way of getting 21/22 in AL
		and	AL,03h
		or	[SEEK_STATUS],AL				; reset restore flag
		mov	AL,0
		call	disk_io_save_track

.no_recal:	cmp	DL,[DSK_PREV_DRV]
		jz	.no_track

		call	disk_io_getcur_track
		call	FDC_TRK_WRITE	
		mov	[DSK_PREV_DRV],DL	
.no_track:

		clc
.r		pop	AX
		pop	DX
		ret

		; save track number for disk A/B (bit 0 of DL)
disk_io_save_track:
		push	AX
		xor	AH,AH
		mov	AL,DL
		and	AL,1
		mov	SI,AX
		pop	AX
		mov	byte [DS:SI+DSK_TRACK_TBL],AL
		ret

		; get track number for disk A/B (bit 0 of DL)
disk_io_getcur_track:
		push	AX
		xor	AH,AH
		mov	AL,DL
		and	AL,1
		mov	SI,AX
		pop	AX
		mov	AL,byte [DS:SI+DSK_TRACK_TBL]
		ret

disk_io_type_I_err:
		xor	AH,AH
		call	FDC_STAT_READ
		test	AL,FDC_I_STAT_ERRMASK
		jz	.r
		test	AL,FDC_I_STAT_BITS_SE
		jz	.sk0
		mov	AH,BAD_SEEK
		jmp	.secr
.sk0		test	AL,FDC_I_STAT_BITS_CRC
		jz	.r
		mov	AH,BAD_CRC
.secr		stc
.r		mov	[DISKETTE_STATUS],AH
		ret

disk_io_type_II_err_rd:
		xor	AH,AH
		call	FDC_STAT_READ
		test	AL,FDC_II_STAT_RD_ERRMASK
		jmp	short disk_io_type_II_err_wr2
disk_io_type_II_err_wr:
		xor	AH,AH
		call	FDC_STAT_READ
		test	AL,FDC_II_STAT_WR_ERRMASK
disk_io_type_II_err_wr2:
		jz	.r

		DBG_C	'F'
		DBG_C	'D'
		DBG_C	'C'
		DBG_C	'E'
		call	deice_HEX2		
		call	deice_CRLF

		test	AL,FDC_II_STAT_BITS_LD
		jz	.sk0
		mov	AH,BAD_DMA
		jmp	short	.secr
.sk0		test	AL,FDC_II_STAT_BITS_CRC
		jz	.sk1
		mov	AH,BAD_CRC
		jmp	short	.secr
.sk1		test	AL,FDC_II_STAT_BITS_RNF
		jz	.sk2
		mov	AH,RECORD_NOT_FND
		jmp	short	.secr
.sk2		test	AL,FDC_II_STAT_BITS_WP
		jz	.r
		mov	AH,WRITE_PROTECT
		jmp	short	.secr
.secr		stc
.r		mov	[DISKETTE_STATUS],AH
		ret


; DISK_BASE								:
;	THIS IS THE SET OF PARAMETERS REQUIRED FOR DISKETTE OPERATION,	:
;	THEY ARE POINTED AT BY THE DATA VARIABLE DISK_POINTER. TO	:
;	MODIFY THE PARAMETERS, BUILD ANOTHER PARAMETER BLOCK AND POINT	:
;	DISK_POINTER TO IT						:
;------------------------------------------------------------------------
DISK_BASE:
	DB	11001111B	; SRT=C, HD UNLOAD=0F - 1ST SPECIFY BYTE
	DB	2		; HD LOAD=1, MODE=DMA - 2ND SPECIFY BYTE
	DB	MOTOR_WAIT	; WAIT AFTER OPN TIL MOTOR OFF
	DB	2		; 512 BYTES/SECTOR
	DB	8		; EOT (LAST SECTOR ON TRACK)
	DB	02AH		; GAP LENGTH
	DB	0FFH		; DTL
	DB	050H		; GAP LENGTH FOR FORMAT
	DB	0F6H		; FILL BYTE FOR FORMAT
	DB	25		; HEAD SETTLE TIME (MILLISECONDS)
	DB	4		; MOTOR START TIME (1/8 SECONDS)


INT7_HANDLE:
	push	BP
	mov	BP,SP
	push	AX
	DBG_C 	'I'
	DBG_C 	'N'
	DBG_C 	'T'
	DBG_C 	'7'
	DBG_C 	'@'
	mov	AX,[SS:BP+4]
	mov	AL,AH
	call	deice_HEX2
	mov	AX,[SS:BP+4]
	call	deice_HEX2
	DBG_C 	':'
	mov	AX,[SS:BP+2]
	mov	AL,AH
	call	deice_HEX2
	mov	AX,[SS:BP+2]
	call	deice_HEX2
	DBG_C 	'['
	mov	AX,[SS:BP+6]
	mov	AL,AH
	call	deice_HEX2
	mov	AX,[SS:BP+6]
	call	deice_HEX2
	DBG_C 	']'

	; step over it!?
	inc	word [SS:BP+2]

	pop	AX
	pop	BP
	iret

RS232_IO:
	call	DBG_DUMPREGS_I
	sti
	call deice_CRLF
	DBG_C 'R'
	DBG_C 'S'
	DBG_C '2'
	DBG_C '3'
	DBG_C '2'
	MOV	AL,AH
	call	deice_HEX2
	DBG_C '.'
	push	BP
	mov	BP,SP
	mov	AL,[SS:BP+5]
	call	deice_HEX2
	mov	AL,[SS:BP+4]
	call	deice_HEX2
	DBG_C	':'
	mov	AL,[SS:BP+3]
	call	deice_HEX2
	mov	AL,[SS:BP+2]
	call	deice_HEX2
	pop	BP


	call deice_CRLF
	mov	AH,080h
	STC	
	RETF	2

CASSETTE_IO:
	push	AX
	sti
	call deice_CRLF
	DBG_C 'C'
	DBG_C 'A'
	DBG_C 'S'
	MOV	AL,AH
	call	deice_HEX2
	call deice_CRLF
	pop	AX
	mov	AH,086h
	STC	
	RETF	2			; return flags

KEYBOARD_IO:
	STI				; INTERRUPTS BACK ON
	PUSH	DS			; SAVE CURRENT DS
	PUSH	BX			; SAVE BX TEMPORARILY
	CALL	DDS
	OR	AH,AH			; AH=0
	JZ	.K1			; ASCII_READ
	DEC	AH			; AH=1
	JZ	.K2			; ASCII_STATUS
	DEC	AH			; AH=2
	JZ	.K3			; SHIFT_STATUS
	JMP	SHORT .INT10_END 	; EXIT

;----- READ THE KEY TO FIGURE OUT WHAT TO DO

.K1:					; ASCII READ
	STI				; INTERRUPTS BACK ON DURING LOOP
	NOP				; ALLOW AN INTERRUPT TO OCCUR
	CLI				; INTERRUPTS BACK OFF
	MOV	BX,[BUFFER_HEAD]	; GET POINTER TO HEAD OF BUFFER
	CMP	BX,[BUFFER_TAIL]	; TEST END OF BUFFER
	JZ	.K1			; LOOP UNTIL SOMETHING IN BUFFER
	MOV	AX,[BX] 		; GET SCAN CODE AND ASCII CODE
	CALL	dom_keyb_buffer_inc_wrap; MOVE POINTER TO NEXT POSITION
	MOV	[BUFFER_HEAD],BX		; STORE VALUE IN VARIABLE
	JMP	SHORT .INT10_END 	; RETURN

;------ ASCII STATUS

.K2:
	CLI				; INTERRUPTS OFF
	MOV	BX,[BUFFER_HEAD]	; GET HEAD POINTER
	CMP	BX,[BUFFER_TAIL]	; IF EQUAL (Z=1) THEN NOTHING HERE
	MOV	AX,[BX]
	STI				; INTERRUPTS BACK ON
	POP	BX			; RECOVER REGISTER
	POP	DS			; RECOVER SEGMENT
	RETF	2			; THROW AWAY FLAGS

;------ SHIFT STATUS

.K3:
	MOV	AL,[KB_FLAG]		; GET THE SHIFT STATUS FLAGS
.INT10_END:
	POP	BX			; RECOVER REGISTER
	POP	DS			; RECOVER REGISTERS
	IRET				; RETURN TO CALLER




PRINTER_IO:
	sti
	call	deice_CRLF
	DBG_C 	'R'
	DBG_C 'S'
	DBG_C '2'
	DBG_C '3'
	DBG_C '2'
	MOV	AL,AH
	call	deice_HEX2
	call	deice_CRLF
	mov	AH,080h
	STC	
	IRET

DBG_DUMPREGS_I:				; dump interrupt registers (i.e. CS,IP,F come from IRET)
	pushf		;18
	push	DS	;16
	push	ES	;14
	push	SI	;12
	push	DI	;10
	push	BP	;8
	push	DX	;6
	push	CX	;4
	push	BX	;2
	push	AX	;0
	mov	BP,SP

	DBG_C	'A'
	DBG_C	'X'
	DBG_C	'='
	mov	AL,[SS:BP+1]
	call	deice_HEX2
	mov	AL,[SS:BP+0]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'B'
	DBG_C	'X'
	DBG_C	'='
	mov	AL,[SS:BP+3]
	call	deice_HEX2
	mov	AL,[SS:BP+2]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'C'
	DBG_C	'X'
	DBG_C	'='
	mov	AL,[SS:BP+5]
	call	deice_HEX2
	mov	AL,[SS:BP+4]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'D'
	DBG_C	'X'
	DBG_C	'='
	mov	AL,[SS:BP+7]
	call	deice_HEX2
	mov	AL,[SS:BP+6]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'B'
	DBG_C	'P'
	DBG_C	'='
	mov	AL,[SS:BP+9]
	call	deice_HEX2
	mov	AL,[SS:BP+8]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'D'
	DBG_C	'I'
	DBG_C	'='
	mov	AL,[SS:BP+11]
	call	deice_HEX2
	mov	AL,[SS:BP+10]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'S'
	DBG_C	'I'
	DBG_C	'='
	mov	AL,[SS:BP+13]
	call	deice_HEX2
	mov	AL,[SS:BP+12]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'E'
	DBG_C	'S'
	DBG_C	'='
	mov	AL,[SS:BP+15]
	call	deice_HEX2
	mov	AL,[SS:BP+14]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'D'
	DBG_C	'S'
	DBG_C	'='
	mov	AL,[SS:BP+17]
	call	deice_HEX2
	mov	AL,[SS:BP+16]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'S'
	DBG_C	'S'
	DBG_C	'='
	mov	AX,SS
	xchg	AL,AH
	call	deice_HEX2
	xchg	AL,AH
	call	deice_HEX2
	DBG_C	','

	DBG_C	'S'
	DBG_C	'P'
	DBG_C	'='
	mov	AX,BP
	add	AX,28
	xchg	AL,AH
	call	deice_HEX2
	xchg	AL,AH
	call	deice_HEX2
	DBG_C	','

	; skip 4 for local flags and return to

	DBG_C	'C'
	DBG_C	'S'
	DBG_C	'='
	mov	AL,[SS:BP+25]
	call	deice_HEX2
	mov	AL,[SS:BP+24]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'I'
	DBG_C	'P'
	DBG_C	'='
	mov	AL,[SS:BP+23]
	call	deice_HEX2
	mov	AL,[SS:BP+22]
	call	deice_HEX2
	DBG_C	','

	DBG_C	'F'
	DBG_C	'='
	mov	AL,[SS:BP+27]
	call	deice_HEX2
	mov	AL,[SS:BP+26]
	call	deice_HEX2
	
	call	deice_CRLF

	pop	AX
	pop	BX
	pop	CX
	pop	DX
	pop	BP
	pop	DI
	pop	SI
	pop	ES
	pop	DS
	popf
	ret


		%include "bbc_keyboard.asm"

		%include "deice.asm"


;=======================================================================:
; RESET VECTOR
;=======================================================================:


		TIMES 0x3FF0-($-$$) db 0xFF

		section RESETVEC

		jmp 	SEG_ROMBASE:handle_res

		TIMES 0x10-($-$$) db 0xFF
