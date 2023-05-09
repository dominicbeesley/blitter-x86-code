[map all]


	; TODO: Norton utilities get stuck reading io 3DAh - VGA?!?/CGA control/status - make it toggle bits in
	;	fb_cpu80188.vhd to fool programs that wait for retrace?


cpu 186


%macro	DBG_C	1
		pushf
		push	AX
		mov	AL,%1
		call	deice_print_char
		pop	AX
		popf
%endmacro


CLOCK_SPEED	equ	16000000

;***********************************************************************
;* 80C18x Peripherals	                                               *
;***********************************************************************
	
io_PCB_BASE		equ	0FF00h				; this is the default location
io_PCB_EOI 		equ	io_PCB_BASE+022h
io_PCB_POLL 		equ	io_PCB_BASE+024h
io_PCB_POLLSTS 		equ	io_PCB_BASE+026h
io_PCB_IMASK 		equ	io_PCB_BASE+028h
io_PCB_PRIMSK 		equ	io_PCB_BASE+02Ah
io_PCB_INSERV 		equ	io_PCB_BASE+02Ch
io_PCB_REQST 		equ	io_PCB_BASE+02Eh
io_PCB_INSTS 		equ	io_PCB_BASE+030h
io_PCB_TCUCON 		equ	io_PCB_BASE+032h
io_PCB_DMA0CON 		equ	io_PCB_BASE+034h
io_PCB_DMA1CON 		equ	io_PCB_BASE+036h
io_PCB_I0CON 		equ	io_PCB_BASE+038h
io_PCB_I1CON 		equ	io_PCB_BASE+03Ah
io_PCB_I2CON 		equ	io_PCB_BASE+03Ch
io_PCB_I3CON 		equ	io_PCB_BASE+03Eh
io_PCB_T0CNT 		equ	io_PCB_BASE+050h
io_PCB_T0CMPA 		equ	io_PCB_BASE+052h
io_PCB_T0CMPB 		equ	io_PCB_BASE+054h
io_PCB_T0CON 		equ	io_PCB_BASE+056h
io_PCB_T1CNT 		equ	io_PCB_BASE+058h
io_PCB_T1CMPA 		equ	io_PCB_BASE+05Ah
io_PCB_T1CMPB 		equ	io_PCB_BASE+05Ch
io_PCB_T1CON 		equ	io_PCB_BASE+05Eh
io_PCB_T2CNT 		equ	io_PCB_BASE+060h
io_PCB_T2CMPA 		equ	io_PCB_BASE+062h
io_PCB_T2CON 		equ	io_PCB_BASE+066h
io_PCB_UMCS 		equ	io_PCB_BASE+0A0h
io_PCB_LMCS 		equ	io_PCB_BASE+0A2h
io_PCB_PACS 		equ	io_PCB_BASE+0A4h
io_PCB_MMCS 		equ	io_PCB_BASE+0A6h
io_PCB_MPCS 		equ	io_PCB_BASE+0A8h
io_PCB_D0SRCL		equ	io_PCB_BASE+0C0h
io_PCB_D0SRCH		equ	io_PCB_BASE+0C2h
io_PCB_D0DSTL		equ	io_PCB_BASE+0C4h
io_PCB_D0DSTH		equ	io_PCB_BASE+0C6h
io_PCB_D0TC		equ	io_PCB_BASE+0C8h
io_PCB_D0CON		equ	io_PCB_BASE+0CAh
io_PCB_D1SRCL		equ	io_PCB_BASE+0D0h
io_PCB_D1SRCH		equ	io_PCB_BASE+0D2h
io_PCB_D1DSTL		equ	io_PCB_BASE+0D4h
io_PCB_D1DSTH		equ	io_PCB_BASE+0D6h
io_PCB_D1TC		equ	io_PCB_BASE+0D8h
io_PCB_D1CON		equ	io_PCB_BASE+0DAh
io_PCB_RFBASE		equ	io_PCB_BASE+0E0h
io_PCB_RFTIME		equ	io_PCB_BASE+0E2h
io_PCB_RFCON		equ	io_PCB_BASE+0E4h
io_PCB_PWRSAV		equ	io_PCB_BASE+0F0h
io_PCB_PWRCON		equ	io_PCB_BASE+0F2h
io_PCB_STEPID		equ	io_PCB_BASE+0F6h
io_PCB_RELREG		equ	io_PCB_BASE+0FEh



;***********************************************************************
;* System VIA                                                          *
;***********************************************************************
io_SHEILA_SYSVIA_ORB	equ	0FE40h
io_SHEILA_SYSVIA_ORA	equ	0FE41h
io_SHEILA_SYSVIA_DDRB	equ	0FE42h
io_SHEILA_SYSVIA_DDRA	equ	0FE43h
io_SHEILA_SYSVIA_T1CL	equ	0FE44h
io_SHEILA_SYSVIA_T1CH	equ	0FE45h
io_SHEILA_SYSVIA_T1LL	equ	0FE46h
io_SHEILA_SYSVIA_T1LH	equ	0FE47h
io_SHEILA_SYSVIA_T2CL	equ	0FE48h
io_SHEILA_SYSVIA_T2CH	equ	0FE49h
io_SHEILA_SYSVIA_SR	equ	0FE4Ah
io_SHEILA_SYSVIA_ACR	equ	0FE4Bh
io_SHEILA_SYSVIA_PCR	equ	0FE4Ch
io_SHEILA_SYSVIA_IFR	equ	0FE4Dh
io_SHEILA_SYSVIA_IER	equ	0FE4Eh
io_SHEILA_SYSVIA_ORA_NH	equ	0FE4Fh

;***********************************************************************
;* User VIA                                                            *
;***********************************************************************
io_SHEILA_USRVIA_ORB	equ	0FE60h
io_SHEILA_USRVIA_ORA	equ	0FE61h
io_SHEILA_USRVIA_DDRB	equ	0FE62h
io_SHEILA_USRVIA_DDRA	equ	0FE63h
io_SHEILA_USRVIA_T1CL	equ	0FE64h
io_SHEILA_USRVIA_T1CH	equ	0FE65h
io_SHEILA_USRVIA_T1LL	equ	0FE66h
io_SHEILA_USRVIA_T1LH	equ	0FE67h
io_SHEILA_USRVIA_T2CL	equ	0FE68h
io_SHEILA_USRVIA_T2CH	equ	0FE69h
io_SHEILA_USRVIA_SR	equ	0FE6Ah
io_SHEILA_USRVIA_ACR	equ	0FE6Bh
io_SHEILA_USRVIA_PCR	equ	0FE6Ch
io_SHEILA_USRVIA_IFR	equ	0FE6Dh
io_SHEILA_USRVIA_IER	equ	0FE6Eh
io_SHEILA_USRVIA_ORA_NH	equ	0FE6Fh

io_SHEILA_CRTC		equ	0FE00h
io_SHEILA_ULA		equ	0FE20h
io_SHEILA_ULA_PAL	equ	0FE21h

io_SHEILA_1770_CTL	equ	0FE80h
io_SHEILA_1770_CMD	equ	0FE84h
io_SHEILA_1770_TRK	equ	0FE85h
io_SHEILA_1770_SEC	equ	0FE86h
io_SHEILA_1770_DAT	equ	0FE87h

io_sheila_ACIA_CTL	equ	0FE08h
io_sheila_ACIA_DATA	equ	0FE09h
io_sheila_SERIAL_ULA	equ	0FE10h
ACIA_RDRF		equ	$01
ACIA_TDRE		equ	$02


FDC_CMD_BITS_R3			equ	003h
FDC_CMD_BITS_SEEKSPEED		equ	000h


FDC_I_CMD_REST			equ	000h
FDC_I_CMD_SEEK			equ	010h
FDC_I_CMD_STEP			equ	020h
FDC_I_CMD_STEPI			equ	040h
FDC_I_CMD_STEPO			equ	060h

FDC_I_CMD_BITS_H		equ	008h
FDC_I_CMD_BITS_V		equ	004h
FDC_I_CMD_BITS_U		equ	010h

FDC_I_STAT_BITS_MO		equ	080h
FDC_I_STAT_BITS_SU		equ	020h
FDC_I_STAT_BITS_SE		equ	010h
FDC_I_STAT_BITS_CRC		equ	008h
FDC_I_STAT_BITS_T0		equ	004h
FDC_I_STAT_BITS_IP		equ	002h
FDC_I_STAT_BITS_BSY		equ	001h

FDC_I_STAT_ERRMASK		equ	018h

FDC_II_CMD_RDSEC		equ	080h

FDC_II_CMD_BITS_M		equ	010h
FDC_II_CMD_BITS_H		equ	008h
FDC_II_CMD_BITS_E		equ	004h
FDC_II_CMD_BITS_P		equ	002h
FDC_II_CMD_BITS_A0		equ	001h


FDC_II_STAT_BITS_MO		equ	080h
FDC_II_STAT_BITS_WP		equ	040h
FDC_II_STAT_BITS_RT		equ	020h
FDC_II_STAT_BITS_RNF		equ	010h
FDC_II_STAT_BITS_CRC		equ	008h
FDC_II_STAT_BITS_LD		equ	004h
FDC_II_STAT_BITS_DRQ		equ	002h
FDC_II_STAT_BITS_BSY		equ	001h

FDC_II_STAT_WR_ERRMASK		equ	07Ch
FDC_II_STAT_RD_ERRMASK		equ	03Ch



		
		SEG_ROMBASE 	equ 	0xFC00
		SEG_RESET 	equ 	0xFFFF
		SEG_SCREEN	equ	0xF300

		; boot time stack
		SEG_INIT_STACK	equ	0x30
		INIT_STACK_TOS	equ	0x80


		SEG_INT		equ	0x00
		absolute	2*4
NMI_PTR 	resw		1
		absolute	5*4
INT5_PTR	resw		1
		absolute	8*4
INT_PTR 	resw		1
		absolute	10H*4
VIDEO_INT	resw		1
		absolute	1DH*4
PARM_PTR	resw		1		; video parameters pointer
		absolute	18H*4
BASIC_PTR	resw		1		; cassette BASIC entry point
		absolute	01EH*4		; int 1EH
DISK_POINTER	resw		1
		absolute	01FH*4		
EXT_PTR 	resw		1		; extension routine pointer
		absolute	040H*4
IO_ROM_INIT	resw		1		
IO_ROM_SEG	resw		1		; option ROM SEG
		absolute	400H
DATA_AREA	resb		1		; BIOS data segment absolute (0:400) address, usually 40:0

		
BOOT_LOCN	equ		7C00H



		SEG_BIOS_DATA	equ	0x40
		absolute	0
;----------------------------------------
;	 ROM BIOS DATA AREAS		:
;----------------------------------------
RS232_BASE	resw	4		; ADDRESSES OF RS232 ADAPTERS
PRINTER_BASE	resw	4		; ADDRESSES OF PRINTERS
EQUIP_FLAG	resw	1		; INSTALLED HARDWARE
MFG_TST 	resb	1		; INITIALIZATION FLAG
MEMORY_SIZE	resw	1		; MEMORY SIZE IN K BYTES
IO_RAM_SIZE	resw	1		; MEMORY IN I/O CHANNEL
;----------------------------------------
;	   KEYBOARD DATA AREAS		:
;----------------------------------------
KB_FLAG 	resb	1		; toggles when a shift key is depressed
KB_FLAG_1	resb	1		; reflects if a "shift" key is depressed

;----- SHIFT FLAG EQUATES WITHIN KB_FLAG/1

INS_STATE	EQU	80H		; INSERT STATE IS ACTIVE
CAPS_STATE	EQU	40H		; CAPS LOCK STATE HAS BEEN TOGGLED
NUM_STATE	EQU	20H		; NUM LOCK STATE HAS BEEN TOGGLED
SCROLL_STATE	EQU	10H		; SCROLL LOCK STATE HAS BEEN TOGGLED
ALT_SHIFT	EQU	08H		; ALTERNATE SHIFT KEY DEPRESSED
CTL_SHIFT	EQU	04H		; CONTROL SHIFT KEY DEPRESSED
LEFT_SHIFT	EQU	02H		; LEFT SHIFT KEY DEPRESSED
RIGHT_SHIFT	EQU	01H		; RIGHT SHIFT KEY DEPRESSED

ALT_INPUT	resb	1		; STORAGE FOR ALTERNATE KEYPAD ENTRY
BUFFER_HEAD	resw	1		; POINTER TO HEAD OF KEYBOARD BUFFER
BUFFER_TAIL	resw	1		; POINTER TO TAIL OF KEYBOARD BUFFER
KB_BUFFER	resw	16		; ROOM FOR 15 ENTRIES
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

		absolute	3Eh
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


;----------------------------------------
;	VIDEO DISPLAY DATA AREA 	:
;----------------------------------------
		absolute	49H
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

		absolute	6Ch
;----------------------------------------
;	    TIMER DATA AREA		:
;----------------------------------------
TIMER_LOW	resw	1		; LOW WORD OF TIMER COUNT
TIMER_HIGH	resw	1		; HIGH WORD OF TIMER COUNT
TIMER_OFL	resb	1		; TIMER HAS ROLLED OVER SINCE LAST READ







		absolute	80H	;TODO: check!
;----------------------------------------
;	EXTRA KEYBOARD DATA AREA	:
;----------------------------------------
BUFFER_START	resw	1
BUFFER_END	resw	1


		absolute	86H	; BBC KEYBOARD stuff uses PCjr slots
KB_BBC_ROLL	resb	2		; 2 key roll over buffer


		section .text
font:
		incbin	"font.bin"

_start:
handle_res:	

		; set up stack and data

		mov	AX,SEG_INIT_STACK		; Get stack segment
		mov	SS,AX	
		mov	SP,INIT_STACK_TOS		; put SP at TOS


		; initialise DEICE
		call	deice_init
		mov	AL,65
		call	DEICE_PUTCHAR

		call	deice_printstr
		db	"HELLO",0
		mov	AL,33
		call	DEICE_PUTCHAR


		; set up sys via

		mov	AL,0FH
		mov	DX,io_SHEILA_SYSVIA_DDRB
		out	DX,AL

		; set latches 6..0 to 1
		mov	DX,io_SHEILA_SYSVIA_ORB
.llp:		dec	AL
		out	DX,AL
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

;;		DBG_C	'%'
;;		call	deice_HEX2

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
		jz	.blp


.blp:		call	FDC_STAT_READ
		test	AL,1
		jnz	.blp

		ret


snd_init:
		mov	AL,0FFh
		call	snd_poke
		mov	AL,0DFh
		call	snd_poke
		mov	AL,0BFh
		call	snd_poke
		mov	AL,09Fh
		call	snd_poke
		ret


snd_poke:	pushf
		push	DX
		push	AX
		cli
		mov	DX,io_SHEILA_SYSVIA_DDRA
		mov	AL,0FFh
		out	DX,AL
		pop	AX
		mov	DX,io_SHEILA_SYSVIA_ORA_NH
		out	DX,AL
		mov	AL,000h
		mov	DX,io_SHEILA_SYSVIA_ORB
		out	DX,AL
		call	snd_wait_2
		mov	AL,008h
		mov	DX,io_SHEILA_SYSVIA_ORB
		out	DX,AL
		call	snd_wait_8

		pop	DX
		popf
		ret


; TODO: find more exact delay 
; TODO: this also used in FDC code
snd_wait_8:	call	snd_wait_4
snd_wait_4:	call	snd_wait_2
snd_wait_2:	call	snd_wait_1
snd_wait_1:	ret
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


;--- INT 10 -------------------------------------------------------------
; VIDEO_IO								:
;	THESE ROUTINES PROVIDE THE CRT INTERFACE			:
;	THE FOLLOWING FUNCTIONS ARE PROVIDED:				:
;	(AH)=0	SET MODE (AL) CONTAINS MODE VALUE			:
;		(AL)=0 40X25 BW (POWER ON DEFAULT)			:
;		(AL)=1 40X25 COLOR					:
;		(AL)=2 80X25 BW 					:
;		(AL)=3 80X25 COLOR					:
;		GRAPHICS MODES						:
;		(AL)=4 320X200 COLOR					:
;		(AL)=5 320X200 BW					:
;		(AL)=6 640X200 BW					:
;		CRT MODE=7 80X25 B&W CARD (USED INTERNAL TO VIDEO ONLY) :
;		*** NOTE BW MODES OPERATE SAME AS COLOR MODES, BUT	:
;			 COLOR BURST IS NOT ENABLED			:
;	(AH)=1	SET CURSOR TYPE 					:
;		(CH) = BITS 4-0 = START LINE FOR CURSOR 		:
;		       ** HARDWARE WILL ALWAYS CAUSE BLINK		:
;		       ** SETTING BIT 5 OR 6 WILL CAUSE ERRATIC 	:
;			  BLINKING OR NO CURSOR AT ALL			:
;		(CL) = BITS 4-0 = END LINE FOR CURSOR			:
;	(AH)=2	SET CURSOR POSITION					:
;		(DH,DL) = ROW,COLUMN  (0,0) IS UPPER LEFT		:
;		(DH) = PAGE NUMBER (MUST BE 0 FOR GRAPHICS MODES)	:
;	(AH)=3	READ CURSOR POSITION					:
;		(BH) = PAGE NUMBER (MUST BE 0 FOR GRAPHICS MODES)	:
;		ON EXIT (DH,DL) = ROW,COLUMN OF CURRENT CURSOR		:
;			(CH,CL) = CURSOR MODE CURRENTLY SET		:
;	(AH)=4	READ LIGHT PEN POSITION 				:
;		ON EXIT:						:
;		(AH) = 0 -- LIGHT PEN SWITCH NOT DOWN/NOT TRIGGERED	:
;		(AH) = 1 -- VALID LIGHT PEN VALUE IN REGISTERS		:
;			(DH,DL) = ROW, COLUMN OF CHARACTER LP POSN	:
;			(CH) = RASTER LINE (0-199)			:
;			(BX) = PIXEL COLUMN (0-319,639) 		:
;	(AH)=5	SELECT ACTIVE DISPLAY PAGE (VALID ONLY FOR ALPHA MODES) :
;		(AL)=NEW PAGE VAL (0-7 FOR MODES 0&1, 0-3 FOR MODES 2&3):
;	(AH)=6	SCROLL ACTIVE PAGE UP					:
;		(AL) = NUMBER OF LINES, INPUT LINES BLANKED AT BOTTOM	:
;		       OF WINDOW					:
;			AL = 0 MEANS BLANK ENTIRE WINDOW		:
;		(CH,CL) = ROW,COLUMN OF UPPER LEFT CORNER OF SCROLL	:
;		(DH,DL) = ROW,COLUMN OF LOWER RIGHT CORNER OF SCROLL	:
;		(BH) = ATTRIBUTE TO BE USED ON BLANK LINE		:
;	(AH)=7	SCROLL ACTIVE PAGE DOWN 				:
;		(AL) = NUMBER OF LINES, INPUT LINES BLANKED AT TOP	:
;		       OF WINDOW					:
;			AL = 0 MEANS BLANK ENTIRE WINDOW		:
;		(CH,CL) = ROW,COLUMN OF UPPER LEFT CORNER OF SCROLL	:
;		(DH,DL) = ROW,COLUMN OF LOWER RIGHT CORNER OF SCROLL	:
;		(BH) = ATTRIBUTE TO BE USED ON BLANK LINE		:
;									:
;	CHARACTER HANDLING ROUTINES					:
;									:
;	(AH) = 8 READ ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION	:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)	:
;		ON EXIT:						:
;		(AL) = CHAR READ					:
;		(AH) = ATTRIBUTE OF CHARACTER READ (ALPHA MODES ONLY)	:
;	(AH) = 9 WRITE ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION	:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)	:
;		(CX) = COUNT OF CHARACTERS TO WRITE			:
;		(AL) = CHAR TO WRITE					:
;		(BL) = ATTRIBUTE OF CHARACTER (ALPHA)/COLOR OF CHAR	:
;		       (GRAPHICS)					:
;			SEE NOTE ON WRITE DOT FOR BIT 7 OF BL = 1.	:
;	(AH) = 10 WRITE CHARACTER ONLY AT CURRENT CURSOR POSITION	:
;		(BH) = DISPLAY PAGE (VALID FOR ALPHA MODES ONLY)	:
;		(CX) = COUNT OF CHARACTERS TO WRITE			:
;		(AL) = CHAR TO WRITE					:
;	FOR READ/WRITE CHARACTER INTERFACE WHILE IN GRAPHICS MODE, THE	:
;		CHARACTERS ARE FORMED FROM A CHARACTER GENERATOR IMAGE	:
;		MAINTAINED IN THE SYSTEM ROM.  ONLY THE 1ST 128 CHARS	:
;		ARE CONTAINED THERE.  TO READ/WRITE THE SECOND 128	:
;		CHARS. THE USER MUST INITIALIZE THE POINTER AT		:
;		INTERRUPT 1FH (LOCATION 0007CH) TO POINT TO THE 1K BYTE :
;		TABLE CONTAINING THE CODE POINTS FOR THE SECOND 	:
;		128 CHARS (128-255).					:
;	FOR WRITE CHARACTER INTERFACE IN GRAPHICS MODE, THE REPLICATION :
;		FACTOR CONTAINED IN (CX) ON ENTRY WILL PRODUCE VALID	:
;		RESULTS ONLY FOR CHARACTERS CONTAINED ON THE SAME ROW.	:
;		CONTINUATION TO SUCCEEDING LINES WILL NOT PRODUCE	:
;		CORRECTLY.						:
;									:
;	GRAPHICS INTERFACE						:
;	(AH) = 11 SET COLOR PALETTE					:
;		(BH) = PALETTE COLOR ID BEING SET (0-127)		:
;		(BL) = COLOR VALUE TO BE USED WITH THAT COLOR ID	:
;		   NOTE: FOR THE CURRENT COLOR CARD, THIS ENTRY POINT	:
;			 HAS MEANING ONLY FOR 320X200 GRAPHICS. 	:
;			COLOR ID = 0 SELECTS THE BACKGROUND COLOR (0-15):
;			COLOR ID = 1 SELECTS THE PALETTE TO BE USED:	:
;				0 = GREEN(1)/RED(2)/YELLOW(3)		:
;				1 = CYAN(1)/MAGENTA(2)/WHITE(3) 	:
;			IN 40X25 OR 80X25 ALPHA MODES, THE VALUE SET	:
;				FOR PALETTE COLOR 0 INDICATES THE	:
;				BORDER COLOR TO BE USED (VALUES 0-31,	:
;				WHERE 16-31 SELECT THE HIGH INTENSITY	:
;				BACKGROUND SET. 			:
;	(AH) = 12 WRITE DOT						:
;		(DX) = ROW NUMBER					:
;		(CX) = COLUMN NUMBER					:
;		(AL) = COLOR VALUE					:
;			IF BIT 7 OF AL = 1, THEN THE COLOR VALUE IS	:
;			EXCLUSIVE OR'D WITH THE CURRENT CONTENTS OF     :
;			THE DOT 					:
;	(AH) = 13 READ DOT						:
;		(DX) = ROW NUMBER					:
;		(CX) = COLUMN NUMBER					:
;		(AL) RETURNS THE DOT READ				:
;									:
; ASCII TELETYPE ROUTINE FOR OUTPUT					:
;									:
;	(AH) = 14 WRITE TELETYPE TO ACTIVE PAGE 			:
;		(AL) = CHAR TO WRITE					:
;		(BL) = FOREGROUND COLOR IN GRAPHICS MODE		:
;		NOTE -- SCREEN WIDTH IS CONTROLLED BY PREVIOUS MODE SET :
;									:
;	(AH) = 15 CURRENT VIDEO STATE					:
;		RETURNS THE CURRENT VIDEO STATE 			:
;		(AL) = MODE CURRENTLY SET (SEE AH=0 FOR EXPLANATION)	:
;		(AH) = NUMBER OF CHARACTER COLUMNS ON SCREEN		:
;		(BH) = CURRENT ACTIVE DISPLAY PAGE			:
;									:
;	CS,SS,DS,ES,BX,CX,DX PRESERVED DURING CALL			:
;	ALL OTHERS DESTROYED						:
;------------------------------------------------------------------------

TBL_VID:	;TABLE OF ROUTINES WITHIN VIDEO I/O
	dw	SET_MODE		;0
	dw	SET_CTYPE		;1
	dw	SET_CPOS		;2
	dw	READ_CURSOR		;3
	dw	READ_LPEN		;4
	dw	ACT_DISP_PAGE		;5
	dw	SCROLL_UP		;6
	dw	SCROLL_DOWN		;7
	dw	READ_AC_CURRENT		;8
	dw	WRITE_AC_CURRENT	;9
	dw	WRITE_C_CURRENT		;10
	dw	SET_COLOR		;11
	dw	WRITE_DOT		;12
	dw	READ_DOT		;13
	dw	WRITE_TTY		;14
	dw	VIDEO_STATE		;15
TBL_VID_LEN	EQU	$-TBL_VID

VIDEO_IO:
	sti				
	cld				
	; SAVE SEGMENT REGISTERS - restore with jmp to VIDEO_RETURN
	push	ES
	push	DS			
	push	DX
	push	CX
	push	BX
	push	SI
	push	DI
	push	AX			; save AX
	mov	AL,AH			; 
	xor	AH,AH			; 
	sal	AX,1			; 
	mov	SI,AX			; SI = 2*AH
	cmp	AX,TBL_VID_LEN		; check against table len
	jb	.M2			; ok
	pop	AX			; restore parameter
	jmp	VIDEO_RETURN		; and do nowt

.M2:	call	DDS			; 
	mov	AX,0B000H		; TODO: where to look this up? A table?
	mov	ES,AX			; SET UP TO POINT AT VIDEO RAM AREAS
	pop	AX			; RECOVER VALUE
	mov	AH,[CRT_MODE]		; GET CURRENT MODE INTO AH
	jmp	word [CS:SI+TBL_VID]


;--------------------------------------------------------
; SET_MODE						:
;	THIS ROUTINE INITIALIZES THE ATTACHMENT TO	:
;	THE SELECTED MODE.  THE SCREEN IS BLANKED.	:
; INPUT 						:
;	(AL) = MODE SELECTED (RANGE 0-9)		:
; OUTPUT						:
;	NONE						:
;--------------------------------------------------------

;----- TABLES FOR USE IN SETTING OF MODE

VIDEO_PARMS:
;----- INIT_TABLE
	DB	07FH,050H,062H,028H,026H,000H,020H,022H	; actually mode 0,12 from MOS
	DB	001H,007H,066H,007H
	DB	006H,000H,0,0
M4	EQU	$-VIDEO_PARMS

	DB	07FH,050H,062H,028H,026H,000H,020H,022H	; actually mode 0,12 from MOS
	DB	001H,007H,066H,007H
	DB	006H,000H,0,0

	DB	07FH,050H,062H,028H,026H,000H,020H,022H	; actually mode 0,12 from MOS
	DB	001H,007H,066H,007H
	DB	006H,000H,0,0

	DB	07FH,050H,062H,028H,026H,000H,020H,022H	; actually mode 0,12 from MOS
	DB	001H,007H,066H,007H
	DB	006H,000H,0,0

M5:					; TABLE OF REGEN LENGTHS
	DW	2048			; 40X25
	DW	4096			; 80X25
	DW	16384			; GRAPHICS
	DW	16384

;----- COLUMNS

M6:
	DB	40,40,80,80,40,40,80,80

;----- C_REG_TAB

M7:					; TABLE OF MODE SETS
	DB	0D8H			; PC MODE 0 = BBC MODE 1
	DB	0D8H			; PC MODE 0 = BBC MODE 1
	DB	09CH			; PC MODE 0 = BBC MODE 0
	DB	09CH			; PC MODE 0 = BBC MODE 0
	DB	0D8H			; PC MODE 0 = BBC MODE 1
	DB	0D8H			; PC MODE 0 = BBC MODE 1
	DB	0D8H			; PC MODE 0 = BBC MODE 1
	DB	09CH			; PC MODE 0 = BBC MODE 0

SET_MODE:

	mov	DX,io_SHEILA_CRTC	; SHEILA address of CRTC
	mov	BL,0			; MODE SET FOR COLOR CARD

	;TODO: force BW mode 7
	mov	AL,7
	mov	[CRT_MODE],AL		; save in global variable

	push	DS


	sub	AX,AX			; data segment to ABS 0
	mov	DS,AX			

	lds	BX,[PARM_PTR]		; DS:BX now points at video parameters

	mov	AH,[CRT_MODE]
	
	mov	CX,M4			; Table entry length - TODO: change?
	cmp	AH,2			
	jc	.M9			; modes 0,1
	add	BX,CX			; next entry
	cmp	AH,4
	jc	.M9			; modes 2,3
	add	BX,CX			; gfx rows TODO: sort these out
	cmp	AH,7
	jc	.M9			; modes 4,5,6
	add	BX,CX			; BW modes

;----- DS:BX now points to a CRTC initialise table row

.M9:					
	push	AX			; save mode in ah
	xor	AH,AH			; ah=0, use as CRTC register index					

;----- initialise CRTC from table

.M10:					; CRTC init loop
	mov	AL,AH			; 6845 addr
	out	DX,AL
	inc	DX			; point to data port
	inc	AH			; next register value
	mov	AL,[BX] 		; get table value
	out	DX,AL			; poke CRTC data
	inc	BX			
	dec	DX			
	loop	.M10			


	pop	AX			; GET MODE BACK
	pop	DS			; RECOVER SEGMENT VALUE

;----- blank REGEN area AND CRT area
 
 	xor	DI,DI			; SET UP POINTER FOR REGEN
 	mov	[CRT_START],DI		; START ADDRESS SAVED IN GLOBAL
 	mov	byte [ACTIVE_PAGE],0	; SET PAGE VALUE
 	mov	CX,8192 		; NUMBER OF WORDS IN COLOR CARD
;; 	cmp	AH,4			; TEST FOR GRAPHICS
;; 	jc	.M12			; NO_GRAPHICS_INIT
;; 	cmp	AH,7			; TEST FOR BW CARD
;; 	je	.M11			; BW_CARD_INIT
;; 	xor	AX,AX			; FILL FOR GRAPHICS MODE
;; 	jmp	SHORT .M13		; CLEAR_BUFFER
;;.M11:					; BW_CARD_INIT
;;	mov	CH,08H			; BUFFER SIZE ON BW CARD
;;.M12:					; NO_GRAPHICS_INIT
	mov	AX,' '+7*256            ; FILL CHAR FOR ALPHA
.M13:					; CLEAR BUFFER

.cllp:	mov	[ES:DI],AX
	call	bbc_update_char_AX
	inc	DI
	inc	DI
	loop	.cllp			; FILL THE REGEN BUFFER WITH BLANKS

;----- get ULA control byte and set and save
	mov	AL,[CRT_MODE]		; get back mode number
	xor	AH,AH			; 
	mov	SI,AX			; and make a table index

	mov	DX,io_SHEILA_ULA
	mov	AL,[CS:SI+M7]
	out	DX,AL			; set ULA
	mov	[CRT_MODE_SET],AL 	; save ULA value TODO: is this needed?

;----- set the default cursor

	mov	word [CURSOR_MODE],607H	; reset current cursor mode



;----- set number of columns for this mode

	mov	AL,[CS:SI+M6]
	xor	AH,AH
	mov	[CRT_COLS],AX		; NUMBER OF COLUMNS IN THIS SCREEN

;----- set REGEN buffer len - TODO: move above during length calc?!

	AND	SI,0EH			; WORD OFFSET INTO CLEAR LENGTH TABLE
	MOV	CX,[CS:SI+M5]		; LENGTH TO CLEAR
	MOV	[CRT_LEN],CX		; SAVE LENGTH OF CRT -- NOT USED FOR BW


	mov	CX,8			; CLEAR ALL CURSOR POSITIONS
	mov	DI,CURSOR_POSN
	push	DS			; ESTABLISH SEGMENT
	pop	ES			;   ADDRESSING
	xor	AX,AX
	rep	STOSW			; FILL WITH ZEROES


; -- TODO: proper palette handling, only B&W no 4 colours here!
	
	mov	CX,008h
	mov	AL,00Fh
	mov	DX,io_SHEILA_ULA_PAL
.plp:	out	DX,AL
	add	AL,010h
	loop	.plp
	and	AL,0F0h
	mov	CX,008h
.plp2:	out	DX,AL
	add	AL,010h
	loop	.plp2

	xor	AX,AX
	call	M18			; set cursor to top left

;----- shared VIDEO exit

VIDEO_RETURN:
	pop	DI
	pop	SI
	pop	BX
M15:					; VIDEO_RETURN_C
	pop	CX
	pop	DX
	pop	DS
	pop	ES			; RECOVER SEGMENTS
	iret				; ALL DONE


;----------------------------------------------------------------
; SET_CTYPE							:
;	THIS ROUTINE SETS THE CURSOR VALUE			:
; INPUT 							:
;	(CX) HAS CURSOR VALUE CH-START LINE, CL-STOP LINE	:
; OUTPUT							:
;	NONE							:
;----------------------------------------------------------------
SET_CTYPE:
	mov	AH,10			; 6845 register for cursor set
	mov	[CURSOR_MODE],CX	; save in data area

	;TODO: remove / refine
	mov	CX,0607h		; force cursor

	call	M16			; output CX reg
	jmp	VIDEO_RETURN

;----- Poke CRTC register AH with CH, AH+1 with CL

M16_600:
	push	CX
	and	CH,07h			; constrain to BBC CRTC addresses
	add	CH,06h
	call	M16
	pop	CX
	ret

M16:

	mov	DX,io_SHEILA_CRTC	; CRTC address reg
	mov	AL,AH			
	out	DX,AL			; set addr = AH+0

	inc	DX			
	mov	AL,CH			
	out	DX,AL			; output data
	dec	DX			
	mov	AL,AH
	inc	AL			
	out	DX,AL			; set addr = AH+1
	inc	DX
	mov	AL,CL			
	out	DX,AL
	ret				

;------------------------------------------------
; SET_CPOS					:
;	THIS ROUTINE SETS THE CURRENT CURSOR	:
;	POSITION TO THE NEW X-Y VALUES PASSED	:
; INPUT 					:
;	DX - ROW, COLUMN OF NEW CURSOR		:
;	BH - DISPLAY PAGE OF CURSOR		:
; OUTPUT					:
;	CURSOR IS SET AT 6845 IF DISPLAY PAGE	:
;	IS CURRENT DISPLAY			:
;------------------------------------------------
SET_CPOS:
	mov	CL,BH
	xor	CH,CH			
	sal	CX,1			
	mov	SI,CX			
	mov	[SI+CURSOR_POSN],DX	; save cursor for page
	cmp	[ACTIVE_PAGE],BH
	jnz	.M17			; not current, return
	mov	AX,DX			; move to AX
	call	M18			; and call M18
.M17:	jmp	VIDEO_RETURN

M18:
	; TODO: this assumes that it is only called for current page
	call	POSITION		; DETERMINE LOCATION IN REGEN BUFFER
	mov	CX,AX
	add	CX,[CRT_START]		; ADD IN THE START ADDR FOR THIS PAGE
	sar	CX,1			; DIVIDE BY 2 FOR CHAR ONLY COUNT
	mov	AH,14			; REGISTER NUMBER FOR CURSOR
	call	M16_600			; OUTPUT THE VALUE TO THE 6845
	ret

;----------------------------------------------------------------
; ACT_DISP_PAGE 						:
;	TODO: this does nothing at present!			:
; INPUT 							:
;	AL HAS THE NEW ACTIVE DISPLAY PAGE			:
; OUTPUT							:
;	THE 6845 IS RESET TO DISPLAY THAT PAGE			:
;----------------------------------------------------------------
ACT_DISP_PAGE:

	call	deice_printstr
	db	'PAG:',0
	call	deice_HEX2
	call	deice_CRLF


	cmp	[ACTIVE_PAGE],AL
	; page is changing - restore screen
	pushf

	MOV	[ACTIVE_PAGE],AL	; SAVE ACTIVE PAGE VALUE
	MOV	CX,[CRT_LEN]		; GET SAVED LENGTH OF REGEN BUFFER
	CBW				; CONVERT AL TO WORD
	PUSH	AX			; SAVE PAGE VALUE
	MUL	CX			; DISPLAY PAGE TIMES REGEN LENGTH
	MOV	[CRT_START],AX		; SAVE START ADDRESS FOR
					;  LATER REQUIREMENTS
	MOV	CX,AX			; START ADDRESS TO CX
	SAR	CX,1			; DIVIDE BY 2 FOR 6845 HANDLING
	MOV	AH,12			; 6845 REGISTER FOR START ADDRESS
	CALL	M16_600
	POP	BX			; RECOVER PAGE VALUE
	SAL	BX,1			; *2 FOR WORD OFFSET
	MOV	AX,[BX+CURSOR_POSN]	; GET CURSOR FOR THIS PAGE
	call	M18
	popf
	jz	.sk
	; page changed - refresh BBC screen
	push	DI
	push	AX
	push	CX
	cld
	mov	CX,[CRT_LEN]
	mov	DI,[CRT_START]
.lp:	mov	AX,[ES:DI]
	call	bbc_update_char_AX
	inc	DI
	inc	DI
	loop	.lp
	pop	CX
	pop	AX
	pop	DI

.sk:	JMP	VIDEO_RETURN

;----------------------------------------------------------------
; READ_CURSOR							:
;	THIS ROUTINE READS THE CURRENT CURSOR VALUE FROM THE	:
;	6845, FORMATS IT, AND SENDS IT BACK TO THE CALLER	:
; INPUT 							:
;	BH - PAGE OF CURSOR					:
; OUTPUT							:
;	DX - ROW, COLUMN OF THE CURRENT CURSOR POSITION 	:
;	CX - CURRENT CURSOR MODE				:
;----------------------------------------------------------------
READ_CURSOR:
	mov	BL,BH
	xor	BH,BH
	sal	BX,1		
	mov	DX,[BX+CURSOR_POSN]
	mov	CX,[CURSOR_MODE]
	pop	DI
	pop	SI
	pop	BX
	pop	AX			; don't pop CX AND DX, TODO: poke to stack?
	pop	AX
	pop	DS
	pop	ES
	iret

;------------------------------------------------------------------------
; SET_COLOR								:
;	THIS ROUTINE WILL ESTABLISH THE BACKGROUND COLOR, THE OVERSCAN	:
;	COLOR, AND THE FOREGROUND COLOR SET FOR MEDIUM RESOLUTION	:
;	GRAPHICS							:
;	TODO: does nothing
; INPUT 								:
;	(BH) HAS COLOR ID						:
;		IF BH=0, THE BACKGROUND COLOR VALUE IS SET		:
;			FROM THE LOW BITS OF BL (0-31)			:
;		IF BH=1, THE PALETTE SELECTION IS MADE			:
;			BASED ON THE LOW BIT OF BL:			:
;				0=GREEN, RED, YELLOW FOR COLORS 1,2,3	:
;				1=BLUE, CYAN, MAGENTA FOR COLORS 1,2,3	:
;	(BL) HAS THE COLOR VALUE TO BE USED				:
; OUTPUT								:
;	THE COLOR SELECTION IS UPDATED					:
;------------------------------------------------------------------------
SET_COLOR:
	jmp	VIDEO_RETURN


;------------------------------------------------
; VIDEO STATE					:
;  RETURNS THE CURRENT VIDEO STATE IN AX	:
;  AH = NUMBER OF COLUMNS ON THE SCREEN 	:
;  AL = CURRENT VIDEO MODE			:
;  BH = CURRENT ACTIVE PAGE			:
;------------------------------------------------
VIDEO_STATE:
	mov	AH,byte [CRT_COLS]
	mov	AL,[CRT_MODE]
	mov	BH,[ACTIVE_PAGE]
	pop	DI
	pop	SI
	pop	CX			; this is the passed in BX but M15 will restore CX over this
	jmp	M15

;--------------------------------------------------------
; POSITION						:
;	Calculate a regen buffer char address		:
; INPUT 						:
;	AH = row, AL = col				:
; OUTPUT						:
;	AX = offset from start of regen buffer (2 	:
;       bytes per char!)				:
;--------------------------------------------------------
POSITION:
	push	BX			
	mov	BX,AX
	mov	AL,AH			
	mul	byte [CRT_COLS]
	xor	BH,BH
	add	AX,BX			
	sal	AX,1			
	pop	BX
	ret

	;TODO: faster scrolling
;--------------------------------------------------------
; SCROLL_UP						:
;	THIS ROUTINE MOVES A BLOCK OF CHARACTERS UP	:
;	ON THE SCREEN					:
; INPUT 						:
;	(AH) = CURRENT CRT MODE 			:
;	(AL) = NUMBER OF ROWS TO SCROLL 		:
;	(CX) = ROW/COLUMN OF UPPER LEFT CORNER		:
;	(DX) = ROW/COLUMN OF LOWER RIGHT CORNER 	:
;	(BH) = ATTRIBUTE TO BE USED ON BLANKED LINE	:
;	(DS) = DATA SEGMENT				:
;	(ES) = REGEN BUFFER SEGMENT			:
; OUTPUT						:
;	NONE -- THE REGEN BUFFER IS MODIFIED		:
;--------------------------------------------------------
SCROLL_UP:
	MOV	BL,AL			; SAVE LINE COUNT IN BL
;;	CMP	AH,4			; TEST FOR GRAPHICS MODE
;;	JC	N1			; HANDLE SEPARATELY
;;	CMP	AH,7			; TEST FOR BW CARD
;;	JE	N1
;;	JMP	GRAPHICS_UP
;;N1:					; UP_CONTINUE
	PUSH	BX			; SAVE FILL ATTRIBUTE IN BH
	MOV	AX,CX			; UPPER LEFT POSITION
	CALL	SCROLL_POSITION 	; DO SETUP FOR SCROLL
	JZ	N7			; BLANK_FIELD
	ADD	SI,AX			; FROM ADDRESS
	MOV	AH,DH			; # ROWS IN BLOCK
	SUB	AH,BL			; # ROWS TO BE MOVED
N2:					; ROW_LOOP
	CALL	N10			; MOVE ONE ROW
	ADD	SI,BP
	ADD	DI,BP			; POINT TO NEXT LINE IN BLOCK
	DEC	AH			; COUNT OF LINES TO MOVE
	JNZ	N2			; ROW_LOOP
N3:					; CLEAR_ENTRY
	POP	AX			; RECOVER ATTRIBUTE IN AH
	MOV	AL,' '                  ; FILL WITH BLANKS
N4:					; CLEAR_LOOP
	CALL	N11			; CLEAR THE ROW
	ADD	DI,BP			; POINT TO NEXT LINE
	DEC	BL			; COUNTER OF LINES TO SCROLL
	JNZ	N4			; CLEAR_LOOP
N5:					; SCROLL_END
	CALL	DDS
;;	CMP	[CRT_MODE],7		; IS THIS THE BLACK AND WHITE CARD
;;	JE	N6			; IF SO, SKIP THE MODE RESET
;;	MOV	AL,CRT_MODE_SET 	; GET THE VALUE OF THE MODE SET
;;	MOV	DX,03D8H		; ALWAYS SET COLOR CARD PORT
;;	OUT	DX,AL
;;N6:					; VIDEO_RET_HERE
	JMP	VIDEO_RETURN
N7:					; BLANK_FIELD
	MOV	BL,DH			; GET ROW COUNT
	JMP	N3			; GO CLEAR THAT AREA

;----- HANDLE COMMON SCROLL SET UP HERE

SCROLL_POSITION:
;;	CMP	CRT_MODE,2		; TEST FOR SPECIAL CASE HERE
;;	JB	N9			; HAVE TO HANDLE 80X25 SEPARATELY
;;	CMP	CRT_MODE,3
;;	JA	N9
;;
;;;----- 80X25 COLOR CARD SCROLL
;;
;;	PUSH	DX
;;	MOV	DX,3DAH 		; GUARANTEED TO BE COLOR CARD HERE
;;	PUSH	AX
;;N8:					; WAIT_DISP_ENABLE
;;	IN	AL,DX			; GET PORT
;;	TEST	AL,8			; WAIT FOR VERTICAL RETRACE
;;	JZ	N8			; WAIT_DISP_ENABLE
;;	MOV	AL,25H
;;	MOV	DL,0D8H 		; DX=3D8
;;	OUT	DX,AL			; TURN OFF VIDEO
;;	POP	AX			; DURING VERTICAL RETRACE
;;	POP	DX
N9:
	CALL	POSITION		; CONVERT TO REGEN POINTER
	ADD	AX,[CRT_START]		; OFFSET OF ACTIVE PAGE
	MOV	DI,AX			; TO ADDRESS FOR SCROLL
	MOV	SI,AX			; FROM ADDRESS FOR SCROLL
	SUB	DX,CX			; DX = #ROWS, #COLS IN BLOCK
	INC	DH
	INC	DL			; INCREMENT FOR 0 ORIGIN
	XOR	CH,CH			; SET HIGH BYTE OF COUNT TO ZERO
	MOV	BP,[CRT_COLS]		; GET NUMBER OF COLUMNS IN DISPLAY
	ADD	BP,BP			; TIMES 2 FOR ATTRIBUTE BYTE
	MOV	AL,BL			; GET LINE COUNT
	MUL	byte [CRT_COLS]		; DETERMINE OFFSET TO FROM ADDRESS
	ADD	AX,AX			; *2 FOR ATTRIBUTE BYTE
	PUSH	ES			; ESTABLISH ADDRESSING TO REGEN BUFFER
	POP	DS			;  FOR BOTH POINTERS
	CMP	BL,0			; 0 SCROLL MEANS BLANK FIELD
	RET				; RETURN WITH FLAGS SET

;----- MOVE_ROW

N10:
	MOV	CL,DL			; GET # OF COLS TO MOVE
	PUSH	SI
	PUSH	DI			; SAVE START ADDRESS
	PUSH	AX
	xor	CH,CH
	or	CL,CL
	jz	.sk
.lp:	mov	AX,[DS:SI]
	inc	SI
	inc	SI
	call	bbc_update_char_AX
	stosw		; MOVE THAT LINE ON SCREEN
	loop	.lp
.sk	POP	AX
	POP	DI
	POP	SI			; RECOVER ADDRESSES
	RET

;----- CLEAR_ROW

N11:
	MOV	CL,DL			; GET # COLUMNS TO CLEAR
	PUSH	DI
	xor	CH,CH
	or	CL,CL
	jz	.sk
.lp:	call	bbc_update_char_AX
	STOSW			; STORE THE FILL CHARACTER
	loop	.lp
.sk:	POP	DI
	RET

SCROLL_DOWN:

	STD				; DIRECTION FOR SCROLL
	MOV	BL,AL			; LINE COUNT TO BL
;;	CMP	AH,4			; TEST FOR GRAPHICS
;;	JC	N12
;;	CMP	AH,7			; TEST FOR BW CARD
;;	JE	N12
;;	JMP	GRAPHICS_DOWN
N12:					; CONTINUE DOWN
	PUSH	BX			; SAVE ATTRIBUTE IN BH
	MOV	AX,DX			; LOWER RIGHT CORNER
	CALL	SCROLL_POSITION 	; GET REGEN LOCATION
	JZ	N16
	SUB	SI,AX			; SI IS FROM ADDRESS
	MOV	AH,DH			; GET TOTAL # ROWS
	SUB	AH,BL			; COUNT TO MOVE IN SCROLL
N13:
	CALL	N10			; MOVE ONE ROW
	SUB	SI,BP
	SUB	DI,BP
	DEC	AH
	JNZ	N13
N14:
	POP	AX			; RECOVER ATTRIBUTE IN AH
	MOV	AL,' '
N15:
	CALL	N11			; CLEAR ONE ROW
	SUB	DI,BP			; GO TO NEXT ROW
	DEC	BL
	JNZ	N15
	JMP	N5			; SCROLL_END
N16:
	MOV	BL,DH
	JMP	N14


;--------------------------------------------------------
; READ_AC_CURRENT					:
;	THIS ROUTINE READS THE ATTRIBUTE AND CHARACTER	:
;	AT THE CURRENT CURSOR POSITION AND RETURNS THEM :
;	TO THE CALLER					:
; INPUT 						:
;	(AH) = CURRENT CRT MODE 			:
;	(BH) = DISPLAY PAGE ( ALPHA MODES ONLY )	:
;	(DS) = DATA SEGMENT				:
;	(ES) = REGEN SEGMENT				:
; OUTPUT						:
;	(AL) = CHAR READ				:
;	(AH) = ATTRIBUTE READ				:
;--------------------------------------------------------
READ_AC_CURRENT:
	;TODO: graphics
;	cmp	AH,4			; IS THIS GRAPHICS
;	jc	P1
;	cmp	AH,7			; IS THIS BW CARD
;	je	P1
;	jmp	GRAPHICS_READ
;P1:					; READ_AC_CONTINUE
	call	FIND_POSITION
	mov	SI,BX			; ESTABLISH ADDRESSING IN SI

	mov	AX,[ES:SI]
	jmp	VIDEO_RETURN

FIND_POSITION:
	mov	AL,[CRT_LEN+1]
	mul	BH			; start of buffer/256
	xchg	BH,AL
	xor	BL,BL			; now BX is page base	
	xor	AH,AH			; AX is page index
	mov	SI,AX
	mov	AX,[SI+CURSOR_POSN]
	call	POSITION
	add	BX,AX
	ret

;------------------------------------------------
; WRITE_AC_CURRENT				:
;	THIS ROUTINE WRITES THE ATTRIBUTE	:
;	AND CHARACTER AT THE CURRENT CURSOR	:
;	POSITION				:
; INPUT 					:
;	(AH) = CURRENT CRT MODE 		:
;	(BH) = DISPLAY PAGE			:
;	(CX) = COUNT OF CHARACTERS TO WRITE	:
;	(AL) = CHAR TO WRITE			:
;	(BL) = ATTRIBUTE OF CHAR TO WRITE	:
;	(DS) = DATA SEGMENT			:
;	(ES) = REGEN SEGMENT			:
; OUTPUT					:
;	NONE					:
;------------------------------------------------
WRITE_AC_CURRENT:

; TODO: graphics
;	CMP	AH,4			; IS THIS GRAPHICS
;	JC	P6
;	CMP	AH,7			; IS THIS BW CARD
;	JE	P6
;	JMP	GRAPHICS_WRITE
;P6:					; WRITE_AC_CONTINUE
	push	BX
	mov	AH,BL			; get attribute to ah
	push	AX			; save on stack
	push	CX			; save write count
	call	FIND_POSITION		; get buffer address
	mov	DI,BX			; address to di register
	pop	CX			; write count
	pop	AX			; char and attr in AX
	pop	BX			; get back page
	cmp	[ACTIVE_PAGE],BH
	jnz	.P7_fast

.P7:	mov	[ES:DI],AX		; PUT THE CHAR/ATTR
	call	bbc_update_char_AX
	inc	DI
	inc	DI
	loop	.P7			;  AS MANY TIMES AS REQUESTED
	jmp	VIDEO_RETURN
.P7_fast:
	rep	stosw
	jmp	VIDEO_RETURN


;------------------------------------------------
; WRITE_C_CURRENT				:
;	THIS ROUTINE WRITES THE CHARACTER AT	:
;	THE CURRENT CURSOR POSITION, ATTRIBUTE	:
;	UNCHANGED				:
; INPUT 					:
;	(AH) = CURRENT CRT MODE 		:
;	(BH) = DISPLAY PAGE			:
;	(CX) = COUNT OF CHARACTERS TO WRITE	:
;	(AL) = CHAR TO WRITE			:
;	(DS) = DATA SEGMENT			:
;	(ES) = REGEN SEGMENT			:
;OUTPUT 					:
;	NONE					:
;------------------------------------------------
WRITE_C_CURRENT:
; TODO: graphics
;	CMP	AH,4			; IS THIS GRAPHICS
;	JC	P10
;	CMP	AH,7			; IS THIS BW CARD
;	JE	P10
;	JMP	GRAPHICS_WRITE
;P10:
	push	BX
	mov	AH,BL			; get attribute to ah
	push	AX			; save on stack
	push	CX			; save write count
	call	FIND_POSITION		; get buffer address
	mov	DI,BX			; address to di register
	pop	CX			; write count
	pop	AX			; char and attr in AX
	pop	BX			; get back page
	cmp	[ACTIVE_PAGE],BH
	jnz	.P7_fast

.P7:	mov	[ES:DI],AL		; PUT THE CHAR/ATTR
	mov	AH,[ES:DI+1]
	call	bbc_update_char_AX
	inc	DI
	inc	DI
	loop	.P7			;  AS MANY TIMES AS REQUESTED
	jmp	VIDEO_RETURN
.P7_fast:
	cld
	stosb
	inc	DI
	loop	.P7_fast

	jmp	VIDEO_RETURN


;;;;TODO: graphics
READ_DOT:
WRITE_DOT:
	jmp	VIDEO_RETURN


;------------------------------------------------------------------------
; WRITE_TTY								:
;	THIS INTERFACE PROVIDES A TELETYPE LIKE INTERFACE TO THE VIDEO	:
;	CARD. THE INPUT CHARACTER IS WRITTEN TO THE CURRENT CURSOR	:
;	POSITION, AND THE CURSOR IS MOVED TO THE NEXT POSITION. IF THE	:
;	CURSOR LEAVES THE LAST COLUMN OF THE FIELD, THE COLUMN IS SET	:
;	TO ZERO, AND THE ROW VALUE IS INCREMENTED. IF THE ROW VALUE	:
;	LEAVES THE FIELD, THE CURSOR IS PLACED ON THE LAST ROW, FIRST	:
;	COLUMN, AND THE ENTIRE SCREEN IS SCROLLED UP ONE LINE. WHEN	:
;	THE SCREEN IS SCROLLED UP, THE ATTRIBUTE FOR FILLING THE NEWLY	:
;	BLANKED LINE IS READ FROM THE CURSOR POSITION ON THE PREVIOUS	:
;	LINE BEFORE THE SCROLL, IN CHARACTER MODE. IN GRAPHICS MODE,	:
;	THE 0 COLOR IS USED.						:
; ENTRY 								:
;	(AH) = CURRENT CRT MODE 					:
;	(AL) = CHARACTER TO BE WRITTEN					:
;	 NOTE THAT BACK SPACE, CAR RET, BELL AND LINE FEED ARE HANDLED	:
;	 AS COMMANDS RATHER THAN AS DISPLAYABLE GRAPHICS		:
;	(BL) = FOREGROUND COLOR FOR CHAR WRITE IF CURRENTLY IN A	:
;	 GRAPHICS MODE							:
; EXIT									:
;	ALL REGISTERS SAVED						:
;------------------------------------------------------------------------
WRITE_TTY:
	PUSH	AX			; SAVE REGISTERS
	PUSH	AX			; SAVE CHAR TO WRITE
	MOV	AH,3
	MOV	BH,[ACTIVE_PAGE]	; GET THE CURRENT ACTIVE PAGE
	INT	10H			; READ THE CURRENT CURSOR POSITION
	POP	AX			; RECOVER CHAR

;----- DX NOW HAS THE CURRENT CURSOR POSITION

	CMP	AL,8			; IS IT A BACKSPACE
	JE	U8			; BACK_SPACE
	CMP	AL,0DH			; IS IT CARRIAGE RETURN
	JE	U9			; CAR_RET
	CMP	AL,0AH			; IS IT A LINE FEED
	JE	U10			; LINE_FEED
	CMP	AL,07H			; IS IT A BELL
	JE	U11			; BELL

;----- WRITE THE CHAR TO THE SCREEN


	MOV	AH,10			; WRITE CHAR ONLY
	MOV	CX,1			; ONLY ONE CHAR
	INT	10H			; WRITE THE CHAR

;----- POSITION THE CURSOR FOR NEXT CHAR

	INC	DL
	CMP	DL,[CRT_COLS]		; TEST FOR COLUMN OVERFLOW
	JNZ	U7			; SET CURSOR
	MOV	DL,0			; COLUMN FOR CURSOR
	CMP	DH,24
	JNZ	U6			; SET_CURSOR_INC

;----- SCROLL REQUIRED

U1:
	MOV	AH,2
	INT	10H			; SET THE CURSOR

;----- DETERMINE VALUE TO FILL WITH DURING SCROLL

	MOV	AL,[CRT_MODE]		; GET THE CURRENT MODE
	CMP	AL,4
	JC	U2			; READ_CURSOR
	CMP	AL,7
	MOV	BH,0			; FILL WITH BACKGROUND
	JNE	U3			; SCROLL_UP
U2:					; READ_CURSOR
	MOV	AH,8
	INT	10H			; READ CHAR/ATTR AT CURRENT CURSOR
	MOV	BH,AH			; STORE IN BH
U3:					; SCROLL-UP
	MOV	AX,601H 		; SCROLL ONE LINE
	SUB	CX,CX			; UPPER LEFT CORNER
	MOV	DH,24			; LOWER RIGHT ROW
	MOV	DL,[CRT_COLS]		; LOWER RIGHT COLUMN
	DEC	DL
U4:					; VIDEO-CALL-RETURN
	INT	10H			; SCROLL UP THE SCREEN
U5:					; TTY-RETURN
	POP	AX			; RESTORE THE CHARACTER
	JMP	VIDEO_RETURN		; RETURN TO CALLER
U6:					; SET-CURSOR-INC
	INC	DH			; NEXT ROW
U7:					; SET-CURSOR
	MOV	AH,2
	JMP	U4			; ESTABLISH THE NEW CURSOR

;----- BACK SPACE FOUND

U8:
	CMP	DL,0			; ALREADY AT END OF LINE
	JE	U7			; SET_CURSOR
	DEC	DL			; NO -- JUST MOVE IT BACK
	JMP	U7			; SET_CURSOR

;----- CARRIAGE RETURN FOUND

U9:
	MOV	DL,0			; MOVE TO FIRST COLUMN
	JMP	U7			; SET_CURSOR

;----- LINE FEED FOUND

U10:
	CMP	DH,24			; BOTTOM OF SCREEN
	JNE	U6			; YES, SCROLL THE SCREEN
	JMP	U1			; NO, JUST SET THE CURSOR

;----- BELL FOUND

U11:
	MOV	BL,2			; SET UP COUNT FOR BEEP
	CALL	BEEP			; SOUND THE POD BELL
	JMP	U5			; TTY_RETURN




;----------------------------------------------------------------
; bbc_update_char						:
;	blit a char to the display				:
; INPUT								:
;	AX	attr/char to update				:
;	ES:DI - pointer to regen buffer				:
;----------------------------------------------------------------

bbc_update_char_AX:
	pushf
	push	ES
	push	DS
	push	CX
	push	BX
	push	SI
	push	DI
	push	AX
	call	DDS
;	mov	BH,0FFH
;.lpDI:	inc	BH
;	sub	DI,[CRT_LEN]
;	jnc	.lpDI
;;;	cmp	BH,[ACTIVE_PAGE]
;;;	jnz	.ex
;	add	DI,[CRT_LEN]
	and	DI,00FFEh
	add	DI,DI
	add	DI,DI
	cmp	DI,05000h
	jae	.ex	

	;TODO: 40 cols
	;TODO: attributes

	mov	AH,8
	mul	AH
	add	AX,font
	mov	SI,AX
	mov	AX,CS
	mov	DS,AX

	; get ready to store/move
	cld
	mov	AX,0F300h
	mov	ES,AX
	mov	CX,8

	pop	AX
	push	AX		; get back attribute
	mov	AL,AH
	and	AL,77h
	jz	.blank
	cmp	AL,70h
	jz	.inv		; TODO: high intensity inverted
	test	AH,8
	jnz	.bri


.norm:
	rep	movsb

.ex:
	; do underline
	and	AH,07h
	cmp	AH,01h
	jz	.ul

.ex2:	pop	AX
	pop	DI
	pop	SI
	pop	BX
	pop	CX
	pop	DS
	pop	ES
	popf
	ret

.blank	rep	stosb
	jmp short .ex

.inv	lodsb
	not	AL
	stosb
	loop	.inv
	jmp short .ex

.bri	lodsb
	mov	BL,AL
	shr	BL,1
	or	AL,BL
	stosb
	loop	.bri
	jmp short .ex


.ul:	dec	DI
	mov	AL,0FFh
	xor	[ES:DI],AL
	jmp short .ex2

;----------------------------------------------------------------
; LIGHT PEN							:
;	TODO: always returns AH = 0				:
; ON EXIT							:
;	(AH) = 0 IF NO LIGHT PEN INFORMATION IS AVAILABLE	:
;		BX,CX,DX ARE DESTROYED				:
;	(AH) = 1 IF LIGHT PEN IS AVAILABLE			:
;		(DH,DL) = ROW,COLUMN OF CURRENT LIGHT PEN	:
;			  POSITION				:
;		(CH) = RASTER POSITION				:
;		(BX) = BEST GUESS AT PIXEL HORIZONTAL POSITION	:
;----------------------------------------------------------------
READ_LPEN:
	xor	AH,AH
	jmp	VIDEO_RETURN


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

	mov	AX,08000h
	mov	DX,io_PCB_EOI
	out	DX,AL			; EOI
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
	mov	AX,08000h
	mov	DX,io_PCB_EOI
	out	DX,AL
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

		call	deice_CRLF
		call	deice_CRLF

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
		call	deice_CRLF
		DBG_C	'B'
		DBG_C	'A'
		DBG_C	'D'
		DBG_C	'D'
		DBG_C	'S'
		DBG_C	'K'
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



disk_io_read:	push	AX

;;		call	deice_CRLF
;;		DBG_C	'r'
;;
;;		push	AX
;;		mov	AL,AH
;;		call	deice_HEX2
;;		pop	AX
;;		push	AX
;;		call	deice_HEX2			; print OP/# sectors
;;
;;		DBG_C	'D'
;;		DBG_C	'C'
;;		DBG_C	'H'
;;		DBG_C	'S'
;;
;;		mov	AL,DL			; drive
;;		call	deice_HEX2
;;		mov	AL,CH			; track
;;		call	deice_HEX2			
;;		mov	AL,DH			; head
;;		call	deice_HEX2
;;		mov	AL,CL			; sector
;;		call	deice_HEX2			
;;
;;		pop	AX
;;

		call	disk_io_restore
		jc	.r

;;		DBG_C	's'

		call	disk_io_seek
		jc	.r
		
		; set up DMA controller


;;		DBG_C	'#'
;;
;;		push	AX
;;		mov	AX,ES
;;		mov	AL,AH
;;		call	deice_HEX2
;;		mov	AX,ES
;;		call	deice_HEX2
;;		DBG_C	':'
;;		mov	AL,BH
;;		call	deice_HEX2
;;		mov	AL,BL
;;		call	deice_HEX2
;;		pop	AX


		push	DX

		mov	DX,io_PCB_D0CON
		mov	AX,00004h
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
		pop	DX

		xor	AH,AH					; zero count of actual sectors read

.next_sector:
		pop	AX
		dec	AL					; any sectors left to read?
		push	AX
		js	.r

		push	DX
		mov	DX,io_PCB_D0TC
		mov	AX,00200h				; a single sector	: TODO: multiple sectors!
		out	DX,AL
		inc	DX
		inc	DX			;io_PCB_D0CON
		mov	AX,0A2A6h
		out	DX,AL	
		pop	DX

		; TODO: check sector number overflow against drive type somehow?
		mov	AL,CL
		call	FDC_SEC_WRITE

;;		DBG_C	'T'
;;		DBG_C	'S'
;;		call	FDC_TRK_READ
;;		call	deice_HEX2
;;		call	FDC_SEC_READ
;;		call	deice_HEX2
;;
;;
;;		DBG_C	'e'

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
;;		push	DX
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
;;		pop	DX
;;		popf
		jc	.r

		; check on DMA status should be off with TC=0
		push	DX
		mov	DX,io_PCB_D0TC
		in	AX,DX
		or	AX,AX
		jz	.skokdma
.baddma		stc
		pop	DX
		mov	byte [DISKETTE_STATUS],BAD_DMA
.skokdma:	mov	DX,io_PCB_D0CON
		in	AX,DX
		test	AL,2
		jnz	.baddma
		pop	DX

		pop	AX
		inc	AH
		push	AX

		inc	CL	;TODO: check for overrun track?

		jmp	.next_sector


.r:
;;		call	deice_CRLF
		pop	AX

		mov	AL,AH				; count of sectors actually read

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

dom_keyb_scan_tbl:
	;; ROW 0 is not represented here
	;;	0,	0,	0,	0	; 00h,	 SHIFT
	;;	0,	0,	0,	0	; 01h,	 CTRL

	;; the following PC keys are not mapped:
	;; F11, F12, End, Home, PgUp, PgDn and Number Pad


	; following table is accessed by 10*((scan code - 10h) div 16) + (scan code & 0fh)
	dw	1071h,	1051h,	1011h,	1000h	; 10h,	 'Q'
	dw	0433h,	0423h,	0,	7A00h	; 11h,	 '3'
	dw	0534h,	0524h,	0,	7B00h	; 12h,	 '4'
	dw	0635h,	0625h,	0,	7C00h	; 13h,	 '5'
	dw	3F00h,	5800h,	6200h,	6C00h	; 14h,	 f4	note F5
	dw	0938h,	092Ah,	0,	7F00h	; 15h,	 '8'
	dw	4200h,	5B00h,	6500h,	6F00h	; 16h,	 f7	note F8
	dw	0C2Dh,	0C5Fh,	0C1Fh,	8200h	; 17h,	 '–'
	dw	0D3Dh,	0D2Bh,	0,	8300h	; 18h,	 '^' note '='
	dw	4B00h,	4B34h,	7300h,	9B00h	; 19h,	 LT_CUR
	dw	3B00h,	5400h,	5E00h,	6800h	; 20h,	 f0	note F1
	dw	1177h,	1157h,	1117h,	1100h	; 21h,	 'W'
	dw	1265h,	1245h,	1205h,	1200h	; 22h,	 'E'
	dw	1474h,	1454h,	1414h,	1400h	; 23h,	 'T'
	dw	0837h,	0826h,	0,	7E00h	; 24h,	 '7'
	dw	1769h,	1749h,	1709h,	1700h	; 25h,	 'I'
	dw	0A39h,	0A28h,	0,	8000h	; 26h,	 '9'
	dw	0B30h,	0B29h,	0,	8100h	; 27h,	 '0'
	dw	5300h,	532Eh,	9300h,	0A300h	; 28h,	 '_' note Del
	dw	5000h,	5032h,	9100h,	0A000h	; 29h,	 DN_CUR
	dw	0231h,	0221h,	0,	7800h	; 30h,	 '1'
	dw	0332h,	0340h,	0300h,	7900h	; 31h,	 '2'
	dw	2064h,	2044h,	2004h,	2000h	; 32h,	 'D'
	dw	1372h,	1352h,	1312h,	1300h	; 33h,	 'R'
	dw	0736h,	075Eh,	071Eh,	7D00h	; 34h,	 '6'
	dw	1675h,	1655h,	1615h,	1600h	; 35h,	 'U'
	dw	186Fh,	184Fh,	180Fh,	1800h	; 36h,	 'O'
	dw	1970h,	1950h,	1910h,	1900h	; 37h,	 'P'
	dw	1A5Bh,	1A7Bh,	1A1Bh,	1A00h	; 38h,	 '['
	dw	4800h,	4838h,	8D00h,	9800h	; 39h,	 UP_CUR
	dw	0,	0,	0,	0	; 40h,	 CAPS_LOCK
	dw	1E61h,	1E41h,	1E01h,	1E00h	; 41h,	 'A'
	dw	2D78h,	2D58h,	2D18h,	2D00h	; 42h,	 'X'
	dw	2166h,	2146h,	2106h,	2100h	; 43h,	 'F'
	dw	1579h,	1559h,	1519h,	1500h	; 44h,	 'Y'
	dw	246Ah,	244Ah,	240Ah,	2400h	; 45h,	 'J'
	dw	256Bh,	254Bh,	250Bh,	2500h	; 46h,	 'K'
	dw	2960h,	297Eh,	0,	0	; 47h,	 '@'   note '`'
	dw	2827h,	2822h,	0,	0	; 48h,	 ':'	note apos
	dw	1C0Dh,	1C0Dh,	1C0Ah,	0A600h	; 49h,	 RETURN
	dw	0,	0,	0,	0	; 50h,	 SHIFT_LOC
	dw	1F73h,	1F53h,	1F13h,	1F00h	; 51h,	 'S'
	dw	2E63h,	2E42h,	2E03h,	2E00h	; 52h,	 'C'
	dw	2267h,	2247h,	2207h,	2200h	; 53h,	 'G'
	dw	2368h,	2348h,	2308h,	2300h	; 54h,	 'H'
	dw	316Eh,	314Eh,	310Eh,	3100h	; 55h,	 'N'
	dw	266Ch,	264Ch,	260Ch,	2600h	; 56h,	 'L'
	dw	273Bh,	273Ah,	0,	2700h	; 57h,	 ';'
	dw	1B5Dh,	1B7Dh,	1B1Dh,	1B00h	; 58h,	 ']'
	dw	0E08h,	0E08h,	0E7Fh,	0E00h	; 59h,	 DELETE note Backspace
	dw	0F09h,	0F00h,	9400h,	0A500h	; 60h,	 TAB
	dw	2C7Ah,	2C5Ah,	2C1Ah,	2C00h	; 61h,	 'Z'
	dw	3920h,	3920h,	3920h,	3920h	; 62h,	 ' '
	dw	2F76h,	2F56h,	2F16h,	2F00h	; 63h,	 'V'
	dw	3062h,	3042h,	3002h,	3000h	; 64h,	 'B'
	dw	326Dh,	324Dh,	320Dh,	3200h	; 65h,	 'M'
	dw	332Ch,	333Ch,	0,	0	; 66h,	 ','
	dw	342Eh,	343Eh,	0,	0	; 67h,	 '.'
	dw	352Fh,	353Fh,	0,	0	; 68h,	 '/'
	dw	5200h,	5230h,	9200h,	0A200h	; 69h,	 COPY
	dw	011Bh,	011Bh,	011Bh,	0100h	; 70h,	 ESCAPE
	dw	3C00h,	5500h,	5F00h,	6900h	; 71h,	 f1	note F2
	dw	3D00h,	5600h,	6000h,	6A00h	; 72h,	 f2	note F3
	dw	3E00h,	5700h,	6100h,	6B00h	; 73h,	 f3	note F4
	dw	4000h,	5900h,	6300h,	6D00h	; 74h,	 f5	note F6
	dw	4100h,	5A00h,	6400h,	6E00h	; 75h,	 f6	note F7
	dw	4300h,	5C00h,	6600h,	7000h	; 76h,	 f8	note F9
	dw	4400h,	5D00h,	6700h,	7100h	; 77h,	 f9	note F10
	dw	2B5Ch,	2B7Ch,	2B1Ch,	2600h	; 78h,	 '\'
	dw	4D00h,	4D36h,	7400h,	9D00h	; 79h,	 RT_CUR



dom_keyb_bbc2scan:
	push	BX
	push	SI

	; convert AL to table index i.e. (bbc scan code -16  bcd to decimal)*8
	xor	BH,BH
	mov	BL,AL
	and	BL,0Fh	
	and	AL,70h
	sub	AL,10h
	shr	AL,1
	add	BL,AL
	shr	AL,2
	add	BL,AL
	shl	BX,3

	; add shift/ctrl offset
	; TODO: caps lock, ALT, Insert
	mov	AL,CTL_SHIFT
	test	[KB_FLAG],AL
	jz	.nosh1
	inc	BX
	inc	BX
	jmp	.sk1

.nosh1:	mov	AL,LEFT_SHIFT+RIGHT_SHIFT
	test	[KB_FLAG],AL
	jz	.noshift
.sk1:	inc	BX
	inc	BX
.noshift:

	mov	SI,dom_keyb_scan_tbl
	mov	AX,[CS:SI+BX]

	pop	SI
	pop	BX
	ret


dom_keyb_tick:
	pushf
	push	AX
	push	BX
	push	DX
	push	DS

	call	DDS

	; get shift/ctrl status
	call	dom_keyb_get_row0

	; TODO: shift/ctrl are reversed!, do something about LEFT/RIGHT shift
	rcr	AH,1
	pushf
	rcl	AH,1
	popf
	rcl	AH,1
	and	AH,07h
	mov	AL,[KB_FLAG]
	and	AL,0F8h
	or	AL,AH
	mov	[KB_FLAG],AL

	; check for CA2 in SYSVIA IFR indicating a key is pressed
	mov	DX,io_SHEILA_SYSVIA_IFR
	in	AL,DX
	test	AL,1
	jz	.ex_nokeys

	; check rollover keys are still pressed
	mov	AL,[KB_BBC_ROLL]
	or	AL,AL
	jns	.nr1
	call	keyb_pressed
	mov	[KB_BBC_ROLL],AL
.nr1:	mov	AL,[KB_BBC_ROLL+1]
	or	AL,AL
	jns	.nr2
	call	keyb_pressed
	mov	[KB_BBC_ROLL+1],AL
.nr2:
	and	AL,[KB_BBC_ROLL]
	js	.excl				;both full

	mov	AL,09h
.again:	and	AL,7Fh
	add	AL,01h
	daa
	call	dom_scan_keys
	or	AL,AL
	jns	.excl

	; check rollover 
	cmp	[KB_BBC_ROLL],AL
	jz	.again
	cmp	[KB_BBC_ROLL+1],AL
	jz	.again
	; not already pressed check for a space
	mov	AH,80h
	test	[KB_BBC_ROLL],AH
	js	.sk1
	mov	[KB_BBC_ROLL],AL
	jmp short .ok
.sk1	test	[KB_BBC_ROLL+1],AH
	js	.excl
	mov	[KB_BBC_ROLL+1],AL

.ok:

	call	dom_keyb_bbc2scan
	or	AX,AX
	jz	.excl

	call	dom_keyb_insert
	jnc	.excl
	mov	BL,1
	call	BEEP


.excl:
	; make sure to clear CA2
	push	AX
	mov	DX,io_SHEILA_SYSVIA_IFR
	mov	AL,1
	out	DX,AL
	pop	AX

	call	dom_keyb_auto_on


.ex	pop	DS
	pop	DX
	pop	BX
	pop	AX
	popf
	ret

.ex_nokeys:
	; no keys pressed, clear rollovers
	xor	AX,AX
	mov	[KB_BBC_ROLL],AX
	jmp short .excl


	; prep the keyboard for a row/column scan, turn off auto scan
	; trashes DX, AL
dom_keyb_auto_off:
	push	AX
	; set PA0..6 as outputs, PA7 input
	mov	DX,io_SHEILA_SYSVIA_DDRA
	mov	AL,07Fh
	out	DX,AL

	; select a non-existent column
	mov	DX,io_SHEILA_SYSVIA_ORA_NH
	mov	AL,0Fh
	out	DX,AL

	; disable keyboard autoscan
	mov	DX,io_SHEILA_SYSVIA_ORB
	mov	AL,03h
	out	DX,AL

	; clear CA2 interrupt flag
	mov	DX,io_SHEILA_SYSVIA_IFR
	mov	AL,1
	out	DX,AL
	pop	AX
	ret

	; set keyboard back to autoscan
	; corrupts DX, AL
dom_keyb_auto_on:
	push	AX
	; clear CA2 interrupt flag
	mov	DX,io_SHEILA_SYSVIA_IFR
	mov	AL,1
	out	DX,AL

	; enable keyboard autoscan
	mov	DX,io_SHEILA_SYSVIA_ORB
	mov	AL,03h+08h
	out	DX,AL

	; portA all inputs
	mov	DX,io_SHEILA_SYSVIA_DDRA
	xor	AX,AX
	out	DX,AL
	pop	AX
	ret


	; scan the keyboard for pressed keys, only rows 1 to 7 scanned here
	; bbc key code returned in AL
	; shift status and config switches in BX
	; TODO: this will likely fail on certain 2 key combinations
	; 
	; INPUT		AL should contain the starting scan code usually 10 to ignore shift/ctrl/switches
	; RETURNS	AL = BBC scan code
	; CORRUPTS 	AH, DX
dom_scan_keys:
	pushf
	cli				; need to stop interrupts
	push	CX
	mov	CH,AL				; original row/col start

	call	dom_keyb_auto_off

	mov	AH,10				; col counter
	mov	CL,CH				; col #
	and	CL,0Fh				; start at this column
.collp	mov	DX,io_SHEILA_SYSVIA_ORA_NH	; select col
	mov	AL,CL
	out	DX,AL
	mov	DX,io_SHEILA_SYSVIA_IFR
	in	AL,DX
	test	AL,1			; check for CA2
	jnz	.gotcol	

.colcot:inc	CL
	cmp	CL,10
	jb	.colsk
	xor	CL,CL
.colsk:	dec	AH
	jns	.collp
	; check to see if we started at col 0, if so increment row and try again
.r_nowt:
	xor	AL,AL			; no scan code found
.r:
	call	dom_keyb_auto_on
	pop	CX
	popf
	ret

.gotcol:
	mov	AL,CL				; get col code and try each row
	mov	DX,io_SHEILA_SYSVIA_ORA_NH
.rowlp:	cmp	AL,CH				; check against minimum
	jb	.sknxtr				; check if < minimum counter if it is skip this row
	out	DX,AL
	in	AL,DX
	or	AL,AL
	js	.rowsk
.sknxtr:add	AL,10h				; next row
	jns	.rowlp	
	mov	AL,1
	mov	DX,io_SHEILA_SYSVIA_IFR		; reset CA2 flag
	out	DX,AL
	jmp	short .colcot			; continue scan at next col

.rowsk:	or	AL,80h
	jmp	short .r

dom_keyb_get_row0:
	; gets the bottom row of the keyboard shift,ctrl,switches[7..0] into AH
	; returns AX
	pushf
	push	BX
	push	DX
	cli				; interrupts off while we do this
	call	dom_keyb_auto_off
	call	dom_keyb_get_row0_int
	call	dom_keyb_auto_on
	mov	AX,BX
	pop	DX
	pop	BX
	popf
	ret

dom_keyb_get_row0_int:
	mov	DX,io_SHEILA_SYSVIA_ORA_NH
	mov	BX,0FF80h
	mov	AL,0FFh
.lp	ror	AL,1			; put back AL
	inc	AL			; next column
	out	DX,AL
	in	AL,DX			; top bit will now be set if key pressed
	rcl	AL,1			; CF has keypress
	rcl	BX,1			; shift into BX, CF is now set if more bits to do
	jc	.lp
	ret

; check to see if a key is pressed
; INPUT AL = bbc scan code
; OUTPUT AL = bbc scan code with top bit set if pressed
; CORRUPTS DX
keyb_pressed:
	call	dom_keyb_auto_off
	mov	DX,io_SHEILA_SYSVIA_ORA_NH
	out	DX,AL
	in	AL,DX
	call	dom_keyb_auto_on
	ret


dom_keyb_insert:
	push	SI
	mov	BX,[BUFFER_TAIL]
	mov	SI,BX
	call	dom_keyb_buffer_inc_wrap
	cmp	BX,[BUFFER_HEAD]
	jz	.rc
	mov	[BUFFER_TAIL],BX
	mov	[DS:SI],AX
	clc
.r2:	pop	SI
	ret
.rc:	stc
	pop	SI
	ret

dom_keyb_buffer_inc_wrap:
	inc	BX
	inc	BX
	cmp	BX,[BUFFER_END]		; AT END OF BUFFER?
	jnz	.sk			; NO, CONTINUE
	mov	BX,[BUFFER_START] 	; YES, RESET TO BUFFER BEGINNING
.sk:	ret

;=======================================================================:
; DeIce stuff
;=======================================================================:

DEICE_SEG		equ	0F000h		; where DEICE stores its stuff - TODO: think of somewhere better

COMBUF_SIZE		equ	080h
DEICE_STACK_SIZE	equ	0A0h

deice_regs		equ	0h

deice_reg_DI		equ	deice_regs+00h
deice_reg_SI		equ	deice_regs+02h
deice_reg_BP		equ	deice_regs+04h
deuce_reg_SP2		equ	deice_regs+06h	; this is ignored, pusha pushes an unwanted copy of SP!
deice_reg_BX		equ	deice_regs+08h
deice_reg_DX		equ	deice_regs+0Ah
deice_reg_CX		equ	deice_regs+0Ch
deice_reg_AX		equ	deice_regs+0Eh
deice_reg_DS		equ	deice_regs+10h
deice_reg_ES		equ	deice_regs+12h
deice_reg_IP		equ	deice_regs+14h
deice_reg_CS		equ	deice_regs+16h
deice_reg_FLAGS		equ	deice_regs+18h
deice_reg_SP		equ	deice_regs+1Ah
deice_reg_SS		equ	deice_regs+1Ch
deice_STATUS		equ	deice_regs+1Eh

deice_regs_top		equ	deice_regs + 1Fh

deice_run_flag		equ	deice_regs_top
deice_ram_top		equ	deice_run_flag + 1		; has to be aligned!


COMBUF			equ	deice_ram_top
deice_stack		equ	COMBUF+COMBUF_SIZE+DEICE_STACK_SIZE


DEICE_STATE_BP		equ	01h			; breakpoint
DEICE_STATE_TRACE	equ	02h			; trace
DEICE_STATE_ILLEGAL	equ	03h
DEICE_STATE_IRQ_x	equ	10h			; add the irq number to this
DEICE_STATE_NMI		equ	17h			; TODO: check I suspect this clashes with other interrupts on x86, this was really for 68k - make it 0x80 or something?


;
;======================================================================
;  HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;
;  Communications function codes.
FN_GET_STAT		equ	0FFh    ; reply with device info
FN_READ_MEM		equ	0FEh    ; reply with data
FN_WRITE_M		equ	0FDh    ; reply with status (+/-)
FN_READ_RG		equ	0FCh    ; reply with registers
FN_WRITE_RG		equ	0FBh    ; reply with status
FN_RUN_TARG		equ	0FAh    ; reply (delayed) with registers
FN_SET_BYTES		equ	0F9h    ; reply with data (truncate if error) - NOTE: different to NoICE
FN_IN			equ	0F8h    ; input from port
FN_OUT			equ	0F7h    ; output to port
;
FN_MIN			equ	0F7h    ; MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR		equ	0F0h    ; error reply to unknown op-code
FN_MAX			equ	0FFh


DEICE_NMI_INT:
			push	AX
			mov	AL,DEICE_STATE_NMI
			jmp	INT_ENTRY

; DeIce interrupt handlers
DEICE_INT1:
			push	AX
			mov	AL,DEICE_STATE_TRACE
			jmp	INT_ENTRY

DEICE_INT3:
			push	AX

			; check if this is an int3(CC) (as opposed to int 3(CD 03)) and if
			; so adjust saved PC to point back at the breakpoint
			push	BP
			push	DS
			push	SI

			mov	BP,SP
			lds	SI,[SS:BP+8]
			sub	SI,1
			cmp	byte [DS:SI],0xCC
			jne	.nocc
			mov	[SS:BP+8],SI
.nocc			pop	SI
			pop	DS
			pop	BP

			mov	AL,DEICE_STATE_BP
			jmp	INT_ENTRY

		; initialise the serial port for deice
deice_init:	pushf
		push	ES
		push	AX
		push	DX
		push	CX
		push	DI

		mov	DX,io_sheila_SERIAL_ULA
		mov	AL,040h				; 19200,19200
		out	DX,AL
		mov	DX,io_sheila_ACIA_CTL
		mov	AL,01010111b			; master reset
		out	DX,AL
		mov	AL,01010110b			; RTS high, no interrupts, 8N1, div64
		out	DX,AL

		mov	CX,deice_ram_top
		mov	AX,DEICE_SEG
		mov	DI,0
		mov	ES,AX
		; clear memory area
		xor	AL,AL
		cld
		rep stosb

		pop	DI
		pop	CX
		pop	DX
		pop	AX
		pop	ES
		popf
		ret

		; print string at PC until 0, advance PC
deice_printstr:
		push	SI
		push	AX
		push	BP
		pushf
		cld
		mov	BP,SP
		mov	SI,[SS:BP+8]			; get return address
.lp:		CS lodsb
		or	AL,AL
		jz	.sk
		call	deice_print_char
		jc	.fl
		jmp short .lp
.sk:		mov	[SS:BP+8],SI
		popf
		pop	BP
		pop	AX
		pop	SI
		ret
.fl		CS lodsb				; skip rest of string if fail to send
		or	AL,AL
		jnz	.fl
		jmp short .sk



deice_HEX4:	pushf
		PUSH	CX
		PUSH	BX
		push	BP
		PUSH	AX
		mov	AL,AH
		CALL	deice_HEX2_int
		pop	AX
		push	AX
		CALL	deice_HEX2_int
		POP	AX
		pop	BP
		POP	BX
		POP	CX
		popf
		RET

		
deice_HEX2:	pushf
		PUSH	AX
		PUSH	CX
		PUSH	BX
		push	BP
		CALL	deice_HEX2_int
		pop	BP
		POP	BX
		POP	CX
		POP	AX
		popf
		RET

deice_HEX2_int:	; TODO: rename
		push	AX			; RESAVE FOR LOW NYBBLE DISPLAY
		shr	AL,4			; NIBBLE SWAP
		call	deice_HEX_nyb 		; DO THE HIGH NIBBLE DISPLAY
		pop	AX			; RECOVER THE NIBBLE
		and	AL,0FH			; ISOLATE TO LOW NIBBLE
					; FALL INTO NIBBLE CONVERSION
deice_HEX_nyb:				; CONVERT 00-0F TO ASCII CHARACTER
		add	AL,090H 		; ADD FIRST CONVERSION FACTOR
		daa				; ADJUST FOR NUMERIC AND ALPHA RANGE
		adc	AL,040H 		; ADD CONVERSION AND ADJUST LOW NIBBLE
		daa				; ADJUST HI NIBBLE TO ASCII RANGE
deice_print_char:					; print ASCII 0..7F
		push	AX
		and	AL,07Fh
		call	DEICE_PUTCHAR
		pop	AX
		ret

deice_CRLF:		;TODO: remove
		pushf
		push	AX
		mov	AL,0Dh
		call	deice_print_char
		mov	AL,0Ah
		call	deice_print_char
		pop	AX
		popf
		ret


;
;===========================================================================
;  Get a character to AL
;
;  Return AL=char, CY=0 if data received
;	  CY=1 if timeout (0.5 seconds)
;  Corrupts AH

DEICE_GETCHAR:	push	CX
		push	DX
		mov	AH,AL
		mov	CX,0FFFFh		; long timeout
		mov	DX,io_sheila_ACIA_CTL
.GC10		in	AL,DX
		test	AL,ACIA_RDRF
		loopz	.GC10
		jz	.SEC
		inc	DX			; data port
		in	AL,DX			; clears Cy		
		clc
.EX:		pop	DX
		pop	CX
		ret
.SEC:		stc
		jmp short .EX



DEICE_PUTCHAR:
		push	CX
		push	DX
		push	AX

		mov	AH,AL
		mov	CX,0FFFFh		; long timeout
		mov	DX,io_sheila_ACIA_CTL
.PC10:		in	AL,DX
		test	AL,ACIA_TDRE
		loopz	.PC10
		jz	.sec
		inc	DX			; data port
		mov	AL,AH
		out	DX,AL
		clc
.r		pop	AX
		pop	DX
		pop	CX
		ret
.sec:		stc
		jmp short .r


;======================================================================
;  Response string for GET TARGET STATUS request
;  Reply describes target:
TSTG:	
		db	86			; 2: PROCESSOR TYPE = 68k
		db	COMBUF_SIZE		; 3: SIZE OF COMMUNICATIONS BUFFER
		db	0			; 4: NO TASKING SUPPORT
		dw	0,0FFFFh			; 5-8: LOW AND HIGH LIMIT OF MAPPED MEM (ALL!)		-- note 68008 has 24 bit address space "paging" register is just the high MSB!
		db	.B1-.B0			; 9:  BREAKPOINT INSTR LENGTH
.B0:		int3				; 10: BREAKPOINT INSTRUCTION
.B1:		
		db	"x86"
		db	" monitor V1.1"	; DESCRIPTION, ZERO
		db	"-BBC"
		db	0 
TSTG_SIZE	equ	$-TSTG		; SIZE OF STRING

;
;===========================================================================
;  Common handler for default interrupt handlers
;  Enter with D0=interrupt code = processor state
;  All registers stacked, PC=next instruction
;
deice_enter:
INT_ENTRY:

; For nmi check to see if we're already running and exit if so
		push	ES
		push	AX					; stack ES and status code
		mov	AX,DEICE_SEG
		mov	ES,AX					; ES now points at our segment
		mov	AH,1
		lock xchg [ES:deice_run_flag],AH
		or	AH,AH
		jz	.S1
		; already running POH
		pop	AX
		pop	ES
		pop	AX
		iret


.S1:		
		
		pop	AX
		mov	[ES:deice_STATUS],AL			; save status # set on entry

		pop	ES					; get back ES
		pop	AX					; get back original AX

		push	ES
		push	DS
		pusha						; push all registers to user's stack


		;stack should now be
		;		flags
		;		CS
		;		IP
		;		ES
		;		DS
		;		AX
		;		CX
		;		DX
		;		BX
		;		SP	(ignored)
		;		BP
		;		SI
		;		DI



		mov	AX,DEICE_SEG				; point ES at our segment
		mov	ES,AX


		mov	AX,SP
		mov	SI,AX					; setup SI ready to copy registers to block
		add	AX,deice_reg_FLAGS-deice_regs+2		; calc original stack pointer before interrupt
		mov	[ES:deice_reg_SP],AX			; and save in regs block
		mov	AX,SS
		mov	[ES:deice_reg_SS],AX			; and stack seg
		mov	DS,AX					; source seg for stack copy
		mov	DI,deice_regs				; base of registers

;  Save stacked registers from stack to reg block for return to master
		cld
		mov	CX,deice_reg_FLAGS-deice_regs+2		; number to copy from original stack
		rep movsb					; bytewise copy stack to regs block

		; reset stack to local stack - this will allow us to alter
		; the real stack areas without conflict

		mov	AX,DEICE_SEG
		mov	SS,AX					; what happens here if there's an NMI?
		mov	SP,deice_stack
		mov	ES,AX
		mov	DS,AX

ENTER_MON:	mov	AL,FN_RUN_TARG
		jmp	RETURN_REGS

MAIN:		
MAIN2		cld
		mov	SP,deice_stack 				; reset stack
		mov	AX,DEICE_SEG
		mov	DS,AX
		mov	ES,AX
		mov	SS,AX
		mov	DI,COMBUF

		call	DEICE_GETCHAR
		jc	MAIN2

		cmp	AL,FN_MIN
		jb	MAIN2

		stosb						; command # into buffer
;
;  Second byte is data byte count (may be zero)
		call	DEICE_GETCHAR			; GET A LENGTH BYTE
		jc	MAIN			; JIF TIMEOUT: RESYNC
		cmp	AL,COMBUF_SIZE
		jae	MAIN			; JIF TOO LONG: ILLEGAL LENGTH
		stosb				; SAVE LENGTH
		or	AL,AL
		jz	.MA80			; SKIP DATA LOOP IF LENGTH = 0
;
;  Loop for data
		xor	CX,CX
		mov	CL,AL			; get # of chars to transfer		
.MA10:		call	DEICE_GETCHAR			; GET A DATA BYTE
		jc	MAIN			; JIF TIMEOUT: RESYNC
		stosb				; SAVE DATA BYTE
		loop	.MA10
;
;  Get the checksum
.MA80:		call	DEICE_GETCHAR			; GET THE CHECKSUM
		jc	MAIN			; JIF TIMEOUT: RESYNC
		mov	AH,AL			; SAVE CHECKSUM
;
;  Compare received checksum to that calculated on received buffer
;  (Sum should be 0)
		call	CHECKSUM
		add	AH,AL			; ADD SAVED CHECKSUM TO COMPUTED
		jnz	MAIN			; JIF BAD CHECKSUM

;
;  Process the message.
		mov	SI,COMBUF		; setup SI to point at COMBUF
		lodsb				; get first byte (FN_*)
		sub	AL,FN_MIN
		jc	.err			; bad FN number!
		cmp	AL,FN_MAX-FN_MIN
		ja	.err

		xor	AH,AH
		sal	AX,1
		mov	BP,AX
		inc	SI
		mov	AL,[COMBUF]
		jmp	word [CS:BP+deice_fn_tab] ; jump to function handler
;
;  Error: unknown function.  Complain
.err:		mov	AL,FN_ERROR
		mov	[COMBUF],AL
		mov	AL,1
		jmp	SEND_STATUS		; VALUE IS "ERROR"


deice_fn_tab:	dw	OUT_PORT
		dw	IN_PORT
		dw	SET_BYTES
		dw	RUN_TARGET
		dw	WRITE_REGS
		dw	READ_REGS
		dw	WRITE_MEM
		dw	READ_MEM
		dw	TARGET_STAT


;===========================================================================
;
;  Target Status:  FN, len
;
;  Entry with A=function code, B=data size, DS:SI=COMBUF+2
;
TARGET_STAT:
		mov	AX,CS
		mov	DS,AX
		mov	SI,TSTG			; DATA FOR REPLY
		mov	DI,COMBUF+1		; POINTER TO RETURN BUFFER
		mov	CL,TSTG_SIZE		; LENGTH OF REPLY
		xor	CH,CH
		mov	AL,CL
		stosb				; store length in COMBUF+1
		rep	movsb			; MOVE REPLY DATA TO BUFFER
;
;  Compute checksum on buffer, and send to master, then return
		jmp	SEND


		; NoIce buffer should contain a 32 bit linear address in form SECH, SECL, ADDRH, ADDRL
		; DS:SI is pointer to address and will NOT be advanced
		; ES:DI will point at the address as read
deice_get_seg_addr:

		; address is in form of a 24 bit address PPHHLL
		; need in the form PHH0:00LL

		lodsb	
		mov	AH,AL
		lodsb
		mov	ES,AX
		lodsb
		mov	AH,AL
		lodsb
		mov	DI,AX
		ret
		
;===========================================================================
;
;  Read Memory:	 FN, len, SEGH, SEGL, ADDRH, ADDRL, Nbytes
;
;  Entry with A=function code, B=data size, DS:SI=COMBUF+2
;
READ_MEM:	call	deice_get_seg_addr
;
;  Prepare return buffer: FN (unchanged), LEN, DATA
		xor	CX,CX
		mov	CL,[DS:SI]		; NUMBER OF BYTES TO RETURN
		mov	[COMBUF+1],CL		; RETURN LENGTH = REQUESTED DATA	
		or	CL,CL
		jz	.GLP90			; JIF NO BYTES TO GET


;
;  Read the requested bytes from local memory
		push	ES
		mov	AX,DS
		mov	ES,AX
		pop	DS
		mov	SI,DI
		mov	DI,COMBUF+2

		rep	movsb			; GET BYTE and STORE TO RETURN BUFFER
;
;  Compute checksum on buffer, and send to master, then return
.GLP90		jmp	SEND



;===========================================================================
;
;  Write Memory:  FN, len, SEGH, SEGL, ADDRH, ADDRL, (len-4 bytes of Data)
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
;  Uses 6 bytes of stack
;
WRITE_MEM:	push	SI
		call	deice_get_seg_addr
		pop	SI
;
;  Compute number of bytes to write
		xor	CX,CX
		mov	CL,[COMBUF+1]		; NUMBER OF BYTES TO WRITE+3
		sub	CL,4			; MINUS ADDRESS 
		jna	.WLP50			; JIF NO BYTES TO PUT

		; TODO	check!

;  Write the specified bytes to local memory
		push	CX
		push	SI
		push	DI
		
		rep movsb			; write memory
;
;  Compare to see if the write worked
		pop	DI
		pop	SI
		pop	CX
		repe cmpsb			; compare 
		jne	.WLP80			; BR IF WRITE FAILED
		
;
;  Write succeeded:  return status = 0
.WLP50:		xor	AL,AL			; RETURN STATUS = 0
		jmp	SEND_STATUS
;
;  Write failed:  return status = 1
.WLP80		mov	AL,1

;  Return OK status
.WLP90		jmp	SEND_STATUS


;===========================================================================
;
;  Read registers:  FN, len=0
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
READ_REGS:	; enter with D0 is function code to return either FN_RUN_TARG or FN_READ_REGS
RETURN_REGS:	mov	SI,deice_regs
		mov	DI,COMBUF
		stosb					; store fn
		mov	AL,deice_regs_top-deice_regs
		stosb					; store data len
		xor	CH,CH
		mov	CL,AL
		rep	movsb
		jmp	SEND

;===========================================================================
;
;  Write registers:  FN, len, (register image)
;
;  Entry with A=function code, B=data size, X=COMBUF+2

WRITE_REGS:	xor	CX,CX
		mov	CL,[COMBUF+1]
		or	CL,CL
		js	.WRR80
		jz	.WRR80			; JIF NO REGISTERS

;
;  Copy the registers
		mov	SI,COMBUF+2
		mov	DI,deice_regs
		rep	movsb
;
;  Return OK status
.WRR80		xor	AL,AL
		jmp	SEND_STATUS




;===========================================================================
;
;  Run Target:	FN, len
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
RUN_TARGET:
;
;  Switch to user stack
		mov	AX,[deice_reg_SS]
		mov	SS,AX
		mov	ES,AX
		mov	SP,[deice_reg_SP]
		sub	SP,deice_reg_FLAGS-deice_regs+2
		mov	DI,SP


		cld
		mov	SI,deice_regs
		mov	CX,deice_reg_FLAGS-deice_regs+2
.lp:		rep	movsb

		popa
		pop	DS

		; re-enable reentry
		push	DEICE_SEG
		pop	ES
		push	AX
		xor	AX,AX
		mov [ES:deice_run_flag],AH
		pop	AX

		; original ES
		pop	ES
		iret




;===========================================================================
;
;  Set target byte(s):	FN, len { (SEGH, SEGL, ADDRH, ADDRL, data), (...)... }  - note address sense reversed from noice
;
;  Return has FN, len, (data from memory locations)
;
;  If error in insert (memory not writable), abort to return short data
;
;  This function is used primarily to set and clear breakpoints
;
;  NOTE: this is different to the standard NoICE protocol as it works with 24 bit addresses and 8 bit data
;  
;
SET_BYTES:
		cld
		mov	AL,[COMBUF+1]
		xor	AH,AH
		mov	CL,5
		div	CL			; divide by 5 for length
		jz	.SB99			; JIF NO BYTES (COMBUF+1 = 0)
		mov	CL,AL
		xor	CH,CH			; CX contains count of bytes to set

		mov	[COMBUF+1],CH		; SET RETURN COUNT AS ZERO

		mov	SI,COMBUF+2		; source pointer
		mov	DI,SI			; return bytes pointer
					
;
;  Loop on inserting bytes
.SB10:		
		push	ES			; save return bytes pointer
		push	DI

		call	deice_get_seg_addr	; ES:DI now points to address

		mov	AX,ES
		call	deice_HEX4
		mov	AX,DI
		call	deice_HEX4

		lodsb				; AL now contain byte to set
		call	deice_HEX2
		mov	AH,[ES:DI]		; contains previous value
		mov	[ES:DI],AL		; store dest byte
		cmp	[ES:DI],AL		; compare
		jnz	.SB90

		pop	DI
		pop	ES
		mov	AL,AH
		call	deice_HEX2
		stosb				; store previous value in return buffer
		inc	byte [COMBUF+1]		; increment return length
		
		loop	.SB10			; loop for all CX

		; get 32-bit address (big endian)
;
;  Compute checksum on buffer, and send to master, then return
.SB99:		jmp	SEND

.SB90:		mov	AL,'!'
		call	deice_print_char
		pop	DI
		pop	ES
		jmp short .SB99			; return with count processed


;TODO: word sized read/write of I/O ports

;===========================================================================
;
;  Input from port:  FN, len, PortAddressLo, PAHi 
;
;
IN_PORT:
;
;  Get port address
		mov	DX,[COMBUF+2]

;
;  Read the requested byte from I/O port
		in	AL,DX
;
;  Return byte read as "status"
		jmp	SEND_STATUS

;===========================================================================
;
;  Output to port  FN, len, PAlo, PAHi, Data byte
;
;
OUT_PORT:
;
;  Get port address
		mov	DX,[COMBUF+2]
;  Get data byte
		mov	AL,[COMBUF+3]
;
;  Write value to port
		out	DX,AL
;
;  Do not read port to verify (some I/O devices don't like it)
;
;  Return status of OK
		xor	AL,AL
		jmp	SEND_STATUS


;===========================================================================
;  Build status return with value from D0
;
SEND_STATUS:
		mov	[COMBUF+2],AL		; SET STATUS
		mov	AL,1
		mov	[COMBUF+1],AL		; SET LENGTH
		; fall thru to send

;===========================================================================
;  Append checksum to COMBUF and send to master
;
SEND:		call	CHECKSUM		; GET A=CHECKSUM, X->checksum location
		neg	AL
		mov 	[DS:SI],AL		; STORE NEGATIVE OF CHECKSUM
;
;  Send buffer to master
		mov	SI,COMBUF		; POINTER TO DATA
		xor	CH,CH
		mov	CL,[DS:SI+1]		; LENGTH OF DATA
		add	CL,3			; PLUS FUNCTION, LENGTH, CHECKSUM
.lp		lodsb
		call	DEICE_PUTCHAR			; SEND A BYTE
		jc	MAIN
		loop	.lp
		jmp	MAIN			; BACK TO MAIN LOOP

;===========================================================================
;  Compute checksum on COMBUF.	COMBUF+1 has length of data,
;  Also include function byte and length byte
;
;  Returns:
;	AL = checksum
;	SI = pointer to next byte in buffer (checksum location)
;
CHECKSUM:
		push	DEICE_SEG
		pop	DS
		mov	SI,COMBUF		; pointer to buffer
		xor	CH,CH
		mov	CL,[DS:SI+1]		; length of message
		add	CL,2			; plus function, length
		xor	AL,AL			; init checksum to 0
.lp		add	AL,[DS:SI]
		inc	SI
		loop	.lp
		ret				; return with checksum in A




;=======================================================================:
; RESET VECTOR
;=======================================================================:


		TIMES 0x3FF0-($-$$) db 0xFF

		section RESETVEC

		jmp 	SEG_ROMBASE:handle_res

		TIMES 0x10-($-$$) db 0xFF
