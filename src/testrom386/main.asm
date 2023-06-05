[map all]


cpu 386


;%define MEMMAP 1



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
		SEG_SCREEN_MO0	equ	0xF300
		SEG_SCREEN_MO4	equ	0xF580
		SEG_SCREEN_MO7	equ	0xF7C0
		SEG_DEBUG	equ	0xC000


		SCREEN_SIZE_MO4 	equ	0h2800

		; boot time stack
		SEG_INIT_STACK	equ	0x30
		INIT_STACK_TOS	equ	0x80


		SEG_INT		equ	0x00
		absolute	2*4
NMI_PTR 	resw		1


%macro		OUT_DX_AL 0
%ifdef MEMMAP
		mov  BP,DX
		mov [GS:BP], AL

%else
		out DX,AL
%endif 
%endmacro


%macro 		MODEx 2	; tbl, ula

		mov	CX,16
		mov	AH,0
		mov	SI,%1
		mov	DX,io_SHEILA_CRTC
%%loop:		mov	AL,AH
		OUT_DX_AL
		inc	DX
		mov	AL,[CS:SI]
		OUT_DX_AL
		dec	DX
		inc	SI
		inc	AH
		loop	%%loop
		mov	DX,io_SHEILA_ULA
		mov	AL,%2
		OUT_DX_AL
%endmacro


%macro		OUTAL	1
		mov	DX,%1
		OUT_DX_AL
%endmacro


%macro 		WAIT8US	0
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
%endmacro

%macro 		WAITL 0
		sub	cx,cx
		mov	BL,8
%%ll:		loop	%%ll
		dec	BL
		jne	%%ll
%endmacro

%macro 		PAL 	1

		; set up palette as B&W
		mov	AL, 7
		mov	DX, io_SHEILA_ULA_PAL
%%lp:		OUT_DX_AL
		add	AL, 0h10
		jns	%%lp

		mov	AL,0h80 + %1
%%lp2:		OUT_DX_AL
		add	AL, 0h10
		js	%%lp2
%endmacro

%macro 		MAGNIFY	0
		; show 8 bytes from 3000-3007 magnified at bottom of screen
		mov	AX, SEG_SCREEN_MO4
		mov	DS,AX
		mov	ES,AX
		mov	SI, 0
		mov	DI, (320*24)
		mov	BL, 8
%%lp:		lodsb
		mov	AH,AL
		mov	BH, 8
%%lp2:		xor	AL,AL
		rcl	AH,1		; shift out leftmost
		jnc	%%sk2
		dec	AL
%%sk2:		mov	CX,16
		rep stosb
		dec	BH
		jnz	%%lp2
		add	DI,320-8*16
		dec	BL
		jnz	%%lp
%endmacro


REMAPCFGH  equ     0x0023
REMAPCFGL  equ     0x0022
REMAPCFG   equ     0x0022

		section .text

_start:
handle_res:	
		cli	; no interrupts please!
		cld

%ifdef MEMMAP
		mov	AX,0F000h
		mov	GS,AX
%endif

        		in	AL,REMAPCFGH	; reset the state machine (state A)
        		mov	AX,0x8000
        		out	REMAPCFGH,AL	; move to (state B)
        		xchg	AH,AL
        		out	REMAPCFGL,AL	; move to (state C)
        		out	REMAPCFG,AX	; move to (state D), sets ESE bit


		mov	DX,0F438h
		mov	AX,0FF80h
		out	(DX),AX



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


		; set up stack and data

		mov	AX,SEG_INIT_STACK		; Get stack segment
		mov	SS,AX	
		mov	SP,INIT_STACK_TOS		; put SP at TOS


		; Disable and clear all VIA interrupts

		mov	AL, 0h7F
		OUTAL	io_SHEILA_SYSVIA_IER
		OUTAL	io_SHEILA_SYSVIA_IFR
		OUTAL	io_SHEILA_USRVIA_IER
		OUTAL	io_SHEILA_USRVIA_IFR

		mov	AL, 0hFF
		OUTAL	io_SHEILA_SYSVIA_DDRA
		OUTAL	io_SHEILA_SYSVIA_DDRB

		mov	AL, 4
		OUTAL	io_SHEILA_SYSVIA_PCR ; vsync \\ CA1 negative-active-edge CA2 input-positive-active-edge CB1 negative-active-edge CB2 input-nagative-active-edge
		mov	AL, 0
		OUTAL	io_SHEILA_SYSVIA_ACR ; none  \\ PA latch-disable PB latch-disable SRC disabled T2 timed-interrupt T1 interrupt-t1-loaded PB7 disabled


		; set up sys via

		mov	AL,0FH
		OUTAL	io_SHEILA_SYSVIA_DDRB

		; set latches 6..0 to 1
		mov	DX,io_SHEILA_SYSVIA_ORB
.llp:		dec	AL
		OUT_DX_AL
		cmp	AL,9
		jnc	.llp

	; SN76489 data byte format
	; %1110-wnn latch noise (channel 3) w=white noise (otherwise periodic), nn: 0=hi, 1=med, 2=lo, 3=freq from channel %10
	; %1cc0pppp latch channel (%00-%10) period (low bits)
	; %1cc1aaaa latch channel (0-3) atenuation (%0000=loudest..%1111=silent)
	; if latched 1110---- %0----nnn noise (channel 3)
	; else                %0-pppppp period (high bits)
	; See SMS Power! for details http://www.smspower.org/Development/SN76489?sid=ae16503f2fb18070f3f40f2af56807f1
	; int volume_table[16]={32767, 26028, 20675, 16422, 13045, 10362, 8231, 6568, 5193, 4125, 3277, 2603, 2067, 1642, 1304, 0};

		mov	AL, 0hFF
		OUTAL	io_SHEILA_SYSVIA_DDRA

		mov	AL, 0h9F      		; silence channel 0
.l101:
		OUTAL	io_SHEILA_SYSVIA_ORA_NH  	; sample says SysViaRegH but OS uses no handshake \\ handshake regA
		mov	CL,AL
		mov	AL,0
		OUTAL	io_SHEILA_SYSVIA_ORB		; enable sound for 8us

		WAIT8US

		mov	AL,8
		OUTAL	io_SHEILA_SYSVIA_ORB

		WAIT8US

		mov	AL,CL
		add	AL, 0h20
		jnc	.l101


		; set DDRA of user via to all outputs
		mov	DX,io_SHEILA_USRVIA_DDRA
		mov	AL,0FFh
		OUT_DX_AL

		; disable VIA interrupts and clear flags
		mov	AL,07Fh

		mov	DX,io_SHEILA_USRVIA_IER
		OUT_DX_AL
		dec	DX
		OUT_DX_AL

		mov	DX,io_SHEILA_SYSVIA_IER
		OUT_DX_AL
		dec	DX
		OUT_DX_AL

		inc	DX
		mov	AL,0F2h				; reenable interrupts for T1,T2,CB1,CA1
		OUT_DX_AL
        	
	        	mov	AL,004h
	        	mov	DX,io_SHEILA_SYSVIA_PCR		
	        	OUT_DX_AL				; CB2=in-neg, CB1=neg, CA2=in-pos, CA1=neg
	
	        	mov	AL,060h
	        	mov	DX,io_SHEILA_SYSVIA_ACR		
	        	OUT_DX_AL				; T1I=cont, T2=pulse count, SR=disabled, PA/B latch=disabled
	
	        	mov	AL,00Eh
	        	mov	DX,io_SHEILA_SYSVIA_T1LL
	        	OUT_DX_AL



		;	switch to mode 7
		MODEx	mode_7_setup, 04Bh

		; copy logo to screen ram

		mov	AX,SEG_SCREEN_MO7
		mov	ES,AX
		sub	DI,DI

		mov	AX,CS
		mov	DS,AX
		mov	SI,MODE7DATA

		mov	CX,0200h

		rep	movsw


		WAITL
		WAITL

		mov	AX,SEG_SCREEN_MO7
		mov	ES,AX
		mov	DI,12*40
		mov	AL,0
		mov	CX,40*4
		rep	stosb

;;;		mov	AX,SEG_DEBUG
;;;		mov	DS,AX
;;;		sub	SI,SI
;;;		mov	AX,SEG_SCREEN_MO7
;;;		mov	ES,AX
;;;		mov	DI,12*40
;;;
;;;		mov	BH,129		; colour char
;;;.ll102:		mov	BL,0		; number of bytes this string
;;;
;;;		; a bit of colour
;;;		mov	AL,BH
;;;		stosb
;;;		inc	BH
;;;
;;;		; get first byte of version string
;;;		lodsb	
;;;		or	AL,AL
;;;		jz	endvers
;;;
;;;.ll104:		inc	BL
;;;		stosb
;;;		lodsb
;;;		or	AL,AL
;;;		jnz	.ll104
;;;
;;;		;move to next line
;;;.ll107:		sub	BL,40
;;;		jns	.ll107
;;;		neg	BL
;;;		dec	BL
;;;		mov	CL,BL
;;;		xor	AL,AL		
;;;		mov	CH,AL
;;;		rep	stosb
;;;
;;;		jmp	.ll102

		

endvers:
		WAITL
		WAITL



;;		; switch to mode 4 and do some reading an writing
		MODEx	mode_4_setup, 0h88

		PAL	3

		WAITL

		; clear screen memory
		mov	AX,SEG_SCREEN_MO4
		mov	ES,AX
		sub	DI,DI
		xor	AX,AX
		mov	CX,SCREEN_SIZE_MO4/2
		rep stosw

		mov	DH,0
ilo:		sub	DI,DI
		mov	CX,320*24
ilo2:		inc	BYTE [ES:DI]
		inc	DI
		loop	ilo2

		MAGNIFY

		dec	DH
		jnz	ilo


HERE:		jmp	_start


DUMMY_RETURN	IRET

mode_7_setup 	db 03Fh, 028h, 033h, 024h, 01Eh, 002h, 019h, 01Ch, 093h, 012h, 072h, 013h, 028h, 000h, 028h, 000h ;; HI(((mode_7_screen) - &74) EOR &20), LO(mode_7_screen)
mode_0_setup 	db 07Fh, 050h, 062h, 028h, 026h, 000h, 020h, 023h, 001h, 007h, 067h, 008h, 006h, 000h, 006h, 000h ;; addr / 8
mode_4_setup 	db 03Fh, 028h, 031h, 024h, 026h, 000h, 020h, 022h, 001h, 007h, 067h, 008h, 00Bh, 000h, 00Bh, 000h ;; addr / 8


MODE7DATA:
		incbin "x86logo.bin"

;=======================================================================:
; RESET VECTOR
;=======================================================================:


		TIMES 0x3FF0-($-$$) db 0xFF

		section RESETVEC

		jmp 	SEG_ROMBASE:handle_res

		TIMES 0x10-($-$$) db 0xFF
