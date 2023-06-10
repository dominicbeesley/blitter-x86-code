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


