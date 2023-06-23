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
deice_printstr:	push	DS
		push	SI
		push	AX
		push	BP
		pushf
		cld
		mov	BP,SP
		mov	SI,[SS:BP+10]			; get return address
		mov	AX,CS
		mov	DS,AX
		call	deice_printstr_SI
		mov	[SS:BP+10],SI
		popf
		pop	BP
		pop	AX
		pop	SI
		pop	DS
		ret


deice_printstr_SI:
.lp:		lodsb
		or	AL,AL
		jz	.sk
		call	deice_print_char
		jc	.fl
		jmp	short .lp
.sk:		ret
.fl		lodsb
		or	AL,AL
		jnz	.fl
		ret

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
%ifdef	BOARD_18x
		db	87			; 2: PROCESSOR TYPE = 16-bit x86
%else
		db	89			; 2: PROCESSOR TYPE = 386
%endif
		db	COMBUF_SIZE		; 3: SIZE OF COMMUNICATIONS BUFFER
		db	0			; 4: NO TASKING SUPPORT
		dw	0,0FFFFh			; 5-8: LOW AND HIGH LIMIT OF MAPPED MEM (ALL!)		-- note 68008 has 24 bit address space "paging" register is just the high MSB!
		db	.B1-.B0			; 9:  BREAKPOINT INSTR LENGTH
.B0:		int3				; 10: BREAKPOINT INSTRUCTION
.B1:		
%ifdef	BOARD_18x
		db	"186/188"
%else
		db	"386"
%endif
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
%ifdef BOARD_18x
		pusha						; push all registers to user's stack
%else
		push	AX
		push	CX
		push	DX
		push	BX
		push	SP
		push	BP
		push	SI
		push	DI
%endif

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


%ifdef BOARD_18x
		popa						; push all registers to user's stack
%else
		pop	DI
		pop	SI
		pop	BP
		pop	BX	; phoney space for SP/popA
		pop	BX
		pop	DX
		pop	CX
		pop	AX
%endif
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



