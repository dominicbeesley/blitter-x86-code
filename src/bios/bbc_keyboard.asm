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
