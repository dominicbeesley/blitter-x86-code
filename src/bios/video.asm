
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

