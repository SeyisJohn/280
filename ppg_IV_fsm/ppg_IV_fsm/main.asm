
;***************************************************************************
;*
;* Title:   Simplified Table Driven FSM
;* Author:          Ken Short
;* Version:         2.0
;* Last updated:    11/09/15
;* Target:          ATmega16 
;*
;* DESCRIPTION
;* This is a simplified version of the table driven FSM. It handles only 255
;* or less input symbols.
;* 
;* A sample table is included for a simple FSM. This table can be modified to
;* handle any FSM by equating the input symbols to byte values starting at
;* $00 and entering the appropriate next state and task subroutine names.
;* 
;*
;* VERSION HISTORY
;* 1.0 Original version
;* 2.0 Subroutines moved to end of file
;***************************************************************************
.nolist
.include "m324adef.inc"
.list

.dseg	;The variable below are in SRAM
burst_count_setting_bcd:		.byte 3; setting unpacked BCD ;THIS HAS THREE BTYE allocated to the variable name
burst_count:					.byte 1; pulses left to generated in burst
keyvalue:						.byte 1; stores the keyvalue into a variable
make_pulse:						.byte 1; 
is_burst_zero:					.byte 1; Used to check if burst count is equal to zero. 1 means burst count is equal to zero
input:							.byte 1; input

     
;burst_count_setting_bcd is right most digit and
; (burst_count_setting_bcd + 2) is the left most digit 

.cseg
reset:
.org RESET              ;reset interrupt vector
    rjmp start          ;program starts here at reset
.org INT0addr           ;INT0 interrupt vector
    rjmp keypress_ISR 
.org INT1addr
	rjmp pb_press_ISR

start:
    ldi r16, LOW(RAMEND)    ;initialize SP to point to top of stack
    out SPL, r16
    ldi r16, HIGH(RAMEND)
    out SPH, r16

    ldi r16, (1 << ISC00) | (1 << ISC01) | (1 << ISC10) | (1 << ISC11)
    sts EICRA, r16      
	ldi r16, $03		; Enable interrupt request at INTO & INT1   
    out EIMSK, r16

    ldi r16, $ff      ; load r16 with all 1s.
    out DDRB, r16     ; set portB = output
	
	ldi r16, $03		; Set pin 0 & pin 1 to output, everyother pin is an input
	out DDRD, r16

	sbi DDRA, 6			;Set Pin 6 on PORTA (Buzzer)

	sbi DDRA, 7			; Set pin 7 on PORTA to output (OUTPUT)

    sbi portB, 4      ; set /SS of DOG LCD = 1 (Deselected)
	
    rcall init_lcd_dog        ; init display, using SPI serial interface    
	rcall clr_dsp_buffs       ; clear all three buffer lines
	rcall update_lcd_dog      ; update the display

	ldi YH, high (dsp_buff_1) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).

	;put FSM in initial state
    ldi pstatel, LOW(display)
    ldi pstateh, HIGH(display)

	sei                 ;set global interrupt enable

variable_reset:
	; RESET THE VARIABLES WITH ZERO
	ldi r17, $00
	sts burst_count_setting_bcd + 2, r17	
	sts burst_count_setting_bcd + 1, r17	
	sts burst_count_setting_bcd + 0, r17	
	
	sts make_pulse, r17
	sts is_burst_zero, r17
			
	sts burst_count, r17	
	
	sts keyvalue, r17			


test:
    lds r16, make_pulse
	sbrs r16, 0				; Skip the rjmp instruction if the make_pulse flag is set
    rjmp test

	lds r16, is_burst_zero
	cpi r16, 1
	brne gen_1_pulse

	call generate_a_pulse
	rjmp test

gen_1_pulse:
	call pulse_generator
	rjmp test
	


;***************************************************************************
;* 
;* "fsm" - Simplified Table Driven Finite State Machine
;*
;* Description:
;* This table driven FSM can handle 255 or fewer input symbols.
;*
;* Author:              Ken Short
;* Version:             2.0
;* Last updated:        11/09/15
;* Target:              ATmega16
;* Number of words:
;* Number of cycles:
;* Low regs modified:   r16, r18, r20, r21, r31, and r31
;* High registers used:
;*
;* Parameters:          present state in r25:r24 prior to call
;*                      input symbol in r16 prior to call
;*
;* Notes: 
;*
;***************************************************************************

.def pstatel = r24 ;low byte of present state address
.def pstateh = r25 ;high byte of present state address

;input symbols for example finite state machine
.equ number = $00   ;input symbols equated to numerical values ;
.equ enter = $01
.equ clear = $02
.equ pushb = $03
                ;additional symbols would go here
.equ eol = $FF  ;end of list (subtable) do not change

;state table for example finite state machine
;each row consists of input symbol, next state address, task
;subroutine address

state_table:

display: .dw number,	display,	display_the_value
		 .dw enter,		burst,		convert_to_Binary
		 .dw eol,		display,	buzz

burst:	 .dw pushb,		burst,		update_flags
		 .dw clear,		display,	clear_flags
		 .dw eol,		burst,		buzz


fsm:
;load Z with a byte pointer to the subtable corresponding to the
;present state
    mov ZL, pstatel ;load Z pointer with pstate address * 2
    add ZL, ZL ;since Z will be used as a byte pointer with the lpm instr.
    mov ZH, pstateh
    adc ZH, ZH

;search subtable rows for input symbol match
search:
    lpm r18, Z ;get symbol from state table
    cp r18, r16 ;compare table entry with input symbol
    breq match

;check input symbol against eol
check_eol:
    cpi r18, eol ;compare low byte of table entry with eol
    breq match

nomatch:
    adiw ZL, $06 ;adjust Z to point to next row of state table
    rjmp search ;continue searching

;a match on input value to row input value has been found
;the next word in this row is the next state address
;the word following that is the task subroutine's address
match:
	;make preseent state equal to next state value in row
	;this accomplishes the stat transition
    adiw ZL, $02 ;point to low byte of state address
    lpm pstatel, Z+; ;copy next state addr. from table to preseent stat
    lpm pstateh, Z+

	;execute the subroutine that accomplihes the task associated
	;with the transition
    lpm r20, Z+ ;get subroutine address from state table
    lpm r21, Z ;and put it in Z pointer
    mov ZL, r20
    mov ZH, r21
    icall ;Z pointer is now used as a word pointer
    ret


;************************
;NAME:      clr_dsp_buffs
;FUNCTION:  Initializes dsp_buffers 1, 2, and 3 with blanks (0x20)
;ASSUMES:   Three CONTIGUOUS 16-byte dram based buffers named
;           dsp_buff_1, dsp_buff_2, dsp_buff_3.
;RETURNS:   nothing.
;MODIFIES:  r25,r26, Z-ptr
;CALLS:     none
;CALLED BY: main application and diagnostics
;********************************************************************
clr_dsp_buffs:
     ldi R25, 48			; load total length of both buffer.
     ldi R26, ' '              ; load blank/space into R26.
     ldi ZH, high (dsp_buff_1) ; Load ZH and ZL as a pointer to 1st
     ldi ZL, low (dsp_buff_1)  ; byte of buffer for line 1.
   
    ;set DDRAM address to 1st position of first line.
store_bytes:
     st  Z+, R26       ; store ' ' into 1st/next buffer byte and
                       ; auto inc ptr to next location.
     dec  R25          ; 
     brne store_bytes  ; cont until r25=0, all bytes written.
     ret


;**********************************************
;SUBROUTINE FOR DISPLAYING THE INPUT TO LCD
;**********************************************
display_the_value:
	ldi YH, high (dsp_buff_1) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).
	
	ldi r16, 'n'
	st Y+, r16
	ldi r16, ' '
	st Y+, r16
	ldi r16, '='
	st Y+, r16
	ldi r16, ' '
	st Y+, r16

	ldi r17, $30							; Load $30 into r16
	; store the ascii representation of the digit in the buffer
	lds r16, (burst_count_setting_bcd + 2)			; Store the leftmost keyvalue into r16
	
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16										; Put the value into the display buffer
	
	lds r16, (burst_count_setting_bcd + 1)			; 
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16

	lds r16, (burst_count_setting_bcd + 0)			; Store the rightmost keyvalue into r16
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16										; Put the value into the display buffer	

	call update_lcd_dog								; update the display
	ret

;***********************************************
;SUBROUTINE FOR STORING THE VALUE INTO THE Variable
;**********************************************
store_value:
	;r18 is the value read by the input
	lds r18, keyvalue
	lds r16, burst_count_setting_bcd + 1	; Load r16 with the middle digit
	sts burst_count_setting_bcd + 2, r16	; Put the middle digit into the leftmost digit

	lds r16, burst_count_setting_bcd + 0	; Load r16 with the Rightmost digit
	sts burst_count_setting_bcd + 1, r16	; Put the rightmost digit into the middle digit

	sts burst_count_setting_bcd + 0, r18	; Store the new number into the rightmost digit
	ret

;**********************************
;SUBROUTINE FOR RETRIEVING INPUT(PART 2)
;*********************************
get_key_value:
	in r18, PIND			; Store the Input into r18
	andi r18, $F0			; Clear the low nibble of r18
	swap r18				; Swap the nibble
	call keycode2keyvalue	; Convert the input into HEXVALUES (NOT ASCII)
	ret

;********************************
;SUBROUTINE FOR LOOKUP TABLE
;******************************
keycode2keyvalue:
lookup:
	ldi ZH, high (keytable * 2)		;set Z to point to start of table 
	ldi ZL, low (keytable * 2)
	ldi r16, $00					;add offset to Z pointer 
	add ZL, r18						;originally r18
	add ZH, r16
	lpm r18, Z	
	ret

;**************************
;SUBROUTINE FOR DELAY
;************************
var_delay: ;delay for ATmega324 @ 1MHz = r16 * 0.1 ms
outer_loop:
	ldi r17, 32
inner_loop:
	dec r17
	brne inner_loop
	dec r16
	brne outer_loop
	ret


;**********************************
;SUBROUTINE FOR BUZZER
;*********************************
buzz:
	sbi PORTA, 6
	ldi r16 , 255	; For delay
	call var_delay
	cbi PORTA, 6
	ret


;**********************************************
;SUBROUTINE FOR PULSE GENERATOR 
;**********************************************
pulse_generator:
	sbi PORTA, 7						; set bit for pulse
	rcall var_delay	
	ldi r16, 10							; pulse width delay
	cbi PORTA, 7						; clear bit for pulse 
	rcall var_delay	
	ldi r16, 10							; time between pulses delay
	dec r19								; decrement the binary value
	brne pulse_generator
	
	call clear_flags

	ret


;**********************************************
;SUBROUTINE FOR GENERATING A PULSES
;**********************************************
generate_a_pulse:	
	ldi r16, 10							; pulse width
	sbi PORTA, 7						; set bit for pulse
	rcall var_delay	
	cbi PORTA, 7						; clear bit for pulse 
	ret


;**********************************************
;SUBROUTINE FOR ASSIGNING FLAGS
;**********************************************
update_flags:
	ldi r16, 1					; Set the make_pulse flag
	sts make_pulse, r16

	lds r16, burst_count
	cpi r16, $00
	breq burst_is_zero

	ldi r16, 0
	sts is_burst_zero, r16

please_go_here:
	ret

burst_is_zero:
	ldi r16, 1
	sts is_burst_zero, r16
	rjmp please_go_here


;**********************************************
;SUBROUTINE FOR CLEARING FLAGS
;**********************************************
clear_flags:
	ldi r16, 0
	sts make_pulse, r16					; Reset the make_pluse to zero
	ret


;**********************************************
;SUBROUTINE FOR CONVERTING UNPACKED BCD TO BINARY
;**********************************************
convert_to_Binary:
	lds r16, burst_count_setting_bcd		; Retrieve the value store in the FIRST byte of burst_count_setting_bcd	and store it in r16	
	lds r17, burst_count_setting_bcd + 1	; Retrieve the value store in the SECOND byte of burst_count_setting_bcd and store it in r17
	lds r18, burst_count_setting_bcd + 2	; Retrieve the value store in the THIRD byte of burst_count_setting_bcd and store it in r18

	swap r17								; Swap the nibble in r17								
	or r16, r17								; Or r16 & r17, Combine the two contents of two registers into one register (r16)
	andi r18, $0F							; AND r18 & $0F, clear the high nibble of r18
	mov r17, r18							; Move the content of r18 into r17
	ldi r18, $00							; Load r18 with zero, this will be useful when we are trying to convert
											; Packed BCD into a 16-bit 		R16	0x0a	byte{registers}@R16bianry value
;This converts the Packed BCD into the 16-bit binary
	call BCD2bin16

	mov r19, r14							; Moves the low byte of the 16-bit binary value into r17
	sts burst_count, r19					; Store the value of r17 into burst_count_bin
	ret


;***************************************************************************
;*
;* "BCD2bin16" - BCD to 16-Bit Binary Conversion
;*
;* This subroutine converts a 5-digit packed BCD number represented by 
;* 3 bytes (fBCD2:fBCD1:fBCD0) to a 16-bit number (tbinH:tbinL).
;* MSD of the 5-digit number must be placed in the lowermost nibble of fBCD2.
;* 
;* Let "abcde" denote the 5-digit number. The conversion is done by
;* computing the formula: 10(10(10(10a+b)+c)+d)+e.
;* The subroutine "mul10a"/"mul10b" does the multiply-and-add operation 
;* which is repeated four times during the computation.
;*  
;* Number of words	:30 
;* Number of cycles	:108 
;* Low registers used	:4 (copyL,copyH,mp10L/tbinL,mp10H/tbinH)
;* High registers used  :4 (fBCD0,fBCD1,fBCD2,adder)	
;*
;***************************************************************************

;***** "mul10a"/"mul10b" Subroutine Register Variables

.def	copyL	=r12		;temporary register
.def	copyH	=r13		;temporary register
.def	mp10L	=r14		;Low byte of number to be multiplied by 10
.def	mp10H	=r15		;High byte of number to be multiplied by 10
.def	adder	=r19		;value to add after multiplication	

;***** Code

mul10a:	;***** multiplies "mp10H:mp10L" with 10 and adds "adder" high nibble 
	swap	adder
mul10b:	;***** multiplies "mp10H:mp10L" with 10 and adds "adder" low nibble 
	mov	copyL,mp10L	;make copy
	mov	copyH,mp10H
	lsl	mp10L		;multiply original by 2
	rol	mp10H
	lsl	copyL		;multiply copy by 2
	rol	copyH		
	lsl	copyL		;multiply copy by 2 (4)
	rol	copyH		
	lsl	copyL		;multiply copy by 2 (8)
	rol	copyH		
	add	mp10L,copyL	;add copy to original
	adc	mp10H,copyH	
	andi	adder,0x0f	;mask away upper nibble of adder
	add	mp10L,adder	;add lower nibble of adder
	brcc	m10_1		;if carry not cleared
	inc	mp10H		;	inc high byte
m10_1:	ret	

;***** Main Routine Register Variables

.def	tbinL	=r14		;Low byte of binary result (same as mp10L)
.def	tbinH	=r15		;High byte of binary result (same as mp10H)
.def	fBCD0	=r16		;BCD value digits 1 and 0
.def	fBCD1	=r17		;BCD value digits 2 and 3
.def	fBCD2	=r18		;BCD value digit 5

;***** Code

BCD2bin16:
	andi	fBCD2,0x0f	;mask away upper nibble of fBCD2
	clr	mp10H		
	mov	mp10L,fBCD2	;mp10H:mp10L = a
	mov	adder,fBCD1
	rcall	mul10a		;mp10H:mp10L = 10a+b
	mov	adder,fBCD1
	rcall	mul10b		;mp10H:mp10L = 10(10a+b)+c
	mov	adder,fBCD0		
	rcall	mul10a		;mp10H:mp10L = 10(10(10a+b)+c)+d
	mov	adder,fBCD0
	rcall	mul10b		;mp10H:mp10L = 10(10(10(10a+b)+c)+d)+e
	ret
	

;***************************************************************************
;* 
;* "keypress_ISR" - Check Interrupts at INT0
;*
;* Description: Get the keyvalue if the key is pressed, the keyvalue is stored if the key is a number
;*
;* Author:                  Seyi Olajuyi & Bassel El Amine
;* Version:
;* Last updated:            11/21/19
;* Target:                  ATmega324A
;* Number of words:
;* Number of cycles:        N/A
;* Low registers modified:  none
;* High registers modified: none
;*
;* Parameters:  
;* Notes: 
;*
;***************************************************************************

    ;INT0 interrupt service routine
keypress_ISR:
	push r18
    push r16            ;save r16	
	in r16, SREG        ;save SREG
    push r16
	
	ldi r16, (1 <<INTF0)
	out EIFR, r16
		
	rcall get_key_value
	cpi r18, $0A	
	brlo skip_line_1

	breq input_clear

	cpi r18, $0C
	breq input_enter

	ldi r16, $FF
	sts input, r16

restore_values_1:
	call fsm

    pop r16             ;restore SREG
    out SREG,r16
	pop r16             ;restore r16
	pop r18				;restore r18

    reti                ;return from interrupt

skip_line_1:
	sts keyvalue, r18						; if key value is a number

	ldi r16, $00							; input is assign as a number
	sts input, r16

	rcall store_value
	rjmp restore_values_1
	
input_clear:
	ldi r16, $02
	sts input, r16
	rjmp restore_values_1

input_enter:
	ldi r16, $01
	sts input, r16
	rjmp restore_values_1



;***************************************************************************
;* 
;* "pb_press_ISR" - Check Interrupts at INT1
;*
;* Description: Checks if the push button is pressed
;*
;* Author:                  Ken Short
;* Version:
;* Last updated:            11/21/19
;* Target:                  ATmega324A
;* Number of words:
;* Number of cycles:        16
;* Low registers modified:  none
;* High registers modified: none
;*
;* Parameters:  Uses PORTB register to hold the count and drive LEDs
;* connected to that port.
;*
;* Notes: 
;*
;***************************************************************************

    ;INT1 interrupt service routine
pb_press_ISR:
	push r16            ;save r16
	in r16, SREG        ;save SREG
    push r16

wait_for_bounce_1:
	sbic PIND, 3
	rjmp wait_for_bounce_1
	ldi r16, 100
	rcall var_delay
	sbic PIND, 3
	rjmp wait_for_bounce_1

	ldi r16, (1 <<INTF1)
	out EIFR, r16

	ldi r16 , $03						; Set polling_for_button
	sts input, r16		; Use to find out if the button was pressed

restore_value_2:
	call fsm
    pop r16             ;restore SREG
    out SREG,r16
	pop r16             ;restore r16

    reti                ;return from interrupt




keytable: .db $01, $02, $03, $0F, $04, $05, $06, $0E, $07, $08, $09, $0D
		.db $0A, $00, $0B, $0C
      

.nolist
.include "lcd_dog_asm_driver_m324a.inc"
.list

