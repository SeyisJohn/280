;***************************************************************************
;*
;* Title: programmable_pulse_generator_I
;* Author: Seyi Olajuyi & Bassel El Amine
;* Version: 1.0
;* Last updated: 2019/11/07
;* Target: ATmega 324
;*
;* DESCRIPTION
;* 
;* 
;*
;*
;* VERSION HISTORY
;* 1.0 Original version
;***************************************************************************
.nolist
.include "m324adef.inc"
.list

.dseg	;The variable below are in SRAM
burst_count_setting_bcd:		.byte 3; setting unpacked BCD ;THIS HAS THREE BTYE allocated to the variable name
burst_count_setting_bin:		.byte 1; setting in binary
burst_count_bin:				.byte 1; pulses left to generated in burst
pulse_width_bcd:				.byte 3
pulse_width:					.byte 1
pulse_delay_bcd:				.byte 3
pulse_delay:					.byte 1
line:							.byte 3

     
;burst_count_setting_bcd is right most digit and
; (burst_count_setting_bcd + 2) is the left most digit 
	 
	 .CSEG


     ; interrupt vector table, with several 'safety' stubs
     rjmp RESET      ;Reset/Cold start vector
     reti            ;External Intr0 vector
     reti            ;External Intr1 vector

	
;**********************************************************************
;************* M A I N   A P P L I C A T I O N   C O D E  *************
;**********************************************************************


RESET:
    ldi r16, low(RAMEND)  ; init stack/pointer
    out SPL, r16          ;
    ldi r16, high(RAMEND) ;
    out SPH, r16


    ldi r16, $ff      ; load r16 with all 1s.
    out DDRB, r16     ; set portB = output
	ldi r17, $00	  ; load r17 with all 0s.
	out DDRD, r17	  ; set portD = input
	ldi r16, $0f
	out PORTD, r16    ; enable pull-up resistor on PD0-PD3
	sbi DDRA, 7		  ; Set Pin 7(PORTA) to Output
	sbi DDRA, 2		  ; Set Pin 2(PORTA) to Output
	cbi DDRA, 3       ; Set Pin 3(PORTA) to Input
	cbi DDRA, 4		  ; Set Pin 4(PORTA) to Input
	ldi r18, $80	  ; load r18 with $80
	out DDRC, r18     ; set pin 6 to input & set pin 7 to output(also set all other pins to input)
	cbi PORTC, 7	  ; activate CLR on Flip Flop(Keypad)
	sbi PORTC, 7      ; Deactivate CLR on Flip Flop(Keypad)

	cbi PORTA, 2	  ; activate CLR on Flip Flop(PushButton)
	sbi PORTA, 2      ; Deactivate CLR on Flip Flop(PushButton)
    sbi portB, 4      ; set /SS of DOG LCD = 1 (Deselected)
	

    rcall init_lcd_dog        ; init display, using SPI serial interface    
	rcall clr_dsp_buffs       ; clear all three buffer lines
	rcall update_lcd_dog      ; update the display

	ldi YH, high (dsp_buff_1) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).

	; RESET THE VARIABLES WITH ZERO
	sts burst_count_setting_bcd + 2, r17	
	sts burst_count_setting_bcd + 1, r17	
	sts burst_count_setting_bcd + 0, r17	
	sts burst_count_bin, r17

	ldi r16, 10
	sts pulse_width, r16
	sts pulse_delay, r16			  
	
	ldi r16, '#'
	ldi r17, ' '
	sts line, r16
	sts line + 1, r17
	sts line + 2, r17

;****************************************************************
;***********************CODE BEGINS******************************
;****************************************************************

;This runs after the peripherals are initalized
after_peri:
	call get_key_value						; get the value for the key
	cpi r18, $0A							; compare if r18 is equal to CLEAR
	brne after_peri							; it the key is not equal to clear, it will loop until it is

	rcall display_the_value

idiot:
	rcall get_key_value
	cpi r18, $0F			; compare key value to Up arrow
	brne we_back_from_switch_lines_up
	rcall switch_lines_up
we_back_from_switch_lines_up:
	cpi r18, $0E			; compare key value to down arrow
	brne we_back_from_switch_lines_down
	rcall switch_lines_down
we_back_from_switch_lines_down:
	cpi r18, $0C			; compare key value to ENTER
	brne we_back_from_Enter
	rcall compare_enter
we_back_from_Enter:
	rcall display_the_value
	in r20, PINA						; Read in PIN A, used to read in the value for bit3 in Pin A
	andi r20, $08						; Mask r20, used to compare third bit in r20
	sbrs r20, 3							;skips the rjmp and goes to converting the value into binary
	rjmp idiot
	rjmp prompt

prompt:
	rcall convert_line1_to_Packed_BCD
	rcall convert_BCD_to_Binary
	sts burst_count_setting_bin, r14		; Store the value of r17 into burst_count_setting_bin
	sts burst_count_bin, r14				; Store the value of r17 into burst_count_bin
	

	rcall convert_line2_to_Packed_BCD
	rcall convert_BCD_to_Binary
	sts pulse_width, r14
	
	
	rcall convert_line3_to_Packed_BCD
	rcall convert_BCD_to_Binary
	sts pulse_delay, r14

	rjmp re_init
	;;;;THIS IS WHERE I STOPPED
	;;;;AAAAAAAAAAAHHHHHHHHHHHHHHHHHHHHHHHHHHH!!!!!!!!!!!!!!!!!!!!!!!!



check_flag:
	sbis PINA, 3							; Check if the push button is pushed		
	rjmp check_flag							; It will continually loop until the push button is pressed
	;;THIS WILL BOUNCE

		
;This is really useful when we want to generate another set of pulses
re_init:
	cbi PORTA, 2						    ; Activate the flip-flop that is connected to the push button
	sbi PORTA, 2		                    ; Deactivate the flip-flop that is connected to the push button
	
	;lds r16, pulse_width					; Load pulse_width into r16, This is to create the delay
	lds r19, burst_count_bin				; This loads r19 with the orginal binary value	

check_zero:
	cpi r19, $00
	breq generate_a_pulse

;This generate a pulse that is supposed to be n ms wide
pulse_generator:
	lds r16, pulse_width					; Load pulse_width into r16, This is to create the delay
	sbi PORTA, 7							; set bit for pulse
	rcall var_delay	
	cbi PORTA, 7							; clear bit for pulse 
	lds r16, pulse_delay					; Load pulse_width into r16, This is to create the delay
	rcall var_delay	
	dec r19									; decrement the binary value
	brne pulse_generator	

;This part is reached if the binary value is equal to zero
check_flag_2:
get_key_value_2:

	in r20, PINA						; Read in PIN A, used to read in the value for bit3 in Pin A
	in r21, PINC						; Read in PIN C, used to read in the value for bit6

	andi r20, $08						; Mask r20, used to compare third bit in r20
	andi r21, $40						; Mask r21, used to compare sixth bit in r21

	cpi r20, $08						; Check if the flip-flop that is connected to push button is set
	breq re_init

	cpi r21, $40						; Check if the flip-flop that is connected to the keypad is set
	breq service_keypad_input
	rjmp get_key_value_2

service_keypad_input:
	rcall get_key_value

	cpi r18, $0A							; checks if the key value is equal to CLEAR
	breq prompt								; goes to the beginning if the key value is equal to CLEAR
	jmp check_flag_2							; goes back to generate another set of pulses


generate_a_pulse:
	lds r16, pulse_width					; Load pulse_width into r16, This is to create the delay
	sbi PORTA, 7							; set bit for pulse
	rcall var_delay	
	cbi PORTA, 7							; clear bit for pulse 
	lds r16, pulse_delay					; Load pulse_width into r16, This is to create the delay
	rcall var_delay	

	sbis PINC, 6 
	rjmp generate_a_pulse							; decrement the binary value
	rcall get_key_value_3
	cpi r18, $0A
	breq prompt1

	prompt1:
	rjmp prompt

;---------------------------- SUBROUTINES ----------------------------

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


;**************************************
;SUBROUTINE FOR SWITCHING LINES DOWN
;************************************
switch_lines_down:
	lds r16, line
	lds r17, line + 1
	lds r18, line + 2

	sts line + 1, r16
	sts line + 2, r17
	sts line, r18

	ret

;***************************************
;SUBROUTINE FOR SWITCHING LINES UP
;***************************************
switch_lines_up:
	lds r16, line
	lds r17, line + 1
	lds r18, line + 2

	sts line + 2, r16
	sts line + 1, r18
	sts line, r17

	ret


;*************************************
;SUBROUTINE FOR STORING LINE 1
;*************************************
store_value_line_1:
	rcall get_key_value_3
	lds r16, burst_count_setting_bcd + 1	; Load r16 with the middle digit
	sts burst_count_setting_bcd + 2, r16	; Put the middle digit into the leftmost digit

	lds r16, burst_count_setting_bcd + 0	; Load r16 with the Rightmost digit
	sts burst_count_setting_bcd + 1, r16	; Put the rightmost digit into the middle digit

	sts burst_count_setting_bcd + 0, r18	; Store the new number into the rightmost digit
	ret


;*********************************
;SUBROUTINE FOR STORING LINE 2
;********************************
store_value_line_2:
	rcall get_key_value_3
	lds r16, pulse_width_bcd + 1	; Load r16 with the middle digit
	sts pulse_width_bcd + 2, r16	; Put the middle digit into the leftmost digit

	lds r16, pulse_width_bcd + 0	; Load r16 with the Rightmost digit
	sts pulse_width_bcd + 1, r16	; Put the rightmost digit into the middle digit

	sts pulse_width_bcd + 0, r18	; Store the new number into the rightmost digit
	ret


;**************************************
;SUBROUTINE FOR STORING LINE 3
;*************************************
store_value_line_3:
	rcall get_key_value_3
	lds r16, pulse_delay_bcd + 1	; Load r16 with the middle digit
	sts pulse_delay_bcd + 2, r16	; Put the middle digit into the leftmost digit

	lds r16, pulse_delay_bcd + 0	; Load r16 with the Rightmost digit
	sts pulse_delay_bcd + 1, r16	; Put the rightmost digit into the middle digit

	sts pulse_delay_bcd + 0, r18	; Store the new number into the rightmost digit
	ret 


;*************************************
;SUBROUTINE FOR COMPARING THE ENTER
;************************************
compare_enter:
	ldi YH, high (dsp_buff_1 + 8) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1 + 8)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).
	
	ld r16, Y					  ; Retrieve character from buffer
	cpi r16, '#'				  ; Compare the registers with #
	brne store_line_2
	rcall store_value_line_1	
			
store_line_2:
	ldi YH, high (dsp_buff_2 + 8) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_2 + 8)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).

	ld r16, Y
	cpi r16, '#'
	brne store_line_3
	rcall store_value_line_2

store_line_3:
	ldi YH, high (dsp_buff_3 + 8) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_3 + 8)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).

	ld r16, Y
	cpi r16, '#'
	brne end_comparing 
	rcall store_value_line_3
end_comparing:
	ret


;********************************************************************************
;This calls the subroutine that converts the Packed BCD into the 16-bit binary
;********************************************************************************
convert_BCD_to_Binary:
	call BCD2bin16
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
	
	ldi r16, ' '
	st Y+, r16

	lds r16, line
	st Y+, r16

	call update_lcd_dog								; update the display

	ldi YH, high (dsp_buff_2) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_2)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).

	ldi r16, 't'
	st Y+, r16
	ldi r16, ' '
	st Y+, r16
	ldi r16, '='
	st Y+, r16
	ldi r16, ' '
	st Y+, r16
	
	ldi r17, $30							; Load $30 into r16
	; store the ascii representation of the digit in the buffer
	lds r16, (pulse_width_bcd + 2)			; Store the leftmost keyvalue into r16
	
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16										; Put the value into the display buffer
	
	lds r16, (pulse_width_bcd + 1)			; 
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16

	lds r16, (pulse_width_bcd + 0)			; Store the rightmost keyvalue into r16
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16								; Put the value into the display buffer	

	ldi r16, ' '
	st Y+, r16

	lds r16, line + 1
	st Y+, r16

	call update_lcd_dog								; update the display

	ldi YH, high (dsp_buff_3) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_3)  ; byte of dsp_buff_3 (Note - assuming 
                              ; (dsp_buff_3 for now).

	ldi r16, 't'
	st Y+, r16
	ldi r16, ' '
	st Y+, r16
	ldi r16, '='
	st Y+, r16
	ldi r16, ' '
	st Y+, r16
	
	ldi r17, $30							; Load $30 into r16
	; store the ascii representation of the digit in the buffer
	lds r16, (pulse_delay_bcd + 2)			; Store the leftmost keyvalue into r16
	
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16										; Put the value into the display buffer
	
	lds r16, (pulse_delay_bcd + 1)			; 
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16

	lds r16, (pulse_delay_bcd + 0)			; Store the rightmost keyvalue into r16
	or r16, r17								; Adds $30 to the keyvalue, which turn the keyvalue into ASCII
	st Y+, r16								; Put the value into the display buffer	

	ldi r16, ' '
	st Y+, r16

	lds r16, line + 1
	st Y+, r16

	call update_lcd_dog								; update the display
	ret


;*****************************************************		
;SUBROUTINE convert the line 1 to PACKED BCD
;****************************************************
convert_line1_to_Packed_BCD:
	lds r16, burst_count_setting_bcd		; Retrieve the value store in the FIRST byte of burst_count_setting_bcd	and store it in r16	
	lds r17, burst_count_setting_bcd + 1	; Retrieve the value store in the SECOND byte of burst_count_setting_bcd and store it in r17
	lds r18, burst_count_setting_bcd + 2	; Retrieve the value store in the THIRD byte of burst_count_setting_bcd and store it in r18

	swap r17								; Swap the nibble in r17								
	or r16, r17								; Or r16 & r17, Combine the two contents of two registers into one register (r16)
	andi r18, $0F							; AND r18 & $0F, clear the high nibble of r18
	mov r17, r18							; Move the content of r18 into r17
	ldi r18, $00							; Load r18 with zero, this will be useful when we are trying to convert
											; Packed BCD into a 16-bit
	ret 


;*****************************************************		
;SUBROUTINE convert the line 2 to PACKED BCD
;****************************************************
convert_line2_to_Packed_BCD:
	lds r16, pulse_width_bcd				
	lds r17, pulse_width_bcd + 1	
	lds r18, pulse_width_bcd + 2	

	swap r17								; Swap the nibble in r17								
	or r16, r17								; Or r16 & r17, Combine the two contents of two registers into one register (r16)
	andi r18, $0F							; AND r18 & $0F, clear the high nibble of r18
	mov r17, r18							; Move the content of r18 into r17
	ldi r18, $00							; Load r18 with zero, this will be useful when we are trying to convert
											; Packed BCD into a 16-bit
	ret 


;*****************************************************		
;SUBROUTINE convert the line 3 to PACKED BCD
;****************************************************
convert_line3_to_Packed_BCD:
	lds r16, pulse_delay_bcd				
	lds r17, pulse_delay_bcd + 1	
	lds r18, pulse_delay_bcd + 2	

	swap r17								; Swap the nibble in r17								
	or r16, r17								; Or r16 & r17, Combine the two contents of two registers into one register (r16)
	andi r18, $0F							; AND r18 & $0F, clear the high nibble of r18
	mov r17, r18							; Move the content of r18 into r17
	ldi r18, $00							; Load r18 with zero, this will be useful when we are trying to convert
											; Packed BCD into a 16-bit
	ret 


;**********************************
;SUBROUTINE FOR RETRIEVING INPUT(PART 1)
;*********************************
get_key_value:
	sbis PINC, 6			; Check if any value on the keypad is press
	rjmp get_key_value		; Loop back if no keypad is pressed
	in r18, PIND			; Store the Input into r18
	andi r18, $F0			; Clear the low nibble of r18
	swap r18				; Swap the nibble
	call keycode2keyvalue	; Convert the input into HEXVALUES (NOT ASCII)
	cbi PORTC, 7			; Clear the FLip Flop that is connected to the encoder
	sbi PORTC, 7			;
	ret



;**********************************
;SUBROUTINE FOR RETRIEVING INPUT(PART 2)
;*********************************
get_key_value_3:
	in r18, PIND			; Store the Input into r18
	andi r18, $F0			; Clear the low nibble of r18
	swap r18				; Swap the nibble
	call keycode2keyvalue	; Convert the input into HEXVALUES (NOT ASCII)
	cbi PORTC, 7			; Clear the FLip Flop that is connected to the encoder
	sbi PORTC, 7			;
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
outer_loop:; r16 should equal to 10	
	ldi r17, 32
inner_loop:
	dec r17
	brne inner_loop
	dec r16
	brne outer_loop
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
	
keytable: .db $01, $02, $03, $0F, $04, $05, $06, $0E, $07, $08, $09, $0D
		.db $0A, $00, $0B, $0C
      

.nolist
.include "lcd_dog_asm_driver_m324a.inc"
.list
