;***************************************************************************
;*
;* Title:           pos_edge_ints
;* Author:          Seyi Olajuyi & Bassel El Amine
;* Version:         1.0
;* Last updated:    11/21/19
;* Target:          ATmega324A 
;*
;* DESCRIPTION
;* This program counts the number of times a key (any key) on the keypad
;* is pressed and the number of times the pushbutton is pressed.
;*
;*
;* VERSION HISTORY
;* 1.0 Original version
;***************************************************************************

.nolist
.include "m324adef.inc"
.list

.dseg
key_presses:	.byte 1
pb_presses:		.byte 1


.cseg
reset:
.org RESET              ;reset interrupt vector
    rjmp start          ;program starts here at reset
.org INT0addr           ;INT0 interrupt vector
    rjmp keypress_ISR 
.org INT1addr
	rjmp pb_press_ISR


start:    
    cbi DDRD, 2				; Set pin 2 on PORTD to input
	cbi DDRD, 3				; Set pin 3 on PORTD to input
	
	sbi DDRA, 2				; Set pin 2 on PORTA to output
	sbi DDRC, 7				; Set pin 7 on PORTC to output

	cbi DDRA, 4				; Set pin 4 on PORTA to output

	ldi r16, 0				; Clear the variables
	sts key_presses, r16
	sts pb_presses, r16

    ldi r16, LOW(RAMEND)    ;initialize SP to point to top of stack
    out SPL, r16
    ldi r16, HIGH(RAMEND)
    out SPH, r16

	ldi r16, (1 << ISC11) | (1 << ISC10) | (1 << ISC01) | (1 << ISC00)
	sts EICRA, r16
	ldi r16, (1 << INT0) | (1 << INT1)
	out EIMSK, r16

	sei

;***************************************************************************
;* 
;* "keypress_ISR" - Count Interrupts at INT0
;*
;* Description: Counts rising edges at INT0 (PD2)
;*
;* Author:                  Ken Short
;* Version:
;* Last updated:            10/23/17
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

    ;INT0 interrupt service routine
keypress_ISR:
	;cli					; Disable global interrupt
    in r16, SREG        ;save SREG
    push r16

    lds r16, key_presses       ;increment count
    inc r16 
	sts key_presses, r16

restore_values_1:

    pop r16             ;restore SREG
    out SREG,r16

    reti                ;return from interrupt


;***************************************************************************
;* 
;* "pb_press_ISR" - Count Interrupts at INT1
;*
;* Description: Counts rising edges at INT1 (PD3)
;*
;* Author:                  Ken Short
;* Version:
;* Last updated:            10/23/17
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
wait_for_bounce_1:
	sbic PINA, 4
	rjmp wait_for_bounce_1
	ldi r16, 100
	rcall var_delay
	sbic PINA, 4
	rjmp wait_for_bounce_1

	ldi r16, (1 <<INTF1)
	out EIFR, r16

    in r16, SREG        ;save SREG
    push r16

    lds r16, pb_presses       ;increment count
    inc r16 
	sts pb_presses, r16

restore_value_2:
    pop r16             ;restore SREG
    out SREG,r16

    reti                ;return from interrupt

;***********************************************
;SUBROUTINE FOR VAR DELAY
;***********************************************
var_delay:
	outer_loop:
		ldi r17, 32
	inner_loop:
		dec r17
		brne inner_loop
		dec r16
		brne outer_loop
		ret