;;***************************************************************************
;*
;* Title:               pulsed_drive.asm
;* Author:              Ken Short
;* Version:             1.0
;* Last updated:        09/11/17
;* Target:              ATmega324 @ 1 MHz 
;*
;* DESCRIPTION
;* Reads a BCD digit from pins PD3 - PD0 and displays the value on a seven-
;* segment display. The display is common anode and is connected to PB6 - PB0.
;* The segments are assigned in order a through g to pins PB6 - PB0.
;*
;* The display is operated in pulsed mode with a 50% duty cycle. The driver
;* transisor is a ZTX555 with its base connected through a 330 ohm resisor
;* to PA0.
;*
;* r20 is used to control the refresh frequency.
;* Increasing r20 decreases the frequency.
;*
;*
;* VERSION HISTORY
;* 1.0 Original version
;* 2.0 Modified to user r20 rather than the memory variable delay_param
;* 2.1 Modified for ATmega324A
;***************************************************************************

.nolist
.include "m324adef.inc"
.list

.cseg
reset:
    ;Configure port B as an output port
    ldi r16, $FF        ;load r16 with all 1s
    out DDRB, r16       ;port B - all bits configured as outputs

    ;Configure port D as an input port
    ldi r16, $00        ;load r16 with all 0s
    out DDRD, r16       ;port D - all bits configured as inputs
    ldi r16, $FF        ;enable pull-up resistors by outputting
    out PORTD, r16      ;all 1s to PORTD

    ;Configure port A bit 0 as an output
    ldi r16, $03        ;load r16 with a 1 in the bit 0 position
    out DDRA, r16       ;port A - bit 0 as an output
    ldi r16, $03        ;turn OFF digit driver
    out PORTA, r16

    ;Inital delay value
    ldi r20, 32         ; 32

    ;Initialize stack pointer to allow subroutine calls
    ldi r16, LOW(RAMEND)    ;load low byte of stack pointer    
    out SPL, r16
    ldi r16, HIGH(RAMEND)   ;load high byte of stack pointer
    out SPH, r16

main_loop:
    in r18, PIND        ;input switch values
    andi r18, $0f       ;force ms nibble to 0
	in r19, PIND
	andi r19, $f0
	swap r19               

    ;Code to perform table lookup operation,
    ;Nibble from switches is offset into table
bcd_7seg_1:
    ldi ZH, high (hextable * 2)    ;set Z to point to start of table
    ldi ZL, low (hextable * 2)
    ldi r16, $00                ;add offset to Z pointer
    add ZL, r18
    adc ZH, r16
    lpm r18, Z                  ;load byte from table pointed to by Z

bcd_7seg_2:
    ldi ZH, high (hextable * 2)    ;set Z to point to start of table
    ldi ZL, low (hextable * 2)
    ldi r16, $00                ;add offset to Z pointer
    add ZL, r19
    adc ZH, r16
    lpm r19, Z                  ;load byte from table pointed to by Z

display:
    out PORTB, r18     ;output image to seven-segment display
    ldi r16, $02        ;turn ON digit
    out PORTA, r16
    call on_delay       ;timeout digit ON time
    ldi r16, $03        ;turn OFF digit
    out PORTA, r16
	nop
	nop


	out PORTB, r19     ;output image to seven-segment display
    ldi r16, $01        ;turn ON digit
    out PORTA, r16
    call on_delay       ;timeout digit ON time
    ldi r16, $03        ;turn OFF digit
   	nop
	nop 


    rjmp main_loop      ;jump back to read switches again


    ;Table of segment values to display digits 0 - F
    ;!!! seven values must be added
hextable: .db $01, $4F, $12, $06, $4C, $24, $60, $0F, $00, $04, $08, $60, $31, $42, $30, $38



;***************************************************************************
;* 
;* "on_delay" - On delay time interval
;* "off_delay" - Off delay time interval
;*
;* Description:
;* A single subroutine with two different names. Provides the ON time and OFF
;* for a pulsed LED display that has a 50% duty cycle.
;*
;* Author:              Ken Short
;* Version:             1.0
;* Last updated:        09/11/17
;* Target:              ATmega324 @ 1 MHz
;* Number of words:
;* Number of cycles:    123 * delay_param
;* Low registers used:  none
;* High registers used: r16, r17
;*
;* Parameters:          delay_param - outer loop control variable
;*
;* Notes: 
;*
;***************************************************************************

on_delay:   ;delay for ATmega324 @ 1MHz
off_delay:
    mov r17, r20		; load outer loop count
outer_loop:
    ldi r16, 40         ; 40 inner loop is about 123 us (called twice per period)
inner_loop:
    dec r16
    brne inner_loop
    dec r17
    brne outer_loop
    ret