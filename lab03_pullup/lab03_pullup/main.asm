;
; lab03_pullup.asm
;
; Created: 10/4/2019 1:19:35 PM
; Author : user38x
;


; Replace with your application code

.nolist
.include "m324adef.inc"
.list

reset:
	ldi r16, $FF
	out DDRB, r16
	ldi r16, $00
	out DDRD, r16
	ldi r16, $FF
	out PORTD, r16

again:
	in r16, PIND
	com r16
	out PORTB, r16
	rjmp again
