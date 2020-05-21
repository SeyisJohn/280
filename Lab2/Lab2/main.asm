;
; Lab2.asm
;
; Created: 9/27/2019 1:14:25 PM
; Author : user38x
;

.nolist
.include "m324adef.inc"
.list

reset:

ldi r16, $FF
out DDRB, r16
ldi r16, $00
out DDRD, r16

again:

in r16, PIND
com r16
out PORTB, r16
rjmp again
