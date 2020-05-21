;
; lab03_level.asm
;
; Created: 10/4/2019 1:50:22 PM
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

main_loop:
	in r16, PIND

	ldi r17, 8
	ldi r18, $00

next_bit:

	lsl r16
	brcc dec_bitcounter
	ror r18

dec_bitcounter:
	dec r17
	brne next_bit
	com r18
	out PORTB, r18
	rjmp main_loop

	table: .db $01, $4F, $12, $06, $4C, $24, $60, $0F, $00