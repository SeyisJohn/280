;
; lab03_sws_tbl.asm
;
; Created: 10/4/2019 2:34:13 PM
; Author : user38x
;
.nolist
.include"m324adef.inc"
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
	inc r18

dec_bitcounter:
	dec r17
	brne next_bit

bcd_7seg:
	ldi ZH, high (table * 2)
	ldi ZL, low (table * 2)
	ldi r16, $00
	add ZL, r18
	adc ZH, r16
	lpm r18, Z

display:
	out PORTB, r18
	rjmp main_loop

table: .db $01, $4F, $12, $06, $4C, $24, $60, $0F, $00