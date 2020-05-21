;
; lab03_square_wave.asm
;
; Created: 10/4/2019 2:43:20 PM
; Author : user38x
;

.nolist
.include"m324adef.inc"
.list

reset:
    ldi r16, 0x01
	out DDRA, r16
	cbi PORTA, 0

main_loop:
	cbi PORTA, 0
	nop
	nop
	nop
	sbi PORTA, 0
	jmp main_loop
