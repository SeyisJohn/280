;***************************************************************************
;* 
;* var_delay - title
;*
;* Description:
;*
;* Author: Kenneth Short
;* Version: 1.0
;* Last updated: N/A
;* Target: ATmega324 @ 1MHz
;* Number of words:
;* Number of cycles: 100
;* Low registers modified:
;* High registers modified: r16, r17
;*
;* Parameters: r16, r17
;*
;* Returns: N/A
;*
;* Notes: 
;*
;***************************************************************************

.nolist
.include "m324adef.inc"
.list

RESET:
    ldi r16, low(RAMEND)  ; init stack/pointer
    out SPL, r16          ;
    ldi r16, high(RAMEND) ;
    out SPH, r16		  ;

	ldi r16, 0xff     ; set portB = output.
    out DDRB, r16     ; 
    sbi portB, 4      ; set /SS of DOG LCD = 1 (Deselected)

	ldi r16, 10

	rcall var_delay
	rjmp RESET

var_delay: ;delay for ATmega324 @ 1MHz = r16 * 0.1 ms
outer_loop:
	ldi r17, 32
inner_loop:
	dec r17
	brne inner_loop
	dec r16
	brne outer_loop
	ret