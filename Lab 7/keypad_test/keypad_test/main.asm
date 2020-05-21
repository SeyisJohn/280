;***************************************************************************
;*
;* Title: Keypad test
;* Author: Bassel El Amine & Seyi Olajuyi
;* Version: 1.0
;* Last updated: Oct 31, 2019
;* 
;* DESCRIPTION:
;* The purpose of this code is to use a table lookup to map the key scan scodes of a 4x4 Keypad
;* to the desired key values.
;* VERSION HISTORY
;* 1.0 Original version
;***************************************************************************
.nolist
.include "m324adef.inc"
.list

     .CSEG


     ; interrupt vector table, with several 'safety' stubs
     rjmp RESET      ;Reset/Cold start vector
     reti            ;External Intr0 vector
     reti            ;External Intr1 vector

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
     ldi R25, 48               ; load total length of both buffer.
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


	
;**********************************************************************
;************* M A I N   A P P L I C A T I O N   C O D E  *************
;**********************************************************************


RESET:
    ldi r16, low(RAMEND)  ; init stack/pointer
    out SPL, r16          ;
    ldi r16, high(RAMEND) ;
    out SPH, r16


    ldi r16, $ff     ; load r16 with all 1s.
    out DDRB, r16     ; set portB = output
	ldi r17, $00	  ; load r17 with all 0s.
	out DDRD, r17	  ; set portD = input
	ldi r16, $0f
	out PORTD, r16    ; enable pull-up resistor on PD0-PD3
	ldi r18, $80	  ; load r18 with $80
	out DDRC, r18     ; set pin 6 to input & set pin 7 to output(also set all other pins to input)
	cbi PORTC, 7	  ; activate CLR on Flip Flop
	sbi PORTC, 7      ; Deactivate CLR on Flip Flop
    sbi portB, 4      ; set /SS of DOG LCD = 1 (Deselected)


    rcall init_lcd_dog        ; init display, using SPI serial interface
    rcall clr_dsp_buffs       ; clear all three buffer lines
	rcall update_lcd_dog      ; update the display

reset_buffer:
	ldi YH, high (dsp_buff_1) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).
	ldi r18, 48				  ; Load 48 into r18

check_flag:
	sbis PINC, 6		   ;check if the push button is pushed		
	rjmp check_flag

	in r21, PIND           ;store the PIND in r21
	andi r21, $f0
	swap r21
	rcall lookup
	st Y+, r21             ;send r21 to Y pointer
	rcall update_lcd_dog   ;update the display
	cbi PORTC, 7		   ;activate the flip-flop
	sbi PORTC, 7		   ;deactivate the flip-flop
	dec r18				   ;decrement r18
	brne check_flag		   ;check if r18 is equal to 0
	rjmp reset_buffer	   ;reset the buffer 

lookup:
	ldi ZH, high (table * 2)		;set Z to point to start of table 
	ldi ZL, low (table * 2)
	ldi r16, $00					;add offset to Z pointer 
	add ZL, r21						;originally r18
	add ZH, r16
	lpm r21, Z	
	ret
	
;table: .db $31, $32, $33, $46, $34, $35, $36, $45, $37, $38, $39, $44, $41, $30, $42, $43
table: .db "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"

      

.nolist
.include "lcd_dog_asm_driver_m324a.inc"
.list