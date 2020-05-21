;***************************************************************************
;*
;* Title: Desgin Task 2
;* Author: Seyi Olajuyi & Bassel El Amine
;* Version: 1.0
;* Last updated: Oct 24, 2019
;* 
;* DESCRIPTION:
;* The purpose of this code is to display the Hex to ASCII Characters read by PortD 
;* This code should only display A - F 
;* This code does NOT protect against debouncing
;*
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
	out PORTD, r16    ; enable pull-up resistor by outputting all 1s to port D
	ldi r18, $f7	  ; load r18 with $f7
	out DDRA, r18     ; set pin 3 to input & set pin 2 to output(also set every other pins to output)
	cbi PORTA, 2	  ; activate CLR on Flip Flop
	sbi PORTA, 2      ; Deactivate CLR on Flip Flop
    sbi portB, 4      ; set /SS of DOG LCD = 1 (Deselected)


    rcall init_lcd_dog        ; init display, using SPI serial interface
    rcall clr_dsp_buffs       ; clear all three buffer lines
	rcall update_lcd_dog      ; update the display

reset_buffer:
	ldi YH, high (dsp_buff_1) ; Load YH and YL as a pointer to 1st
    ldi YL, low (dsp_buff_1)  ; byte of dsp_buff_1 (Note - assuming 
                              ; (dsp_buff_1 for now).
	ldi r18, 48				  ; Load 48 into r20

check_flag:
	sbis PINA, 3		   ;check if the push button is pushed		
	rjmp check_flag

	in r21, PIND           ;store the PIND in r21
	andi r21, $0f
	rcall lookup
	st Y+, r21             ;send r21 to Y pointer
	rcall update_lcd_dog   ;update the display
	cbi PORTA, 2		   ;activate the flip-flop
	sbi PORTA, 2		   ;deactivate the flip-flop
	dec r18				   ;decrement r20
	brne check_flag		   ;check if r20 is equal to 0
	rjmp reset_buffer	   ;reset the buffer if

lookup:
	ldi ZH, high (table * 2)  ; set Z to point to start of table
	ldi ZL, low (table * 2)   ;
	ldi r19, $00              ; add offset to Z pointer 
	add ZL, r21               ;
	adc ZH, r19               ;
	lpm r21, Z                ;
	ret        

	;table of ASCII value from 0 - F
table: .db '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' 

.nolist
.include "lcd_dog_asm_driver_m324a.inc"
.list