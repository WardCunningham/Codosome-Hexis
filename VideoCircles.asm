;--------------------------------------------------------------------
;
;	VideoCircles v4  - Third try at a TV circles program ala Ward. This version
;		uses A/D to read values for parameters and is for the Teensy++.
;		It will flash a LED twice at start and draw circles using code
;		from Codosome.
;
;	The vertical interval runs entirely
;		on interrupts and has double rate for Equalizers and Broad Pulses.
;		Equalizers are sync width, not half that width. Uses 525 line frame.
;
;	This software is (c) 2009 Ward Cunningham and Jim Larson
;	Released under GPL v2 or later
;
;--------------------------------------------------------------------
;
;	VideoCircles - TV circles program From Codosome work by Ward Cunningham.
;		Flashes a LED twice at start, has full interlaced NTSC video.
;		Uses Port A0 and A1 for video output.
;
;	Register Usage:
;	Needed for TV support (Initialization and ISRs)
;		R9, R10, R20 - R24 (don't use)
;		R12-R15 - Video levels shouldn't be changed, but are output values.
;		R16, R17 (available outside ISRs, but clobbered),
;		R18 (available after initialization)
;		Port A0 and A1 (video)
;	Needed for Codosome VideoCircles App - Available for Application
;		R1-R6, R16-R18 (overlap is OK)
;		R13 & R15 video levels are output.
;
;	The vertical interval runs entirely
;		on interrupts and has double rate for Equalizers and Broad Pulses.
;		Equalizers are sync width, not half that width. but works fine.
;	Hardware details: Uses AT90USB646. This is Teensy++ with 16MHz Crystal.
;		PortB0 is connected to a 1K resistor;
;		PortB1 is connected to a 470 Ohm resistor (could be 300 to 500 Ohm).
;		The two resistors form an R2R DAC. Their outputs are connected to
;		signal side of video-input on TV. Connect the video-in ground to
;		the same ground as the Teensy++. The Teensy++ LED is used and blinks
;		(very briefly) at start.
;
;  8/19/09 -- First version for release. A/D is currently disabled.
;  8/22/09 -- Implements A/D converter. Reads one channel. Seems to be a
;	      stability issue with vertical interval? How many lines of
;	      the vertical interval can be used? Is a stabilizing
;	      interrupt needed like for horizontal?
;  8/24/09 -- Revised Vertical Interval handling to use same stabilizing
;	      technique as Horizontal Interval. Now want to add reading
;	      of more A/D channels to see how it works.
;  8/24/09 -- Now reading 4 A/D channels solidly. Should run a demo like
;	      we would like.
;
;------------------------------------------------------------------------
.include "usb646def.inc"


; Defines for A/D Conversion (maybe)

; Define Control Flags for Vertical Interval
; NOTE: bit 0 is the restart flag

	.equ VI_TIME		=0x01
	.equ VI_2X_RATE		=0x02
	.equ VI_BROAD_TIME	=0x03

; Output pins for video (two bits)

	.set vl		=0 
	.set vh		=1


;========================================================================
;	Register Allocations
;========================================================================

	.def px2	=r1	; polynomial coefficients
	.def py2	=r2
	.def px1	=r3
	.def py1	=r4
	.def px0	=r5
	.def py0	=r6

	.def VI_FLAGS	=r9
	.def drift	=r10

	.def vs		=r12 	; register constants for video generation
	.def vb		=r13
	.def vg		=r14
	.def vw		=r15

	.def a		=r16 	; temporary variables
	.def b		=r17
	.def c		=r18	; Used to load px & py registers - don't use in ISR!!

	.def line	=r20
	.def PWM_MODE	=r21
	.def PWM_MODE2	=r22	; These should be one register!
	.def adrsL	=r23	; address pointer
	.def adrsH	=r24



;-------------------------------------------------------------------------

.CSEG
.org 0x0000
	jmp	my_RESET

.org 0x0022
	jmp	Timer1CompA
	jmp	Timer1CompB
	nop
	nop
	jmp	Timer1Overflow


my_RESET:
				; Clock Divider = 1
	ldi	a,(1<<CLKPCE)
	ldi	b,0		; divide clock by 1
	sts	CLKPR,a		; enable write
	sts	CLKPR,b		; write new value

; Initialize A/D

	ldi	 a, (1<< ADLAR) | 0x40 ; select AVCC as ref
	sts	 ADMUX, a

; Enable A/D, select prescale of 64
; Leave REFS1 & 0 at 0 - selects Vcc as reference.

	ldi	a, (1<<ADEN) | 0x03 ; try faster clock -- Made things much more stable!
	sts	ADCSRA, a

; init constants for video levels

	ldi	a,0
	mov	vs,a
	ldi	a,(1<<vl)
	mov	vb,a
	ldi	a,(1<<vh)
	mov	vg,a
	ldi	a,(1<<vl)+(1<<vh)
	mov	vw,a
	out	ddrb,vw		; outputs for video

; Set up Counter/Timer 1
;
; At 16MHz, 63.6usec = 1018 clocks. 1018/2 = 509
; Run at 10MHz. Use 640(636) counts/line (divide by 2 for P&F correct).
; P&F correct PWM Mode w/ ICR1 as TOP.
; load times, setup mode, enable interrupts
; Try 256+80 Counts per line (closer to correct)


	ldi	a,253
	ldi	b,1		; 509 - 1*256 = 253
	sts	ICR1H,b		; 16 bit write
	sts	ICR1L,a

	clr	a		; P&F Correct PWM, ICR1 TOP
	sts	TCCR1A,a
	mov	PWM_MODE, a	; set up this register to track mode

	ldi	a, (1<<WGM13) | (1<<CS10)
	sts	TCCR1B,a
	mov	PWM_MODE2, a	; set up this register to track mode


	ldi	a, (1<<TOIE1)
	sts	TIMSK1,a

	ldi	a,48		; 16 bit write
	clr	b
	sts	OCR1AH,b
	sts	OCR1AL,a

	ldi	a,128		; 16 bit write
	clr	b
	sts	OCR1BH,b
	sts	OCR1BL,a

	ldi	ZH,high(TABL<<1)
	ldi	ZL,low(TABL<<1)	; load pointer into Z register
	ldi	line,1		; force start condition

	sei			; enable interrupts
wait4:	rjmp 	wait4		; wait for first interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer1 Compare Register A ISR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; output sync level, disable this interrupt, load next line
; length and pointer to routine to run.

Timer1CompA:

	out	portb,vs
	ldi	a, (1<<TOIE1)| (1<<OCIE1B)
	sts	TIMSK1,a

; This is setup for next line.

	dec	line
	brne	TCcont

	lpm	line,Z+		; load new line count
	lpm	VI_FLAGS,Z+	; get flags

	sbrs	VI_FLAGS,VI_TIME
	rjmp	TCA1
	lpm	a,Z+		; Read and ignore these	if in Vert Intrvl
	lpm	a,Z+
	rjmp	TCA2

TCA1:	lpm	adrsL,Z+	; Load new starting address	if not Vert Intrvl
	lpm	adrsH,Z+

TCA2:	sbrs	VI_FLAGS, 0	; the restart bit at end of table
	rjmp	TCcont
	ldi	ZH,high(TABL<<1)
	ldi	ZL,low(TABL<<1)

TCcont:	reti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer1 Overflow ISR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; output blank level (front porch), enable A & B compare interrupts.

Timer1Overflow:
	out	portb,vb

; must clear these flags since they are set on down count even
; though interrupt is blocked.

	ldi	a,(1<<OCF1A) | (1<<OCF1B)	; must set bits to clear!!
	out	TIFR1, a
	ldi	a, (1<<OCIE1A)| (1<<TOIE1)| (1<<OCIE1B)
	sts	TIMSK1,a
	sts	TCCR1A, PWM_MODE
	sts	TCCR1B, PWM_MODE2

; This ends active video for front porch.
; wait for sync interrupt

	pop	a		; clean up stack
	pop	b

; Go into Idle - save the return address if in Vert Intrvl

	sbrs	VI_FLAGS,VI_TIME
	rjmp	TOIdle
	mov	adrsL,b		; saves the address we were at.
	mov	adrsH,a
TOIdle:
	ldi	a,low(wait4)
	push	a
	ldi	a,high(wait4)
	push	a		; return to idle
TOEnd:
	reti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer1 Compare Register B ISR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; output blank level (back porch), disable this interrupt,
; jump to correct routine.

Timer1CompB:
       out	portb,vb

; disable this interrupt
       ldi	a, (1<<TOIE1)
       sts	TIMSK1,a

       sbrc	VI_FLAGS,VI_2X_RATE
       rjmp	CheckMode	; Do this IF in 2X moce

       sbrs	VI_FLAGS,VI_TIME
       rjmp	TCBvi1		; Do this IF not in VI

; Be sure that P&F Correct mode is selected.

       clr	a		; P&F Correct PWM, ICR1 TOP
       mov	PWM_MODE, a	; set up this register to track mode

       ldi	a, (1<<WGM13) | (1<<CS10)
       mov	PWM_MODE2, a

;Restore a return address and go to it

TCBvi1:
	pop	a		; clean up stack
	pop	a
	push	adrsL		; return to correct routine
	push	adrsH

TCBviEnd:
	reti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CheckMode - Routine used *only* if in 2X_RATE.
;;;	Called from OC1B ISR.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckMode:

; DO NOT disable interrupts
; Shift PWM mode

	ldi	a, (1<<WGM11) 	; Fast PWM
	mov	PWM_MODE, a
	ldi	a, (1<<WGM13) | (1<<WGM12) | (1<<CS10)	; Fast PWM
	mov	PWM_MODE2, a

; 	if EQ - load time

	sbrc	VI_FLAGS,VI_BROAD_TIME
	rjmp	CM_H1BR

	ldi	a,128		; 16 bit write
	clr	b
	sts	OCR1BH,b
	sts	OCR1BL,a
	rjmp	CM_End

CM_H1BR:			; else BR - time
	ldi	a,173		; 16 bit write
	ldi	b,0x01
	sts	OCR1BH,b
	sts	OCR1BL,a

CM_End:
	rjmp 	TCBvi1		; returns to correct address

;*******************************************************************
; Code to run during Vertical Interval goes here.
; This code will run continuously (without restart)
;  during Vertical Interval.

vIntrvl:
; Let interrupts happen - other setup could happen here.

vSetup:

.macro read_adc
	lds	c,ADMUX		; Read ADMUX
	andi	c,0xE0		; Clear channel number
	ori	c,@0		; New channel number
	sts	ADMUX,c		; Put back in ADMUX

	lds	c,ADCSRA	; set bit ADSC to start conversion.
	ori	c,(1<<ADSC)
	sts	ADCSRA,c

vS1:	lds	c,ADCSRA	; Hang until the ADSC bit clears
	sbrc	c, ADSC
	rjmp	vS1
	lds			c,ADCH	; Read the value
.endm


; Must set Vertical INterval Flag for ISRs.

	ldi	c,(1<<VI_TIME)
	or 	VI_FLAGS, c

	ldi	c,6
	sub	drift,c

; second order coefficients

	ldi	c,2
	mov	px2,c
	ldi	c,1
	mov	py2,c

; first order coefficients

	read_adc 3		; ward's outside hand
	add	c,c
	add	c,c
	mov 	px1,c

	read_adc 0		; jim's outside hand
	add	c,c
	mov 	py1,c


; zeroth order coefficients

	read_adc 1		; jim's inside hand
	read_adc 2		; ward's inside hand

	mov	px0,drift
	mov	py0,drift

Finish:
	rjmp Finish		; wait for Interrupt

;*********************************************************************
; Vertical Interval Code Ends Here.
;*********************************************************************

; horizontal line (non-blanked)
; This is the main program loop. This routine will be restarted after
; each horizontal sync that is not in the Vertical Interval.

horz:
	out	portb,vb	; provide a little blanking
	ldi	a,10
hL1:
	dec	a
	brne	hL1

;---------------------------------------------------------------------
; Code to run on each horizontal line goes here.
; This is from Codosome

.macro add_frac
	mov	c,@1
	asr	c
	asr	c
	add	@0,c
.endm

	add_frac py0,py1
	add	py1,py2

	mov	b,px1
	mov	a,px0
	add	a,py0

tl1:
	add 	b,px2
	add 	a,b

	brpl 	tl2
	out 	portb,r15
	nop
	rjmp 	tl1
tl2:
	out 	portb,r13
	rjmp 	tl1

;  Horizontal line code ends here.
;-------------------------------------------------------------------------
;*********************************************************************


;-----------------------------------------------------------
; TABL controls operation of the program. The ISR for
;	each hsync reads this data.
;	Format is line count,flag for sequence restart with
;	Vertical Interval Control, and Address to go to.
;	Note that Address is read at start of vertical
;	interval, then doesn't change until end of VI. Horizontal is read each
;	time, so could be changed on the fly.
;-------------------------------------------------------------

TABL:
	.DB	1,0
	.DW	vIntrvl		; One blank line to start Vert Intrvl

	.DB	6,(1<<VI_TIME) | (1<<VI_2X_RATE)
	.DW	vIntrvl		; This address is ignored - 3 Equalizers

	.DB	6,(1<<VI_TIME) | (1<<VI_2X_RATE) | (1<< VI_BROAD_TIME)
	.DW	vIntrvl		; This address is ignored - 3 Broad pulses

	.DB	6,(1<<VI_TIME) | (1<<VI_2X_RATE)
	.DW	vIntrvl		; This address is ignored - 3 Equalizers

	.DB	10,(1<<VI_TIME)
	.DW	vIntrvl		; This address is ignored - 10 blank lines minimum.

	.DB	241,0
	.DW	horz		; 241 normal horiz lines

	.DB	1,0
	.DW	vIntrvl		; One blank line to start Vert Intrvl

	.DB	7,(1<<VI_TIME) | (1<<VI_2X_RATE)
	.DW	vIntrvl		; This address is ignored - 3 1/2 Equalizers

	.DB	6,(1<<VI_TIME) | (1<<VI_2X_RATE) | (1<< VI_BROAD_TIME)
	.DW	vIntrvl		; This address is ignored - 3 Broads

	.DB	5,(1<<VI_TIME) | (1<<VI_2X_RATE)
	.DW	vIntrvl		; This address is ignored - 2 1/2 Equalizers

	.DB	11,(1<<VI_TIME)
	.DW	vIntrvl		; This address is ignored - 10 blank lines minimum.

	.DB	242,1		; Flag for restart of sequence.
	.DW	horz		; 242 normal horiz lines
