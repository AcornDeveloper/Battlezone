; multiplication
;
; square table must be aligned on a page boundary

.square1_lo_16
 FOR number, 0, 511                               ;low  ( sqr(x)=x^2/4 )
   EQUB LO(number * number DIV 4)
 NEXT
.square1_hi_16
 FOR number, 0, 511                               ;high ( sqr(x)=x^2/4 )
   EQUB HI(number * number DIV 4)
 NEXT
.square2_lo_16
 FOR number, 0, 511
   EQUB LO((255 - number) * (255 - number) DIV 4) ;low  ( negsqr(x)=(255-x)^2/4 )
 NEXT
.square2_hi_16
 FOR number, 0, 511
   EQUB HI((255 - number) * (255 - number) DIV 4) ;high ( negsqr(x)=(255-x)^2/4 )
 NEXT

.eight_bit_unsigned                     ;y = multiplicand
 LDA clock_move_counter_reload
 STA square1_lo
 STA square1_hi
 EOR #&FF
 STA square2_lo
 STA square2_hi
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA move_counter                       ;frame rate adjusted move counter
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 BEQ not_clip_result                    ;clip result to a byte, for electron mainly
 LDA #&FF
 STA move_counter
.not_clip_result
 RTS

.multiply_16_signed
 LDA multiplicand_16 + &01
 EOR multiplier_16 + &01
 PHP
 BIT multiplicand_16 + &01
 BPL multiplicand_16_positive_b
 LDA #&00
 SEC
 SBC multiplicand_16
 STA multiplicand_16
 LDA #&00
 SBC multiplicand_16 + &01
 STA multiplicand_16 + &01
.multiplicand_16_positive_b
 BIT multiplier_16 + &01
 BPL multiplier_16_positive_b
 LDA #&00
 SEC
 SBC multiplier_16
 STA multiplier_16
 LDA #&00
 SBC multiplier_16 + &01
 STA multiplier_16 + &01
.multiplier_16_positive_b
 LDA multiplicand_16                    ;compute (x0 * y0) + (x0 * y1) + (x1 * y0) + (x1 *y1)
 STA square1_lo
 STA square1_hi
 EOR #&FF
 STA square2_lo
 STA square2_hi
 LDY multiplier_16
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA product_16                         ;product_16          = low  (x0 * y0) z0
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 STA product_16 + &01                   ;product_16 + &01    = high (x0 * y0) c1a
 LDY multiplier_16 + &01
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA product_16_t1                      ;product_16_t1       = low  (x0 * y1) c1b
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 STA product_16_t1 + &01                ;product_16_t1 + &01 = high (x0 * y1) c2a
 LDA multiplicand_16 + &01
 STA square1_lo
 STA square1_hi
 EOR #&FF
 STA square2_lo
 STA square2_hi
 LDY multiplier_16
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA product_16_t2                      ;product_16_t2       = low  (x1 * y0) c1c
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 STA product_16_t2 + &01                ;product_16_t2 + &01 = high (x1 * y1) c2b
 LDY multiplier_16 + &01
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA product_16 + &03                   ;product_16 + &03    = low  (x1 * y1) c2c
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 TAY                                    ;y                   = high (x1 * y1)
 LDA product_16 + &01
 CLC
 ADC product_16_t1
 STA product_16 + &01
 LDA product_16_t1 + &01
 ADC product_16_t2 + &01
 TAX
 BCC no_inc_hi_y_00
 CLC
 INY
.no_inc_hi_y_00
 LDA product_16_t2
 ADC product_16 + &01
 STA product_16 + &01
 TXA
 ADC product_16 + &03
 BCC no_inc_hi_y_01
 INY
.no_inc_hi_y_01
 STA product_16 + &02
 STY product_16 + &03
 PLP
 BPL multiply_16_exit_b
 LDA #&00
 SEC
 SBC product_16
 STA product_16
 LDA #&00
 SBC product_16 + &01
 STA product_16 + &01
 LDA #&00
 SBC product_16 + &02
 STA product_16 + &02
 LDA #&00
 SBC product_16 + &03
 STA product_16 + &03
.multiply_16_exit_b
 RTS
