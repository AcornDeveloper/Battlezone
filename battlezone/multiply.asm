; multiplication/division/square roots/distance
;
; square table must be aligned on a page boundary
;

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

.multiply_08_signed
 LDA multiplicand_08                    ;multiplier/multiplicand/x preserved over call
 STA square1_lo
 STA square1_hi
 EOR #&FF
 STA square2_lo
 STA square2_hi
 LDY multiplier_08
 LDA (square1_lo),Y
 SEC
 SBC (square2_lo),Y
 STA product_08
 LDA (square1_hi),Y
 SBC (square2_hi),Y
 BIT multiplier_08
 BPL positive_multiplier
 SEC
 SBC square1_lo
.positive_multiplier
 BIT square1_lo
 BPL multiply_08_exit
 SEC
 SBC multiplier_08
.multiply_08_exit                       ;a = product_08 + &01
 STA product_08 + &01
 RTS

.multiply_16_signed_6502
 LDA multiplicand_16 + &01
 EOR multiplier_16 + &01
 STA result_sign_16
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
 BIT result_sign_16
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

.division_16_signed_6502
 LDX #&00                               ;clear remainder
 STX division_remainder_16
 STX division_remainder_16 + &01
 LDA dividend_16 + &01                  ;result sign
 EOR divisor_16 + &01
 STA result_sign_16
 BIT dividend_16 + &01
 BPL dividend_16_positive
 TXA                                    ;x = 0
 SEC
 SBC dividend_16
 STA dividend_16
 TXA
 SBC dividend_16 + &01
 STA dividend_16 + &01
.dividend_16_positive
 BIT divisor_16 + &01
 BPL divisor_16_positive
 TXA
 SEC
 SBC divisor_16
 STA divisor_16
 TXA
 SBC divisor_16 + &01
 STA divisor_16 + &01
.divisor_16_positive
 LDX #&10
.divide_16_loop
 ASL dividend_16                        ;dividend lb & hb * 2, msb -> carry
 ROL dividend_16 + &01
 ROL division_remainder_16              ;remainder lb & hb * 2 + msb from carry
 ROL division_remainder_16 + &01
 LDA division_remainder_16
 SEC
 SBC divisor_16                         ;subtract remainder to see if it fits in
 TAY                                    ;lb divide result -> y, for we may need it later
 LDA division_remainder_16 + &01
 SBC divisor_16 + &01
 BCC divide_16_bypass                   ;if carry = 0 then divisor didn't fit in yet
 STA division_remainder_16 + &01        ;else save subtraction divide_result as new remainder
 STY division_remainder_16
 INC division_result_16                 ;increment result
.divide_16_bypass
 DEX
 BNE divide_16_loop
 BIT result_sign_16
 BPL divide_16_exit
 TXA                                    ;x = 0
 SEC
 SBC division_result_16
 STA division_result_16
 TXA
 SBC division_result_16 + &01
 STA division_result_16 + &01
.divide_16_exit
 RTS

.square_root_16_6502
 LDY #&00                               ;r = 0
 LDX #&07
 CLC                                    ;clear bit 16 of m
.square_root_16_00
 TYA
 ORA root_square_table - &01,X
 STA squared_work_16                    ;(r asl 8) | (d asl 7)
 LDA squared_16 + &01
 BCS square_root_16_01                  ;m >= 65536 then t <= m always true
 CMP squared_work_16
 BCC square_root_16_02                  ;t <= m
.square_root_16_01
 SBC squared_work_16
 STA squared_16 + &01                   ;m = m - t
 TYA
 ORA root_square_table,X
 TAY                                    ;r = r or d
.square_root_16_02
 ASL squared_16
 ROL squared_16 + &01                   ;m = m asl 1
 DEX
 BNE square_root_16_00
 BCS square_root_16_03
 STY squared_work_16
 LDA squared_16
 CMP #&80
 LDA squared_16 + &01
 SBC squared_work_16
 BCC square_root_16_04
.square_root_16_03
 INY                                    ;r = r or d (d is 1 here)
.square_root_16_04
 RTS

.root_square_table
 EQUD &08040201
 EQUD &80402010

.division_24_signed_6502                ;24bit division, note this is optimised for a 16 bit result
 LDX #&00	                            ;clear remainder
 STX division_remainder_24
 STX division_remainder_24 + &01
 LDA dividend_24 + &02                  ;result sign
 EOR divisor_24 + &01
 STA division_result_sign_24
 BIT dividend_24 + &02
 BPL dividend_24_positive
 TXA                                    ;x = 0
 SEC
 SBC dividend_24
 STA dividend_24
 TXA
 SBC dividend_24 + &01
 STA dividend_24 + &01
 TXA
 SBC dividend_24 + &02
 STA dividend_24 + &02
.dividend_24_positive
 BIT divisor_24 + &01
 BPL divisor_24_positive
 TXA                                    ;x = 0
 SEC
 SBC divisor_24
 STA divisor_24
 TXA
 SBC divisor_24 + &01
 STA divisor_24 + &01
.divisor_24_positive
 LDX #&18 	                            ;repeat for each bit
.divide_24_loop
 ASL dividend_24	                    ;dividend lb & hb * 2, msb -> Carry
 ROL dividend_24 + &01
 ROL dividend_24 + &02
 ROL division_remainder_24	            ;remainder lb & hb * 2 + msb from carry
 ROL division_remainder_24 + &01
 LDA division_remainder_24
 SEC
 SBC divisor_24	                        ;subtract divisor to see if it fits in
 TAY
 LDA division_remainder_24 + &01
 SBC divisor_24 + &01
 BCC division_24_bypass
 STA division_remainder_24 + &01	    ;save substraction result as new remainder
 STY division_remainder_24
 INC dividend_24 	                    ;increment result as divisor fits in once
.division_24_bypass
 DEX
 BNE divide_24_loop
 BIT division_result_sign_24
 BPL divide_24_exit
 TXA                                    ;x = 0
 SEC
 SBC dividend_24
 STA dividend_24
 TXA
 SBC dividend_24 + &01
 STA dividend_24 + &01
.divide_24_exit
 RTS

.division_32_signed_6502                ;32bit division, note this is optimised for a 16 bit result
 LDA division_n_32 + &03
 STA division_result_sign_32            ;divisor is always positive
 BPL division_n_32_positive             ;dividend high to low n + &03, &02, &05, &02
 LDA #&00
 SEC
 SBC division_n_32 + &04
 STA division_n_32 + &04
 LDA #&00
 SBC division_n_32 + &05
 STA division_n_32 + &05
 LDA #&00
 SBC division_n_32 + &02
 STA division_n_32 + &02
 LDA #&00
 SBC division_n_32 + &03
 STA division_n_32 + &03
.division_n_32_positive
 CLC
 LDX #&11
.division_32_loop
 ROL division_n_32 + &04
 ROL division_n_32 + &05
 DEX
 BEQ division_32_loop_exit              ;exit loop
 ROL division_n_32 + &02                ;shift high cell of dividend left one bit
 ROL division_n_32 + &03                ;shifting next bit in from high bit of low cell
 LDA #&00
 ROL A                                  ;store old high bit of dividend in carry
 STA division_carry_32                  ;zero old bits of carry so subtraction works right
 LDA division_n_32 + &02
 SEC
 SBC division_n_32                      ;do low byte
 STA division_n_32 + &06                ;save difference low byte until we know if we need it
 LDA division_n_32 + &03
 SBC division_n_32 + &01                ;do high byte
 TAY                                    ;save difference high byte until we know if we need it
 LDA division_carry_32                  ;bit 0 of carry serves as 17th bit
 SBC #&00                               ;complete the subtraction by doing the 17th bit before
 BCC division_32_loop                   ;determining if the divisor fits into the high 17 bits
 LDA division_n_32 + &06                ;if divisor fit into dividend high 17 bits, update
 STA division_n_32 + &02                ;dividend high cell to what it would be after subtraction
 STY division_n_32 + &03
 BCS division_32_loop                   ;always
.division_32_loop_exit
 BIT division_result_sign_32
 BPL division_32_exit
 TXA                                    ;x = 0
 SEC
 SBC division_result_32
 STA division_result_32
 TXA
 SBC division_result_32 + &01
 STA division_result_32 + &01
.division_32_exit
 RTS

.distance_16_6502                       ;distance dx/dz
 LDX distance_dx + &01                  ;convert dx/dz to absolute numbers
 BPL distance_dx_positive               ;approx. distance = 0.41 * dx + 0.941246 * dz, ~2.5% accuracy
 LDA distance_dx
 CLC
 EOR #&FF
 ADC #&01
 STA distance_dx
 TXA
 EOR #&FF
 ADC #&00
 STA distance_dx + &01
.distance_dx_positive
 LDY distance_dz + &01
 BPL distance_dz_positive
 LDA distance_dz
 CLC
 EOR #&FF
 ADC #&01
 STA distance_dz
 TYA
 EOR #&FF
 ADC #&00
 STA distance_dz + &01
.distance_dz_positive
 LDY distance_dz                        ;if dz < dx swap over
 CPY distance_dx                        ;battlezone distance = 0.375 * dx + 1.0 * dz
 LDA distance_dz + &01                  ;0.375 = 3/8 approx. ~5% accuracy
 SBC distance_dx + &01
 BCS z_greater_than_x
 LDA distance_dx                        ;swap x/z distances
 STA distance_dz
 STY distance_dx
 LDA distance_dx + &01
 LDY distance_dz + &01
 STA distance_dz + &01
 STY distance_dx + &01
.z_greater_than_x
 LDX distance_dx                        ;x/y distance x
 LDY distance_dx + &01
 ASL distance_dx                        ;dx * 2
 ROL distance_dx + &01
 TXA
 CLC
 ADC distance_dx
 STA distance_dx
 TYA
 ADC distance_dx + &01
 LSR A                                  ;dx * 3,  now / 8
 ROR distance_dx
 LSR A
 ROR distance_dx
 LSR A
 ROR distance_dx
 STA distance_dx + &01
 LDA distance_dz                        ;+dz
 CLC
 ADC distance_dx
 STA d_object_distance
 LDA distance_dz + &01
 ADC distance_dx + &01
 STA d_object_distance + &01
 RTS
