; division

.division_24_signed                     ;24bit division, note this is optimised for a 16 bit result
 LDX #&00	                            ;clear remainder
 STX division_remainder_24
 STX division_remainder_24 + &01
 LDA dividend_24 + &02                  ;result sign
 EOR divisor_24 + &01
 PHP
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
 LDX #&18
.division_24_loop
 ASL dividend_24	                    ;dividend lb & hb * 2, msb -> carry
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
 BNE division_24_loop
 PLP
 BPL divide_24_exit
 TXA                                    ;x=0
 SEC
 SBC dividend_24
 STA dividend_24
 TXA
 SBC dividend_24 + &01
 STA dividend_24 + &01
.divide_24_exit
 RTS

.division_32_signed                     ;32bit / 16bit division
 LDA dividend_32
 EOR divisor_32 + &01
 PHP                                    ;store sign result
 LDX #&00
 STX dividend_32 + &01
 STX dividend_32 + &02
 BIT dividend_32
 BPL division_32_check_divisor
 TXA
 SEC
 SBC dividend_32 + &03
 STA dividend_32 + &03
 TXA
 SBC dividend_32
 STA dividend_32
.division_32_check_divisor
 BIT divisor_32 + &01                   ;check for negative divisor
 BPL divisor_32_positive
 TXA
 SEC
 SBC divisor_32
 STA divisor_32
 TXA
 SBC divisor_32 + &01
 STA divisor_32 + &01
.divisor_32_positive
 LDX #&10
 CLC
.divisor_32_loop
 ROL dividend_32 + &02
 ROL dividend_32 + &03
 ROL dividend_32
 ROL dividend_32 + &01
 LDA #&00
 STA division_carry_32
 ROL division_carry_32
 LDA dividend_32
 SEC
 SBC divisor_32
 STA division_scratch_32
 LDA dividend_32 + &01
 SBC divisor_32 + &01
 TAY
 LDA division_carry_32
 SBC #&00
 BCC division_32_next
 LDA division_scratch_32
 STA dividend_32
 STY dividend_32 + &01
.division_32_next
 DEX
 BNE divisor_32_loop
 ROL dividend_32 + &02
 ROL dividend_32 + &03
 PLP
 BPL division_32_signed_leave
 LDA #&00
 SEC
 SBC division_quotient_32
 STA division_quotient_32
 LDA #&00
 SBC division_quotient_32 + &01
 STA division_quotient_32 + &01
.division_32_signed_leave
 RTS
