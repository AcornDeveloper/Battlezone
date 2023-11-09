; horizon
; draw horizon line
; draw tank sights
; draw cracked screen
;
; constants
 crack_scale                            = 0.24
 horizon_offset                         = 15 * screen_row + &06
 screen_bank_00                         = &3000
 screen_bank_01                         = &5800
 x_adjust                               = -70
 y_adjust                               = -3

 a01x = INT(0828 * crack_scale + x_adjust) : a01y = INT(0478 * crack_scale + y_adjust)
 a02x = INT(0694 * crack_scale + x_adjust) : a02y = INT(0344 * crack_scale + y_adjust)
 a03x = INT(0730 * crack_scale + x_adjust) : a03y = INT(0476 * crack_scale + y_adjust)
 a04x = INT(0776 * crack_scale + x_adjust) : a04y = INT(0604 * crack_scale + y_adjust)
 a05x = INT(0954 * crack_scale + x_adjust) : a05y = INT(0540 * crack_scale + y_adjust)
 a06x = INT(0992 * crack_scale + x_adjust) : a06y = INT(0310 * crack_scale + y_adjust)
 a07x = INT(0774 * crack_scale + x_adjust) : a07y = INT(0264 * crack_scale + y_adjust)
 a08x = INT(0500 * crack_scale + x_adjust) : a08y = INT(0284 * crack_scale + y_adjust)
 a09x = INT(0730 * crack_scale + x_adjust) : a09y = INT(0534 * crack_scale + y_adjust)
 a10x = INT(0858 * crack_scale + x_adjust) : a10y = INT(0680 * crack_scale + y_adjust)
 a11x = INT(1062 * crack_scale + x_adjust) : a11y = INT(0544 * crack_scale + y_adjust)
 a12x = INT(0996 * crack_scale + x_adjust) : a12y = INT(0264 * crack_scale + y_adjust)
 a13x = INT(1068 * crack_scale + x_adjust) : a13y = INT(0264 * crack_scale + y_adjust)
 a14x = INT(0430 * crack_scale + x_adjust) : a14y = INT(0270 * crack_scale + y_adjust)
 a15x = INT(0342 * crack_scale + x_adjust) : a15y = INT(0444 * crack_scale + y_adjust)
 a16x = INT(0610 * crack_scale + x_adjust) : a16y = INT(0540 * crack_scale + y_adjust)
 a17x = INT(0918 * crack_scale + x_adjust) : a17y = INT(0854 * crack_scale + y_adjust)
 a18x = INT(1062 * crack_scale + x_adjust) : a18y = INT(0590 * crack_scale + y_adjust)
 a19x = INT(0500 * crack_scale + x_adjust) : a19y = INT(0508 * crack_scale + y_adjust)
 a20x = INT(0610 * crack_scale + x_adjust) : a20y = INT(0604 * crack_scale + y_adjust)
 a21x = INT(0814 * crack_scale + x_adjust) : a21y = INT(1056 * crack_scale + y_adjust)
 a22x = INT(1200 * crack_scale + x_adjust) : a22y = INT(0590 * crack_scale + y_adjust)
 a23x = INT(0512 * crack_scale + x_adjust) : a23y = INT(0700 * crack_scale + y_adjust)
 a24x = INT(0564 * crack_scale + x_adjust) : a24y = INT(0646 * crack_scale + y_adjust)
 a25x = INT(0650 * crack_scale + x_adjust) : a25y = INT(0736 * crack_scale + y_adjust)
 a26x = INT(1188 * crack_scale + x_adjust) : a26y = INT(0674 * crack_scale + y_adjust)
 a27x = INT(1234 * crack_scale + x_adjust) : a27y = INT(0672 * crack_scale + y_adjust)
 a28x = INT(1124 * crack_scale + x_adjust) : a28y = INT(0738 * crack_scale + y_adjust)
 a29x = INT(1330 * crack_scale + x_adjust) : a29y = INT(0462 * crack_scale + y_adjust)
 a30x = INT(1472 * crack_scale + x_adjust) : a30y = INT(0600 * crack_scale + y_adjust)
 a31x = INT(0792 * crack_scale + x_adjust) : a31y = INT(1075 * crack_scale + y_adjust)
 a32x = INT(0836 * crack_scale + x_adjust) : a32y = INT(1075 * crack_scale + y_adjust)
 a33x = INT(1498 * crack_scale + x_adjust) : a33y = INT(0658 * crack_scale + y_adjust)
 a34x = INT(1616 * crack_scale + x_adjust) : a34y = INT(0512 * crack_scale + y_adjust)
 a35x = INT(0412 * crack_scale + x_adjust) : a35y = INT(0718 * crack_scale + y_adjust)
 a36x = INT(0306 * crack_scale + x_adjust) : a36y = INT(0644 * crack_scale + y_adjust)
 a37x = INT(0366 * crack_scale + x_adjust) : a37y = INT(0786 * crack_scale + y_adjust)
 a38x = INT(1602 * crack_scale + x_adjust) : a38y = INT(0620 * crack_scale + y_adjust)
 a39x = INT(1522 * crack_scale + x_adjust) : a39y = INT(0736 * crack_scale + y_adjust)

 sight_offset_top                       = screen_row * &08 + &98
 sight_offset_bot                       = sight_offset_top + screen_row * &0F
 sight_offset_h01                       = sight_offset_top + screen_row * &04 - &10 + &07
 sight_offset_h02                       = sight_offset_bot - screen_row * &04 - &10
 sight_offset_p01                       = sight_offset_h01 + &139
 sight_offset_p02                       = sight_offset_h02 - screen_row + &01
 sight_offset_p03                       = sight_offset_p01 + &28
 sight_offset_p04                       = sight_offset_p02 + &28

; add constant to zero page
MACRO add_to_zero_page address, value
 LDA address
 CLC
 ADC #LO(value)
 STA address
 LDA address + &01
 ADC #HI(value)
 STA address + &01
ENDMACRO

; subtract constant from zero page
MACRO subtract_from_zero_page address, value
 LDA address
 SEC
 SBC #LO(value)
 STA address
 LDA address + &01
 SBC #HI(value)
 STA address + &01
ENDMACRO

MACRO full_line screen_address
FOR line, 0, 39
  STA screen_address + horizon_offset + line * &08,X
NEXT
ENDMACRO

.horizon
 LDA #&AA
 LDX b_object_bounce_far
 LDY screen_hidden
 CPY #&30
 BEQ horizon_one
 full_line screen_bank_01
 RTS

.horizon_one
 full_line screen_bank_00
.no_sights_on
 RTS

.tank_sights                            ;render the correct shape for the tank sights
 BIT m_tank_status                      ;player alive?
 BMI no_sights_on                       ;do not render if dead
 BIT m_shell                            ;if shell in flight then test if flashing
 BMI sights_draw_vertical
 LDA console_press_start_etc
 AND #&02
 BEQ no_sights_on

.sights_draw_vertical

 initialise_hidden sight_address_01, sight_offset_top + 320
 initialise_hidden sight_address_02, sight_offset_bot - 320

 LDX #&04
.sights_draw
 LDY #&07
.sight_cell
 LDA #&01
 STA (sight_address_02),Y               ;or byte on as moon could be underneath
 ORA (sight_address_01),Y
 STA (sight_address_01),Y
 DEY
 BPL sight_cell

 add_to_zero_page sight_address_01, screen_row
 subtract_from_zero_page sight_address_02, screen_row

 DEX
 BNE sights_draw

.sights_horizontal
 LDA screen_hidden
 ADC #HI(sight_offset_h01 - &100)       ;relies on c=0 from previous subroutine
 STA sights_level_00 + &02
 LDA screen_hidden
 ADC #HI(sight_offset_h02)
 STA sights_level_01 + &02
 LDY #&28
 SEC
.sights_level
 LDA #&FF
.sights_level_00
 STA sight_offset_h01,Y
.sights_level_01
 STA sight_offset_h02,Y
 TYA
 SBC #&08
 TAY
 BPL sights_level
 BIT on_target                          ;if opponent in target then switch sights
 BMI dotted_line
 JMP sights_pins                        ;not on target so pins
.dotted_line                            ;dotted line straight after vertical as relies on page zero

 initialise_hidden sight_address_01, sight_offset_top + 5 * screen_row - &01
 initialise_hidden sight_address_02, sight_offset_bot - 5 * screen_row - &01

 LDY #&08                               ;place vertical dotted line top/bottom
 LDA #&01
.small_dotted_line
 STA (sight_address_01),Y
 DEY
 STA (sight_address_02),Y
 DEY
 BNE small_dotted_line

 add_to_zero_page sight_address_01, screen_row + &02
 subtract_from_zero_page sight_address_02, screen_row - &05

 LDA #&01                               ;y=0
 STA (sight_address_01),Y
 STA (sight_address_02),Y
 LDY #&02
 STA (sight_address_01),Y
 STA (sight_address_02),Y
.sights_diagonal                        ;sights on target

 initialise_hidden sight_address_01, sight_offset_p01
 initialise_hidden sight_address_02, sight_offset_p04 - &01

 LDA #&01
 LDY #&07
.sights_rotate_01
 STA (sight_address_01),Y
 STA (sight_address_02),Y
 ASL A
 DEY
 BPL sights_rotate_01

 add_to_zero_page sight_address_01, &148
 subtract_from_zero_page sight_address_02, &148

 LDA #&80
 LDX #&04
.sights_lsr_00
 INY
 STA (sight_address_01),Y
 LSR A
 DEX
 BNE sights_lsr_00
 LDX #&04
.sights_lsr_01
 INY
 STA (sight_address_02),Y
 LSR A
 DEX
 BNE sights_lsr_01

 initialise_hidden sight_address_01, sight_offset_p03
 initialise_hidden sight_address_02, sight_offset_p02 - &01

 LDA #&80
 LDY #&07
.sights_rotate_02
 STA (sight_address_01),Y
 STA (sight_address_02),Y
 LSR A
 DEY
 BPL sights_rotate_02
 add_to_zero_page sight_address_01, &138
 subtract_from_zero_page sight_address_02, &138
 LDA #&01
 LDX #&04
.sights_asl_00
 INY
 STA (sight_address_01),Y
 ASL A
 DEX
 BNE sights_asl_00
 LDX #&04
.sights_asl_01
 INY
 STA (sight_address_02),Y
 ASL A
 DEX
 BNE sights_asl_01
 RTS

.sights_pins
 LDX screen_hidden
 TXA
 CLC
 ADC #HI(sight_offset_p01)
 STA sight_01 + &02
 TXA
 ADC #HI(sight_offset_p02)
 STA sight_02 + &02
 TXA
 ADC #HI(sight_offset_p03)
 STA sight_03 + &02
 TXA
 ADC #HI(sight_offset_p04)
 STA sight_04 + &02
 LDY #&06
.tank_pins
 LDA #&80
.sight_01
 STA sight_offset_p01,Y
.sight_02
 STA sight_offset_p02,Y
 LDA #&01
.sight_03
 STA sight_offset_p03,Y
.sight_04
 STA sight_offset_p04,Y
 DEY
 BPL tank_pins
 RTS

.crack_screen_open                      ;animation for cracking screen
 LDX crack_counter                      ;get crack stage at
 CPX #&07
 BCC crack_cycle
 LDX #&06
.crack_cycle
 LDY crack_stage,X                      ;get number of cracks
.all_cracks_at_this_part
 LDX crack_start,Y                      ;transfer coordinates to zero page
 LDA crack_x_lsb - &01,X
 STA graphic_x_00
 LDA crack_x_msb - &01,X
 STA graphic_x_00 + &01
 LDA crack_y - &01,X
 STA graphic_y_00
 LDX crack_end,Y
 LDA crack_x_lsb - &01,X
 STA graphic_x_01
 LDA crack_x_msb - &01,X
 STA graphic_x_01 + &01
 LDA crack_y - &01,X
 STA graphic_y_01
 LDA #&00
 STA graphic_y_00 + &01
 STA graphic_y_01 + &01
 TYA
 PHA
 LDA crack_clip - &01,X                 ;test clip codes for start/end
 LDX crack_start,Y
 ORA crack_clip - &01,X
 BNE crack_16_00
 JSR mathbox_line_draw08
 JMP crack_16_01
.crack_16_00
 JSR mathbox_line_draw16
.crack_16_01
 PLA
 TAY
 DEY
 BPL all_cracks_at_this_part
 INC crack_counter                      ;increase segments displayed
 RTS

.crack_stage                            ;number of line segments to draw
 EQUB crack_b_start - crack_a_start - &01
 EQUB crack_c_start - crack_a_start - &01
 EQUB crack_d_start - crack_a_start - &01
 EQUB crack_e_start - crack_a_start - &01
 EQUB crack_f_start - crack_a_start - &01
 EQUB crack_g_start - crack_a_start - &01
 EQUB crack_end     - crack_a_start - &01

.crack_start                            ;crack line segments

.crack_a_start
 EQUB &01                               ;01:02
 EQUB &01                               ;01:03
 EQUB &01                               ;01:04
 EQUB &01                               ;01:05
 EQUB &01                               ;01:06

.crack_b_start
 EQUB &02                               ;02:07
 EQUB &02                               ;02:08
 EQUB &06                               ;06:12
 EQUB &06                               ;06:13
 EQUB &04                               ;04:10
 EQUB &03                               ;03:09

.crack_c_start
 EQUB &09                               ;09:16
 EQUB &08                               ;08:15
 EQUB &08                               ;08:14
 EQUB &0A                               ;10:17
 EQUB &0B                               ;11:18

.crack_d_start
 EQUB &10                               ;16:19
 EQUB &10                               ;16:20
 EQUB &11                               ;17:21
 EQUB &12                               ;18:22

.crack_e_start
 EQUB &14                               ;20:23
 EQUB &18                               ;24:25
 EQUB &15                               ;21:31
 EQUB &15                               ;21:32
 EQUB &16                               ;22:26
 EQUB &16                               ;22:27
 EQUB &1A                               ;26:28
 EQUB &16                               ;22:29
 EQUB &1D                               ;29:30

.crack_f_start
 EQUB &17                               ;23:35
 EQUB &1E                               ;30:34
 EQUB &1E                               ;30:33

.crack_g_start
 EQUB &23                               ;35:36
 EQUB &23                               ;35:37
 EQUB &1E                               ;30:34
 EQUB &21                               ;33:38
 EQUB &21                               ;33:39

.crack_end
 EQUB &02                               ;01:02
 EQUB &03                               ;01:03
 EQUB &04                               ;01:04
 EQUB &05                               ;01:05
 EQUB &06                               ;01:06
 EQUB &07                               ;02:07
 EQUB &08                               ;02:08
 EQUB &0C                               ;06:12
 EQUB &0D                               ;06:13
 EQUB &0A                               ;04:10
 EQUB &09                               ;03:09
 EQUB &10                               ;09:16
 EQUB &0F                               ;08:15
 EQUB &0E                               ;08:14
 EQUB &11                               ;10:17
 EQUB &12                               ;11:18
 EQUB &13                               ;16:19
 EQUB &14                               ;16:20
 EQUB &15                               ;17:21
 EQUB &16                               ;18:22
 EQUB &17                               ;20:23
 EQUB &19                               ;24:25
 EQUB &1F                               ;21:31
 EQUB &20                               ;21:32
 EQUB &1A                               ;22:26
 EQUB &1B                               ;22:27
 EQUB &1C                               ;26:28
 EQUB &1D                               ;22:29
 EQUB &1E                               ;29:30
 EQUB &23                               ;23:35
 EQUB &22                               ;30:34
 EQUB &21                               ;30:33
 EQUB &24                               ;35:36
 EQUB &25                               ;35:37
 EQUB &22                               ;30:34
 EQUB &26                               ;33:38
 EQUB &27                               ;33:39

.crack_x_lsb
 EQUB LO(a01x)
 EQUB LO(a02x)
 EQUB LO(a03x)
 EQUB LO(a04x)
 EQUB LO(a05x)
 EQUB LO(a06x)
 EQUB LO(a07x)
 EQUB LO(a08x)
 EQUB LO(a09x)
 EQUB LO(a10x)
 EQUB LO(a11x)
 EQUB LO(a12x)
 EQUB LO(a13x)
 EQUB LO(a14x)
 EQUB LO(a15x)
 EQUB LO(a16x)
 EQUB LO(a17x)
 EQUB LO(a18x)
 EQUB LO(a19x)
 EQUB LO(a20x)
 EQUB LO(a21x)
 EQUB LO(a22x)
 EQUB LO(a23x)
 EQUB LO(a24x)
 EQUB LO(a25x)
 EQUB LO(a26x)
 EQUB LO(a27x)
 EQUB LO(a28x)
 EQUB LO(a29x)
 EQUB LO(a30x)
 EQUB LO(a31x)
 EQUB LO(a32x)
 EQUB LO(a33x)
 EQUB LO(a34x)
 EQUB LO(a35x)
 EQUB LO(a36x)
 EQUB LO(a37x)
 EQUB LO(a38x)
 EQUB LO(a39x)

.crack_x_msb
 EQUB HI(a01x)
 EQUB HI(a02x)
 EQUB HI(a03x)
 EQUB HI(a04x)
 EQUB HI(a05x)
 EQUB HI(a06x)
 EQUB HI(a07x)
 EQUB HI(a08x)
 EQUB HI(a09x)
 EQUB HI(a10x)
 EQUB HI(a11x)
 EQUB HI(a12x)
 EQUB HI(a13x)
 EQUB HI(a14x)
 EQUB HI(a15x)
 EQUB HI(a16x)
 EQUB HI(a17x)
 EQUB HI(a18x)
 EQUB HI(a19x)
 EQUB HI(a20x)
 EQUB HI(a21x)
 EQUB HI(a22x)
 EQUB HI(a23x)
 EQUB HI(a24x)
 EQUB HI(a25x)
 EQUB HI(a26x)
 EQUB HI(a27x)
 EQUB HI(a28x)
 EQUB HI(a29x)
 EQUB HI(a30x)
 EQUB HI(a31x)
 EQUB HI(a32x)
 EQUB HI(a33x)
 EQUB HI(a34x)
 EQUB HI(a35x)
 EQUB HI(a36x)
 EQUB HI(a37x)
 EQUB HI(a38x)
 EQUB HI(a39x)

.crack_y
 EQUB a01y
 EQUB a02y
 EQUB a03y
 EQUB a04y
 EQUB a05y
 EQUB a06y
 EQUB a07y
 EQUB a08y
 EQUB a09y
 EQUB a10y
 EQUB a11y
 EQUB a12y
 EQUB a13y
 EQUB a14y
 EQUB a15y
 EQUB a16y
 EQUB a17y
 EQUB a18y
 EQUB a19y
 EQUB a20y
 EQUB a21y
 EQUB a22y
 EQUB a23y
 EQUB a24y
 EQUB a25y
 EQUB a26y
 EQUB a27y
 EQUB a28y
 EQUB a29y
 EQUB a30y
 EQUB a31y
 EQUB a32y
 EQUB a33y
 EQUB a34y
 EQUB a35y
 EQUB a36y
 EQUB a37y
 EQUB a38y
 EQUB a39y

MACRO crack_clip_code x
 IF (x > 31) AND (x < (32 + 256))
   EQUB &00
 ELSE
   EQUB &FF
 ENDIF
ENDMACRO

.crack_clip
 crack_clip_code a01x
 crack_clip_code a02x
 crack_clip_code a03x
 crack_clip_code a04x
 crack_clip_code a05x
 crack_clip_code a06x
 crack_clip_code a07x
 crack_clip_code a08x
 crack_clip_code a09x
 crack_clip_code a10x
 crack_clip_code a11x
 crack_clip_code a12x
 crack_clip_code a13x
 crack_clip_code a14x
 crack_clip_code a15x
 crack_clip_code a16x
 crack_clip_code a17x
 crack_clip_code a18x
 crack_clip_code a19x
 crack_clip_code a20x
 crack_clip_code a21x
 crack_clip_code a22x
 crack_clip_code a23x
 crack_clip_code a24x
 crack_clip_code a25x
 crack_clip_code a26x
 crack_clip_code a27x
 crack_clip_code a28x
 crack_clip_code a29x
 crack_clip_code a30x
 crack_clip_code a31x
 crack_clip_code a32x
 crack_clip_code a33x
 crack_clip_code a34x
 crack_clip_code a35x
 crack_clip_code a36x
 crack_clip_code a37x
 crack_clip_code a38x
 crack_clip_code a39x
