; horizon
; draw horizon line
; draw tank sights
; draw cracked screen
;
; constants
 crack_scale                            = 0.24
 horizon_offset_01                      = 15 * screen_row
 horizon_offset_02                      = 15 * screen_row + &A0
 x_adjust                               = -70
 y_adjust                               = -3

 a01x = 0828 * crack_scale + x_adjust : a01y = 0478 * crack_scale + y_adjust
 a02x = 0694 * crack_scale + x_adjust : a02y = 0344 * crack_scale + y_adjust
 a03x = 0730 * crack_scale + x_adjust : a03y = 0476 * crack_scale + y_adjust
 a04x = 0776 * crack_scale + x_adjust : a04y = 0604 * crack_scale + y_adjust
 a05x = 0954 * crack_scale + x_adjust : a05y = 0540 * crack_scale + y_adjust
 a06x = 0992 * crack_scale + x_adjust : a06y = 0310 * crack_scale + y_adjust
 a07x = 0774 * crack_scale + x_adjust : a07y = 0264 * crack_scale + y_adjust
 a08x = 0500 * crack_scale + x_adjust : a08y = 0284 * crack_scale + y_adjust
 a09x = 0730 * crack_scale + x_adjust : a09y = 0534 * crack_scale + y_adjust
 a10x = 0858 * crack_scale + x_adjust : a10y = 0680 * crack_scale + y_adjust
 a11x = 1062 * crack_scale + x_adjust : a11y = 0544 * crack_scale + y_adjust
 a12x = 0996 * crack_scale + x_adjust : a12y = 0264 * crack_scale + y_adjust
 a13x = 1068 * crack_scale + x_adjust : a13y = 0264 * crack_scale + y_adjust
 a14x = 0430 * crack_scale + x_adjust : a14y = 0270 * crack_scale + y_adjust
 a15x = 0342 * crack_scale + x_adjust : a15y = 0444 * crack_scale + y_adjust
 a16x = 0610 * crack_scale + x_adjust : a16y = 0540 * crack_scale + y_adjust
 a17x = 0918 * crack_scale + x_adjust : a17y = 0854 * crack_scale + y_adjust
 a18x = 1062 * crack_scale + x_adjust : a18y = 0590 * crack_scale + y_adjust
 a19x = 0500 * crack_scale + x_adjust : a19y = 0508 * crack_scale + y_adjust
 a20x = 0610 * crack_scale + x_adjust : a20y = 0604 * crack_scale + y_adjust
 a21x = 0814 * crack_scale + x_adjust : a21y = 1056 * crack_scale + y_adjust
 a22x = 1200 * crack_scale + x_adjust : a22y = 0590 * crack_scale + y_adjust
 a23x = 0512 * crack_scale + x_adjust : a23y = 0700 * crack_scale + y_adjust
 a24x = 0564 * crack_scale + x_adjust : a24y = 0646 * crack_scale + y_adjust
 a25x = 0650 * crack_scale + x_adjust : a25y = 0736 * crack_scale + y_adjust
 a26x = 1188 * crack_scale + x_adjust : a26y = 0674 * crack_scale + y_adjust
 a27x = 1234 * crack_scale + x_adjust : a27y = 0672 * crack_scale + y_adjust
 a28x = 1124 * crack_scale + x_adjust : a28y = 0738 * crack_scale + y_adjust
 a29x = 1330 * crack_scale + x_adjust : a29y = 0462 * crack_scale + y_adjust
 a30x = 1472 * crack_scale + x_adjust : a30y = 0600 * crack_scale + y_adjust
 a31x = 0792 * crack_scale + x_adjust : a31y = 1075 * crack_scale + y_adjust
 a32x = 0836 * crack_scale + x_adjust : a32y = 1075 * crack_scale + y_adjust
 a33x = 1498 * crack_scale + x_adjust : a33y = 0658 * crack_scale + y_adjust
 a34x = 1616 * crack_scale + x_adjust : a34y = 0512 * crack_scale + y_adjust
 a35x = 0412 * crack_scale + x_adjust : a35y = 0718 * crack_scale + y_adjust
 a36x = 0306 * crack_scale + x_adjust : a36y = 0644 * crack_scale + y_adjust
 a37x = 0366 * crack_scale + x_adjust : a37y = 0786 * crack_scale + y_adjust
 a38x = 1602 * crack_scale + x_adjust : a38y = 0620 * crack_scale + y_adjust
 a39x = 1522 * crack_scale + x_adjust : a39y = 0736 * crack_scale + y_adjust

 sight_offset_top                       = screen_row * &08 + &98
 sight_offset_bot                       = sight_offset_top + screen_row * &0F
 sight_offset_h01                       = sight_offset_top + screen_row * &04 - &10 + &07
 sight_offset_h02                       = sight_offset_bot - screen_row * &04 - &10
 sight_offset_p01                       = sight_offset_h01 + &139
 sight_offset_p02                       = sight_offset_h02 - screen_row
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

.horizon
 LDA #HI(horizon_offset_01)
 CLC
 ADC screen_hidden
 STA horizon_01 + &02                   ;horizon offset 1/2 high byte = &12, &13
 ADC #&01
 STA horizon_02 + &02
 LDA b_object_bounce_far
 ADC #&9E                               ;c=0
 TAY
 SEC
.draw_horizon
 LDA #&FF
.horizon_01
 STA horizon_offset_01,Y
.horizon_02
 STA horizon_offset_02,Y
 TYA
 SBC #&08
 TAY
 BCS draw_horizon
.no_sights_on
 RTS

.tank_sights                            ;render the correct shape for the tank sights
 BIT m_shell                            ;if shell in flight then test if flashing
 BMI sights_draw_vertical
 LDA console_sights_flashing
 AND #&02
 BEQ no_sights_on

.sights_draw_vertical

 initialise_hidden sight_address_01, sight_offset_top
 initialise_hidden sight_address_02, sight_offset_bot

 LDX #&05
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

 BIT on_target                          ;if opponent on target then switch sights
 BMI dotted_line
 JMP sights_pins                        ;not on target so pins
.dotted_line                            ;dotted line straight after vertical as relies on page zero

 initialise_hidden sight_address_01, sight_offset_top + 5 * screen_row - &01
 initialise_hidden sight_address_02, sight_offset_bot - 5 * screen_row - &01

 LDY #&08
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
 initialise_hidden sight_address_02, sight_offset_p04

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
 initialise_hidden sight_address_02, sight_offset_p02

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
 LDY #&07
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
 CPX #&06
 BCC crack_cycle
 LDX #&06
.crack_cycle
 LDA crack_stage,X                      ;get number of cracks
 STA working_crack_counter              ;loop counter
 LDY #&00
 STY working_crack_storage
.all_cracks_at_this_part
 LDY working_crack_storage
.crack_coordinates
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
 JSR crack_line
 INC working_crack_storage
 DEC working_crack_counter
 BNE all_cracks_at_this_part
.crack_exit
 RTS

.crack_line                             ;test clip codes for start/end
 LDA crack_clip - &01,X
 LDX crack_start,Y
 ORA crack_clip - &01,X
 BNE crack_16_00
 JMP mathbox_line_draw08
.crack_16_00
 JMP mathbox_line_draw16

.crack_stage                            ;number of line segments to draw
 EQUB crack_b_start - crack_a_start
 EQUB crack_c_start - crack_a_start
 EQUB crack_d_start - crack_a_start
 EQUB crack_e_start - crack_a_start
 EQUB crack_f_start - crack_a_start
 EQUB crack_g_start - crack_a_start
 EQUB crack_end     - crack_a_start

.crack_start
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
 IF x < 32 OR x >= (32 + 256)
   EQUB &FF
 ELSE
   EQUB &00
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
