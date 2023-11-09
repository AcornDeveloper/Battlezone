; radar
; clear screen area and play area
; render radar, battlezone receding text and sweep arm
;
; arctangent
; calculate the angle, in a 256-degree circle, between two points, one at the origin to reduce complexity
; x1, y1 = +127/-128 coordinates relative to the center
; returns a = angle 0-255
;
; arctangent      negated         unit            player
;       C0              40              00              00
;        |               |               |               |
; 80 -------- 00  80 -------- 00  40 -------- C0  C0 -------- 40
;        |               |               |               |
;       40              C0              80              80
;
; code fragment to rotate return angle to match unit
; SEC                                    ;rotate to have 00 at the top and ccw
; SBC #&C1                               ;rotate +90', +1 for negate
; EOR #&FF                               ;invert

; constants
 number_of_sectors                      = 16
 radar_size                             = 22
 radar_x_offset                         = 159
 radar_y_offset                         = 23
 piece                                  = 360 / number_of_sectors
 dial_address                           = &88

 text_distance_delta                    = &40
 text_height_delta                      = &04
 text_zone_clip                         = &260

 bank_00 = &3000 + &800
 bank_01 = &5800 + &800

 offset = -90

.radar_arm_x
 FOR arm_x, 0 + offset, 359 + offset, piece
   IF arm_x <= 90  OR arm_x >= 270
     EQUB COS(RAD(arm_x)) * radar_size + radar_x_offset + &01
   ELSE
     EQUB COS(RAD(arm_x)) * radar_size + radar_x_offset
   ENDIF
 NEXT

.radar_arm_y
 FOR arm_y, 0 + offset, 359 + offset, piece
   IF arm_y <= 180
     EQUB SIN(RAD(arm_y)) * radar_size + radar_y_offset + &01
   ELSE
     EQUB SIN(RAD(arm_y)) * radar_size + radar_y_offset
   ENDIF
 NEXT

.dial_parameters

 fast_block dial_address, battlezone_sprites + dial_offset, &06, &30

.radar
 LDX #LO(dial_parameters)
 LDY #HI(dial_parameters)
 JSR multiple_row_sprite
 LDX radar_arm_position                 ;get radar arm position
 LDA radar_arm_x,X
 STA graphic_x_00
 LDA radar_arm_y,X
 STA graphic_y_00
 LDA #radar_x_offset
 STA graphic_x_01
 LDA #radar_y_offset
 STA graphic_y_01
 LDA #&00
 STA graphic_x_00 + &01
 STA graphic_y_00 + &01
 STA graphic_x_01 + &01
 STA graphic_y_01 + &01
 JMP mathbox_line_draw08

.radar_spot                             ;routine for radar spot
 BIT radar_scr_a                        ;test bit 7
 BMI finished_radar                     ;not on so exit
 LDY radar_scr_y
 LDA radar_scr_x
 AND #&F8
 CLC
 ADC screen_access_y_lo,Y
 STA radar_address
 LDA screen_access_y_hi,Y
 ADC screen_hidden
 STA radar_address + &01
 LDA radar_scr_x
 AND #&07
 TAX
 LDY #&00
 LDA pixel_mask,X
 LSR radar_scr_a                        ;radar blip size?
 BCC small_radar_spot                   ;clear so place small single pixel
 LDA double_pixel_mask,X
 TAX                                    ;save mask
 ORA (radar_address),Y
 STA (radar_address),Y
 INY
 LDA radar_scr_y
 AND #&07
 CMP #&07
 BNE following_row
 INC radar_address + &01
 LDY #&39
.following_row
 TXA
.small_radar_spot
 ORA (radar_address),Y
 STA (radar_address),Y
.finished_radar
 RTS

.battlezone_text                        ;render floating text in attract mode
 LDA object_relative_z                  ;now recede the text a little
 CLC
 ADC #text_distance_delta
 STA object_relative_z
 BCC no_inc_text_distance
 INC object_relative_z + &01
.no_inc_text_distance
 LDA object_relative_y                  ;increase height a little
 SEC
 SBC #text_height_delta
 STA object_relative_y
 BCS no_dec_text_height
 DEC object_relative_y + &01
.no_dec_text_height
 LDA object_relative_y                  ;check to display "zone"
 CMP #LO(text_zone_clip)
 LDA object_relative_y + &01
 SBC #HI(text_zone_clip)
 BCS not_show_zone                      ;still behind us
 LDX #object_x1A_battlezone_part03      ;part03 "zone"
 JSR render_text
.not_show_zone
 LDX #object_x18_battlezone_part01      ;part01 "ba"
 JSR render_text
 LDX #object_x19_battlezone_part02      ;part02 "ttle"
.render_text
 JSR object_transfer_16
 JSR object_view_transform_16
 JMP object_draw_16

.clear_play_area                        ;fast clear bz play area
 LDX #&00
 LDA screen_hidden
 EOR #&30
 BNE other_bank                         ;a=0
.play_00_jump

 FOR screen_page, 0, 31
   STA bank_00 + screen_page * &100,X
 NEXT

 DEX
 BNE play_00_jump
 RTS

.other_bank
 TXA
.play_01_jump

 FOR screen_page, 0, 31
   STA bank_01 + screen_page * &100,X
 NEXT

 DEX
 BNE play_01_jump
 RTS

.calc_angle_to_player                   ;angle unit must turn to face player
 LDA tank_or_super_or_missile_z         ;z difference
 SEC
 SBC m_tank_z
 STA zdiff
 STA zdiff_abs
 LDA tank_or_super_or_missile_z + &01
 SBC m_tank_z + &01
 STA zdiff + &01
 STA zdiff_abs + &01
 BPL zdiff_abs_value
 LDA #&00
 SEC
 SBC zdiff_abs
 STA zdiff_abs
 LDA #&00
 SBC zdiff_abs + &01
 STA zdiff_abs + &01
.zdiff_abs_value
 LDA tank_or_super_or_missile_x         ;x difference
 SEC
 SBC m_tank_x
 STA xdiff
 STA xdiff_abs
 LDA tank_or_super_or_missile_x + &01
 SBC m_tank_x + &01
 STA xdiff + &01
 STA xdiff_abs + &01
 BPL xdiff_abs_value
 LDA #&00                               ;value is negative so make positive
 SEC
 SBC xdiff_abs
 STA xdiff_abs
 LDA #&00
 SBC xdiff_abs + &01
 STA xdiff_abs + &01
.xdiff_abs_value
 LDX #&08                               ;scale xdiff/zdiff to increase accuracy
.keep_scaling
 ASL xdiff_abs
 ROL xdiff_abs + &01
 BMI calculate_angle                    ;finished scaling
 ASL zdiff_abs
 ROL zdiff_abs + &01
 BMI calculate_angle                    ;finished scaling
 ASL xdiff                              ;scale coordinate differences
 ROL xdiff + &01
 ASL zdiff
 ROL zdiff + &01
 DEX
 BNE keep_scaling                       ;x_coor_tan_01 and z_coor_tan_01 already loaded
.calculate_angle
 JSR arctangent
 SEC
 SBC #&40
 TAY                                    ;save in y
 BPL enemy_angle_is_positive
 EOR #&FF
 CLC
 ADC #&01
.enemy_angle_is_positive                ;abs angle tested for in ai at +-&20 (unit in view)
 STA enemy_ang_delt_abs                 ;makes unit less aggressive early on
 TYA                                    ;restore a
 RTS

.arctangent                             ;find angle between 0, 0 (adjusted coordinates) and
 LDA x_coor_tan_01                      ;arctangent object x/z (-128, 127) returning value 0-255 in accumulator
 ASL A                                  ;special case routine
 ROL octant
 LDA x_coor_tan_01
 BPL arctan_01
 EOR #&FF
 CLC
 ADC #&01
.arctan_01
 TAX
 LDA z_coor_tan_01
 EOR #&FF
 ASL A
 ROL octant
 LDA z_coor_tan_01
 BPL arctan_02
 EOR #&FF
 CLC
 ADC #&01
.arctan_02
 TAY
 LDA log2_tab,X
 SEC
 SBC log2_tab,Y
 BCC arctan_03
 EOR #&FF
.arctan_03
 TAX
 LDA octant
 ROL A
 AND #&07
 TAY
 LDA atan_tab,X
 EOR octant_adjust,Y                    ;a equals angle 0-255
 RTS
