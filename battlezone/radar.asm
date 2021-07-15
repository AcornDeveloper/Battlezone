; radar
; clear screen area, draw radar point, area of view and sweep arm
;
; arctangent
; calculate the angle, in a 256-degree circle, between two points, one at the origin to reduce complexity
; use logarithmic division to get the y/x ratio and integrate the power function into the atan table
; x1, y1 = +127, -128 coordinates relative to the center
; returns a = angle 0-255
;
;       192
;        |
; 128 ------- 000
;        |
;       064
;
; battlezone receding text

; constants
 adjust                                 = 0
 number_of_sectors                      = 16
 radar_size                             = 22
 radar_x_offset                         = 159
 radar_y_offset                         = 24
 piece                                  = 360 / number_of_sectors
 dial_address                           = &88

 text_distance_delta                    = &40
 text_height_delta                      = &04
 text_zone_clip                         = &240

 bank_00 = &3000 + &880
 bank_01 = &5800 + &880

; atan(2^(x/32))*128/pi
.atan_tab
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00
 EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &00: EQUB &01: EQUB &01: EQUB &01
 EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01
 EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01
 EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01
 EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &01: EQUB &02: EQUB &02: EQUB &02
 EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02
 EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02: EQUB &02
 EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &03
 EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &03: EQUB &04: EQUB &04: EQUB &04
 EQUB &04: EQUB &04: EQUB &04: EQUB &04: EQUB &04: EQUB &04: EQUB &04: EQUB &04
 EQUB &05: EQUB &05: EQUB &05: EQUB &05: EQUB &05: EQUB &05: EQUB &05: EQUB &05
 EQUB &06: EQUB &06: EQUB &06: EQUB &06: EQUB &06: EQUB &06: EQUB &06: EQUB &06
 EQUB &07: EQUB &07: EQUB &07: EQUB &07: EQUB &07: EQUB &07: EQUB &08: EQUB &08
 EQUB &08: EQUB &08: EQUB &08: EQUB &08: EQUB &09: EQUB &09: EQUB &09: EQUB &09
 EQUB &09: EQUB &0A: EQUB &0A: EQUB &0A: EQUB &0A: EQUB &0B: EQUB &0B: EQUB &0B
 EQUB &0B: EQUB &0C: EQUB &0C: EQUB &0C: EQUB &0C: EQUB &0D: EQUB &0D: EQUB &0D
 EQUB &0D: EQUB &0E: EQUB &0E: EQUB &0E: EQUB &0E: EQUB &0F: EQUB &0F: EQUB &0F
 EQUB &10: EQUB &10: EQUB &10: EQUB &11: EQUB &11: EQUB &11: EQUB &12: EQUB &12
 EQUB &12: EQUB &13: EQUB &13: EQUB &13: EQUB &14: EQUB &14: EQUB &15: EQUB &15
 EQUB &15: EQUB &16: EQUB &16: EQUB &17: EQUB &17: EQUB &17: EQUB &18: EQUB &18
 EQUB &19: EQUB &19: EQUB &19: EQUB &1A: EQUB &1A: EQUB &1B: EQUB &1B: EQUB &1C
 EQUB &1C: EQUB &1C: EQUB &1D: EQUB &1D: EQUB &1E: EQUB &1E: EQUB &1F: EQUB &1F

; log2(x)*32
.log2_tab
 EQUB &00: EQUB &00: EQUB &20: EQUB &32: EQUB &40: EQUB &4A: EQUB &52: EQUB &59
 EQUB &60: EQUB &65: EQUB &6A: EQUB &6E: EQUB &72: EQUB &76: EQUB &79: EQUB &7D
 EQUB &80: EQUB &82: EQUB &85: EQUB &87: EQUB &8A: EQUB &8C: EQUB &8E: EQUB &90
 EQUB &92: EQUB &94: EQUB &96: EQUB &98: EQUB &99: EQUB &9B: EQUB &9D: EQUB &9E
 EQUB &A0: EQUB &A1: EQUB &A2: EQUB &A4: EQUB &A5: EQUB &A6: EQUB &A7: EQUB &A9
 EQUB &AA: EQUB &AB: EQUB &AC: EQUB &AD: EQUB &AE: EQUB &AF: EQUB &B0: EQUB &B1
 EQUB &B2: EQUB &B3: EQUB &B4: EQUB &B5: EQUB &B6: EQUB &B7: EQUB &B8: EQUB &B9
 EQUB &B9: EQUB &BA: EQUB &BB: EQUB &BC: EQUB &BD: EQUB &BD: EQUB &BE: EQUB &BF
 EQUB &C0: EQUB &C0: EQUB &C1: EQUB &C2: EQUB &C2: EQUB &C3: EQUB &C4: EQUB &C4
 EQUB &C5: EQUB &C6: EQUB &C6: EQUB &C7: EQUB &C7: EQUB &C8: EQUB &C9: EQUB &C9
 EQUB &CA: EQUB &CA: EQUB &CB: EQUB &CC: EQUB &CC: EQUB &CD: EQUB &CD: EQUB &CE
 EQUB &CE: EQUB &CF: EQUB &CF: EQUB &D0: EQUB &D0: EQUB &D1: EQUB &D1: EQUB &D2
 EQUB &D2: EQUB &D3: EQUB &D3: EQUB &D4: EQUB &D4: EQUB &D5: EQUB &D5: EQUB &D5
 EQUB &D6: EQUB &D6: EQUB &D7: EQUB &D7: EQUB &D8: EQUB &D8: EQUB &D9: EQUB &D9
 EQUB &D9: EQUB &DA: EQUB &DA: EQUB &DB: EQUB &DB: EQUB &DB: EQUB &DC: EQUB &DC
 EQUB &DD: EQUB &DD: EQUB &DD: EQUB &DE: EQUB &DE: EQUB &DE: EQUB &DF: EQUB &DF
 EQUB &DF: EQUB &E0: EQUB &E0: EQUB &E1: EQUB &E1: EQUB &E1: EQUB &E2: EQUB &E2
 EQUB &E2: EQUB &E3: EQUB &E3: EQUB &E3: EQUB &E4: EQUB &E4: EQUB &E4: EQUB &E5
 EQUB &E5: EQUB &E5: EQUB &E6: EQUB &E6: EQUB &E6: EQUB &E7: EQUB &E7: EQUB &E7
 EQUB &E7: EQUB &E8: EQUB &E8: EQUB &E8: EQUB &E9: EQUB &E9: EQUB &E9: EQUB &EA
 EQUB &EA: EQUB &EA: EQUB &EA: EQUB &EB: EQUB &EB: EQUB &EB: EQUB &EC: EQUB &EC
 EQUB &EC: EQUB &EC: EQUB &ED: EQUB &ED: EQUB &ED: EQUB &ED: EQUB &EE: EQUB &EE
 EQUB &EE: EQUB &EE: EQUB &EF: EQUB &EF: EQUB &EF: EQUB &EF: EQUB &F0: EQUB &F0
 EQUB &F0: EQUB &F1: EQUB &F1: EQUB &F1: EQUB &F1: EQUB &F1: EQUB &F2: EQUB &F2
 EQUB &F2: EQUB &F2: EQUB &F3: EQUB &F3: EQUB &F3: EQUB &F3: EQUB &F4: EQUB &F4
 EQUB &F4: EQUB &F4: EQUB &F5: EQUB &F5: EQUB &F5: EQUB &F5: EQUB &F5: EQUB &F6
 EQUB &F6: EQUB &F6: EQUB &F6: EQUB &F7: EQUB &F7: EQUB &F7: EQUB &F7: EQUB &F7
 EQUB &F8: EQUB &F8: EQUB &F8: EQUB &F8: EQUB &F9: EQUB &F9: EQUB &F9: EQUB &F9
 EQUB &F9: EQUB &FA: EQUB &FA: EQUB &FA: EQUB &FA: EQUB &FA: EQUB &FB: EQUB &FB
 EQUB &FB: EQUB &FB: EQUB &FB: EQUB &FC: EQUB &FC: EQUB &FC: EQUB &FC: EQUB &FC
 EQUB &FD: EQUB &FD: EQUB &FD: EQUB &FD: EQUB &FD: EQUB &FD: EQUB &FE: EQUB &FE
 EQUB &FE: EQUB &FE: EQUB &FE: EQUB &FF: EQUB &FF: EQUB &FF: EQUB &FF: EQUB &FF

.octant_adjust
 EQUB &3F                               ;00111111 x+,y+,|x|>|y|
 EQUB &00                               ;00000000 x+,y+,|x|<|y|
 EQUB &C0                               ;11000000 x+,y-,|x|>|y|
 EQUB &FF                               ;11111111 x+,y-,|x|<|y|
 EQUB &40                               ;01000000 x-,y+,|x|>|y|
 EQUB &7F                               ;01111111 x-,y+,|x|<|y|
 EQUB &BF                               ;10111111 x-,y-,|x|>|y|
 EQUB &80                               ;10000000 x-,y-,|x|<|y|

.arctangent                             ;find angle between 0, 0 (adjusted coordinates) and
 LDA x_coor_tan_01                      ;object x/z (-128, 127) returning value 0-255 in accumulator
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
.radar_exit
 RTS

.radar_arm_x
 FOR arm_x, 0 - adjust , 359 - adjust, piece
  IF arm_x <= 90  OR arm_x >= 270
    EQUB COS(RAD(arm_x)) * radar_size + radar_x_offset + &01
  ELSE
    EQUB COS(RAD(arm_x)) * radar_size + radar_x_offset
  ENDIF
 NEXT

.radar_arm_y
 FOR arm_y, 0 - adjust , 359 - adjust, piece
  IF arm_y <= 180
    EQUB SIN(RAD(arm_y)) * radar_size + radar_y_offset + &01
  ELSE
    EQUB SIN(RAD(arm_y)) * radar_size + radar_y_offset
  ENDIF
 NEXT

.dial_parameters

 fast_block dial_address, battlezone_sprites + dial_offset, &06, &30

.radar
 LDA console_radar                      ;display radar?
 BEQ radar_exit                         ;not on
 INC console_radar
 LDX #LO(dial_parameters)
 LDY #HI(dial_parameters)
 JSR multiple_row_sprite
 JSR render_radar_spot
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

.battlezone_text                        ;render floating text in attract mode
 LDX #LO(copyright)
 LDY #HI(copyright)
 JSR print
 JSR print_player_score
 LDA object_relative_y                  ;check to display "zone"
 CMP #LO(text_zone_clip)
 LDA object_relative_y + &01
 SBC #HI(text_zone_clip)
 BCS not_show_zone                      ;still behind us
 LDX #object_x1A_battlezone_part03      ;part03 "zone"
 JSR object_transfer_16
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
.not_show_zone
 LDX #object_x18_battlezone_part01      ;part01 "ba"
 JSR object_transfer_16
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 LDX #object_x19_battlezone_part02      ;part02 "ttle"
 JSR object_transfer_16
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 LDA object_relative_z                  ;now recede the text a little
 CLC
 ADC #text_distance_delta
 STA object_relative_z
 BCC no_inc_text_distance
 INC object_relative_z + &01
.no_inc_text_distance
 LDA object_relative_y                  ;increase height
 SEC
 SBC #text_height_delta
 STA object_relative_y
 BCS no_dec_text_height
 DEC object_relative_y + &01
.no_dec_text_height
 RTS

.clear_play_area                        ;fast clear bz play area
 LDX #&3F
 LDA screen_hidden
 EOR #&30
 BEQ play_00_jump
 JMP other_bank
.play_00_jump

 FOR screen_page, 0, 57
   STA bank_00 + screen_page * &80,X
   STA bank_00 + screen_page * &80 + &40,X
 NEXT

 DEX
 BMI exit_bank_00
 JMP play_00_jump
.exit_bank_00
 RTS

.other_bank
 LDA #&00
.play_01_jump

 FOR screen_page, 0, 57
   STA bank_01 + screen_page * &80,X
   STA bank_01 + screen_page * &80 + &40,X
 NEXT

 DEX
 BMI exit_bank_01
 JMP play_01_jump
.exit_bank_01
 RTS
