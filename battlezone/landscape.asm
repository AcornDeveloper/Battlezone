; landscape
; render landscape, this is a series of line vectors x0/y0 to x1/y1 rotation around
; the y-axis becomes a simple subtraction of the x-coordinates then clipped when rendered
; note that y-coordinate clipping is not required
;
; moon, a sprite that enables the complex shape of the original to be easily represented
;
; particle_maintenance
;
; print text stored/or'ed
; entry parameter block at print parameter in zero page
; +&00 text address lsb
; +&01 text address msb
; +&02 screen x position
; +&03 screen y position
;
; constants
 landscape_limit                        = &500
 particle_gravity                       = &48
 large_particle                         = &12
 landscape_horizon                      = 166    ;landscape horizon y coordinate 0, 0 top left corner
 particle_window                        = screen_row - &01
 scale_factor                           = 0.31   ;landscape scaling factor, two decimal places 1 / (4096/1280)

 particle_maintenance_x_coordinate      = &32A
 
 screen_span_moon                       = &C0
 screen_span_particle                   = &5C
 screen_span                            = &48

 moon_x_coor                            = &00
 moon_left_sprite_edge                  = -&20
 moon_right_sprite_edge                 = screen_row

; addresses
 moon_screen_offset                     = screen_row * &09
 moon_data_00                           = battlezone_sprites + moon_00_offset
 moon_data_01                           = battlezone_sprites + moon_01_offset
 moon_data_02                           = battlezone_sprites + moon_02_offset
 moon_data_03                           = battlezone_sprites + moon_03_offset
 moon_data_04                           = battlezone_sprites + moon_04_offset
 moon_data_05                           = battlezone_sprites + moon_05_offset
 moon_data_06                           = battlezone_sprites + moon_06_offset
 moon_data_07                           = battlezone_sprites + moon_07_offset

; landscape vertices
 x01  = 0108 * scale_factor : y01 = 125 - (landscape_horizon - 166) * scale_factor
 x02  = 0158 * scale_factor : y02 = 125 - (landscape_horizon - 118) * scale_factor
 x03  = 0204 * scale_factor : y03 = 125 - (landscape_horizon - 166) * scale_factor
 x04  = 0192 * scale_factor : y04 = 125 - (landscape_horizon - 132) * scale_factor
 x05  = 0256 * scale_factor : y05 = 125 - (landscape_horizon - 100) * scale_factor
 x06  = 0286 * scale_factor : y06 = 125 - (landscape_horizon - 166) * scale_factor
 x07  = 0292 * scale_factor : y07 = 125 - (landscape_horizon - 135) * scale_factor
 x08  = 0412 * scale_factor : y08 = 125 - (landscape_horizon - 166) * scale_factor
 x09  = 0574 * scale_factor : y09 = 125 - (landscape_horizon - 134) * scale_factor
 x10  = 0640 * scale_factor : y10 = 125 - (landscape_horizon - 166) * scale_factor
 x11  = 0640 * scale_factor : y11 = 125 - (landscape_horizon - 134) * scale_factor
 x12  = 0732 * scale_factor : y12 = 125 - (landscape_horizon - 166) * scale_factor
 x13  = 0702 * scale_factor : y13 = 125 - (landscape_horizon - 102) * scale_factor
 x14  = 0732 * scale_factor : y14 = 125 - (landscape_horizon - 102) * scale_factor
 x15  = 0768 * scale_factor : y15 = 125 - (landscape_horizon - 128) * scale_factor
 x16  = 0800 * scale_factor : y16 = 125 - (landscape_horizon - 119) * scale_factor
 x17  = 0830 * scale_factor : y17 = 125 - (landscape_horizon - 130) * scale_factor
 x18  = 0860 * scale_factor : y18 = 125 - (landscape_horizon - 118) * scale_factor
 x19  = 0896 * scale_factor : y19 = 125 - (landscape_horizon - 134) * scale_factor
 x20  = 0930 * scale_factor : y20 = 125 - (landscape_horizon - 118) * scale_factor
 x21  = 0896 * scale_factor : y21 = 125 - (landscape_horizon - 102) * scale_factor
 x22  = 0932 * scale_factor : y22 = 125 - (landscape_horizon - 166) * scale_factor
 x23  = 0956 * scale_factor : y23 = 125 - (landscape_horizon - 128) * scale_factor
 x24  = 0986 * scale_factor : y24 = 125 - (landscape_horizon - 118) * scale_factor
 x25  = 1020 * scale_factor : y25 = 125 - (landscape_horizon - 156) * scale_factor
 x26  = 1052 * scale_factor : y26 = 125 - (landscape_horizon - 134) * scale_factor
 x27  = 1086 * scale_factor : y27 = 125 - (landscape_horizon - 134) * scale_factor
 x28  = 1148 * scale_factor : y28 = 125 - (landscape_horizon - 166) * scale_factor
 x29  = 1212 * scale_factor : y29 = 125 - (landscape_horizon - 166) * scale_factor
 x30  = 1372 * scale_factor : y30 = 125 - (landscape_horizon - 166) * scale_factor
 x31  = 1500 * scale_factor : y31 = 125 - (landscape_horizon - 166) * scale_factor
 x32  = 1534 * scale_factor : y32 = 125 - (landscape_horizon - 166) * scale_factor
 x33  = 1566 * scale_factor : y33 = 125 - (landscape_horizon - 166) * scale_factor
 x34  = 1500 * scale_factor : y34 = 125 - (landscape_horizon - 104) * scale_factor
 x35  = 1532 * scale_factor : y35 = 125 - (landscape_horizon - 132) * scale_factor
 x36  = 1564 * scale_factor : y36 = 125 - (landscape_horizon - 100) * scale_factor
 x37  = 1598 * scale_factor : y37 = 125 - (landscape_horizon - 134) * scale_factor
 x38  = 1660 * scale_factor : y38 = 125 - (landscape_horizon - 104) * scale_factor
 x39  = 1692 * scale_factor : y39 = 125 - (landscape_horizon - 166) * scale_factor
 x40  = 1694 * scale_factor : y40 = 125 - (landscape_horizon - 132) * scale_factor
 x41  = 1724 * scale_factor : y41 = 125 - (landscape_horizon - 166) * scale_factor
 x42  = 1724 * scale_factor : y42 = 125 - (landscape_horizon - 120) * scale_factor
 x43  = 1824 * scale_factor : y43 = 125 - (landscape_horizon - 166) * scale_factor
 x44  = 1936 * scale_factor : y44 = 125 - (landscape_horizon - 166) * scale_factor
 x45  = 1468 * scale_factor : y45 = 125 - (landscape_horizon - 134) * scale_factor
 x46  = 2110 * scale_factor : y46 = 125 - (landscape_horizon - 134) * scale_factor
 x47  = 2172 * scale_factor : y47 = 125 - (landscape_horizon - 134) * scale_factor
 x48  = 2392 * scale_factor : y48 = 125 - (landscape_horizon - 166) * scale_factor
 x49  = 2300 * scale_factor : y49 = 125 - (landscape_horizon - 154) * scale_factor
 x50  = 2364 * scale_factor : y50 = 125 - (landscape_horizon - 122) * scale_factor
 x51  = 2492 * scale_factor : y51 = 125 - (landscape_horizon - 102) * scale_factor
 x52  = 2542 * scale_factor : y52 = 125 - (landscape_horizon - 166) * scale_factor
 x53  = 2558 * scale_factor : y53 = 125 - (landscape_horizon - 142) * scale_factor
 x54  = 2600 * scale_factor : y54 = 125 - (landscape_horizon - 076) * scale_factor
 x55  = 2608 * scale_factor : y55 = 125 - (landscape_horizon - 080) * scale_factor
 x56  = 2618 * scale_factor : y56 = 125 - (landscape_horizon - 082) * scale_factor
 x57  = 2612 * scale_factor : y57 = 125 - (landscape_horizon - 078) * scale_factor
 x58  = 2624 * scale_factor : y58 = 125 - (landscape_horizon - 076) * scale_factor
 x59  = 2682 * scale_factor : y59 = 125 - (landscape_horizon - 166) * scale_factor
 x60  = 2780 * scale_factor : y60 = 125 - (landscape_horizon - 102) * scale_factor
 x61  = 2844 * scale_factor : y61 = 125 - (landscape_horizon - 166) * scale_factor
 x62  = 2844 * scale_factor : y62 = 125 - (landscape_horizon - 134) * scale_factor
 x63  = 2910 * scale_factor : y63 = 125 - (landscape_horizon - 166) * scale_factor
 x64  = 2910 * scale_factor : y64 = 125 - (landscape_horizon - 102) * scale_factor
 x65  = 2988 * scale_factor : y65 = 125 - (landscape_horizon - 134) * scale_factor
 x66  = 3042 * scale_factor : y66 = 125 - (landscape_horizon - 118) * scale_factor
 x67  = 3042 * scale_factor : y67 = 125 - (landscape_horizon - 166) * scale_factor
 x68  = 3070 * scale_factor : y68 = 125 - (landscape_horizon - 166) * scale_factor
 x69  = 3144 * scale_factor : y69 = 125 - (landscape_horizon - 166) * scale_factor
 x70  = 3220 * scale_factor : y70 = 125 - (landscape_horizon - 166) * scale_factor
 x71  = 3550 * scale_factor : y71 = 125 - (landscape_horizon - 134) * scale_factor
 x72  = 3602 * scale_factor : y72 = 125 - (landscape_horizon - 154) * scale_factor
 x73  = 3618 * scale_factor : y73 = 125 - (landscape_horizon - 166) * scale_factor
 x74  = 3652 * scale_factor : y74 = 125 - (landscape_horizon - 104) * scale_factor
 x75  = 3662 * scale_factor : y75 = 125 - (landscape_horizon - 142) * scale_factor
 x76  = 3678 * scale_factor : y76 = 125 - (landscape_horizon - 130) * scale_factor
 x77  = 3742 * scale_factor : y77 = 125 - (landscape_horizon - 102) * scale_factor
 x78  = 3774 * scale_factor : y78 = 125 - (landscape_horizon - 102) * scale_factor
 x79  = 3806 * scale_factor : y79 = 125 - (landscape_horizon - 134) * scale_factor
 x80  = 3734 * scale_factor : y80 = 125 - (landscape_horizon - 166) * scale_factor
 x81  = 3934 * scale_factor : y81 = 125 - (landscape_horizon - 166) * scale_factor
 x82  = 3904 * scale_factor : y82 = 125 - (landscape_horizon - 134) * scale_factor
 x83  = 3870 * scale_factor : y83 = 125 - (landscape_horizon - 102) * scale_factor
 x84  = 3974 * scale_factor : y84 = 125 - (landscape_horizon - 166) * scale_factor
 x85  = 3934 * scale_factor : y85 = 125 - (landscape_horizon - 134) * scale_factor
 x86  = 4000 * scale_factor : y86 = 125 - (landscape_horizon - 152) * scale_factor
 x87  = 4094 * scale_factor : y87 = 125 - (landscape_horizon - 166) * scale_factor
 x101 = 0108 * scale_factor + landscape_limit : y101 = 125 - (landscape_horizon - 166) * scale_factor
 x102 = 0158 * scale_factor + landscape_limit : y102 = 125 - (landscape_horizon - 118) * scale_factor
 x103 = 0204 * scale_factor + landscape_limit : y103 = 125 - (landscape_horizon - 166) * scale_factor
 x104 = 0192 * scale_factor + landscape_limit : y104 = 125 - (landscape_horizon - 132) * scale_factor
 x105 = 0256 * scale_factor + landscape_limit : y105 = 125 - (landscape_horizon - 100) * scale_factor
 x106 = 0286 * scale_factor + landscape_limit : y106 = 125 - (landscape_horizon - 166) * scale_factor
 x107 = 0292 * scale_factor + landscape_limit : y107 = 125 - (landscape_horizon - 135) * scale_factor
 x108 = 0412 * scale_factor + landscape_limit : y108 = 125 - (landscape_horizon - 166) * scale_factor
 x109 = 0574 * scale_factor + landscape_limit : y109 = 125 - (landscape_horizon - 134) * scale_factor
 x110 = 0640 * scale_factor + landscape_limit : y110 = 125 - (landscape_horizon - 166) * scale_factor
 x111 = 0640 * scale_factor + landscape_limit : y111 = 125 - (landscape_horizon - 134) * scale_factor
 x112 = 0732 * scale_factor + landscape_limit : y112 = 125 - (landscape_horizon - 166) * scale_factor
 x113 = 0702 * scale_factor + landscape_limit : y113 = 125 - (landscape_horizon - 102) * scale_factor
 x114 = 0732 * scale_factor + landscape_limit : y114 = 125 - (landscape_horizon - 102) * scale_factor
 x115 = 0768 * scale_factor + landscape_limit : y115 = 125 - (landscape_horizon - 128) * scale_factor
 x116 = 0800 * scale_factor + landscape_limit : y116 = 125 - (landscape_horizon - 119) * scale_factor
 x117 = 0830 * scale_factor + landscape_limit : y117 = 125 - (landscape_horizon - 130) * scale_factor
 x118 = 0860 * scale_factor + landscape_limit : y118 = 125 - (landscape_horizon - 118) * scale_factor
 x119 = 0896 * scale_factor + landscape_limit : y119 = 125 - (landscape_horizon - 134) * scale_factor
 x120 = 0930 * scale_factor + landscape_limit : y120 = 125 - (landscape_horizon - 118) * scale_factor
 x121 = 0896 * scale_factor + landscape_limit : y121 = 125 - (landscape_horizon - 102) * scale_factor
 x122 = 0932 * scale_factor + landscape_limit : y122 = 125 - (landscape_horizon - 166) * scale_factor
 x123 = 0956 * scale_factor + landscape_limit : y123 = 125 - (landscape_horizon - 128) * scale_factor
 x124 = 0986 * scale_factor + landscape_limit : y124 = 125 - (landscape_horizon - 118) * scale_factor
 x125 = 1020 * scale_factor + landscape_limit : y125 = 125 - (landscape_horizon - 156) * scale_factor
 x126 = 1052 * scale_factor + landscape_limit : y126 = 125 - (landscape_horizon - 134) * scale_factor

.landscape
 LDA #&00                               ;clear high bytes
 STA graphic_y_00 + &01
 STA graphic_y_01 + &01
 STA landscape_screen_entry             ;flag for entered landscape
 LDA m_tank_rotation                    ;values 0-8
 ASL A
 LDA m_tank_rotation + &01
 ROL A
 TAX
 LDA landscape_start,X                  ;start landscape index
 STA landscape_vectors_ix
 LDA landscape_line_counter,X           ;number of possible 'dead' lines to test
 STA landscape_visibility               ;to complete landscape render
.landscape_draw
 LDX landscape_vectors_ix               ;check index for wrap around
 LDA landscape_vectors_x1_lo,X          ;calculate adjusted x0, x1 and store
 SEC                                    ;in line draw parameters
 SBC m_tank_rotation
 STA graphic_x_00
 LDA landscape_vectors_x1_hi,X
 SBC m_tank_rotation + &01
 STA graphic_x_00 + &01
 ROL landscape_result                   ;save result
 LDA landscape_vectors_x2_lo,X
 SEC
 SBC m_tank_rotation
 STA graphic_x_01
 LDA landscape_vectors_x2_hi,X
 SBC m_tank_rotation + &01
 STA graphic_x_01 + &01
 ROL A
 ORA landscape_result                   ;or carry flags
 LSR A                                  ;put result bit 0 in c
 BCC increment_index
 LDA landscape_vectors_y1,X             ;line segment to right of start coordinates, so test more
 STA graphic_y_00                       ;high bytes already zero
 LDA landscape_vectors_y2,X
 STA graphic_y_01
 LDA b_object_bounce_far
 BEQ no_landscape_bounce                ;landscape bounce for explosion/collision?
 INC graphic_y_00
 INC graphic_y_01                         
.no_landscape_bounce
 LDA graphic_x_00                       ;check x00, x01 in 32 - 287
 CMP #&20
 LDA graphic_x_00 + &01
 SBC #&00
 STA cs_value_00
 LDA graphic_x_01
 CMP #&20
 LDA graphic_x_01 + &01
 SBC #&00
 ORA cs_value_00
 BNE not_in_central_window              ;line segment outside 8 bit window
 INC landscape_screen_entry
 JSR mathbox_line_draw08                ;line x0, x1 both 0 - 255
.increment_index
 INC landscape_vectors_ix
 BNE landscape_draw                     ;always
.not_in_central_window                  ;tests for line segment end points both at left so test right only
 LDA graphic_x_00                       ;check x00, x01 in 32 - 287
 CMP #LO(screen_row)
 LDA graphic_x_00 + &01
 SBC #HI(screen_row)
 ROL A
 TAX
 LDA graphic_x_01                       ;check x00, x01 in 32 - 287
 CMP #LO(screen_row)
 LDA graphic_x_01 + &01
 SBC #HI(screen_row)
 TXA
 ROL A
 AND #&03
 CMP #&03
 BEQ up_visibility_counter              ;line segment off so next segment
 INC landscape_screen_entry
 JSR mathbox_line_draw16
 INC landscape_vectors_ix               ;next segment
 BNE landscape_draw                     ;always
.up_visibility_counter
 LDA landscape_screen_entry
 BEQ increment_index
 INC landscape_visibility
 BNE increment_index
 RTS

.landscape_start                        ;quick index into coordinates from the rotation to save on
 EQUB x_00  - x_00                      ;testing line segments that are not on
 EQUB xa_00 - x_00
 EQUB x_01  - x_00
 EQUB xa_01 - x_00
 EQUB x_02  - x_00
 EQUB xa_02 - x_00
 EQUB x_03  - x_00
 EQUB xa_03 - x_00
 EQUB x_04  - x_00
 EQUB xa_04 - x_00

.landscape_line_counter
 EQUB LO(-&01)
 EQUB LO(-&01)
 EQUB LO(-&01)
 EQUB LO(-&01)
 EQUB LO(-&01)
 EQUB LO(-&01)
 EQUB LO(-&02)
 EQUB LO(-&02)
 EQUB LO(-&02)
 EQUB LO(-&03)

.landscape_vectors_x1_lo
.x_00
 EQUB LO(x01)                           ;01:02
 EQUB LO(x02)                           ;02:03
 EQUB LO(x02)                           ;02:06
 EQUB LO(x04)                           ;04:05
 EQUB LO(x05)                           ;05:08
.xa_00
 EQUB LO(x05)                           ;05:07
 EQUB LO(x07)                           ;07:08
 EQUB LO(x08)                           ;08:09
 EQUB LO(x09)                           ;09:10
 EQUB LO(x09)                           ;09:11
 EQUB LO(x11)                           ;11:13
.x_01
 EQUB LO(x11)                           ;11:12
 EQUB LO(x13)                           ;13:15
 EQUB LO(x12)                           ;12:15
 EQUB LO(x15)                           ;15:14
 EQUB LO(x14)                           ;14:13
 EQUB LO(x12)                           ;12:21
 EQUB LO(x15)                           ;15:16
 EQUB LO(x16)                           ;16:17
 EQUB LO(x21)                           ;21:25
 EQUB LO(x25)                           ;25:22
 EQUB LO(x18)                           ;18:19
 EQUB LO(x19)                           ;19:20
 EQUB LO(x25)                           ;25:26
.xa_01
 EQUB LO(x26)                           ;26:24
 EQUB LO(x24)                           ;24:23
 EQUB LO(x25)                           ;25:24
 EQUB LO(x26)                           ;26:27
 EQUB LO(x27)                           ;27:28
 EQUB LO(x27)                           ;27:29
 EQUB LO(x30)                           ;30:45
 EQUB LO(x45)                           ;45:31
 EQUB LO(x45)                           ;45:34
 EQUB LO(x34)                           ;34:32
 EQUB LO(x34)                           ;34:33
 EQUB LO(x35)                           ;35:36
 EQUB LO(x36)                           ;36:37
.x_02
 EQUB LO(x37)                           ;37:38
 EQUB LO(x38)                           ;38:39
 EQUB LO(x38)                           ;38:41
.xa_02
 EQUB LO(x40)                           ;40:42
 EQUB LO(x42)                           ;42:43
 EQUB LO(x44)                           ;44:46
 EQUB LO(x46)                           ;46:47
.x_03
 EQUB LO(x47)                           ;47:48
 EQUB LO(x49)                           ;49:50
 EQUB LO(x50)                           ;50:48
 EQUB LO(x48)                           ;48:51
 EQUB LO(x51)                           ;51:53
 EQUB LO(x52)                           ;52:54
 EQUB LO(x54)                           ;54:55
 EQUB LO(x55)                           ;55:57
 EQUB LO(x57)                           ;57:56
.xa_03
 EQUB LO(x56)                           ;56:58
 EQUB LO(x58)                           ;58:59
 EQUB LO(x59)                           ;59:60
 EQUB LO(x60)                           ;60:61
 EQUB LO(x60)                           ;60:63
 EQUB LO(x62)                           ;62:64
 EQUB LO(x64)                           ;64:67
 EQUB LO(x65)                           ;65:66
 EQUB LO(x66)                           ;66:68
.x_04
 EQUB LO(x66)                           ;66:69
 EQUB LO(x70)                           ;70:71
 EQUB LO(x71)                           ;71:73
 EQUB LO(x72)                           ;72:74
.xa_04
 EQUB LO(x74)                           ;74:76
 EQUB LO(x75)                           ;75:77
 EQUB LO(x77)                           ;77:78
 EQUB LO(x78)                           ;78:79
 EQUB LO(x77)                           ;77:79
 EQUB LO(x80)                           ;80:83
 EQUB LO(x83)                           ;83:81
 EQUB LO(x82)                           ;82:84
 EQUB LO(x83)                           ;83:85
 EQUB LO(x85)                           ;85:86
 EQUB LO(x86)                           ;86:87
 EQUB LO(x101)                          ;101:102
 EQUB LO(x102)                          ;102:103
 EQUB LO(x102)                          ;102:106
 EQUB LO(x104)                          ;104:105
 EQUB LO(x105)                          ;105:108
 EQUB LO(x105)                          ;105:107
;.xa_04
 EQUB LO(x107)                          ;107:108
 EQUB LO(x108)                          ;108:109
 EQUB LO(x109)                          ;109:110
 EQUB LO(x109)                          ;109:111
 EQUB LO(x111)                          ;111:113
 EQUB LO(x111)                          ;111:112
 EQUB LO(x113)                          ;113:115
 EQUB LO(x112)                          ;112:115
 EQUB LO(x115)                          ;115:114
 EQUB LO(x114)                          ;114:113
 EQUB LO(x112)                          ;112:121
 EQUB LO(x115)                          ;115:116
 EQUB LO(x116)                          ;116:117
 EQUB LO(x121)                          ;121:125
 EQUB LO(x125)                          ;125:122
 EQUB LO(x118)                          ;118:119
 EQUB LO(x119)                          ;119:120
 EQUB LO(x125)                          ;125:126
 EQUB LO(x126)                          ;126:124
 EQUB LO(x124)                          ;124:123
 EQUB LO(x125)                          ;125:124

.landscape_vectors_x1_hi
 EQUB HI(x01)                           ;01:02
 EQUB HI(x02)                           ;02:03
 EQUB HI(x02)                           ;02:06
 EQUB HI(x04)                           ;04:05
 EQUB HI(x05)                           ;05:08
 EQUB HI(x05)                           ;05:07
 EQUB HI(x07)                           ;07:08
 EQUB HI(x08)                           ;08:09
 EQUB HI(x09)                           ;09:10
 EQUB HI(x09)                           ;09:11
 EQUB HI(x11)                           ;11:13
 EQUB HI(x11)                           ;11:12
 EQUB HI(x13)                           ;13:15
 EQUB HI(x12)                           ;12:15
 EQUB HI(x15)                           ;15:14
 EQUB HI(x14)                           ;14:13
 EQUB HI(x12)                           ;12:21
 EQUB HI(x15)                           ;15:16
 EQUB HI(x16)                           ;16:17
 EQUB HI(x21)                           ;21:25
 EQUB HI(x25)                           ;25:22
 EQUB HI(x18)                           ;18:19
 EQUB HI(x19)                           ;19:20
 EQUB HI(x25)                           ;25:26
 EQUB HI(x26)                           ;26:24
 EQUB HI(x24)                           ;24:23
 EQUB HI(x25)                           ;25:24
 EQUB HI(x26)                           ;26:27
 EQUB HI(x27)                           ;27:28
 EQUB HI(x27)                           ;27:29
 EQUB HI(x30)                           ;30:45
 EQUB HI(x45)                           ;45:31
 EQUB HI(x45)                           ;45:34
 EQUB HI(x34)                           ;34:32
 EQUB HI(x34)                           ;34:33
 EQUB HI(x35)                           ;35:36
 EQUB HI(x36)                           ;36:37
 EQUB HI(x37)                           ;37:38
 EQUB HI(x38)                           ;38:39
 EQUB HI(x38)                           ;38:41
 EQUB HI(x40)                           ;40:42
 EQUB HI(x42)                           ;42:43
 EQUB HI(x44)                           ;44:46
 EQUB HI(x46)                           ;46:47
 EQUB HI(x47)                           ;47:48
 EQUB HI(x49)                           ;49:50
 EQUB HI(x50)                           ;50:48
 EQUB HI(x48)                           ;48:51
 EQUB HI(x51)                           ;51:53
 EQUB HI(x52)                           ;52:54
 EQUB HI(x54)                           ;54:55
 EQUB HI(x55)                           ;55:57
 EQUB HI(x57)                           ;57:56
 EQUB HI(x56)                           ;56:58
 EQUB HI(x58)                           ;58:59
 EQUB HI(x59)                           ;59:60
 EQUB HI(x60)                           ;60:61
 EQUB HI(x60)                           ;60:63
 EQUB HI(x62)                           ;62:64
 EQUB HI(x64)                           ;64:67
 EQUB HI(x65)                           ;65:66
 EQUB HI(x66)                           ;66:68
 EQUB HI(x66)                           ;66:69
 EQUB HI(x70)                           ;70:71
 EQUB HI(x71)                           ;71:73
 EQUB HI(x72)                           ;72:74
 EQUB HI(x74)                           ;74:76
 EQUB HI(x75)                           ;75:77
 EQUB HI(x77)                           ;77:78
 EQUB HI(x78)                           ;78:79
 EQUB HI(x77)                           ;77:79
 EQUB HI(x80)                           ;80:83
 EQUB HI(x83)                           ;83:81
 EQUB HI(x82)                           ;82:84
 EQUB HI(x83)                           ;83:85
 EQUB HI(x85)                           ;85:86
 EQUB HI(x86)                           ;86:87
 EQUB HI(x101)                          ;101:102
 EQUB HI(x102)                          ;102:103
 EQUB HI(x102)                          ;102:106
 EQUB HI(x104)                          ;104:105
 EQUB HI(x105)                          ;105:108
 EQUB HI(x105)                          ;105:107
 EQUB HI(x107)                          ;107:108
 EQUB HI(x108)                          ;108:109
 EQUB HI(x109)                          ;109:110
 EQUB HI(x109)                          ;109:111
 EQUB HI(x111)                          ;111:113
 EQUB HI(x111)                          ;111:112
 EQUB HI(x113)                          ;113:115
 EQUB HI(x112)                          ;112:115
 EQUB HI(x115)                          ;115:114
 EQUB HI(x114)                          ;114:113
 EQUB HI(x112)                          ;112:121
 EQUB HI(x115)                          ;115:116
 EQUB HI(x116)                          ;116:117
 EQUB HI(x121)                          ;121:125
 EQUB HI(x125)                          ;125:122
 EQUB HI(x118)                          ;118:119
 EQUB HI(x119)                          ;119:120
 EQUB HI(x125)                          ;125:126
 EQUB HI(x126)                          ;126:124
 EQUB HI(x124)                          ;124:123
 EQUB HI(x125)                          ;125:124

.landscape_vectors_y1
 EQUB y01                               ;01:02
 EQUB y02                               ;02:03
 EQUB y02                               ;02:06
 EQUB y04                               ;04:05
 EQUB y05                               ;05:08
 EQUB y05                               ;05:07
 EQUB y07                               ;07:08
 EQUB y08                               ;08:09
 EQUB y09                               ;09:10
 EQUB y09                               ;09:11
 EQUB y11                               ;11:13
 EQUB y11                               ;11:12
 EQUB y13                               ;13:15
 EQUB y12                               ;12:15
 EQUB y15                               ;15:14
 EQUB y14                               ;14:13
 EQUB y12                               ;12:21
 EQUB y15                               ;15:16
 EQUB y16                               ;16:17
 EQUB y21                               ;21:25
 EQUB y25                               ;25:22
 EQUB y18                               ;18:19
 EQUB y19                               ;19:20
 EQUB y25                               ;25:26
 EQUB y26                               ;26:24
 EQUB y24                               ;24:23
 EQUB y25                               ;25:24
 EQUB y26                               ;26:27
 EQUB y27                               ;24:28
 EQUB y27                               ;27:29
 EQUB y30                               ;30:45
 EQUB y45                               ;45:31
 EQUB y45                               ;45:34
 EQUB y34                               ;32:32
 EQUB y34                               ;34:33
 EQUB y35                               ;35:36
 EQUB y36                               ;36:37
 EQUB y37                               ;37:38
 EQUB y38                               ;38:39
 EQUB y38                               ;38:41
 EQUB y40                               ;40:42
 EQUB y42                               ;42:43
 EQUB y44                               ;44:46
 EQUB y46                               ;46:47
 EQUB y47                               ;47:48
 EQUB y49                               ;49:50
 EQUB y50                               ;50:48
 EQUB y48                               ;48:51
 EQUB y51                               ;51:53
 EQUB y52                               ;52:54
 EQUB y54                               ;54:55
 EQUB y55                               ;55:57
 EQUB y57                               ;57:56
 EQUB y56                               ;56:58
 EQUB y58                               ;58:59
 EQUB y59                               ;59:60
 EQUB y60                               ;60:61
 EQUB y60                               ;60:63
 EQUB y62                               ;62:64
 EQUB y64                               ;64:67
 EQUB y65                               ;65:66
 EQUB y66                               ;66:68
 EQUB y66                               ;66:69
 EQUB y70                               ;70:71
 EQUB y71                               ;71:73
 EQUB y72                               ;72:74
 EQUB y74                               ;74:76
 EQUB y75                               ;75:77
 EQUB y77                               ;77:78
 EQUB y78                               ;78:79
 EQUB y77                               ;77:79
 EQUB y80                               ;80:83
 EQUB y83                               ;83:81
 EQUB y82                               ;82:84
 EQUB y83                               ;83:85
 EQUB y85                               ;85:86
 EQUB y86                               ;86:87
 EQUB y101                              ;101:102
 EQUB y102                              ;102:103
 EQUB y102                              ;102:106
 EQUB y104                              ;104:105
 EQUB y105                              ;105:108
 EQUB y105                              ;105:107
 EQUB y107                              ;107:108
 EQUB y108                              ;108:109
 EQUB y109                              ;109:110
 EQUB y109                              ;109:111
 EQUB y111                              ;111:113
 EQUB y111                              ;111:112
 EQUB y113                              ;113:115
 EQUB y112                              ;112:115
 EQUB y115                              ;115:114
 EQUB y114                              ;114:113
 EQUB y112                              ;112:121
 EQUB y115                              ;115:116
 EQUB y116                              ;116:117
 EQUB y121                              ;121:125
 EQUB y125                              ;125:122
 EQUB y118                              ;118:119
 EQUB y119                              ;119:120
 EQUB y125                              ;125:126
 EQUB y126                              ;126:124
 EQUB y124                              ;124:123
 EQUB y125                              ;125:124

.landscape_vectors_x2_lo
 EQUB LO(x02)                           ;01:02
 EQUB LO(x03)                           ;02:03
 EQUB LO(x06)                           ;02:06
 EQUB LO(x05)                           ;04:05
 EQUB LO(x08)                           ;05:08
 EQUB LO(x07)                           ;05:07
 EQUB LO(x08)                           ;07:08
 EQUB LO(x09)                           ;08:09
 EQUB LO(x10)                           ;09:10
 EQUB LO(x11)                           ;09:11
 EQUB LO(x13)                           ;11:13
 EQUB LO(x12)                           ;11:12
 EQUB LO(x15)                           ;13:15
 EQUB LO(x15)                           ;12:15
 EQUB LO(x14)                           ;15:14
 EQUB LO(x13)                           ;14:13
 EQUB LO(x21)                           ;12:21
 EQUB LO(x16)                           ;15:16
 EQUB LO(x17)                           ;16:17
 EQUB LO(x25)                           ;21:25
 EQUB LO(x22)                           ;25:22
 EQUB LO(x19)                           ;18:19
 EQUB LO(x20)                           ;19:20
 EQUB LO(x26)                           ;25:26
 EQUB LO(x24)                           ;26:24
 EQUB LO(x23)                           ;24:23
 EQUB LO(x24)                           ;25:24
 EQUB LO(x27)                           ;26:27
 EQUB LO(x28)                           ;27:28
 EQUB LO(x29)                           ;27:29
 EQUB LO(x45)                           ;30:45
 EQUB LO(x31)                           ;45:31
 EQUB LO(x34)                           ;45:34
 EQUB LO(x32)                           ;32:32
 EQUB LO(x33)                           ;34:33
 EQUB LO(x36)                           ;35:36
 EQUB LO(x37)                           ;36:37
 EQUB LO(x38)                           ;37:38
 EQUB LO(x39)                           ;38:39
 EQUB LO(x41)                           ;38:41
 EQUB LO(x42)                           ;40:42
 EQUB LO(x43)                           ;42:43
 EQUB LO(x46)                           ;44:46
 EQUB LO(x47)                           ;46:47
 EQUB LO(x48)                           ;47:48
 EQUB LO(x50)                           ;49:50
 EQUB LO(x48)                           ;50:48
 EQUB LO(x51)                           ;48:51
 EQUB LO(x53)                           ;51:53
 EQUB LO(x54)                           ;52:54
 EQUB LO(x55)                           ;54:55
 EQUB LO(x57)                           ;55:57
 EQUB LO(x56)                           ;57:56
 EQUB LO(x58)                           ;56:58
 EQUB LO(x59)                           ;58:59
 EQUB LO(x60)                           ;59:60
 EQUB LO(x61)                           ;60:61
 EQUB LO(x63)                           ;60:63
 EQUB LO(x64)                           ;62:64
 EQUB LO(x67)                           ;64:67
 EQUB LO(x66)                           ;65:66
 EQUB LO(x68)                           ;66:68
 EQUB LO(x69)                           ;66:69
 EQUB LO(x71)                           ;70:71
 EQUB LO(x73)                           ;71:73
 EQUB LO(x74)                           ;72:74
 EQUB LO(x76)                           ;74:76
 EQUB LO(x77)                           ;75:77
 EQUB LO(x78)                           ;77:78
 EQUB LO(x79)                           ;78:79
 EQUB LO(x79)                           ;77:79
 EQUB LO(x83)                           ;80:83
 EQUB LO(x81)                           ;83:81
 EQUB LO(x84)                           ;82:84
 EQUB LO(x85)                           ;83:85
 EQUB LO(x86)                           ;85:86
 EQUB LO(x87)                           ;86:87
 EQUB LO(x102)                          ;101:102
 EQUB LO(x103)                          ;102:103
 EQUB LO(x106)                          ;102:106
 EQUB LO(x105)                          ;104:105
 EQUB LO(x108)                          ;105:108
 EQUB LO(x107)                          ;105:107
 EQUB LO(x108)                          ;107:108
 EQUB LO(x109)                          ;108:109
 EQUB LO(x110)                          ;109:110
 EQUB LO(x111)                          ;109:111
 EQUB LO(x113)                          ;111:113
 EQUB LO(x112)                          ;111:112
 EQUB LO(x115)                          ;113:115
 EQUB LO(x115)                          ;112:115
 EQUB LO(x114)                          ;115:114
 EQUB LO(x113)                          ;114:113
 EQUB LO(x121)                          ;112:121
 EQUB LO(x116)                          ;115:116
 EQUB LO(x117)                          ;116:117
 EQUB LO(x125)                          ;121:125
 EQUB LO(x122)                          ;125:122
 EQUB LO(x119)                          ;118:119
 EQUB LO(x120)                          ;119:120
 EQUB LO(x126)                          ;125:126
 EQUB LO(x124)                          ;126:124
 EQUB LO(x123)                          ;124:123
 EQUB LO(x124)                          ;125:124

.landscape_vectors_x2_hi
 EQUB HI(x02)                           ;01:02
 EQUB HI(x03)                           ;02:03
 EQUB HI(x06)                           ;02:06
 EQUB HI(x05)                           ;04:05
 EQUB HI(x08)                           ;05:08
 EQUB HI(x07)                           ;05:07
 EQUB HI(x08)                           ;07:08
 EQUB HI(x09)                           ;08:09
 EQUB HI(x10)                           ;09:10
 EQUB HI(x11)                           ;09:11
 EQUB HI(x13)                           ;11:13
 EQUB HI(x12)                           ;11:12
 EQUB HI(x15)                           ;13:15
 EQUB HI(x15)                           ;12:15
 EQUB HI(x14)                           ;15:14
 EQUB HI(x13)                           ;14:13
 EQUB HI(x21)                           ;12:21
 EQUB HI(x16)                           ;15:16
 EQUB HI(x17)                           ;16:17
 EQUB HI(x25)                           ;21:25
 EQUB HI(x22)                           ;25:22
 EQUB HI(x19)                           ;18:19
 EQUB HI(x20)                           ;19:20
 EQUB HI(x26)                           ;25:26
 EQUB HI(x24)                           ;26:24
 EQUB HI(x23)                           ;24:23
 EQUB HI(x24)                           ;25:24
 EQUB HI(x27)                           ;26:27
 EQUB HI(x28)                           ;27:28
 EQUB HI(x29)                           ;27:29
 EQUB HI(x45)                           ;30:45
 EQUB HI(x31)                           ;45:31
 EQUB HI(x34)                           ;45:34
 EQUB HI(x32)                           ;32:32
 EQUB HI(x33)                           ;34:33
 EQUB HI(x36)                           ;35:36
 EQUB HI(x37)                           ;36:37
 EQUB HI(x38)                           ;37:38
 EQUB HI(x39)                           ;38:39
 EQUB HI(x41)                           ;38:41
 EQUB HI(x42)                           ;40:42
 EQUB HI(x43)                           ;42:43
 EQUB HI(x46)                           ;44:46
 EQUB HI(x47)                           ;46:47
 EQUB HI(x48)                           ;47:48
 EQUB HI(x50)                           ;49:50
 EQUB HI(x48)                           ;50:48
 EQUB HI(x51)                           ;48:51
 EQUB HI(x53)                           ;51:53
 EQUB HI(x54)                           ;52:54
 EQUB HI(x55)                           ;54:55
 EQUB HI(x57)                           ;55:57
 EQUB HI(x56)                           ;57:56
 EQUB HI(x58)                           ;56:58
 EQUB HI(x59)                           ;58:59
 EQUB HI(x60)                           ;59:60
 EQUB HI(x61)                           ;60:61
 EQUB HI(x63)                           ;60:63
 EQUB HI(x64)                           ;62:64
 EQUB HI(x67)                           ;64:67
 EQUB HI(x66)                           ;65:66
 EQUB HI(x68)                           ;66:68
 EQUB HI(x69)                           ;66:69
 EQUB HI(x71)                           ;70:71
 EQUB HI(x73)                           ;71:73
 EQUB HI(x74)                           ;72:74
 EQUB HI(x76)                           ;74:76
 EQUB HI(x77)                           ;75:77
 EQUB HI(x78)                           ;77:78
 EQUB HI(x79)                           ;78:79
 EQUB HI(x79)                           ;77:79
 EQUB HI(x83)                           ;80:83
 EQUB HI(x81)                           ;83:81
 EQUB HI(x84)                           ;82:84
 EQUB HI(x85)                           ;83:85
 EQUB HI(x86)                           ;85:86
 EQUB HI(x87)                           ;86:87
 EQUB HI(x102)                          ;101:102
 EQUB HI(x103)                          ;102:103
 EQUB HI(x106)                          ;102:106
 EQUB HI(x105)                          ;104:105
 EQUB HI(x108)                          ;105:108
 EQUB HI(x107)                          ;105:107
 EQUB HI(x108)                          ;107:108
 EQUB HI(x109)                          ;108:109
 EQUB HI(x110)                          ;109:110
 EQUB HI(x111)                          ;109:111
 EQUB HI(x113)                          ;111:113
 EQUB HI(x112)                          ;111:112
 EQUB HI(x115)                          ;113:115
 EQUB HI(x115)                          ;112:115
 EQUB HI(x114)                          ;115:114
 EQUB HI(x113)                          ;114:113
 EQUB HI(x121)                          ;112:121
 EQUB HI(x116)                          ;115:116
 EQUB HI(x117)                          ;116:117
 EQUB HI(x125)                          ;121:125
 EQUB HI(x122)                          ;125:122
 EQUB HI(x119)                          ;118:119
 EQUB HI(x120)                          ;119:120
 EQUB HI(x126)                          ;125:126
 EQUB HI(x124)                          ;126:124
 EQUB HI(x123)                          ;124:123
 EQUB HI(x124)                          ;125:124

.landscape_vectors_y2
 EQUB y02                               ;01:02
 EQUB y03                               ;02:03
 EQUB y06                               ;02:06
 EQUB y05                               ;04:05
 EQUB y08                               ;05:08
 EQUB y07                               ;05:07
 EQUB y08                               ;07:08
 EQUB y09                               ;08:09
 EQUB y10                               ;09:10
 EQUB y11                               ;09:11
 EQUB y13                               ;11:13
 EQUB y12                               ;11:12
 EQUB y15                               ;13:15
 EQUB y15                               ;12:15
 EQUB y14                               ;15:14
 EQUB y13                               ;14:13
 EQUB y21                               ;12:21
 EQUB y16                               ;15:16
 EQUB y17                               ;16:17
 EQUB y25                               ;21:25
 EQUB y22                               ;25:22
 EQUB y19                               ;18:19
 EQUB y20                               ;19:20
 EQUB y26                               ;25:26
 EQUB y24                               ;26:24
 EQUB y23                               ;24:23
 EQUB y24                               ;25:24
 EQUB y27                               ;26:27
 EQUB y28                               ;27:28
 EQUB y29                               ;27:29
 EQUB y45                               ;30:45
 EQUB y31                               ;45:31
 EQUB y34                               ;45:34
 EQUB y32                               ;32:32
 EQUB y33                               ;34:33
 EQUB y36                               ;35:36
 EQUB y37                               ;36:37
 EQUB y38                               ;37:38
 EQUB y39                               ;38:39
 EQUB y41                               ;38:41
 EQUB y42                               ;40:42
 EQUB y43                               ;42:43
 EQUB y46                               ;44:46
 EQUB y47                               ;46:47
 EQUB y48                               ;47:48
 EQUB y50                               ;49:50
 EQUB y48                               ;50:48
 EQUB y51                               ;48:51
 EQUB y53                               ;51:53
 EQUB y54                               ;52:54
 EQUB y55                               ;54:55
 EQUB y57                               ;55:57
 EQUB y56                               ;57:56
 EQUB y58                               ;56:58
 EQUB y59                               ;58:59
 EQUB y60                               ;59:60
 EQUB y61                               ;60:61
 EQUB y63                               ;60:63
 EQUB y64                               ;62:64
 EQUB y67                               ;64:67
 EQUB y66                               ;65:66
 EQUB y68                               ;66:68
 EQUB y69                               ;66:69
 EQUB y71                               ;70:71
 EQUB y73                               ;71:73
 EQUB y74                               ;72:74
 EQUB y76                               ;74:76
 EQUB y77                               ;75:77
 EQUB y78                               ;77:78
 EQUB y79                               ;78:79
 EQUB y79                               ;77:79
 EQUB y83                               ;80:83
 EQUB y81                               ;83:81
 EQUB y84                               ;82:84
 EQUB y85                               ;83:85
 EQUB y86                               ;85:86
 EQUB y87                               ;86:87
 EQUB y102                              ;101:102
 EQUB y103                              ;102:103
 EQUB y106                              ;102:106
 EQUB y105                              ;104:105
 EQUB y108                              ;105:108
 EQUB y107                              ;105:107
 EQUB y108                              ;107:108
 EQUB y109                              ;108:109
 EQUB y110                              ;109:110
 EQUB y111                              ;109:111
 EQUB y113                              ;111:113
 EQUB y112                              ;111:112
 EQUB y115                              ;113:115
 EQUB y115                              ;112:115
 EQUB y114                              ;115:114
 EQUB y113                              ;114:113
 EQUB y121                              ;112:121
 EQUB y116                              ;115:116
 EQUB y117                              ;116:117
 EQUB y125                              ;121:125
 EQUB y122                              ;125:122
 EQUB y119                              ;118:119
 EQUB y120                              ;119:120
 EQUB y126                              ;125:126
 EQUB y124                              ;126:124
 EQUB y123                              ;124:123
 EQUB y124                              ;125:124

.render_radar_spot                      ;routine for radar spot only
 BIT particle_a
 BMI finished_particles                 ;not on so exit
 LDY particle_y_hi
 LDA particle_x_hi
 AND #&F8
 CLC
 ADC screen_access_y_lo,Y
 STA particle_address
 LDA screen_access_y_hi,Y
 ADC screen_hidden
 STA particle_address + &01
 LDA particle_x_hi
 AND #&07
 TAX
 LDY #&00
 LSR particle_a                         ;radar blip size?
 BCC small_radar_spot
 LDA double_pixel_mask,X
 TAX                                    ;save mask
 ORA (particle_address),Y
 STA (particle_address),Y
 INY
 LDA particle_y_hi
 AND #&07
 CMP #&07
 BNE following_row
 INC particle_address + &01
 LDY #&39
.following_row
 TXA
 ORA (particle_address),Y
 STA (particle_address),Y
 RTS
.small_radar_spot
 LDA pixel_mask,X
 ORA (particle_address),Y
 STA (particle_address),Y
.finished_particles
 RTS

.particle_maintenance                   ;handle all particles
 JSR generate_particle                  ;roll into routine below

.render_particles                       ;render all particles
 LDA m_tank_rotation + &01
 STA particle_work + &01
 LDA m_tank_rotation                    ;get my tank rotation and x16
 ASL A
 ROL particle_work + &01
 ASL A
 ROL particle_work + &01
 ASL A
 ROL particle_work + &01
 ASL A
 ROL particle_work + &01
 STA particle_work
 LDX #particle_number - &01
 STX particle_counter
.plot_particle
 LDA particle_a,X
 BMI particle_inactive                  ;this particle not active
 DEC particle_a,X						;particle in play
 BMI particle_inactive
 JSR place_particle
.particle_inactive 
 DEC particle_counter
 LDX particle_counter
 BNE plot_particle
 RTS

.place_particle
 LDA particle_a,X                       ;move particle
 LDA particle_y_vlo,X                   ;c=0
 ADC #particle_gravity                  ;adjust lava velocity with gravity
 STA particle_y_vlo,X
 BCC particle_finished
 INC particle_y_vhi,X
.particle_finished 
 LDA particle_x_lo,X                    ;now update
 CLC
 ADC particle_x_vlo,X
 STA particle_x_lo,X
 LDA particle_x_hi,X
 ADC particle_x_vhi,X
 BPL check_overflow_move                ;a = msb
 BCC particle_in_range                  ;wrap around within landscape limit
 ADC #HI((landscape_limit << 4) - &0F)
 BCC particle_in_range
.check_overflow_move
 CMP #HI(landscape_limit << 4)
 BCC particle_in_range
 SBC #HI(landscape_limit << 4)
.particle_in_range
 STA particle_x_hi,X                    ;store x coordinate, adjusted if required
 LDA particle_y_lo,X                    ;update y coordinate with y vector
 ADC particle_y_vlo,X
 STA particle_y_lo,X
 LDA particle_y_hi,X
 ADC particle_y_vhi,X
 STA particle_y_hi,X
 LDA particle_x_lo,X
 SEC
 SBC particle_work
 STA particle_x_store
 LDA particle_x_hi,X
 SBC particle_work + &01
 BCS render_test_window
 ADC #HI(landscape_limit << 4)          ;c=0 bring back into range for wrap around
.render_test_window
 STA particle_x_store + &01
 LDA particle_x_store
 CMP #LO(particle_window << 4)
 LDA particle_x_store + &01
 SBC #HI(particle_window << 4)          ;screen width test
 BCS no_render_particle                 ;next particle
 LDA particle_x_store + &01             ;bring x into range
 LSR A
 ROR particle_x_store
 LSR A
 ROR particle_x_store
 LSR A
 ROR particle_x_store
 LSR A
 ROR particle_x_store
 STA particle_x_store + &01
 LDY particle_y_hi,X
 STY particle_y_store
 LDA particle_x_store
 AND #&F8
 CLC
 ADC screen_access_y_lo,Y
 STA particle_address
 LDA screen_access_y_hi,Y
 ADC particle_x_store + &01
 ADC screen_hidden
 STA particle_address + &01
 LDA particle_x_store
 AND #&07
 LDY particle_a,X
 TAX
 CPY #&0C                               ;is it a small lava particle?
 LDY #&00
 BCC small_pixel                        ;yes
 LDA double_pixel_mask,X
 TAX                                    ;save mask
 ORA (particle_address),Y
 STA (particle_address),Y
 INY
 LDA particle_y_store
 AND #&07
 CMP #&07
 BNE next_entry_down
 INC particle_address + &01
 LDY #&39
.next_entry_down
 TXA
 BNE next_pixel                         ;always
.small_pixel
 LDA pixel_mask,X						;get pixel mask
.next_pixel
 ORA (particle_address),Y
 STA (particle_address),Y
.no_render_particle
 RTS

.generate_particle                      ;generate lava particle
 LDA m_tank_rotation_256                ;quick check if volcano on
 SEC
 SBC #screen_span_particle
 CMP #screen_span
 BCS no_render_particle                 ;no point as volcano off screen
 LDX #&05
.check_slots
 LDA particle_a,X
 BPL not_found_particle_slot            ;particle active
 LDA mathbox_random - &01,X
 AND #&03
 BNE generate_particle_exit
 LDA #large_particle
 STA particle_a,X                       ;set life time of particle
 LDA mathbox_random - &01,X
 AND #&03 << &03
 ADC #&01 << &03
 BIT mathbox_random                     ;set x vector
 BVC go_other_way
 EOR #&FF
.go_other_way
 STA particle_x_vlo,X
 ASL A                                  ;sign extend for high byte
 LDA #&00
 ADC #&FF
 EOR #&FF
 STA particle_x_vhi,X
 LDA mathbox_random + &01
 AND #&07
 ADC #&05
 LSR A
 ROR particle_y_vlo,X
 LSR A
 ROR particle_y_vlo,X
 EOR #&FF
 STA particle_y_vhi,X                   ;-ve y vector
 LDA #&5F                               ;particle y coordinate in the landscape
 STA particle_y_hi,X
 LDA #LO(particle_maintenance_x_coordinate << 4)
 STA particle_x_lo,X
 LDA #HI(particle_maintenance_x_coordinate << 4)
 STA particle_x_hi,X                    ;particle x coordinate high in the landscape
.generate_particle_exit
 RTS                                    ;generated a new particle so leave
.not_found_particle_slot
 DEX
 BNE check_slots
 RTS

.particle_system_reset                  ;reset all particles
 LDX #particle_number - &01
.reset_particle_id                      ;set top bit
 SEC
 ROR particle_a,X
 DEX
 BPL reset_particle_id
.none_to_erase
 RTS

; particle split
; &00 - &00 radar spot x 1
; &01 - &05 lava       x 5

 particle_number                        = &06

.particle_a                             SKIP particle_number
.particle_x_lo                          SKIP particle_number
.particle_x_hi                          SKIP particle_number
.particle_y_lo                          SKIP particle_number
.particle_y_hi                          SKIP particle_number
.particle_x_vlo                         SKIP particle_number
.particle_x_vhi                         SKIP particle_number
.particle_y_vlo                         SKIP particle_number
.particle_y_vhi                         SKIP particle_number

.moon
 LDA m_tank_rotation_256                ;quick check if moon on
 SEC
 SBC #screen_span_moon
 CMP #screen_span
 BCS none_to_erase
 LDA #LO(moon_x_coor)                   ;get the moon coordinate in the landscape
 SEC                                    ;subtract the current rotation and store
 SBC m_tank_rotation
 STA moon_new_x_coor
 TAX
 LDA #HI(moon_x_coor)
 SBC m_tank_rotation + &01
 STA moon_new_x_coor + &01
 CPX #LO(moon_left_sprite_edge)
 SBC #HI(moon_left_sprite_edge)
 BVC no_eor_left_edge
 EOR #&80
.no_eor_left_edge
 BPL moon_at_left
 LDA moon_new_x_coor + &01
 CLC
 ADC #HI(landscape_limit)
 STA moon_new_x_coor + &01
.moon_at_left
 LDA moon_new_x_coor + &01
 CPX #LO(moon_right_sprite_edge)
 SBC #HI(moon_right_sprite_edge)
 BVC no_inc_right_edge
 EOR #&80
.no_inc_right_edge
 BPL none_to_erase                      ;moon off screen
 TXA                                    ;set up sprite address
 AND #&07
 TAX
 LDA moon_data_address_lo,X
 STA moon_sprite_store
 LDA moon_data_address_hi,X
 STA moon_sprite_store + &01
 LDA #&04
 STA moon_counter
.moon_column
 BIT moon_new_x_coor + &01              ;-ve then next x coordinate
 BMI moon_add_eight
 LDA moon_new_x_coor
 AND #&F8
 CLC
 ADC #LO(moon_screen_offset)
 STA moon_screen_address
 LDA moon_new_x_coor + &01
 ADC #HI(moon_screen_offset)
 ADC screen_hidden
 STA moon_screen_address + &01
 LDA moon_sprite_store                  ;copy store into working address
 STA moon_sprite_address
 LDA moon_sprite_store + &01
 STA moon_sprite_address + &01
 LDX #&03                               ;place a sprite column on screen
.moon_stack
 LDY #&07
.moon_square
 LDA (moon_sprite_address),Y
 STA (moon_screen_address),Y
 DEY
 BPL moon_square
 LDA moon_screen_address
 CLC
 ADC #LO(screen_row)
 STA moon_screen_address
 LDA moon_screen_address + &01
 ADC #HI(screen_row)
 STA moon_screen_address + &01
 LDA moon_sprite_address
 ADC #&20                               ;c=0
 STA moon_sprite_address
 BCC no_inc_moon_sprite_address
 INC moon_sprite_address + &01
.no_inc_moon_sprite_address
 DEX
 BNE moon_stack
 LDA b_object_bounce_far                ;moon bounce, push data down screen
 BEQ moon_add_eight
 JSR moon_slide
.moon_add_eight
 LDA moon_new_x_coor                    ;add 8 to moon x coordinate
 CLC
 ADC #&08
 STA moon_new_x_coor
 BCC no_inc_moon_high
 INC moon_new_x_coor + &01
.no_inc_moon_high
 CMP #LO(moon_right_sprite_edge)        ;set up carry flag
 LDA moon_new_x_coor + &01
 SBC #HI(moon_right_sprite_edge)
 BVC no_inc_right_edge_01
 EOR #&80
.no_inc_right_edge_01
 BPL moon_off_screen                    ;moon off screen at right so exit
 LDA moon_sprite_store                  ;next sprite column
 CLC
 ADC #&08
 STA moon_sprite_store
 BCC no_inc_moon_sprite_store
 INC moon_sprite_store + &01
.no_inc_moon_sprite_store
 DEC moon_counter
 BNE moon_column
.moon_off_screen
 RTS

.moon_slide                             ;slide a moon column down a row for tank bump
 LDX #&03
.moon_slide_column
 LDA moon_screen_address
 SEC
 SBC #LO(screen_row)
 STA moon_screen_address
 LDA moon_screen_address + &01
 SBC #HI(screen_row)
 STA moon_screen_address + &01
 LDY #&06
.moon_slide_down
 LDA (moon_screen_address),Y
 INY
 STA (moon_screen_address),Y
 DEY
 DEY
 BPL moon_slide_down
 DEC moon_screen_address + &01
 DEC moon_screen_address + &01
 LDY #&C7
 LDA (moon_screen_address),Y
 INC moon_screen_address + &01
 INC moon_screen_address + &01
 LDY #&00
 STA (moon_screen_address),Y
 DEX
 BNE moon_slide_column
 RTS

.moon_data_address_lo
 EQUB LO(moon_data_00)
 EQUB LO(moon_data_01)
 EQUB LO(moon_data_02)
 EQUB LO(moon_data_03)
 EQUB LO(moon_data_04)
 EQUB LO(moon_data_05)
 EQUB LO(moon_data_06)
 EQUB LO(moon_data_07)
.moon_data_address_hi
 EQUB HI(moon_data_00)
 EQUB HI(moon_data_01)
 EQUB HI(moon_data_02)
 EQUB HI(moon_data_03)
 EQUB HI(moon_data_04)
 EQUB HI(moon_data_05)
 EQUB HI(moon_data_06)
 EQUB HI(moon_data_07)

.small_alphabet
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + pling_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + hash_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + n0_offset
 EQUW battlezone_sprites + n1_offset
 EQUW battlezone_sprites + n2_offset
 EQUW battlezone_sprites + n3_offset
 EQUW battlezone_sprites + n4_offset
 EQUW battlezone_sprites + n5_offset
 EQUW battlezone_sprites + n6_offset
 EQUW battlezone_sprites + n7_offset
 EQUW battlezone_sprites + n8_offset
 EQUW battlezone_sprites + n9_offset
 EQUW battlezone_sprites + copyright_offset
 EQUW battlezone_sprites + produced_offset
 EQUW battlezone_sprites + underscore_offset
 EQUW battlezone_sprites + tank_left_offset
 EQUW battlezone_sprites + tank_right_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + sa_offset
 EQUW battlezone_sprites + sb_offset
 EQUW battlezone_sprites + sc_offset
 EQUW battlezone_sprites + sd_offset
 EQUW battlezone_sprites + se_offset
 EQUW battlezone_sprites + sf_offset
 EQUW battlezone_sprites + sg_offset
 EQUW battlezone_sprites + sh_offset
 EQUW battlezone_sprites + si_offset
 EQUW battlezone_sprites + sj_offset
 EQUW battlezone_sprites + sk_offset
 EQUW battlezone_sprites + sl_offset
 EQUW battlezone_sprites + sm_offset
 EQUW battlezone_sprites + sn_offset
 EQUW battlezone_sprites + so_offset
 EQUW battlezone_sprites + sp_offset
 EQUW battlezone_sprites + sq_offset
 EQUW battlezone_sprites + sr_offset
 EQUW battlezone_sprites + ss_offset
 EQUW battlezone_sprites + st_offset
 EQUW battlezone_sprites + su_offset
 EQUW battlezone_sprites + sv_offset
 EQUW battlezone_sprites + sw_offset
 EQUW battlezone_sprites + sx_offset
 EQUW battlezone_sprites + sy_offset
 EQUW battlezone_sprites + sz_offset

.print_or
 LDA #ora_op                            ;set up ora instruction
 STA text_logic
 LDA #print_screen_work
 STA text_logic + &01
 BNE print_in                           ;always

.print
 LDA #nop_op                            ;nop opcode
 STA text_logic
 STA text_logic + &01
.print_in
 STX print_block_address                ;parameter block address
 STY print_block_address + &01
 LDY #&00                               ;text string address
 LDA (print_block_address),Y            ;string address
 INY
 STA print_direct + &01
 LDA (print_block_address),Y
 STA print_direct + &02
 INY                                    ;y=2
 LDA (print_block_address),Y            ;text x coor
 AND #&F8
 STA print_screen
 INY                                    ;y=3
 LDA (print_block_address),Y            ;text y coor
 TAX
 AND #&07
 STA print_y_reg
 LDA screen_access_y_lo,X
 AND #&F8
 CLC
 ADC print_screen
 STA print_screen
 LDA screen_access_y_hi,x
 ADC screen_hidden
 STA print_screen + &01
 LDX #&00                               ;set up text index
 PHP                                    ;flag for last character in print string
.print_direct_loop
 PLP
 BMI print_exit
.print_direct
 LDA print,X                            ;get text character
 PHP
 ASL A
 TAY
 LDA small_alphabet - (&20 * 2),Y
 STA print_load + &01
 LDA small_alphabet - (&20 * 2) + &01,Y
 STA print_load + &02
 LDA print_screen                       ;stored screen address
 STA print_screen_work
 LDA print_screen + &01
 STA print_screen_work + &01
 LDA print_y_reg
 STA print_y_work
 LDA #&00
 STA print_character_height
.print_character
 LDY print_character_height
.print_load
 LDA print_load,Y                       ;get text byte
 LDY print_y_work
.text_logic
 ORA (print_screen_work),Y
 STA (print_screen_work),Y
 INY                                    ;next pixel line down
 TYA
 AND #&07
 STA print_y_work
 BNE no_print_boundary                  ;crossed a screen row?
 LDA print_screen_work                  ;crossed row so calculate next row down
 CLC
 ADC #LO(screen_row)
 STA print_screen_work
 LDA print_screen_work + &01
 ADC #HI(screen_row)
 STA print_screen_work + &01
.no_print_boundary
 INC print_character_height
 LDA print_character_height
 CMP #&08
 BNE print_character
 LDA print_screen                       ;move to next character column
 ADC #&07                               ;c=1 from the compare
 STA print_screen
 BCC print_inx
 INC print_screen + &01
.print_inx
 INX
 BNE print_direct_loop                  ;always, carry on until -ve string terminator
.print_exit
 RTS
