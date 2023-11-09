; landscape
; render landscape, this is a series of line vectors x0/y0 to x1/y1 rotation around
; the y-axis becomes a simple subtraction of the x-coordinates then clipped when rendered
; note that y-coordinate clipping is not required
;
; moon, a sprite that enables the complex shape of the original to be represented
; volcano, generation and update of lava
; print, text stored/or'ed
;
; constants
 landscape_limit                        = &500
 landscape_horizon                      = 166    ;landscape horizon y coordinate 0,0 top left corner
 landscape_scale_factor                 = 0.31   ;landscape scaling factor, two decimal places 1 / (4096/1280)
 landscape_adjust_x                     = &8A
 landscape_wrap_x                       = &500 + landscape_adjust_x

 volcano_number                         = &05
 volcano_x_coordinate                   = &32A + landscape_adjust_x
 volcano_window                         = screen_row - &01
 volcano_gravity                        = &48
 volcano_large                          = &12

 volcano_screen_start                   = &70    ;visible portion of landscape volcano can be seen in
 volcano_screen_end                     = &D0

 moon_screen_start                      = &20    ;visible portion of landscape moon can be seen in
 moon_screen_end                        = &D0

 moon_x_coor                            = landscape_adjust_x
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

MACRO adjust_lo x
  xt0 = x + landscape_adjust_x
  IF xt0 >= landscape_wrap_x
    EQUB LO(xt0 - landscape_wrap_x)
  ELSE
    EQUB LO(xt0)
  ENDIF
ENDMACRO

MACRO adjust_hi x
  xt0 = x + landscape_adjust_x
  IF xt0 >= landscape_wrap_x
    EQUB HI((xt0 - landscape_wrap_x) * landscape_scale_factor)
  ELSE
    EQUB HI(xt0)
  ENDIF
ENDMACRO

MACRO adjust_lo_wrap x
  xt0 = x + &588
  EQUB LO(xt0)
ENDMACRO

MACRO adjust_hi_wrap x
  xt0 = x + &588
  EQUB HI(xt0)
ENDMACRO

MACRO adjust_lo_end x
  xt0 = x - &474
  EQUB LO(xt0)
ENDMACRO

MACRO adjust_hi_end x
  xt0 = x - &474
  EQUB HI(xt0)
ENDMACRO

; landscape vertices
 x01  = 0108 * landscape_scale_factor : y01 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x02  = 0158 * landscape_scale_factor : y02 = 125 - (landscape_horizon - 118) * landscape_scale_factor
 x03  = 0204 * landscape_scale_factor : y03 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x04  = 0192 * landscape_scale_factor : y04 = 125 - (landscape_horizon - 132) * landscape_scale_factor
 x05  = 0256 * landscape_scale_factor : y05 = 125 - (landscape_horizon - 100) * landscape_scale_factor
 x06  = 0286 * landscape_scale_factor : y06 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x07  = 0292 * landscape_scale_factor : y07 = 125 - (landscape_horizon - 135) * landscape_scale_factor
 x08  = 0412 * landscape_scale_factor : y08 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x09  = 0574 * landscape_scale_factor : y09 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x10  = 0640 * landscape_scale_factor : y10 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x11  = 0640 * landscape_scale_factor : y11 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x12  = 0732 * landscape_scale_factor : y12 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x13  = 0702 * landscape_scale_factor : y13 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x14  = 0732 * landscape_scale_factor : y14 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x15  = 0768 * landscape_scale_factor : y15 = 125 - (landscape_horizon - 128) * landscape_scale_factor
 x16  = 0800 * landscape_scale_factor : y16 = 125 - (landscape_horizon - 119) * landscape_scale_factor
 x17  = 0830 * landscape_scale_factor : y17 = 125 - (landscape_horizon - 130) * landscape_scale_factor
 x18  = 0860 * landscape_scale_factor : y18 = 125 - (landscape_horizon - 118) * landscape_scale_factor
 x19  = 0896 * landscape_scale_factor : y19 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x20  = 0930 * landscape_scale_factor : y20 = 125 - (landscape_horizon - 118) * landscape_scale_factor
 x21  = 0896 * landscape_scale_factor : y21 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x22  = 0932 * landscape_scale_factor : y22 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x23  = 0956 * landscape_scale_factor : y23 = 125 - (landscape_horizon - 128) * landscape_scale_factor
 x24  = 0986 * landscape_scale_factor : y24 = 125 - (landscape_horizon - 118) * landscape_scale_factor
 x25  = 1020 * landscape_scale_factor : y25 = 125 - (landscape_horizon - 156) * landscape_scale_factor
 x26  = 1052 * landscape_scale_factor : y26 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x27  = 1086 * landscape_scale_factor : y27 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x28  = 1148 * landscape_scale_factor : y28 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x29  = 1212 * landscape_scale_factor : y29 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x30  = 1372 * landscape_scale_factor : y30 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x31  = 1500 * landscape_scale_factor : y31 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x32  = 1534 * landscape_scale_factor : y32 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x33  = 1566 * landscape_scale_factor : y33 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x34  = 1500 * landscape_scale_factor : y34 = 125 - (landscape_horizon - 104) * landscape_scale_factor
 x35  = 1532 * landscape_scale_factor : y35 = 125 - (landscape_horizon - 132) * landscape_scale_factor
 x36  = 1564 * landscape_scale_factor : y36 = 125 - (landscape_horizon - 100) * landscape_scale_factor
 x37  = 1598 * landscape_scale_factor : y37 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x38  = 1660 * landscape_scale_factor : y38 = 125 - (landscape_horizon - 104) * landscape_scale_factor
 x39  = 1692 * landscape_scale_factor : y39 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x40  = 1694 * landscape_scale_factor : y40 = 125 - (landscape_horizon - 132) * landscape_scale_factor
 x41  = 1724 * landscape_scale_factor : y41 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x42  = 1724 * landscape_scale_factor : y42 = 125 - (landscape_horizon - 120) * landscape_scale_factor
 x43  = 1824 * landscape_scale_factor : y43 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x44  = 1936 * landscape_scale_factor : y44 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x45  = 1468 * landscape_scale_factor : y45 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x46  = 2110 * landscape_scale_factor : y46 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x47  = 2172 * landscape_scale_factor : y47 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x48  = 2392 * landscape_scale_factor : y48 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x49  = 2300 * landscape_scale_factor : y49 = 125 - (landscape_horizon - 154) * landscape_scale_factor
 x50  = 2364 * landscape_scale_factor : y50 = 125 - (landscape_horizon - 122) * landscape_scale_factor
 x51  = 2492 * landscape_scale_factor : y51 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x52  = 2542 * landscape_scale_factor : y52 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x53  = 2558 * landscape_scale_factor : y53 = 125 - (landscape_horizon - 142) * landscape_scale_factor
 x54  = 2600 * landscape_scale_factor : y54 = 125 - (landscape_horizon - 076) * landscape_scale_factor
 x55  = 2608 * landscape_scale_factor : y55 = 125 - (landscape_horizon - 080) * landscape_scale_factor
 x56  = 2618 * landscape_scale_factor : y56 = 125 - (landscape_horizon - 082) * landscape_scale_factor
 x57  = 2612 * landscape_scale_factor : y57 = 125 - (landscape_horizon - 078) * landscape_scale_factor
 x58  = 2624 * landscape_scale_factor : y58 = 125 - (landscape_horizon - 076) * landscape_scale_factor
 x59  = 2682 * landscape_scale_factor : y59 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x60  = 2780 * landscape_scale_factor : y60 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x61  = 2844 * landscape_scale_factor : y61 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x62  = 2844 * landscape_scale_factor : y62 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x63  = 2910 * landscape_scale_factor : y63 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x64  = 2910 * landscape_scale_factor : y64 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x65  = 2988 * landscape_scale_factor : y65 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x66  = 3042 * landscape_scale_factor : y66 = 125 - (landscape_horizon - 118) * landscape_scale_factor
 x67  = 3042 * landscape_scale_factor : y67 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x68  = 3070 * landscape_scale_factor : y68 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x69  = 3144 * landscape_scale_factor : y69 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x70  = 3220 * landscape_scale_factor : y70 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x71  = 3550 * landscape_scale_factor : y71 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x72  = 3602 * landscape_scale_factor : y72 = 125 - (landscape_horizon - 154) * landscape_scale_factor
 x73  = 3618 * landscape_scale_factor : y73 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x74  = 3652 * landscape_scale_factor : y74 = 125 - (landscape_horizon - 104) * landscape_scale_factor
 x75  = 3662 * landscape_scale_factor : y75 = 125 - (landscape_horizon - 142) * landscape_scale_factor
 x76  = 3678 * landscape_scale_factor : y76 = 125 - (landscape_horizon - 130) * landscape_scale_factor
 x77  = 3742 * landscape_scale_factor : y77 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x78  = 3774 * landscape_scale_factor : y78 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x79  = 3806 * landscape_scale_factor : y79 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x80  = 3734 * landscape_scale_factor : y80 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x81  = 3934 * landscape_scale_factor : y81 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x82  = 3904 * landscape_scale_factor : y82 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x83  = 3870 * landscape_scale_factor : y83 = 125 - (landscape_horizon - 102) * landscape_scale_factor
 x84  = 3974 * landscape_scale_factor : y84 = 125 - (landscape_horizon - 166) * landscape_scale_factor
 x85  = 3934 * landscape_scale_factor : y85 = 125 - (landscape_horizon - 134) * landscape_scale_factor
 x86  = 4000 * landscape_scale_factor : y86 = 125 - (landscape_horizon - 152) * landscape_scale_factor
 x87  = 4094 * landscape_scale_factor : y87 = 125 - (landscape_horizon - 166) * landscape_scale_factor

;these two blocks are landscape used for start/end wrap around
 x101  = 0108 * landscape_scale_factor
 x102  = 0158 * landscape_scale_factor
 x103  = 0204 * landscape_scale_factor
 x104  = 0192 * landscape_scale_factor
 x105  = 0256 * landscape_scale_factor
 x106  = 0286 * landscape_scale_factor
 x107  = 0292 * landscape_scale_factor
 x108  = 0412 * landscape_scale_factor
 x109  = 0574 * landscape_scale_factor
 x110  = 0640 * landscape_scale_factor
 x175  = 3662 * landscape_scale_factor
 x177  = 3742 * landscape_scale_factor
 x178  = 3774 * landscape_scale_factor
 x179  = 3806 * landscape_scale_factor
 x180  = 3734 * landscape_scale_factor
 x181  = 3934 * landscape_scale_factor
 x182  = 3904 * landscape_scale_factor
 x183  = 3870 * landscape_scale_factor
 x184  = 3974 * landscape_scale_factor
 x185  = 3934 * landscape_scale_factor
 x186  = 4000 * landscape_scale_factor
 x187  = 4094 * landscape_scale_factor

.landscape_segment_x0_lo
 adjust_lo x01                          ;01:02
 adjust_lo x02                          ;02:03
 adjust_lo x02                          ;02:06
 adjust_lo x04                          ;04:05
 adjust_lo x05                          ;05:08
 adjust_lo x05                          ;05:07
 adjust_lo x07                          ;07:08
 adjust_lo x08                          ;08:09
 adjust_lo x09                          ;09:10
 adjust_lo x09                          ;09:11
 adjust_lo x11                          ;11:13
 adjust_lo x11                          ;11:12
 adjust_lo x13                          ;13:15
 adjust_lo x12                          ;12:15
 adjust_lo x15                          ;15:14
 adjust_lo x14                          ;14:13
 adjust_lo x12                          ;12:21
 adjust_lo x15                          ;15:16
 adjust_lo x16                          ;16:17
 adjust_lo x21                          ;21:25
 adjust_lo x25                          ;25:22
 adjust_lo x18                          ;18:19
 adjust_lo x19                          ;19:20
 adjust_lo x25                          ;25:26
 adjust_lo x26                          ;26:24
 adjust_lo x24                          ;24:23
 adjust_lo x25                          ;25:24
 adjust_lo x26                          ;26:27
 adjust_lo x27                          ;27:28
 adjust_lo x27                          ;27:29
 adjust_lo x30                          ;30:45
 adjust_lo x45                          ;45:31
 adjust_lo x45                          ;45:34
 adjust_lo x34                          ;34:32
 adjust_lo x34                          ;34:33
 adjust_lo x35                          ;35:36
 adjust_lo x36                          ;36:37
 adjust_lo x37                          ;37:38
 adjust_lo x38                          ;38:39
 adjust_lo x38                          ;38:41
 adjust_lo x40                          ;40:42
 adjust_lo x42                          ;42:43
 adjust_lo x44                          ;44:46
 adjust_lo x46                          ;46:47
 adjust_lo x47                          ;47:48
 adjust_lo x49                          ;49:50
 adjust_lo x50                          ;50:48
 adjust_lo x48                          ;48:51
 adjust_lo x51                          ;51:53
 adjust_lo x52                          ;52:54
 adjust_lo x54                          ;54:55
 adjust_lo x55                          ;55:57
 adjust_lo x57                          ;57:56
 adjust_lo x56                          ;56:58
 adjust_lo x58                          ;58:59
 adjust_lo x59                          ;59:60
 adjust_lo x60                          ;60:61
 adjust_lo x60                          ;60:63
 adjust_lo x62                          ;62:64
 adjust_lo x64                          ;64:67
 adjust_lo x65                          ;65:66
 adjust_lo x66                          ;66:68
 adjust_lo x66                          ;66:69
 adjust_lo x70                          ;70:71
 adjust_lo x71                          ;71:73
 adjust_lo x72                          ;72:74
 adjust_lo x74                          ;74:76
 adjust_lo x75                          ;75:77
 adjust_lo x77                          ;77:78
 adjust_lo x78                          ;78:79
 adjust_lo x77                          ;77:79
 adjust_lo x80                          ;80:83
 adjust_lo x83                          ;83:81
 adjust_lo x82                          ;82:84
 adjust_lo x83                          ;83:85
 adjust_lo x85                          ;85:86
 adjust_lo x86                          ;86:87
 adjust_lo_wrap x101                    ;101:102
 adjust_lo_wrap x102                    ;102:103
 adjust_lo_wrap x102                    ;102:106
 adjust_lo_wrap x104                    ;104:105
 adjust_lo_wrap x105                    ;105:108
 adjust_lo_wrap x105                    ;105:107
 adjust_lo_wrap x107                    ;107:108
 adjust_lo_wrap x108                    ;108:109
 adjust_lo_wrap x109                    ;109:110
 adjust_lo_end x175                     ;175:177
 adjust_lo_end x177                     ;177:178
 adjust_lo_end x178                     ;178:179
 adjust_lo_end x177                     ;177:179
 adjust_lo_end x180                     ;180:183
 adjust_lo_end x183                     ;183:181
 adjust_lo_end x182                     ;182:184
 adjust_lo_end x183                     ;183:185
 adjust_lo_end x185                     ;185:186
 adjust_lo_end x186                     ;186:187

.landscape_segment_x0_hi
 adjust_hi x01                          ;01:02
 adjust_hi x02                          ;02:03
 adjust_hi x02                          ;02:06
 adjust_hi x04                          ;04:05
 adjust_hi x05                          ;05:08
 adjust_hi x05                          ;05:07
 adjust_hi x07                          ;07:08
 adjust_hi x08                          ;08:09
 adjust_hi x09                          ;09:10
 adjust_hi x09                          ;09:11
 adjust_hi x11                          ;11:13
 adjust_hi x11                          ;11:12
 adjust_hi x13                          ;13:15
 adjust_hi x12                          ;12:15
 adjust_hi x15                          ;15:14
 adjust_hi x14                          ;14:13
 adjust_hi x12                          ;12:21
 adjust_hi x15                          ;15:16
 adjust_hi x16                          ;16:17
 adjust_hi x21                          ;21:25
 adjust_hi x25                          ;25:22
 adjust_hi x18                          ;18:19
 adjust_hi x19                          ;19:20
 adjust_hi x25                          ;25:26
 adjust_hi x26                          ;26:24
 adjust_hi x24                          ;24:23
 adjust_hi x25                          ;25:24
 adjust_hi x26                          ;26:27
 adjust_hi x27                          ;27:28
 adjust_hi x27                          ;27:29
 adjust_hi x30                          ;30:45
 adjust_hi x45                          ;45:31
 adjust_hi x45                          ;45:34
 adjust_hi x34                          ;34:32
 adjust_hi x34                          ;34:33
 adjust_hi x35                          ;35:36
 adjust_hi x36                          ;36:37
 adjust_hi x37                          ;37:38
 adjust_hi x38                          ;38:39
 adjust_hi x38                          ;38:41
 adjust_hi x40                          ;40:42
 adjust_hi x42                          ;42:43
 adjust_hi x44                          ;44:46
 adjust_hi x46                          ;46:47
 adjust_hi x47                          ;47:48
 adjust_hi x49                          ;49:50
 adjust_hi x50                          ;50:48
 adjust_hi x48                          ;48:51
 adjust_hi x51                          ;51:53
 adjust_hi x52                          ;52:54
 adjust_hi x54                          ;54:55
 adjust_hi x55                          ;55:57
 adjust_hi x57                          ;57:56
 adjust_hi x56                          ;56:58
 adjust_hi x58                          ;58:59
 adjust_hi x59                          ;59:60
 adjust_hi x60                          ;60:61
 adjust_hi x60                          ;60:63
 adjust_hi x62                          ;62:64
 adjust_hi x64                          ;64:67
 adjust_hi x65                          ;65:66
 adjust_hi x66                          ;66:68
 adjust_hi x66                          ;66:69
 adjust_hi x70                          ;70:71
 adjust_hi x71                          ;71:73
 adjust_hi x72                          ;72:74
 adjust_hi x74                          ;74:76
 adjust_hi x75                          ;75:77
 adjust_hi x77                          ;77:78
 adjust_hi x78                          ;78:79
 adjust_hi x77                          ;77:79
 adjust_hi x80                          ;80:83
 adjust_hi x83                          ;83:81
 adjust_hi x82                          ;82:84
 adjust_hi x83                          ;83:85
 adjust_hi x85                          ;85:86
 adjust_hi x86                          ;86:87
 adjust_hi_wrap x101                    ;101:102
 adjust_hi_wrap x102                    ;102:103
 adjust_hi_wrap x102                    ;102:106
 adjust_hi_wrap x104                    ;104:105
 adjust_hi_wrap x105                    ;105:108
 adjust_hi_wrap x105                    ;105:107
 adjust_hi_wrap x107                    ;107:108
 adjust_hi_wrap x108                    ;108:109
 adjust_hi_wrap x109                    ;109:110
 adjust_hi_end x175                     ;175:177
 adjust_hi_end x177                     ;177:178
 adjust_hi_end x178                     ;178:179
 adjust_hi_end x177                     ;177:179
 adjust_hi_end x180                     ;180:183
 adjust_hi_end x183                     ;183:181
 adjust_hi_end x182                     ;182:184
 adjust_hi_end x183                     ;183:185
 adjust_hi_end x185                     ;185:186
 adjust_hi_end x186                     ;186:187

.landscape_segment_y0
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
 EQUB y01                               ;101:102
 EQUB y02                               ;102:103
 EQUB y02                               ;102:106
 EQUB y04                               ;104:105
 EQUB y05                               ;105:108
 EQUB y05                               ;105:107
 EQUB y07                               ;107:108
 EQUB y08                               ;108:109
 EQUB y09                               ;109:110
 EQUB y75                               ;175:177
 EQUB y77                               ;177:178
 EQUB y78                               ;178:179
 EQUB y77                               ;177:179
 EQUB y80                               ;180:183
 EQUB y83                               ;183:181
 EQUB y82                               ;182:184
 EQUB y83                               ;183:185
 EQUB y85                               ;185:186
 EQUB y86                               ;186:187

.landscape_segment_x1_lo
 adjust_lo x02                          ;01:02
 adjust_lo x03                          ;02:03
 adjust_lo x06                          ;02:06
 adjust_lo x05                          ;04:05
 adjust_lo x08                          ;05:08
 adjust_lo x07                          ;05:07
 adjust_lo x08                          ;07:08
 adjust_lo x09                          ;08:09
 adjust_lo x10                          ;09:10
 adjust_lo x11                          ;09:11
 adjust_lo x13                          ;11:13
 adjust_lo x12                          ;11:12
 adjust_lo x15                          ;13:15
 adjust_lo x15                          ;12:15
 adjust_lo x14                          ;15:14
 adjust_lo x13                          ;14:13
 adjust_lo x21                          ;12:21
 adjust_lo x16                          ;15:16
 adjust_lo x17                          ;16:17
 adjust_lo x25                          ;21:25
 adjust_lo x22                          ;25:22
 adjust_lo x19                          ;18:19
 adjust_lo x20                          ;19:20
 adjust_lo x26                          ;25:26
 adjust_lo x24                          ;26:24
 adjust_lo x23                          ;24:23
 adjust_lo x24                          ;25:24
 adjust_lo x27                          ;26:27
 adjust_lo x28                          ;27:28
 adjust_lo x29                          ;27:29
 adjust_lo x45                          ;30:45
 adjust_lo x31                          ;45:31
 adjust_lo x34                          ;45:34
 adjust_lo x32                          ;32:32
 adjust_lo x33                          ;34:33
 adjust_lo x36                          ;35:36
 adjust_lo x37                          ;36:37
 adjust_lo x38                          ;37:38
 adjust_lo x39                          ;38:39
 adjust_lo x41                          ;38:41
 adjust_lo x42                          ;40:42
 adjust_lo x43                          ;42:43
 adjust_lo x46                          ;44:46
 adjust_lo x47                          ;46:47
 adjust_lo x48                          ;47:48
 adjust_lo x50                          ;49:50
 adjust_lo x48                          ;50:48
 adjust_lo x51                          ;48:51
 adjust_lo x53                          ;51:53
 adjust_lo x54                          ;52:54
 adjust_lo x55                          ;54:55
 adjust_lo x57                          ;55:57
 adjust_lo x56                          ;57:56
 adjust_lo x58                          ;56:58
 adjust_lo x59                          ;58:59
 adjust_lo x60                          ;59:60
 adjust_lo x61                          ;60:61
 adjust_lo x63                          ;60:63
 adjust_lo x64                          ;62:64
 adjust_lo x67                          ;64:67
 adjust_lo x66                          ;65:66
 adjust_lo x68                          ;66:68
 adjust_lo x69                          ;66:69
 adjust_lo x71                          ;70:71
 adjust_lo x73                          ;71:73
 adjust_lo x74                          ;72:74
 adjust_lo x76                          ;74:76
 adjust_lo x77                          ;75:77
 adjust_lo x78                          ;77:78
 adjust_lo x79                          ;78:79
 adjust_lo x79                          ;77:79
 adjust_lo x83                          ;80:83
 adjust_lo x81                          ;83:81
 adjust_lo x84                          ;82:84
 adjust_lo x85                          ;83:85
 adjust_lo x86                          ;85:86
 adjust_lo x87                          ;86:87
 adjust_lo_wrap x102                    ;101:102
 adjust_lo_wrap x103                    ;102:103
 adjust_lo_wrap x106                    ;102:106
 adjust_lo_wrap x105                    ;104:105
 adjust_lo_wrap x108                    ;105:108
 adjust_lo_wrap x107                    ;105:107
 adjust_lo_wrap x108                    ;107:108
 adjust_lo_wrap x109                    ;108:109
 adjust_lo_wrap x110                    ;109:110
 adjust_lo_end x177                     ;175:177
 adjust_lo_end x178                     ;177:178
 adjust_lo_end x179                     ;178:179
 adjust_lo_end x179                     ;177:179
 adjust_lo_end x183                     ;180:183
 adjust_lo_end x181                     ;183:181
 adjust_lo_end x184                     ;182:184
 adjust_lo_end x185                     ;183:185
 adjust_lo_end x186                     ;185:186
 adjust_lo_end x187                     ;186:187

.landscape_segment_x1_hi
 adjust_hi x02                          ;01:02
 adjust_hi x03                          ;02:03
 adjust_hi x06                          ;02:06
 adjust_hi x05                          ;04:05
 adjust_hi x08                          ;05:08
 adjust_hi x07                          ;05:07
 adjust_hi x08                          ;07:08
 adjust_hi x09                          ;08:09
 adjust_hi x10                          ;09:10
 adjust_hi x11                          ;09:11
 adjust_hi x13                          ;11:13
 adjust_hi x12                          ;11:12
 adjust_hi x15                          ;13:15
 adjust_hi x15                          ;12:15
 adjust_hi x14                          ;15:14
 adjust_hi x13                          ;14:13
 adjust_hi x21                          ;12:21
 adjust_hi x16                          ;15:16
 adjust_hi x17                          ;16:17
 adjust_hi x25                          ;21:25
 adjust_hi x22                          ;25:22
 adjust_hi x19                          ;18:19
 adjust_hi x20                          ;19:20
 adjust_hi x26                          ;25:26
 adjust_hi x24                          ;26:24
 adjust_hi x23                          ;24:23
 adjust_hi x24                          ;25:24
 adjust_hi x27                          ;26:27
 adjust_hi x28                          ;27:28
 adjust_hi x29                          ;27:29
 adjust_hi x45                          ;30:45
 adjust_hi x31                          ;45:31
 adjust_hi x34                          ;45:34
 adjust_hi x32                          ;32:32
 adjust_hi x33                          ;34:33
 adjust_hi x36                          ;35:36
 adjust_hi x37                          ;36:37
 adjust_hi x38                          ;37:38
 adjust_hi x39                          ;38:39
 adjust_hi x41                          ;38:41
 adjust_hi x42                          ;40:42
 adjust_hi x43                          ;42:43
 adjust_hi x46                          ;44:46
 adjust_hi x47                          ;46:47
 adjust_hi x48                          ;47:48
 adjust_hi x50                          ;49:50
 adjust_hi x48                          ;50:48
 adjust_hi x51                          ;48:51
 adjust_hi x53                          ;51:53
 adjust_hi x54                          ;52:54
 adjust_hi x55                          ;54:55
 adjust_hi x57                          ;55:57
 adjust_hi x56                          ;57:56
 adjust_hi x58                          ;56:58
 adjust_hi x59                          ;58:59
 adjust_hi x60                          ;59:60
 adjust_hi x61                          ;60:61
 adjust_hi x63                          ;60:63
 adjust_hi x64                          ;62:64
 adjust_hi x67                          ;64:67
 adjust_hi x66                          ;65:66
 adjust_hi x68                          ;66:68
 adjust_hi x69                          ;66:69
 adjust_hi x71                          ;70:71
 adjust_hi x73                          ;71:73
 adjust_hi x74                          ;72:74
 adjust_hi x76                          ;74:76
 adjust_hi x77                          ;75:77
 adjust_hi x78                          ;77:78
 adjust_hi x79                          ;78:79
 adjust_hi x79                          ;77:79
 adjust_hi x83                          ;80:83
 adjust_hi x81                          ;83:81
 adjust_hi x84                          ;82:84
 adjust_hi x85                          ;83:85
 adjust_hi x86                          ;85:86
 adjust_hi x87                          ;86:87
 adjust_hi_wrap x102                    ;101:102
 adjust_hi_wrap x103                    ;102:103
 adjust_hi_wrap x106                    ;102:106
 adjust_hi_wrap x105                    ;104:105
 adjust_hi_wrap x108                    ;105:108
 adjust_hi_wrap x107                    ;105:107
 adjust_hi_wrap x108                    ;107:108
 adjust_hi_wrap x109                    ;108:109
 adjust_hi_wrap x110                    ;109:110
 adjust_hi_end x177                     ;175:177
 adjust_hi_end x178                     ;177:178
 adjust_hi_end x179                     ;178:179
 adjust_hi_end x179                     ;177:179
 adjust_hi_end x183                     ;180:183
 adjust_hi_end x181                     ;183:181
 adjust_hi_end x184                     ;182:184
 adjust_hi_end x185                     ;183:185
 adjust_hi_end x186                     ;185:186
 adjust_hi_end x187                     ;186:187

.landscape_segment_y1
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
 EQUB y02                               ;101:102
 EQUB y03                               ;102:103
 EQUB y06                               ;102:106
 EQUB y05                               ;104:105
 EQUB y08                               ;105:108
 EQUB y07                               ;105:107
 EQUB y08                               ;107:108
 EQUB y09                               ;108:109
 EQUB y10                               ;109:110
 EQUB y77                               ;175:177
 EQUB y78                               ;177:178
 EQUB y79                               ;178:179
 EQUB y79                               ;177:179
 EQUB y83                               ;180:183
 EQUB y81                               ;183:181
 EQUB y84                               ;182:184
 EQUB y85                               ;183:185
 EQUB y86                               ;185:186
 EQUB y87                               ;186:187

.update_volcano                         ;handle lava
 LDA m_tank_rotation_512 + &01          ;quick check if volcano in view
 SBC #volcano_screen_start              ;ignore carry status, this is a coarse check
 CMP #volcano_screen_end - volcano_screen_start
 BCS volcano_exit                       ;no point as volcano off screen
 JSR generate_volcano
 LDA m_tank_rotation + &01              ;render lava
 STA volcano_work + &01
 LDA m_tank_rotation                    ;get my tank rotation and x16
 ASL A
 ROL volcano_work + &01
 ASL A
 ROL volcano_work + &01
 ASL A
 ROL volcano_work + &01
 ASL A
 ROL volcano_work + &01
 STA volcano_work
 LDX #&04
.plot_volcano
 DEC volcano_status,X		            ;volcano in play
 BMI volcano_inactive
 STX volcano_counter
 JSR place_volcano
 LDX volcano_counter
.volcano_inactive
 DEX
 BPL plot_volcano
.volcano_exit
 RTS

.place_volcano
 LDA volcano_y_vlo,X
 ADC #volcano_gravity                   ;adjust lava velocity with gravity
 STA volcano_y_vlo,X
 BCC volcano_finished
 INC volcano_y_vhi,X
.volcano_finished
 LDA volcano_x_lo,X                     ;now update
 ADC volcano_x_vlo,X
 STA volcano_x_lo,X
 TAY
 LDA volcano_x_hi,X
 ADC volcano_x_vhi,X
 STA volcano_x_hi,X                     ;store x coordinate
 LDA volcano_y_lo,X                     ;update y coordinate with y vector
 ADC volcano_y_vlo,X
 STA volcano_y_lo,X
 LDA volcano_y_hi,X
 ADC volcano_y_vhi,X
 STA volcano_y_hi,X
 TYA
 SBC volcano_work
 TAY
 LDA volcano_x_hi,X
 SBC volcano_work + &01
 STA volcano_x_store + &01
 CPY #LO(volcano_window << 4)
 SBC #HI(volcano_window << 4)           ;screen width test
 BCS no_render_volcano                  ;next volcano
 TYA
 LSR volcano_x_store + &01              ;bring x into range
 ROR A
 LSR volcano_x_store + &01
 ROR A
 LSR volcano_x_store + &01
 ROR A
 LSR volcano_x_store + &01
 ROR A
 STA volcano_x_store
 LDY volcano_y_hi,X
 STY volcano_y_store
 AND #&F8
 CLC
 ADC screen_access_y_lo,Y
 STA volcano_address
 LDA screen_access_y_hi,Y
 ADC volcano_x_store + &01
 ADC screen_hidden
 STA volcano_address + &01
 LDA volcano_x_store
 AND #&07
 LDY volcano_status,X
 TAX
 CPY #&0C                               ;is it a small lava volcano?
 LDY #&00
 BCC small_pixel                        ;yes
 LDA double_pixel_mask,X
 TAX                                    ;save mask
 ORA (volcano_address),Y
 STA (volcano_address),Y
 INY
 LDA volcano_y_store
 AND #&07
 CMP #&07
 BNE next_entry_down
 INC volcano_address + &01
 LDY #&39
.next_entry_down
 TXA
 BNE next_pixel                         ;always
.small_pixel
 LDA pixel_mask,X						;get pixel mask
.next_pixel
 ORA (volcano_address),Y
 STA (volcano_address),Y
.no_render_volcano
 RTS

.generate_volcano                       ;generate lava
 LDX #&04
.check_slots
 LDA volcano_status,X
 BPL not_found_volcano_slot             ;volcano active
 LDA mathbox_random
 AND #&03
 BNE generate_volcano_exit
 LDA #volcano_large
 STA volcano_status,X                   ;set life time of lava chunk
 LDA mathbox_random + &01
 AND #&03 << &03
 ADC #&01 << &03
 BIT mathbox_random                     ;set x vector
 BVC go_other_way
 EOR #&FF
.go_other_way
 STA volcano_x_vlo,X
 ASL A                                  ;sign extend for high byte
 LDA #&00
 ADC #&FF
 EOR #&FF
 STA volcano_x_vhi,X
 LDA mathbox_random + &02
 AND #&07
 ADC #&05
 LSR A
 ROR volcano_y_vlo,X
 LSR A
 ROR volcano_y_vlo,X
 EOR #&FF
 STA volcano_y_vhi,X                    ;-ve y vector
 LDA #&5F                               ;volcano y coordinate in the landscape
 STA volcano_y_hi,X
 LDA #LO(volcano_x_coordinate << 4)
 STA volcano_x_lo,X
 LDA #HI(volcano_x_coordinate << 4)
 STA volcano_x_hi,X                     ;volcano x coordinate high in the landscape
.generate_volcano_exit
 RTS                                    ;generated a new lava so leave
.not_found_volcano_slot
 DEX
 BPL check_slots
.none_to_erase
 RTS

 MACRO fill_data number
 FOR j, 1, number
   EQUB &FF
 NEXT
 ENDMACRO

.volcano_status                         fill_data volcano_number
 volcano_x_lo                           = vertices_table_end + &00 * volcano_number
 volcano_x_hi                           = vertices_table_end + &01 * volcano_number
 volcano_y_lo                           = vertices_table_end + &02 * volcano_number
 volcano_y_hi                           = vertices_table_end + &03 * volcano_number
 volcano_x_vlo                          = vertices_table_end + &04 * volcano_number
 volcano_x_vhi                          = vertices_table_end + &05 * volcano_number
 volcano_y_vlo                          = vertices_table_end + &06 * volcano_number
 volcano_y_vhi                          = vertices_table_end + &07 * volcano_number
 volcano_end                            = vertices_table_end + &08 * volcano_number

.moon
 LDA m_tank_rotation_512 + &01          ;quick check if moon on
 SBC #moon_screen_start                 ;ignore carry status, this is a coarse check
 CMP #moon_screen_end - moon_screen_start
 BCC none_to_erase
 LDA #LO(moon_x_coor)                   ;get the moon coordinate in the landscape
 SBC m_tank_rotation                    ;subtract the current rotation and store, c=1
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
 BVC no_eor_right_edge
 EOR #&80
.no_eor_right_edge
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
.landscape_exit
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

.landscape
 LDA #&00                               ;clear high bytes
 STA graphic_y_00 + &01
 STA graphic_y_01 + &01
 LDA #landscape_segment_x0_hi - landscape_segment_x0_lo
 STA landscape_segment_ix
.landscape_draw
 DEC landscape_segment_ix               ;index for vectors
 BMI landscape_exit
 LDX landscape_segment_ix
 LDA landscape_segment_x0_lo,X          ;calculate adjusted x0
 SEC
 SBC m_tank_rotation
 STA graphic_x_00
 LDA landscape_segment_x0_hi,X
 SBC m_tank_rotation + &01
 STA graphic_x_00 + &01
 LDA landscape_segment_x1_lo,X          ;calculate adjusted x1
 SEC
 SBC m_tank_rotation
 STA graphic_x_01
 LDA landscape_segment_x1_hi,X
 SBC m_tank_rotation + &01
 STA graphic_x_01 + &01
 LDA graphic_x_00                       ;check if both endpoints off screen
 CMP #LO(screen_row)                    ;either side of window
 LDA graphic_x_00 + &01
 SBC #HI(screen_row)
 ROL landscape_result
 LDA graphic_x_01
 CMP #LO(screen_row)
 LDA graphic_x_01 + &01
 SBC #HI(screen_row)
 ROL A
 AND landscape_result
 LSR A
 BCS landscape_draw                     ;not in view
 LDY landscape_segment_y0,X             ;relevant line draw routine
 LDA landscape_segment_y1,X
 TAX
 LDA b_object_bounce_far
 BEQ no_landscape_bounce                ;landscape bounce for explosion/collision
 INY
 INX
.no_landscape_bounce
 STY graphic_y_00                       ;store y coordinates
 STX graphic_y_01
 LDA graphic_x_00                       ;check x00, x01 in 32 - 287
 CMP #&20
 LDA graphic_x_00 + &01
 SBC #&00
 STA landscape_result
 LDA graphic_x_01
 CMP #&20
 LDA graphic_x_01 + &01
 SBC #&00
 ORA landscape_result
 BNE not_in_central_window              ;line segment outside 8 bit window
 JSR mathbox_line_draw08                ;line x0, x1 both 0 - 255
 JMP landscape_draw                     ;next segment
.not_in_central_window
 JSR mathbox_line_draw16
 JMP landscape_draw                     ;next segment

.small_alphabet
 EQUW battlezone_sprites + dial_offset          ;<32> space, note this is empty space within a sprite
 EQUW battlezone_sprites + pling_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + hash_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + left_bracket_offset  ;<40>
 EQUW battlezone_sprites + right_bracket_offset ;<41>
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + hash_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + n0_offset            ;<48>
 EQUW battlezone_sprites + n1_offset
 EQUW battlezone_sprites + n2_offset
 EQUW battlezone_sprites + n3_offset
 EQUW battlezone_sprites + n4_offset
 EQUW battlezone_sprites + n5_offset
 EQUW battlezone_sprites + n6_offset
 EQUW battlezone_sprites + n7_offset
 EQUW battlezone_sprites + n8_offset
 EQUW battlezone_sprites + n9_offset            ;<57>
 EQUW battlezone_sprites + copyright_offset
 EQUW battlezone_sprites + produced_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + tank_left_offset
 EQUW battlezone_sprites + tank_right_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + sa_offset            ;<65>
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
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + dial_offset
 EQUW battlezone_sprites + underscore_offset

.print_or
 STX print_block_address                ;parameter block address
 STY print_block_address + &01
 LDY #&00                               ;text string address
 LDA (print_block_address),Y            ;string address
 INY
 STA or_print_direct + &01
 LDA (print_block_address),Y
 STA or_print_direct + &02
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
.or_print_direct_loop
 PLP
 BMI or_print_exit
.or_print_direct
 LDA print,X                            ;get text character
 PHP
 ASL A
 TAY
 LDA small_alphabet - (&20 * 2),Y
 STA or_print_load + &01
 LDA small_alphabet - (&20 * 2) + &01,Y
 STA or_print_load + &02
 LDA print_screen                       ;stored screen address
 STA print_screen_work
 LDA print_screen + &01
 STA print_screen_work + &01
 LDA print_y_reg
 STA print_y_work
 LDA #&00
 STA print_character_height
.or_print_character
 LDY print_character_height
.or_print_load
 LDA or_print_load,Y                    ;get text byte
 LDY print_y_work
 ORA (print_screen_work),Y
 STA (print_screen_work),Y
 INY                                    ;next pixel line down
 TYA
 AND #&07
 STA print_y_work
 BNE or_no_print_boundary               ;crossed a screen row?
 LDA print_screen_work                  ;crossed row so calculate next row down
 CLC
 ADC #LO(screen_row)
 STA print_screen_work
 LDA print_screen_work + &01
 ADC #HI(screen_row)
 STA print_screen_work + &01
.or_no_print_boundary
 INC print_character_height
 LDA print_character_height
 CMP #&08
 BNE or_print_character
 LDA print_screen                       ;move to next character column
 ADC #&07                               ;c=1 from the compare
 STA print_screen
 BCC or_print_inx
 INC print_screen + &01
.or_print_inx
 INX
 BNE or_print_direct_loop               ;always, carry on until -ve string terminator
.or_print_exit
 RTS

.print
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