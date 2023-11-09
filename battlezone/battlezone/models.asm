; models
; all model vertices and workspace

; constants
 shift_right                            = &01
 bz_xz_scale_factor                     = 320 / 1024
 bz_y_scale_factor                      = 256 / 768
 bz_text_factor                         = 0.94

 vertices_number                        = &1A

; addresses
 vertices_table_start                   = &100
 vertices_x_lsb                         = vertices_table_start
 vertices_x_msb                         = vertices_table_start + vertices_number * &01
 vertices_y_lsb                         = vertices_table_start + vertices_number * &02
 vertices_y_msb                         = vertices_table_start + vertices_number * &03
 vertices_z_lsb                         = vertices_table_start + vertices_number * &04
 vertices_z_msb                         = vertices_table_start + vertices_number * &05
 vertices_table_end                     = vertices_table_start + vertices_number * &06

; render byte = 0 plot point, 1 move/draw, 2-6 vertice index, 7 exit

MACRO command byte
 EQUB (byte >> &01) AND &FE
ENDMACRO

MACRO point_command byte
 EQUB byte
ENDMACRO

MACRO vertice x, y, z
 EQUW   x * bz_xz_scale_factor * 2      ;scale x/z as y rotation then doesn't require
 EQUW - y * bz_y_scale_factor           ;adjustment after calculation
 EQUW   z * bz_xz_scale_factor * 2
ENDMACRO

MACRO vertice_text x, y, z
 EQUW   bz_text_factor * x * bz_xz_scale_factor
 EQUW - bz_text_factor * y * bz_y_scale_factor
 EQUW   bz_text_factor * z * bz_xz_scale_factor
ENDMACRO

MACRO vertice_explosion x, y, z, m
 EQUW - x * bz_xz_scale_factor * m * 2
 EQUW - y * bz_y_scale_factor  * m
 EQUW   z * bz_xz_scale_factor * m * 2
ENDMACRO

MACRO vertice_exhaust x, y, z, m
 EQUW   x * m * 2
 EQUW   y
 EQUW   z * m * 2
ENDMACRO

;object_x00_pyramid
 npy00x =  512	: npy00y = -640 : npy00z = -512
 npy01x = -512	: npy01y = -640 : npy01z = -512
 npy02x = -512	: npy02y = -640 : npy02z =  512
 npy03x =  512	: npy03y = -640 : npy03z =  512
 npy04x =    0	: npy04y =  640 : npy04z =    0

;object_x01_tall_cube
 cbe00x =  512 : cbe00y = -640 : cbe00z = -512
 cbe01x = -512 : cbe01y = -640 : cbe01z = -512
 cbe02x = -512 : cbe02y = -640 : cbe02z =  512
 cbe03x =  512 : cbe03y = -640 : cbe03z =  512
 cbe04x =  512 : cbe04y =  640 : cbe04z = -512
 cbe05x = -512 : cbe05y =  640 : cbe05z = -512
 cbe06x = -512 : cbe06y =  640 : cbe06z =  512
 cbe07x =  512 : cbe07y =  640 : cbe07z =  512

;object_x02_short_cube
 scb00x =  640 : scb00y = -640 : scb00z = -640
 scb01x = -640 : scb01y = -640 : scb01z = -640
 scb02x = -640 : scb02y = -640 : scb02z =  640
 scb03x =  640 : scb03y = -640 : scb03z =  640
 scb04x =  640 : scb04y =  -80 : scb04z = -640
 scb05x = -640 : scb05y =  -80 : scb05z = -640
 scb06x = -640 : scb06y =  -80 : scb06z =  640
 scb07x =  640 : scb07y =  -80 : scb07z =  640

;object_x03_wide_pyramid
 wpy00x =  800	: wpy00y = -640 : wpy00z = -800
 wpy01x = -800	: wpy01y = -640 : wpy01z = -800
 wpy02x = -800	: wpy02y = -640 : wpy02z =  800
 wpy03x =  800	: wpy03y = -640 : wpy03z =  800
 wpy04x =    0	: wpy04y =  800 : wpy04z =    0

;object_x04_tank
 tnk00x =  512 : tnk00y = -640 : tnk00z =  -736
 tnk01x = -512 : tnk01y = -640 : tnk01z =  -736
 tnk02x = -512 : tnk02y = -640 : tnk02z =   968
 tnk03x =  512 : tnk03y = -640 : tnk03z =   968
 tnk04x =  568 : tnk04y = -416 : tnk04z = -1024
 tnk05x = -568 : tnk05y = -416 : tnk05z = -1024
 tnk06x = -568 : tnk06y = -416 : tnk06z =  1248
 tnk07x =  568 : tnk07y = -416 : tnk07z =  1248
 tnk08x =  344 : tnk08y = -240 : tnk08z =  -680
 tnk09x = -344 : tnk09y = -240 : tnk09z =  -680
 tnk10x = -344 : tnk10y = -240 : tnk10z =   680
 tnk11x =  344 : tnk11y = -240 : tnk11z =   680
 tnk12x =  168 : tnk12y =   96 : tnk12z =  -512
 tnk13x = -168 : tnk13y =   96 : tnk13z =  -512
 tnk14x =   40 : tnk14y =  -16 : tnk14z =  -128
 tnk15x =  -40 : tnk15y =  -16 : tnk15z =  -128
 tnk16x =  -40 : tnk16y =  -96 : tnk16z =   128
 tnk17x =   40 : tnk17y =  -96 : tnk17z =   128
 tnk18x =  -40 : tnk18y =  -16 : tnk18z =  1120
 tnk19x =  -40 : tnk19y =  -96 : tnk19z =  1120
 tnk20x =   40 : tnk20y =  -16 : tnk20z =  1120
 tnk21x =   40 : tnk21y =  -96 : tnk21z =  1120
 tnk22x =    0 : tnk22y =   96 : tnk22z =  -512
 tnk23x =    0 : tnk23y =  160 : tnk23z =  -512

;object_x05_super_tank
 stk00x = -368 : stk00y = -640 : stk00z = 1456
 stk01x = -552 : stk01y = -640 : stk01z = -456
 stk02x =  552 : stk02y = -640 : stk02z = -456
 stk03x =  368 : stk03y = -640 : stk03z = 1456
 stk04x = -456 : stk04y = -184 : stk04z = -456
 stk05x =  456 : stk05y = -184 : stk05z = -456
 stk06x =    0 : stk06y = -552 : stk06z = 1096
 stk07x = -272 : stk07y = -232 : stk07z = -272
 stk08x = -272 : stk08y = -184 : stk08z = -456
 stk09x =  272 : stk09y = -184 : stk09z = -456
 stk10x =  272 : stk10y = -232 : stk10z = -272
 stk11x = -184 : stk11y =   88 : stk11z = -272
 stk12x = -184 : stk12y =   88 : stk12z = -456
 stk13x =  184 : stk13y =   88 : stk13z = -456
 stk14x =  184 : stk14y =   88 : stk14z = -272
 stk15x =  -88 : stk15y =  -88 : stk15z = 1280
 stk16x =  -88 : stk16y =  -88 : stk16z =   88
 stk17x =   88 : stk17y =  -88 : stk17z =   88
 stk18x =   88 : stk18y =  -88 : stk18z = 1280
 stk19x =  -88 : stk19y =    0 : stk19z = 1280
 stk20x =  -88 : stk20y =    0 : stk20z =  -88
 stk21x =   88 : stk21y =    0 : stk21z =  -88
 stk22x =   88 : stk22y =    0 : stk22z = 1280
 stk23x =    0 : stk23y =   88 : stk23z = -456
 stk24x =    0 : stk24y =  552 : stk24z = -456

;object_x06_tank_shot
 sht00x =  40 : sht00y = -96 : sht00z = -40
 sht01x =  40 : sht01y = -16 : sht01z = -40
 sht02x = -40 : sht02y = -16 : sht02z = -40
 sht03x = -40 : sht03y = -96 : sht03z = -40
 sht04x =   0 : sht04y = -56 : sht04z =  80

;object_x07_missile
 mis00x = -144 : mis00y =    0 : mis00z = -384
 mis01x =  -72 : mis01y =   96 : mis01z = -384
 mis02x =   72 : mis02y =   96 : mis02z = -384
 mis03x =  144 : mis03y =    0 : mis03z = -384
 mis04x =   72 : mis04y =  -96 : mis04z = -384
 mis05x =  -72 : mis05y =  -96 : mis05z = -384
 mis06x = -288 : mis06y =    0 : mis06z =  -96
 mis07x = -192 : mis07y =  192 : mis07z =  -96
 mis08x =  192 : mis08y =  192 : mis08z =  -96
 mis09x =  288 : mis09y =    0 : mis09z =  -96
 mis10x =  192 : mis10y = -192 : mis10z =  -96
 mis11x = -192 : mis11y = -192 : mis11z =  -96
 mis12x =    0 : mis12y =    0 : mis12z = 1152
 mis13x =    0 : mis13y =    0 : mis13z = 1392
 mis14x =  144 : mis14y = -336 : mis14z = -144
 mis15x = -144 : mis15y = -336 : mis15z = -144
 mis16x = -144 : mis16y = -336 : mis16z =  144
 mis17x =  144 : mis17y = -336 : mis17z =  144
 mis18x =   48 : mis18y = -184 : mis18z =  -48
 mis19x =  -48 : mis19y = -184 : mis19z =  -48
 mis20x =  -48 : mis20y = -168 : mis20z =   48
 mis21x =   48 : mis21y = -168 : mis21z =   48
 mis22x =    0 : mis22y =  192 : mis22z =  -96
 mis23x =  -72 : mis23y =   96 : mis23z =  528
 mis24x =   72 : mis24y =   96 : mis24z =  528
 mis25x =    0 : mis25y =  288 : mis25z =   48

;object_x08_saucer
 sau00x =    0 : sau00y = -80 : sau00z = -240
 sau01x = -160 : sau01y = -80 : sau01z = -160
 sau02x = -240 : sau02y = -80 : sau02z =    0
 sau03x = -160 : sau03y = -80 : sau03z =  160
 sau04x =    0 : sau04y = -80 : sau04z =  240
 sau05x =  160 : sau05y = -80 : sau05z =  160
 sau06x =  240 : sau06y = -80 : sau06z =    0
 sau07x =  160 : sau07y = -80 : sau07z = -160
 sau08x =    0 : sau08y = 160 : sau08z = -960
 sau09x = -680 : sau09y = 160 : sau09z = -680
 sau10x = -960 : sau10y = 160 : sau10z =    0
 sau11x = -680 : sau11y = 160 : sau11z =  680
 sau12x =    0 : sau12y = 160 : sau12z =  960
 sau13x =  680 : sau13y = 160 : sau13z =  680
 sau14x =  960 : sau14y = 160 : sau14z =    0
 sau15x =  680 : sau15y = 160 : sau15z = -680
 sau16x =    0 : sau16y = 560 : sau16z =    0

;object_x09_m_shell
; same as object_x06_tank_shot

;object_x0A_tank_radar
 rad00x =   80 : rad00y = 160 : rad00z =  0
 rad01x =  160 : rad01y = 200 : rad01z = 80
 rad02x =  160 : rad02y = 240 : rad02z = 80
 rad03x =   80 : rad03y = 280 : rad03z =  0
 rad04x =  -80 : rad04y = 160 : rad04z =  0
 rad05x = -160 : rad05y = 200 : rad05z = 80
 rad06x = -160 : rad06y = 240 : rad06z = 80
 rad07x =  -80 : rad07y = 280 : rad07z =  0

;object_x0B_ship_chunk_00
 shc00_00x =   0 : shc00_00y = -544 : shc00_00z =  220
 shc00_01x =  80 : shc00_01y = -376 : shc00_01z = -320
 shc00_02x = -80 : shc00_02y = -192 : shc00_02z =  340
 shc00_03x =   0 : shc00_03y = -712 : shc00_03z = -184
 shc00_04x =  80 : shc00_04y = -512 : shc00_04z = -124
 shc00_05x = -80 : shc00_05y = -416 : shc00_05z = -116

;object_x0C_ship_chunk_01
 shc01_00x =  120 : shc01_00y = -640 : shc01_00z = -240
 shc01_01x =  -64 : shc01_01y = -560 : shc01_01z = -376
 shc01_02x = -160 : shc01_02y = -768 : shc01_02z =	720
 shc01_03x =  120 : shc01_03y = -640 : shc01_03z =	640
 shc01_04x =   64 : shc01_04y = -160 : shc01_04z =	-40
 shc01_05x =  -32 : shc01_05y = -120 : shc01_05z =	  0
 shc01_06x =  160 : shc01_06y = -400 : shc01_06z =	 56
 shc01_07x = -200 : shc01_07y = -480 : shc01_07z =	120

;object_x0D_ship_chunk_02
 shc02_00x =  344 : shc02_00y =  -296 : shc02_00z = -588
 shc02_01x = -344 : shc02_01y =  -296 : shc02_01z = -588
 shc02_02x = -344 : shc02_02y =  -976 : shc02_02z =  588
 shc02_03x =  344 : shc02_03y =  -976 : shc02_03z =  588
 shc02_04x =  168 : shc02_04y =   -96 : shc02_04z = -272
 shc02_05x = -168 : shc02_05y =   -96 : shc02_05z = -272
 shc02_06x =   40 : shc02_06y =  -376 : shc02_06z =    0
 shc02_07x =  -40 : shc02_07y =  -376 : shc02_07z =    0
 shc02_08x =  -40 : shc02_08y =  -576 : shc02_08z =  180
 shc02_09x =   40 : shc02_09y =  -576 : shc02_09z =  180
 shc02_10x =  -40 : shc02_10y = -1000 : shc02_10z = 1080
 shc02_11x =  -40 : shc02_11y = -1072 : shc02_11z = 1040
 shc02_12x =   40 : shc02_12y = -1000 : shc02_12z = 1080
 shc02_13x =   40 : shc02_13y = -1072 : shc02_13z = 1040

;object_x0E_ship_chunk_04
 shc04_00x =   72 : shc04_00y = -368 : shc04_00z = -300
 shc04_01x =  168 : shc04_01y = -368 : shc04_01z = -232
 shc04_02x =  272 : shc04_02y = -472 : shc04_02z = -232
 shc04_03x =  272 : shc04_03y = -568 : shc04_03z = -300
 shc04_04x = -168 : shc04_04y = -408 : shc04_04z =  -96
 shc04_05x =  -12 : shc04_05y = -384 : shc04_05z =   40
 shc04_06x =  260 : shc04_06y = -648 : shc04_06z =   40
 shc04_07x =  232 : shc04_07y = -808 : shc04_07z =  -96

;object_x0F_ship_chunk_05
 shc05_00x =   12 : shc05_00y = -576 : shc05_00z = -80
 shc05_01x = -112 : shc05_01y = -864 : shc05_01z = 472
 shc05_02x =   44 : shc05_02y =   24 : shc05_02z = 800
 shc05_03x =   16 : shc05_03y =  -53 : shc05_03z =  88

;object_x10_forward_track_00
 ftk00_00x =  568 : ftk00_00y = -416 : ftk00_00z = 1248
 ftk00_01x = -568 : ftk00_01y = -416 : ftk00_01z = 1248
 ftk00_02x =  548 : ftk00_02y = -496 : ftk00_02z = 1152
 ftk00_03x = -548 : ftk00_03y = -496 : ftk00_03z = 1152
 ftk00_04x =  532 : ftk00_04y = -576 : ftk00_04z = 1056
 ftk00_05x = -532 : ftk00_05y = -576 : ftk00_05z = 1056

;object_x11_forward_track_01
 ftk01_00x =  564 : ftk01_00y = -432 : ftk01_00z = 1224
 ftk01_01x = -564 : ftk01_01y = -432 : ftk01_01z = 1224
 ftk01_02x =  544 : ftk01_02y = -512 : ftk01_02z = 1128
 ftk01_03x = -544 : ftk01_03y = -512 : ftk01_03z = 1128
 ftk01_04x =  528 : ftk01_04y = -592 : ftk01_04z = 1032
 ftk01_05x = -528 : ftk01_05y = -592 : ftk01_05z = 1032

;object_x12_forward_track_02
 ftk02_00x =  556 : ftk02_00y = -456 : ftk02_00z = 1200
 ftk02_01x = -556 : ftk02_01y = -456 : ftk02_01z = 1200
 ftk02_02x =  540 : ftk02_02y = -536 : ftk02_02z = 1104
 ftk02_03x = -540 : ftk02_03y = -536 : ftk02_03z = 1104
 ftk02_04x =  520 : ftk02_04y = -616 : ftk02_04z = 1008
 ftk02_05x = -520 : ftk02_05y = -616 : ftk02_05z = 1008

;object_x13_forward_track_03
 ftk03_00x =  552 : ftk03_00y = -472 : ftk03_00z = 1176
 ftk03_01x = -552 : ftk03_01y = -472 : ftk03_01z = 1176
 ftk03_02x =  536 : ftk03_02y = -552 : ftk03_02z = 1080
 ftk03_03x = -536 : ftk03_03y = -552 : ftk03_03z = 1080
 ftk03_04x =  516 : ftk03_04y = -632 : ftk03_04z =  984
 ftk03_05x = -516 : ftk03_05y = -632 : ftk03_05z =  984

;object_x14_reverse_tracks_00
 btk00_00x =  568 : btk00_00y = -416 : btk00_00z = -1024
 btk00_01x = -568 : btk00_01y = -416 : btk00_01z = -1024
 btk00_02x =  548 : btk00_02y = -496 : btk00_02z =  -920
 btk00_03x = -548 : btk00_03y = -496 : btk00_03z =  -920
 btk00_04x =  532 : btk00_04y = -576 : btk00_04z =  -816
 btk00_05x = -532 : btk00_05y = -576 : btk00_05z =  -816

;object_x15_reverse_tracks_01
 btk01_00x =  564 : btk01_00y = -432 : btk01_00z = -1000
 btk01_01x = -564 : btk01_01y = -432 : btk01_01z = -1000
 btk01_02x =  544 : btk01_02y = -512 : btk01_02z =  -896
 btk01_03x = -544 : btk01_03y = -512 : btk01_03z =  -896
 btk01_04x =  528 : btk01_04y = -592 : btk01_04z =  -792
 btk01_05x = -528 : btk01_05y = -592 : btk01_05z =  -792

;object_x16_reverse_tracks_02
 btk02_00x =  556 : btk02_00y = -456 : btk02_00z = -972
 btk02_01x = -556 : btk02_01y = -456 : btk02_01z = -972
 btk02_02x =  540 : btk02_02y = -536 : btk02_02z = -868
 btk02_03x = -540 : btk02_03y = -536 : btk02_03z = -868
 btk02_04x =  520 : btk02_04y = -616 : btk02_04z = -764
 btk02_05x = -520 : btk02_05y = -616 : btk02_05z = -764

;object_x17_reverse_tracks_03
 btk03_00x =  552 : btk03_00y = -472 : btk03_00z = -948
 btk03_01x = -552 : btk03_01y = -472 : btk03_01z = -948
 btk03_02x =  536 : btk03_02y = -552 : btk03_02z = -844
 btk03_03x = -536 : btk03_03y = -552 : btk03_03z = -844
 btk03_04x =  516 : btk03_04y = -632 : btk03_04z = -736
 btk03_05x = -516 : btk03_05y = -632 : btk03_05z = -736

;object_x18_battlezone_part01
 tx00_part01 = -5120 : ty00_part01 =  64 : tz00_part01 =  224
 tx01_part01 = -3840 : ty01_part01 =  64 : tz01_part01 =  224
 tx02_part01 = -3200 : ty02_part01 = 176 : tz02_part01 =  672
 tx03_part01 = -3520 : ty03_part01 = 288 : tz03_part01 = 1120
 tx04_part01 = -3200 : ty04_part01 = 400 : tz04_part01 = 1600
 tx05_part01 = -3840 : ty05_part01 = 512 : tz05_part01 = 2048
 tx06_part01 = -5120 : ty06_part01 = 512 : tz06_part01 = 2048
 tx07_part01 = -4480 : ty07_part01 = 176 : tz07_part01 =  672
 tx08_part01 = -4160 : ty08_part01 = 176 : tz08_part01 =  672
 tx09_part01 = -4480 : ty09_part01 = 288 : tz09_part01 = 1120
 tx10_part01 = -4160 : ty10_part01 = 400 : tz10_part01 = 1600
 tx11_part01 = -4480 : ty11_part01 = 400 : tz11_part01 = 1600
 tx12_part01 = -3200 : ty12_part01 =  64 : tz12_part01 =  224
 tx13_part01 = -2240 : ty13_part01 = 176 : tz13_part01 =   32
 tx14_part01 = -1280 : ty14_part01 =  64 : tz14_part01 =  224
 tx15_part01 = -2240 : ty15_part01 = 512 : tz15_part01 = 2048
 tx16_part01 = -2560 : ty16_part01 = 224 : tz16_part01 =  896
 tx17_part01 = -2240 : ty17_part01 = 256 : tz17_part01 = 1024
 tx18_part01 = -1920 : ty18_part01 = 224 : tz18_part01 =  896
 tx19_part01 = -2240 : ty19_part01 = 336 : tz19_part01 = 1344

;object_x19_battlezone_part02
 tx00_part02 =  -640 : ty00_part02 =  64 : tz00_part02 =  224
 tx01_part02 =  -320 : ty01_part02 = 400 : tz01_part02 = 1600
 tx02_part02 =   640 : ty02_part02 = 400 : tz02_part02 = 1600
 tx03_part02 =   960 : ty03_part02 =  64 : tz03_part02 =  224
 tx04_part02 =  1280 : ty04_part02 = 400 : tz04_part02 = 1600
 tx05_part02 =  2240 : ty05_part02 = 400 : tz05_part02 = 1600
 tx06_part02 =  2240 : ty06_part02 =  64 : tz06_part02 =  224
 tx07_part02 =  3840 : ty07_part02 =  64 : tz07_part02 =  224
 tx08_part02 =  5440 : ty08_part02 = 112 : tz08_part02 =  448
 tx09_part02 =  4480 : ty09_part02 = 176 : tz09_part02 =  672
 tx10_part02 =  4480 : ty10_part02 = 224 : tz10_part02 =  896
 tx11_part02 =  5120 : ty11_part02 = 288 : tz11_part02 = 1120
 tx12_part02 =  4480 : ty12_part02 = 336 : tz12_part02 = 1344
 tx13_part02 =  4480 : ty13_part02 = 400 : tz13_part02 = 1600
 tx14_part02 =  5440 : ty14_part02 = 448 : tz14_part02 = 1824
 tx15_part02 =  3840 : ty15_part02 = 512 : tz15_part02 = 2048
 tx16_part02 =  2880 : ty16_part02 = 176 : tz16_part02 =  672
 tx17_part02 =  2880 : ty17_part02 = 512 : tz17_part02 = 2048
 tx18_part02 = -1920 : ty18_part02 = 512 : tz18_part02 = 2048
 tx19_part02 = -1920 : ty19_part02 = 400 : tz19_part02 = 1600
 tx20_part02 =  -960 : ty20_part02 = 400 : tz20_part02 = 1600

;object_x1A_battlezone_part03
 tx00_part03 = -4800 : ty00_part03 = -512 : tz00_part03 = -2048
 tx01_part03 = -2240 : ty01_part03 = -512 : tz01_part03 = -2048
 tx02_part03 = -3520 : ty02_part03 = -400 : tz02_part03 = -1600
 tx03_part03 = -2240 : ty03_part03 =  -64 : tz03_part03 =  -224
 tx04_part03 = -4800 : ty04_part03 =  -64 : tz04_part03 =  -224
 tx05_part03 = -3520 : ty05_part03 = -176 : tz05_part03 =  -672
 tx06_part03 =  -320 : ty06_part03 = -512 : tz06_part03 = -2048
 tx07_part03 =  -320 : ty07_part03 =  -64 : tz07_part03 =  -224
 tx08_part03 = -1600 : ty08_part03 = -400 : tz08_part03 = -1600
 tx09_part03 =  -960 : ty09_part03 = -400 : tz09_part03 = -1600
 tx10_part03 =  -960 : ty10_part03 = -176 : tz10_part03 =  -672
 tx11_part03 = -1600 : ty11_part03 = -176 : tz11_part03 =  -672
 tx12_part03 =     0 : ty12_part03 = -512 : tz12_part03 = -2048
 tx13_part03 =   640 : ty13_part03 = -288 : tz13_part03 = -1120
 tx14_part03 =  2560 : ty14_part03 = -512 : tz14_part03 = -2048
 tx15_part03 =  4160 : ty15_part03 = -448 : tz15_part03 = -1824
 tx16_part03 =  3200 : ty16_part03 = -400 : tz16_part03 = -1600
 tx17_part03 =  3200 : ty17_part03 = -336 : tz17_part03 = -1344
 tx18_part03 =  3840 : ty18_part03 = -288 : tz18_part03 = -1120
 tx19_part03 =  3200 : ty19_part03 = -224 : tz19_part03 =  -896
 tx20_part03 =  3200 : ty20_part03 = -176 : tz20_part03 =  -672
 tx21_part03 =  4160 : ty21_part03 = -112 : tz21_part03 =  -448
 tx22_part03 =  2560 : ty22_part03 =  -64 : tz22_part03 =  -224
 tx23_part03 =  1920 : ty23_part03 = -288 : tz23_part03 = -1120
 tx24_part03 =     0 : ty24_part03 =  -64 : tz24_part03 =  -224

;object_x1B_explosion
; relative coordinates from center of explosion particles
; explosion points
 explosion_scale                        = 4
 point_x_adj                            = 960  ;screen middle
 point_y_adj                            = 544

 exp_x00 = (1024 - point_x_adj) * explosion_scale : exp_y00 = (0528 - point_y_adj) * explosion_scale
 exp_x01 = (0900 - point_x_adj) * explosion_scale : exp_y01 = (0480 - point_y_adj) * explosion_scale
 exp_x02 = (0900 - point_x_adj) * explosion_scale : exp_y02 = (0573 - point_y_adj) * explosion_scale
 exp_x03 = (0932 - point_x_adj) * explosion_scale : exp_y03 = (0544 - point_y_adj) * explosion_scale
 exp_x04 = (0932 - point_x_adj) * explosion_scale : exp_y04 = (0605 - point_y_adj) * explosion_scale
 exp_x05 = (0947 - point_x_adj) * explosion_scale : exp_y05 = (0494 - point_y_adj) * explosion_scale
 exp_x06 = (0976 - point_x_adj) * explosion_scale : exp_y06 = (0591 - point_y_adj) * explosion_scale
 exp_x07 = (1009 - point_x_adj) * explosion_scale : exp_y07 = (0480 - point_y_adj) * explosion_scale
 exp_x08 = (1009 - point_x_adj) * explosion_scale : exp_y08 = (0573 - point_y_adj) * explosion_scale
 exp_x09 = (1009 - point_x_adj) * explosion_scale : exp_y09 = (0605 - point_y_adj) * explosion_scale

;object_x1C_exhaust
; calculated in listing below

.model_vertices_table_lo
 EQUB LO(pyramid_object)
 EQUB LO(cube_object)
 EQUB LO(short_cube_object)
 EQUB LO(wide_pyramid_object)
 EQUB LO(tank_object)
 EQUB LO(super_tank_object)
 EQUB LO(tank_shot_object)
 EQUB LO(missile_object)
 EQUB LO(saucer_object)
 EQUB LO(tank_shot_object)
 EQUB LO(tank_radar_object)
 EQUB LO(ship_chunk_00_object)
 EQUB LO(ship_chunk_01_object)
 EQUB LO(ship_chunk_02_object)
 EQUB LO(ship_chunk_04_object)
 EQUB LO(ship_chunk_05_object)
 EQUB LO(forward_tracks_00_object)
 EQUB LO(forward_tracks_01_object)
 EQUB LO(forward_tracks_02_object)
 EQUB LO(forward_tracks_03_object)
 EQUB LO(reverse_tracks_00_object)
 EQUB LO(reverse_tracks_01_object)
 EQUB LO(reverse_tracks_02_object)
 EQUB LO(reverse_tracks_03_object)
 EQUB LO(battle_object_part_01)
 EQUB LO(battle_object_part_02)
 EQUB LO(battle_object_part_03)
 EQUB LO(explosion_object_00)
 EQUB LO(explosion_object_01)
 EQUB LO(explosion_object_02)
 EQUB LO(explosion_object_03)
 EQUB LO(exhaust_object_00)
 EQUB LO(exhaust_object_01)
 EQUB LO(exhaust_object_02)
 EQUB LO(exhaust_object_03)
.object_number_finish

.model_vertices_table_hi
 EQUB HI(pyramid_object)
 EQUB HI(cube_object)
 EQUB HI(short_cube_object)
 EQUB HI(wide_pyramid_object)
 EQUB HI(tank_object)
 EQUB HI(super_tank_object)
 EQUB HI(tank_shot_object)
 EQUB HI(missile_object)
 EQUB HI(saucer_object)
 EQUB HI(tank_shot_object)
 EQUB HI(tank_radar_object)
 EQUB HI(ship_chunk_00_object)
 EQUB HI(ship_chunk_01_object)
 EQUB HI(ship_chunk_02_object)
 EQUB HI(ship_chunk_04_object)
 EQUB HI(ship_chunk_05_object)
 EQUB HI(forward_tracks_00_object)
 EQUB HI(forward_tracks_01_object)
 EQUB HI(forward_tracks_02_object)
 EQUB HI(forward_tracks_03_object)
 EQUB HI(reverse_tracks_00_object)
 EQUB HI(reverse_tracks_01_object)
 EQUB HI(reverse_tracks_02_object)
 EQUB HI(reverse_tracks_03_object)
 EQUB HI(battle_object_part_01)
 EQUB HI(battle_object_part_02)
 EQUB HI(battle_object_part_03)
 EQUB HI(explosion_object_00)
 EQUB HI(explosion_object_01)
 EQUB HI(explosion_object_02)
 EQUB HI(explosion_object_03)
 EQUB HI(exhaust_object_00)
 EQUB HI(exhaust_object_01)
 EQUB HI(exhaust_object_02)
 EQUB HI(exhaust_object_03)

.model_segment_table_start_lo
 EQUB LO(pyramid_segments_start)
 EQUB LO(cube_segments_start)
 EQUB LO(cube_segments_start)
 EQUB LO(pyramid_segments_start)
 EQUB LO(tank_segments_start)
 EQUB LO(super_tank_segments_start)
 EQUB LO(tank_shot_segments_start)
 EQUB LO(missile_segments_start)
 EQUB LO(saucer_segments_start)
 EQUB LO(tank_shot_segments_start)
 EQUB LO(tank_radar_segments_start)
 EQUB LO(ship_chunk_00_segments_start)
 EQUB LO(ship_chunk_01_segments_start)
 EQUB LO(ship_chunk_02_segments_start)
 EQUB LO(ship_chunk_04_segments_start)
 EQUB LO(ship_chunk_05_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(forward_tracks_00_segments_start)
 EQUB LO(battle_part_01_segments_start)
 EQUB LO(battle_part_02_segments_start)
 EQUB LO(battle_part_03_segments_start)
 EQUB LO(explosion_segments_start)
 EQUB LO(explosion_segments_start)
 EQUB LO(explosion_segments_start)
 EQUB LO(explosion_segments_start)
 EQUB LO(exhaust_segments_start)
 EQUB LO(exhaust_segments_start)
 EQUB LO(exhaust_segments_start)
 EQUB LO(exhaust_segments_start)

.model_segment_table_start_hi
 EQUB HI(pyramid_segments_start)
 EQUB HI(cube_segments_start)
 EQUB HI(cube_segments_start)
 EQUB HI(pyramid_segments_start)
 EQUB HI(tank_segments_start)
 EQUB HI(super_tank_segments_start)
 EQUB HI(tank_shot_segments_start)
 EQUB HI(missile_segments_start)
 EQUB HI(saucer_segments_start)
 EQUB HI(tank_shot_segments_start)
 EQUB HI(tank_radar_segments_start)
 EQUB HI(ship_chunk_00_segments_start)
 EQUB HI(ship_chunk_01_segments_start)
 EQUB HI(ship_chunk_02_segments_start)
 EQUB HI(ship_chunk_04_segments_start)
 EQUB HI(ship_chunk_05_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(forward_tracks_00_segments_start)
 EQUB HI(battle_part_01_segments_start)
 EQUB HI(battle_part_02_segments_start)
 EQUB HI(battle_part_03_segments_start)
 EQUB HI(explosion_segments_start)
 EQUB HI(explosion_segments_start)
 EQUB HI(explosion_segments_start)
 EQUB HI(explosion_segments_start)
 EQUB HI(exhaust_segments_start)
 EQUB HI(exhaust_segments_start)
 EQUB HI(exhaust_segments_start)
 EQUB HI(exhaust_segments_start)

.model_vertices_index
 EQUB (pyramid_vertices_end - pyramid_vertices) - &01
 EQUB (cube_vertices_end - cube_vertices) - &01
 EQUB (short_cube_vertices_end - short_cube_vertices) - &01
 EQUB (wide_pyramid_vertices_end - wide_pyramid_vertices) - &01
 EQUB (tank_vertices_end - tank_vertices) - &01
 EQUB (super_tank_vertices_end - super_tank_vertices) - &01
 EQUB (tank_shot_vertices_end - tank_shot_vertices) - &01
 EQUB (missile_vertices_end - missile_vertices) - &01
 EQUB (saucer_vertices_end - saucer_vertices) - &01
 EQUB (tank_shot_vertices_end - tank_shot_vertices) - &01
 EQUB (tank_radar_vertices_end - tank_radar_vertices) - &01
 EQUB (ship_chunk_00_vertices_end - ship_chunk_00_vertices) - &01
 EQUB (ship_chunk_01_vertices_end - ship_chunk_01_vertices) - &01
 EQUB (ship_chunk_02_vertices_end - ship_chunk_02_vertices) - &01
 EQUB (ship_chunk_04_vertices_end - ship_chunk_04_vertices) - &01
 EQUB (ship_chunk_05_vertices_end - ship_chunk_05_vertices) - &01
 EQUB (forward_tracks_00_vertices_end - forward_tracks_00_vertices) - &01
 EQUB (forward_tracks_01_vertices_end - forward_tracks_01_vertices) - &01
 EQUB (forward_tracks_02_vertices_end - forward_tracks_02_vertices) - &01
 EQUB (forward_tracks_03_vertices_end - forward_tracks_03_vertices) - &01
 EQUB (reverse_tracks_00_vertices_end - reverse_tracks_00_vertices) - &01
 EQUB (reverse_tracks_01_vertices_end - reverse_tracks_01_vertices) - &01
 EQUB (reverse_tracks_02_vertices_end - reverse_tracks_02_vertices) - &01
 EQUB (reverse_tracks_03_vertices_end - reverse_tracks_03_vertices) - &01
 EQUB (battle_part_01_vertices_end - battle_vertices_01) - &01
 EQUB (battle_part_02_vertices_end - battle_vertices_02) - &01
 EQUB (battle_part_03_vertices_end - battle_vertices_03) - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) - &01

.model_vertices_number
 EQUB (pyramid_vertices_end - pyramid_vertices) / &06 - &01
 EQUB (cube_vertices_end - cube_vertices) / &06 - &01
 EQUB (short_cube_vertices_end - short_cube_vertices) / &06 - &01
 EQUB (wide_pyramid_vertices_end - wide_pyramid_vertices) / &06 - &01
 EQUB (tank_vertices_end - tank_vertices) / &06 - &01
 EQUB (super_tank_vertices_end - super_tank_vertices) / &06 - &01
 EQUB (tank_shot_vertices_end - tank_shot_vertices) / &06 - &01
 EQUB (missile_vertices_end - missile_vertices) / &06 - &01
 EQUB (saucer_vertices_end - saucer_vertices) / &06 - &01
 EQUB (tank_shot_vertices_end - tank_shot_vertices) / &06 - &01
 EQUB (tank_radar_vertices_end - tank_radar_vertices) / &06 - &01
 EQUB (ship_chunk_00_vertices_end - ship_chunk_00_vertices) / &06 - &01
 EQUB (ship_chunk_01_vertices_end - ship_chunk_01_vertices) / &06 - &01
 EQUB (ship_chunk_02_vertices_end - ship_chunk_02_vertices) / &06 - &01
 EQUB (ship_chunk_04_vertices_end - ship_chunk_04_vertices) / &06 - &01
 EQUB (ship_chunk_05_vertices_end - ship_chunk_05_vertices) / &06 - &01
 EQUB (forward_tracks_00_vertices_end - forward_tracks_00_vertices) / &06 - &01
 EQUB (forward_tracks_01_vertices_end - forward_tracks_01_vertices) / &06 - &01
 EQUB (forward_tracks_02_vertices_end - forward_tracks_02_vertices) / &06 - &01
 EQUB (forward_tracks_03_vertices_end - forward_tracks_03_vertices) / &06 - &01
 EQUB (reverse_tracks_00_vertices_end - reverse_tracks_00_vertices) / &06 - &01
 EQUB (reverse_tracks_01_vertices_end - reverse_tracks_01_vertices) / &06 - &01
 EQUB (reverse_tracks_02_vertices_end - reverse_tracks_02_vertices) / &06 - &01
 EQUB (reverse_tracks_03_vertices_end - reverse_tracks_03_vertices) / &06 - &01
 EQUB (battle_part_01_vertices_end - battle_vertices_01) / &06 - &01
 EQUB (battle_part_02_vertices_end - battle_vertices_02) / &06 - &01
 EQUB (battle_part_03_vertices_end - battle_vertices_03) / &06 - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) / &06 - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) / &06 - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) / &06 - &01
 EQUB (explosion_object_vertices_00_end - explosion_object_vertices_00) / &06 - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) / &06 - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) / &06 - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) / &06 - &01
 EQUB (exhaust_object_vertices_00_end - exhaust_object_vertices_00) / &06 - &01

.pyramid_object
.pyramid_vertices
 vertice npy00x, npy00y, npy00z
 vertice npy01x, npy01y, npy01z
 vertice npy02x, npy02y, npy02z
 vertice npy03x, npy03y, npy03z
 vertice npy04x, npy04y, npy04z
.pyramid_vertices_end

.pyramid_segments
.pyramid_segments_start
 command &03
 command &24
 command &0C
 command &04
 command &1C
 command &24
 command &14
 command &1C
 command &1C
 command &13
 command &0C
 EQUB &FF

.cube_object
.cube_vertices
 vertice cbe00x, cbe00y, cbe00z
 vertice cbe01x, cbe01y, cbe01z
 vertice cbe02x, cbe02y, cbe02z
 vertice cbe03x, cbe03y, cbe03z
 vertice cbe04x, cbe04y, cbe04z
 vertice cbe05x, cbe05y, cbe05z
 vertice cbe06x, cbe06y, cbe06z
 vertice cbe07x, cbe07y, cbe07z
.cube_vertices_end

.cube_segments
.cube_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &04
 command &24
 command &2C
 command &34
 command &3C
 command &24
 command &2A
 command &0C
 command &12
 command &34
 command &3A
 command &1C
 EQUB &FF

.short_cube_object
.short_cube_vertices
 vertice scb00x, scb00y, scb00z
 vertice scb01x, scb01y, scb01z
 vertice scb02x, scb02y, scb02z
 vertice scb03x, scb03y, scb03z
 vertice scb04x, scb04y, scb04z
 vertice scb05x, scb05y, scb05z
 vertice scb06x, scb06y, scb06z
 vertice scb07x, scb07y, scb07z
.short_cube_vertices_end

.wide_pyramid_object
.wide_pyramid_vertices
 vertice wpy00x, wpy00y, wpy00z
 vertice wpy01x, wpy01y, wpy01z
 vertice wpy02x, wpy02y, wpy02z
 vertice wpy03x, wpy03y, wpy03z
 vertice wpy04x, wpy04y, wpy04z
.wide_pyramid_vertices_end

.tank_object
.tank_vertices
 vertice tnk00x, tnk00y, tnk00z
 vertice tnk01x, tnk01y, tnk01z
 vertice tnk02x, tnk02y, tnk02z
 vertice tnk03x, tnk03y, tnk03z
 vertice tnk04x, tnk04y, tnk04z
 vertice tnk05x, tnk05y, tnk05z
 vertice tnk06x, tnk06y, tnk06z
 vertice tnk07x, tnk07y, tnk07z
 vertice tnk08x, tnk08y, tnk08z
 vertice tnk09x, tnk09y, tnk09z
 vertice tnk10x, tnk10y, tnk10z
 vertice tnk11x, tnk11y, tnk11z
 vertice tnk12x, tnk12y, tnk12z
 vertice tnk13x, tnk13y, tnk13z
 vertice tnk14x, tnk14y, tnk14z
 vertice tnk15x, tnk15y, tnk15z
 vertice tnk16x, tnk16y, tnk16z
 vertice tnk17x, tnk17y, tnk17z
 vertice tnk18x, tnk18y, tnk18z
 vertice tnk19x, tnk19y, tnk19z
 vertice tnk20x, tnk20y, tnk20z
 vertice tnk21x, tnk21y, tnk21z
 vertice tnk22x, tnk22y, tnk22z
 vertice tnk23x, tnk23y, tnk23z
.tank_vertices_end

.tank_segments_start
 command &BB
 command &B4
 command &62
 command &6C
 command &72
 command &A4
 command &94
 command &7C
 command &74
 command &8C
 command &84
 command &9C
 command &AC
 command &8C
 command &7A
 command &84
 command &9A
 command &94
 command &A2
 command &AC
 command &1B
 command &04
 command &24
 command &3C
 command &34
 command &14
 command &1C
 command &3C
 command &5C
 command &54
 command &34
 command &2C
 command &4C
 command &54
 command &6C
 command &4C
 command &44
 command &5C
 command &64
 command &44
 command &24
 command &2C
 command &0C
 command &14
 command &0A
 command &04
 EQUB &FF

.super_tank_object
.super_tank_vertices
 vertice stk00x, stk00y, stk00z
 vertice stk01x, stk01y, stk01z
 vertice stk02x, stk02y, stk02z
 vertice stk03x, stk03y, stk03z
 vertice stk04x, stk04y, stk04z
 vertice stk05x, stk05y, stk05z
 vertice stk06x, stk06y, stk06z
 vertice stk07x, stk07y, stk07z
 vertice stk08x, stk08y, stk08z
 vertice stk09x, stk09y, stk09z
 vertice stk10x, stk10y, stk10z
 vertice stk11x, stk11y, stk11z
 vertice stk12x, stk12y, stk12z
 vertice stk13x, stk13y, stk13z
 vertice stk14x, stk14y, stk14z
 vertice stk15x, stk15y, stk15z
 vertice stk16x, stk16y, stk16z
 vertice stk17x, stk17y, stk17z
 vertice stk18x, stk18y, stk18z
 vertice stk19x, stk19y, stk19z
 vertice stk20x, stk20y, stk20z
 vertice stk21x, stk21y, stk21z
 vertice stk22x, stk22y, stk22z
 vertice stk23x, stk23y, stk23z
 vertice stk24x, stk24y, stk24z
.super_tank_vertices_end

.super_tank_segments_start
 command &03
 command &0C
 command &24
 command &04
 command &1C
 command &14
 command &2C
 command &1C
 command &12
 command &0C
 command &22
 command &2C
 command &4B
 command &54
 command &34
 command &74
 command &6C
 command &4C
 command &44
 command &3C
 command &34
 command &5C
 command &64
 command &44
 command &62
 command &6C
 command &72
 command &5C
 command &9B
 command &B4
 command &AC
 command &A4
 command &84
 command &7C
 command &94
 command &8C
 command &84
 command &7A
 command &9C
 command &B2
 command &94
 command &8A
 command &AC
 command &BA
 command &C4
 command &9B
 command &A4
 EQUB &FF

.tank_shot_object
.tank_shot_vertices
 vertice sht00x, sht00y, sht00z
 vertice sht01x, sht01y, sht01z
 vertice sht02x, sht02y, sht02z
 vertice sht03x, sht03y, sht03z
 vertice sht04x, sht04y, sht04z
.tank_shot_vertices_end

.tank_shot_segments
.tank_shot_segments_start
 command &03
 command &24
 command &0C
 command &04
 command &1C
 command &24
 command &14
 command &1C
 command &12
 command &0C
 EQUB &FF

.missile_object
.missile_vertices
 vertice mis00x, mis00y, mis00z
 vertice mis01x, mis01y, mis01z
 vertice mis02x, mis02y, mis02z
 vertice mis03x, mis03y, mis03z
 vertice mis04x, mis04y, mis04z
 vertice mis05x, mis05y, mis05z
 vertice mis06x, mis06y, mis06z
 vertice mis07x, mis07y, mis07z
 vertice mis08x, mis08y, mis08z
 vertice mis09x, mis09y, mis09z
 vertice mis10x, mis10y, mis10z
 vertice mis11x, mis11y, mis11z
 vertice mis12x, mis12y, mis12z
 vertice mis13x, mis13y, mis13z
 vertice mis14x, mis14y, mis14z
 vertice mis15x, mis15y, mis15z
 vertice mis16x, mis16y, mis16z
 vertice mis17x, mis17y, mis17z
 vertice mis18x, mis18y, mis18z
 vertice mis19x, mis19y, mis19z
 vertice mis20x, mis20y, mis20z
 vertice mis21x, mis21y, mis21z
 vertice mis22x, mis22y, mis22z
 vertice mis23x, mis23y, mis23z
 vertice mis24x, mis24y, mis24z
 vertice mis25x, mis25y, mis25z
.missile_vertices_end

.missile_segments
.missile_segments_start
 command &6B
 command &64
 command &34
 command &04
 command &0C
 command &3C
 command &44
 command &4C
 command &54
 command &5C
 command &34
 command &3C
 command &64
 command &44
 command &14
 command &1C
 command &4C
 command &64
 command &54
 command &24
 command &2C
 command &5C
 command &64
 command &C3
 command &BC
 command &B4
 command &C4
 command &CC
 command &BC
 command &CA
 command &B4
 command &0A
 command &14
 command &1A
 command &24
 command &2A
 command &04
 command &93
 command &9C
 command &A4
 command &AC
 command &94
 command &74
 command &7C
 command &84
 command &8C
 command &74
 command &7A
 command &9C
 command &A2
 command &84
 command &8A
 command &AC
 EQUB &FF

.saucer_object
.saucer_vertices
 vertice sau00x, sau00y, sau00z
 vertice sau01x, sau01y, sau01z
 vertice sau02x, sau02y, sau02z
 vertice sau03x, sau03y, sau03z
 vertice sau04x, sau04y, sau04z
 vertice sau05x, sau05y, sau05z
 vertice sau06x, sau06y, sau06z
 vertice sau07x, sau07y, sau07z
 vertice sau08x, sau08y, sau08z
 vertice sau09x, sau09y, sau09z
 vertice sau10x, sau10y, sau10z
 vertice sau11x, sau11y, sau11z
 vertice sau12x, sau12y, sau12z
 vertice sau13x, sau13y, sau13z
 vertice sau14x, sau14y, sau14z
 vertice sau15x, sau15y, sau15z
 vertice sau16x, sau16y, sau16z
.saucer_vertices_end

.saucer_segments
.saucer_segments_start
 command &83
 command &44
 command &4C
 command &84
 command &54
 command &5C
 command &84
 command &64
 command &6C
 command &84
 command &74
 command &7C
 command &84
 command &03
 command &3C
 command &7C
 command &44
 command &04
 command &0C
 command &4C
 command &54
 command &14
 command &1C
 command &5C
 command &64
 command &24
 command &2C
 command &6C
 command &74
 command &34
 command &3C
 command &32
 command &2C
 command &22
 command &1C
 command &12
 command &0C
 EQUB &FF

.tank_radar_object
.tank_radar_vertices
 vertice rad00x, rad00y, rad00z
 vertice rad01x, rad01y, rad01z
 vertice rad02x, rad02y, rad02z
 vertice rad03x, rad03y, rad03z
 vertice rad04x, rad04y, rad04z
 vertice rad05x, rad05y, rad05z
 vertice rad06x, rad06y, rad06z
 vertice rad07x, rad07y, rad07z
.tank_radar_vertices_end

.tank_radar_segments
.tank_radar_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &04
 command &24
 command &2C
 command &34
 command &3C
 command &24
 command &3A
 command &1C
 EQUB &FF

.ship_chunk_00_object
.ship_chunk_00_vertices
 vertice shc00_00x, shc00_00y, shc00_00z
 vertice shc00_01x, shc00_01y, shc00_01z
 vertice shc00_02x, shc00_02y, shc00_02z
 vertice shc00_03x, shc00_03y, shc00_03z
 vertice shc00_04x, shc00_04y, shc00_04z
 vertice shc00_05x, shc00_05y, shc00_05z
.ship_chunk_00_vertices_end

.ship_chunk_00_segments
.ship_chunk_00_segments_start
 command &03
 command &1C
 command &2C
 command &14
 command &04
 command &0C
 command &14
 command &2C
 command &24
 command &0C
 command &22
 command &1C
 EQUB &FF

.ship_chunk_01_object
.ship_chunk_01_vertices
 vertice shc01_00x, shc01_00y, shc01_00z
 vertice shc01_01x, shc01_01y, shc01_01z
 vertice shc01_02x, shc01_02y, shc01_02z
 vertice shc01_03x, shc01_03y, shc01_03z
 vertice shc01_04x, shc01_04y, shc01_04z
 vertice shc01_05x, shc01_05y, shc01_05z
 vertice shc01_06x, shc01_06y, shc01_06z
 vertice shc01_07x, shc01_07y, shc01_07z
.ship_chunk_01_vertices_end

.ship_chunk_01_segments
.ship_chunk_01_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &04
 command &24
 command &34
 command &3C
 command &2C
 command &24
 command &2A
 command &0C
 command &3A
 command &14
 command &1A
 command &34
 EQUB &FF

.ship_chunk_02_object
.ship_chunk_02_vertices
 vertice shc02_00x, shc02_00y, shc02_00z
 vertice shc02_01x, shc02_01y, shc02_01z
 vertice shc02_02x, shc02_02y, shc02_02z
 vertice shc02_03x, shc02_03y, shc02_03z
 vertice shc02_04x, shc02_04y, shc02_04z
 vertice shc02_05x, shc02_05y, shc02_05z
 vertice shc02_06x, shc02_06y, shc02_06z
 vertice shc02_07x, shc02_07y, shc02_07z
 vertice shc02_08x, shc02_08y, shc02_08z
 vertice shc02_09x, shc02_09y, shc02_09z
 vertice shc02_10x, shc02_10y, shc02_10z
 vertice shc02_11x, shc02_11y, shc02_11z
 vertice shc02_12x, shc02_12y, shc02_12z
 vertice shc02_13x, shc02_13y, shc02_13z
.ship_chunk_02_vertices_end

.ship_chunk_02_segments
.ship_chunk_02_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &04
 command &24
 command &2C
 command &0C
 command &2A
 command &14
 command &1A
 command &24
 command &32
 command &64
 command &54
 command &3C
 command &34
 command &4C
 command &44
 command &5C
 command &6C
 command &4C
 command &3A
 command &44
 command &5A
 command &54
 command &62
 command &6C
 EQUB &FF

.ship_chunk_04_object
.ship_chunk_04_vertices
 vertice shc04_00x, shc04_00y, shc04_00z
 vertice shc04_01x, shc04_01y, shc04_01z
 vertice shc04_02x, shc04_02y, shc04_02z
 vertice shc04_03x, shc04_03y, shc04_03z
 vertice shc04_04x, shc04_04y, shc04_04z
 vertice shc04_05x, shc04_05y, shc04_05z
 vertice shc04_06x, shc04_06y, shc04_06z
 vertice shc04_07x, shc04_07y, shc04_07z
.ship_chunk_04_vertices_end

.ship_chunk_04_segments
.ship_chunk_04_segments_start
 command &0B
 command &14
 command &1C
 command &3C
 command &34
 command &2C
 command &24
 command &04
 command &0C
 command &2C
 command &32
 command &14
 EQUB &FF

.ship_chunk_05_object
.ship_chunk_05_vertices
 vertice shc05_00x, shc05_00y, shc05_00z
 vertice shc05_01x, shc05_01y, shc05_01z
 vertice shc05_02x, shc05_02y, shc05_02z
 vertice shc05_03x, shc05_03y, shc05_03z
.ship_chunk_05_vertices_end

.ship_chunk_05_segments
.ship_chunk_05_segments_start
 command &03
 command &14
 command &0C
 command &1C
 command &04
 command &0C
 command &12
 command &1C
 EQUB &FF

.forward_tracks_00_object
.forward_tracks_00_vertices
 vertice ftk00_00x, ftk00_00y, ftk00_00z
 vertice ftk00_01x, ftk00_01y, ftk00_01z
 vertice ftk00_02x, ftk00_02y, ftk00_02z
 vertice ftk00_03x, ftk00_03y, ftk00_03z
 vertice ftk00_04x, ftk00_04y, ftk00_04z
 vertice ftk00_05x, ftk00_05y, ftk00_05z
.forward_tracks_00_vertices_end

.forward_tracks_00_segments
.forward_tracks_00_segments_start
 command &03
 command &0C
 command &12
 command &1C
 command &22
 command &2C
 EQUB &FF

.forward_tracks_01_object
.forward_tracks_01_vertices
 vertice ftk01_00x, ftk01_00y, ftk01_00z
 vertice ftk01_01x, ftk01_01y, ftk01_01z
 vertice ftk01_02x, ftk01_02y, ftk01_02z
 vertice ftk01_03x, ftk01_03y, ftk01_03z
 vertice ftk01_04x, ftk01_04y, ftk01_04z
 vertice ftk01_05x, ftk01_05y, ftk01_05z
.forward_tracks_01_vertices_end

.forward_tracks_02_object
.forward_tracks_02_vertices
 vertice ftk02_00x, ftk02_00y, ftk02_00z
 vertice ftk02_01x, ftk02_01y, ftk02_01z
 vertice ftk02_02x, ftk02_02y, ftk02_02z
 vertice ftk02_03x, ftk02_03y, ftk02_03z
 vertice ftk02_04x, ftk02_04y, ftk02_04z
 vertice ftk02_05x, ftk02_05y, ftk02_05z
.forward_tracks_02_vertices_end

.forward_tracks_03_object
.forward_tracks_03_vertices
 vertice ftk03_00x, ftk03_00y, ftk03_00z
 vertice ftk03_01x, ftk03_01y, ftk03_01z
 vertice ftk03_02x, ftk03_02y, ftk03_02z
 vertice ftk03_03x, ftk03_03y, ftk03_03z
 vertice ftk03_04x, ftk03_04y, ftk03_04z
 vertice ftk03_05x, ftk03_05y, ftk03_05z
.forward_tracks_03_vertices_end

.reverse_tracks_00_object
.reverse_tracks_00_vertices
 vertice btk00_00x, btk00_00y, btk00_00z
 vertice btk00_01x, btk00_01y, btk00_01z
 vertice btk00_02x, btk00_02y, btk00_02z
 vertice btk00_03x, btk00_03y, btk00_03z
 vertice btk00_04x, btk00_04y, btk00_04z
 vertice btk00_05x, btk00_05y, btk00_05z
.reverse_tracks_00_vertices_end

.reverse_tracks_01_object
.reverse_tracks_01_vertices
 vertice btk01_00x, btk01_00y, btk01_00z
 vertice btk01_01x, btk01_01y, btk01_01z
 vertice btk01_02x, btk01_02y, btk01_02z
 vertice btk01_03x, btk01_03y, btk01_03z
 vertice btk01_04x, btk01_04y, btk01_04z
 vertice btk01_05x, btk01_05y, btk01_05z
.reverse_tracks_01_vertices_end

.reverse_tracks_02_object
.reverse_tracks_02_vertices
 vertice btk02_00x, btk02_00y, btk02_00z
 vertice btk02_01x, btk02_01y, btk02_01z
 vertice btk02_02x, btk02_02y, btk02_02z
 vertice btk02_03x, btk02_03y, btk02_03z
 vertice btk02_04x, btk02_04y, btk02_04z
 vertice btk02_05x, btk02_05y, btk02_05z
.reverse_tracks_02_vertices_end

.reverse_tracks_03_object
.reverse_tracks_03_vertices
 vertice btk03_00x, btk03_00y, btk03_00z
 vertice btk03_01x, btk03_01y, btk03_01z
 vertice btk03_02x, btk03_02y, btk03_02z
 vertice btk03_03x, btk03_03y, btk03_03z
 vertice btk03_04x, btk03_04y, btk03_04z
 vertice btk03_05x, btk03_05y, btk03_05z
.reverse_tracks_03_vertices_end

.battle_object_part_01
.battle_vertices_01
 vertice_text tx00_part01, ty00_part01, tz00_part01
 vertice_text tx01_part01, ty01_part01, tz01_part01
 vertice_text tx02_part01, ty02_part01, tz02_part01
 vertice_text tx03_part01, ty03_part01, tz03_part01
 vertice_text tx04_part01, ty04_part01, tz04_part01
 vertice_text tx05_part01, ty05_part01, tz05_part01
 vertice_text tx06_part01, ty06_part01, tz06_part01
 vertice_text tx07_part01, ty07_part01, tz07_part01
 vertice_text tx08_part01, ty08_part01, tz08_part01
 vertice_text tx09_part01, ty09_part01, tz09_part01
 vertice_text tx10_part01, ty10_part01, tz10_part01
 vertice_text tx11_part01, ty11_part01, tz11_part01
 vertice_text tx12_part01, ty12_part01, tz12_part01
 vertice_text tx13_part01, ty13_part01, tz13_part01
 vertice_text tx14_part01, ty14_part01, tz14_part01
 vertice_text tx15_part01, ty15_part01, tz15_part01
 vertice_text tx16_part01, ty16_part01, tz16_part01
 vertice_text tx17_part01, ty17_part01, tz17_part01
 vertice_text tx18_part01, ty18_part01, tz18_part01
 vertice_text tx19_part01, ty19_part01, tz19_part01
.battle_part_01_vertices_end

.battle_part_01_segments
.battle_part_01_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &24
 command &2C
 command &34
 command &04
 command &3A
 command &44
 command &4C
 command &54
 command &5C
 command &3C
 command &62
 command &6C
 command &74
 command &7C
 command &64
 command &82
 command &8C
 command &94
 command &9C
 command &84
 EQUB &FF

.battle_object_part_02
.battle_vertices_02
 vertice_text tx00_part02, ty00_part02, tz00_part02
 vertice_text tx01_part02, ty01_part02, tz01_part02
 vertice_text tx02_part02, ty02_part02, tz02_part02
 vertice_text tx03_part02, ty03_part02, tz03_part02
 vertice_text tx04_part02, ty04_part02, tz04_part02
 vertice_text tx05_part02, ty05_part02, tz05_part02
 vertice_text tx06_part02, ty06_part02, tz06_part02
 vertice_text tx07_part02, ty07_part02, tz07_part02
 vertice_text tx08_part02, ty08_part02, tz08_part02
 vertice_text tx09_part02, ty09_part02, tz09_part02
 vertice_text tx10_part02, ty10_part02, tz10_part02
 vertice_text tx11_part02, ty11_part02, tz11_part02
 vertice_text tx12_part02, ty12_part02, tz12_part02
 vertice_text tx13_part02, ty13_part02, tz13_part02
 vertice_text tx14_part02, ty14_part02, tz14_part02
 vertice_text tx15_part02, ty15_part02, tz15_part02
 vertice_text tx16_part02, ty16_part02, tz16_part02
 vertice_text tx17_part02, ty17_part02, tz17_part02
 vertice_text tx18_part02, ty18_part02, tz18_part02
 vertice_text tx19_part02, ty19_part02, tz19_part02
 vertice_text tx20_part02, ty20_part02, tz20_part02
.battle_part_02_vertices_end

.battle_part_02_segments
.battle_part_02_segments_start
 command &03
 command &0C
 command &14
 command &1C
 command &24
 command &2C
 command &34
 command &3C
 command &44
 command &4C
 command &54
 command &5C
 command &64
 command &6C
 command &74
 command &7C
 command &3C
 command &84
 command &8C
 command &94
 command &9C
 command &A4
 command &04
 EQUB &FF

.battle_object_part_03
.battle_vertices_03
 vertice_text tx00_part03, ty00_part03, tz00_part03
 vertice_text tx01_part03, ty01_part03, tz01_part03
 vertice_text tx02_part03, ty02_part03, tz02_part03
 vertice_text tx03_part03, ty03_part03, tz03_part03
 vertice_text tx04_part03, ty04_part03, tz04_part03
 vertice_text tx05_part03, ty05_part03, tz05_part03
 vertice_text tx06_part03, ty06_part03, tz06_part03
 vertice_text tx07_part03, ty07_part03, tz07_part03
 vertice_text tx08_part03, ty08_part03, tz08_part03
 vertice_text tx09_part03, ty09_part03, tz09_part03
 vertice_text tx10_part03, ty10_part03, tz10_part03
 vertice_text tx11_part03, ty11_part03, tz11_part03
 vertice_text tx12_part03, ty12_part03, tz12_part03
 vertice_text tx13_part03, ty13_part03, tz13_part03
 vertice_text tx14_part03, ty14_part03, tz14_part03
 vertice_text tx15_part03, ty15_part03, tz15_part03
 vertice_text tx16_part03, ty16_part03, tz16_part03
 vertice_text tx17_part03, ty17_part03, tz17_part03
 vertice_text tx18_part03, ty18_part03, tz18_part03
 vertice_text tx19_part03, ty19_part03, tz19_part03
 vertice_text tx20_part03, ty20_part03, tz20_part03
 vertice_text tx21_part03, ty21_part03, tz21_part03
 vertice_text tx22_part03, ty22_part03, tz22_part03
 vertice_text tx23_part03, ty23_part03, tz23_part03
 vertice_text tx24_part03, ty24_part03, tz24_part03
.battle_part_03_vertices_end

.battle_part_03_segments
.battle_part_03_segments_start
 command &0B
 command &04
 command &2C
 command &24
 command &1C
 command &14
 command &0C
 command &1C
 command &3C
 command &34
 command &0C
 command &4A
 command &44
 command &5C
 command &54
 command &4C
 command &72
 command &B4
 command &BC
 command &C4
 command &64
 command &6C
 command &74
 command &7C
 command &84
 command &8C
 command &94
 command &9C
 command &A4
 command &AC
 command &B4
 EQUB &FF

.explosion_object_00
.explosion_object_vertices_00
 vertice_explosion exp_x00, exp_y00, &00, 1.00
 vertice_explosion exp_x01, exp_y01, &00, 1.00
 vertice_explosion exp_x02, exp_y02, &00, 1.00
 vertice_explosion exp_x03, exp_y03, &00, 1.00
 vertice_explosion exp_x04, exp_y04, &00, 1.00
 vertice_explosion exp_x05, exp_y05, &00, 1.00
 vertice_explosion exp_x06, exp_y06, &00, 1.00
 vertice_explosion exp_x07, exp_y07, &00, 1.00
 vertice_explosion exp_x08, exp_y08, &00, 1.00
 vertice_explosion exp_x09, exp_y09, &00, 1.00
.explosion_object_vertices_00_end

.explosion_object_01
.explosion_object_vertices_01
 vertice_explosion exp_x00, exp_y00, &00, 1.25
 vertice_explosion exp_x01, exp_y01, &00, 1.25
 vertice_explosion exp_x02, exp_y02, &00, 1.25
 vertice_explosion exp_x03, exp_y03, &00, 1.25
 vertice_explosion exp_x04, exp_y04, &00, 1.25
 vertice_explosion exp_x05, exp_y05, &00, 1.25
 vertice_explosion exp_x06, exp_y06, &00, 1.25
 vertice_explosion exp_x07, exp_y07, &00, 1.25
 vertice_explosion exp_x08, exp_y08, &00, 1.25
 vertice_explosion exp_x09, exp_y09, &00, 1.25
.explosion_object_vertices_01_end

.explosion_object_02
.explosion_object_vertices_02
 vertice_explosion exp_x00, exp_y00, &00, 1.50
 vertice_explosion exp_x01, exp_y01, &00, 1.50
 vertice_explosion exp_x02, exp_y02, &00, 1.50
 vertice_explosion exp_x03, exp_y03, &00, 1.50
 vertice_explosion exp_x04, exp_y04, &00, 1.50
 vertice_explosion exp_x05, exp_y05, &00, 1.50
 vertice_explosion exp_x06, exp_y06, &00, 1.50
 vertice_explosion exp_x07, exp_y07, &00, 1.50
 vertice_explosion exp_x08, exp_y08, &00, 1.50
 vertice_explosion exp_x09, exp_y09, &00, 1.50
.explosion_object_vertices_02_end

.explosion_object_03
.explosion_object_vertices_03
 vertice_explosion exp_x00, exp_y00, &00, 1.75
 vertice_explosion exp_x01, exp_y01, &00, 1.75
 vertice_explosion exp_x02, exp_y02, &00, 1.75
 vertice_explosion exp_x03, exp_y03, &00, 1.75
 vertice_explosion exp_x04, exp_y04, &00, 1.75
 vertice_explosion exp_x05, exp_y05, &00, 1.75
 vertice_explosion exp_x06, exp_y06, &00, 1.75
 vertice_explosion exp_x07, exp_y07, &00, 1.75
 vertice_explosion exp_x08, exp_y08, &00, 1.75
 vertice_explosion exp_x09, exp_y09, &00, 1.75
.explosion_object_vertices_03_end

.explosion_segments
.explosion_segments_start
 point_command (&00 << &01) OR &01
 point_command (&01 << &01) OR &01
 point_command (&02 << &01) OR &01
 point_command (&03 << &01) OR &01
 point_command (&04 << &01) OR &01
 point_command (&05 << &01) OR &01
 point_command (&06 << &01) OR &01
 point_command (&07 << &01) OR &01
 point_command (&08 << &01) OR &01
 point_command (&09 << &01) OR &01
 EQUB &FF

.exhaust_object_00
.exhaust_object_vertices_00
 FOR degree, 0, 359, 360 / 8
   vertice_exhaust COS(RAD(degree)) * 64, &A0, SIN(RAD(degree)) * 64, 0.40
 NEXT

.exhaust_object_vertices_00_end

.exhaust_object_01
.exhaust_object_vertices_01
 FOR degree, 0, 359 , 360 / 8
   vertice_exhaust COS(RAD(degree)) * 64, &A0, SIN(RAD(degree)) * 64, 0.80
 NEXT

.exhaust_object_vertices_01_end

.exhaust_object_02
.exhaust_object_vertices_02
 FOR degree, 0, 359, 360 / 8
   vertice_exhaust COS(RAD(degree)) * 64, &A0, SIN(RAD(degree)) * 64, 1.20
 NEXT

.exhaust_object_vertices_02_end

.exhaust_object_03
.exhaust_object_vertices_03
 FOR degree, 0, 359, 360 / 8
   vertice_exhaust COS(RAD(degree)) * 64, &A0, SIN(RAD(degree)) * 64, 1.60
 NEXT

.exhaust_object_vertices_03_end

.exhaust_segments
.exhaust_segments_start
 point_command (&00 << &01) OR &01
 point_command (&01 << &01) OR &01
 point_command (&02 << &01) OR &01
 point_command (&03 << &01) OR &01
 point_command (&04 << &01) OR &01
 point_command (&05 << &01) OR &01
 point_command (&06 << &01) OR &01
 point_command (&07 << &01) OR &01
 EQUB &FF

.model_display_flag                     ;flags to control which are available in model mode &00/&ff on/off
 EQUB &00                               ;object_x00_narrow_pyramid
 EQUB &00                               ;object_x01_tall_cube
 EQUB &00                               ;object_x02_short_cube
 EQUB &00                               ;object_x03_wide_pyramid
 EQUB &00                               ;object_x04_tank
 EQUB &00                               ;object_x05_super_tank
 EQUB &00                               ;object_x06_tank_shot
 EQUB &00                               ;object_x07_missile
 EQUB &00                               ;object_x08_saucer
 EQUB &00                               ;object_x09_m_shell
 EQUB &FF                               ;object_x0A_tank_radar
 EQUB &00                               ;object_x0B_ship_chunk_00
 EQUB &00                               ;object_x0C_ship_chunk_01
 EQUB &00                               ;object_x0D_ship_chunk_02
 EQUB &00                               ;object_x0E_ship_chunk_04
 EQUB &00                               ;object_x0F_ship_chunk_05
 EQUB &FF                               ;object_x10_forward_track_00
 EQUB &FF                               ;object_x11_forward_track_01
 EQUB &FF                               ;object_x12_forward_track_02
 EQUB &FF                               ;object_x13_forward_track_03
 EQUB &FF                               ;object_x14_reverse_tracks_00
 EQUB &FF                               ;object_x15_reverse_tracks_01
 EQUB &FF                               ;object_x16_reverse_tracks_02
 EQUB &FF                               ;object_x17_reverse_tracks_03
 EQUB &FF                               ;object_x18_battlezone_part01
 EQUB &FF                               ;object_x19_battlezone_part02
 EQUB &FF                               ;object_x1A_battlezone_part03
 EQUB &00                               ;object_x1B_explosion_00
 EQUB &00                               ;object_x1C_explosion_01
 EQUB &00                               ;object_x1D_explosion_02
 EQUB &00                               ;object_x1E_explosion_03
 EQUB &FF                               ;object_x1F_exhaust_00
 EQUB &FF                               ;object_x20_exhaust_01
 EQUB &FF                               ;object_x21_exhaust_02
 EQUB &FF                               ;object_x22_exhaust_03
.model_display_flag_end
