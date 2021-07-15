; render
; render screen maintenance full 3d object render
; my tank movement/attract mode
;
; x' = x
; y' = y * cos q - z * sin q
; z' = y * sin q + z * cos q
;
; x' = x * cos q - z * sin q
; y' = y
; z' = x * sin q + z * cos q
;
; x' = x * cos q - y * sin q
; y' = x * sin q + y * cos q
; z' = z
;
; 3d routines               model  game
; object_transfer_16        x      x
; object_rotate_16g         -      x
; object_rotate_16m         x      -
; object_radar_trans_16     x      x
; object_view_rotate_16     x      x
; object_view_transform_16  x      x
; object_origin_16          x      x
; object_draw_16            x      x
; object_radar_trans_16     x      x
; object_radar_tracks_16g   -      x
; object_radar_tracks_16m   x      -
;
; constants
 object_x00_narrow_pyramid              = &00
 object_x01_tall_cube                   = &01
 object_x02_short_cube                  = &02
 object_x03_wide_pyramid                = &03
 object_x04_tank                        = &04
 object_x05_super_tank                  = &05
 object_x06_tank_shot                   = &06
 object_x07_missile                     = &07
 object_x08_saucer                      = &08
 object_x09_m_shell                     = &09
 object_x0A_tank_radar                  = &0A
 object_x0B_ship_chunk_00               = &0B
 object_x0C_ship_chunk_01               = &0C
 object_x0D_ship_chunk_02               = &0D
 object_x0E_ship_chunk_04               = &0E
 object_x0F_ship_chunk_05               = &0F
 object_x10_forward_track_00            = &10
 object_x11_forward_track_01            = &11
 object_x12_forward_track_02            = &12
 object_x13_forward_track_03            = &13
 object_x14_reverse_tracks_00           = &14
 object_x15_reverse_tracks_01           = &15
 object_x16_reverse_tracks_02           = &16
 object_x17_reverse_tracks_03           = &17
 object_x18_battlezone_part01           = &18
 object_x19_battlezone_part02           = &19
 object_x1A_battlezone_part03           = &1A
 object_x1B_explosion_00                = &1B
 object_x1C_explosion_01                = &1C
 object_x1D_explosion_02                = &1D
 object_x1E_explosion_03                = &1E
 object_x1F_exhaust_00                  = &1F
 object_x20_exhaust_01                  = &20
 object_x21_exhaust_02                  = &21
 object_x22_exhaust_03                  = &22

 object_radar_spin                      = &0B << &02
 object_saucer_spin                     = &03 << &02

 fustrum_near                           = &03FF
 fustrum_far                            = &7AFF

 radar_vanishing                        = &10
 track_vanishing                        = &08

 distance_step                          = &80
 last_object                            = object_x22_exhaust_03 + &01

.rotate_left                            ;y = key bit map
 LDA m_tank_rotation
 CLC
 ADC m_tank_speed_of_rotation
 STA m_tank_rotation
 BCC check_speed_rotate_left            ;skip wrap around check
 INC m_tank_rotation + &01
 LDA m_tank_rotation + &01
 SBC #HI(landscape_limit)               ;wrap around c=1
 BCC check_speed_rotate_left
 STA m_tank_rotation + &01
.check_speed_rotate_left                ;only rotate maximum 4 pixels a time
 LDA m_tank_speed_of_rotation
 CPY #&09                               ;a and m pressed
 BEQ rotating_left_faster
 CMP #&02
 BCS slow_rotation
.rotate_double
 ASL m_tank_speed_of_rotation
 RTS
.rotating_left_faster
 CMP #&04
 BCC rotate_double
.no_move
 RTS

.key_pressed_table
 EQUW pressed_m                         ;m
 EQUW pressed_k                         ;k
 EQUW movement_no_action
 EQUW pressed_z                         ;z
 EQUW pressed_z_m                       ;z + m
 EQUW rotate_right                      ;k + z
 EQUW movement_no_action
 EQUW pressed_a                         ;a
 EQUW rotate_left                       ;m + a
 EQUW pressed_a_k                       ;a + k
.key_pressed_table_end

 IF (key_pressed_table >> 8) <> (key_pressed_table_end >> 8)
   ERROR ">>>> key vector table across two pages"
 ENDIF

.attract_movement                       ;control tank in attract mode
 DEC frame_counter
 LDX #block_size                        ;index for my tank
 JSR calculate_movement_deltas          ;compute delta x/z
 BIT frame_counter
 BVC go_backwards
 JSR move_forward
 JSR move_forward
 JMP turn_around
.go_backwards
 JSR move_backward
 JSR move_backward
.turn_around
 BIT frame_counter
 BPL turn_right
 BVC no_move
 LDY #&09                               ;m + a
 BNE rotate_left                        ;always
.turn_right                             ;k + z
 LDY #&06                               ;roll into routine below

.rotate_right                           ;y = key bit map
 LDA m_tank_rotation
 SEC
 SBC m_tank_speed_of_rotation
 STA m_tank_rotation
 BCS check_speed_rotate_right
 DEC m_tank_rotation + &01
 BPL check_speed_rotate_right
 LDA #HI(landscape_limit - &01)
 STA m_tank_rotation + &01
.check_speed_rotate_right               ;only rotate maximum 4 pixels a time
 LDA m_tank_speed_of_rotation
 CPY #&06                               ;both k and z pressed
 BEQ rotating_right_faster
 CMP #&02
 BCC rotate_double
.slow_rotation
 LSR m_tank_speed_of_rotation
 RTS
.rotating_right_faster
 CMP #&04
 BCC rotate_double
 RTS

.movement_keys                          ;if playing create a bit map of the four movement keys
 ASL combined_a                         ;a-z-k-m keys
 ROL A
 ASL combined_z                         ;8-4-2-1 bits
 ROL A                                  ;certain key combinations are invalid as on the
 ASL combined_k                         ;arcade machine these are mutually exclusive
 ROL A
 ASL combined_m
 ROL A
 AND #&0F
 BEQ movement_no_action                 ;no keys pressed so no action
 CMP #&0B
 BCS movement_no_action                 ;anything => then no action
 PHA                                    ;save key bit map
 LDX #block_size                        ;index for my tank
 JSR copy_position_save                 ;save position
 JSR calculate_movement_deltas          ;compute delta x/z
 PLA                                    ;retrieve key bit map
 TAY                                    ;save bit map in y
 ASL A                                  ;c=0
 ADC #LO(key_pressed_table - &02)
 STA movement_vector + &01
 LDX #block_size
.movement_vector
 JMP (key_pressed_table)                ;vector to movement code
.movement_no_action
 LDA #&01                               ;initialise to slow ready for key press
 STA m_tank_speed_of_rotation
 RTS

.pressed_m
 JSR rotate_left
 JMP move_back_once

.pressed_k
 JSR rotate_right
 JSR move_forward
 JMP check_collision

.pressed_a
 JSR rotate_left
 JSR move_forward
 JMP check_collision

.pressed_z
 JSR rotate_right
 JSR move_backward
 JMP check_collision

.pressed_a_k
 JSR move_forward
 JSR move_forward
 JMP check_collision

.pressed_z_m
 JSR move_backward
.move_back_once
 JSR move_backward                      ;roll into routine below

.check_collision
 LDX #block_size                        ;check object collision against my tank
 JSR object_collision_test
 BCC clear_collision_flag               ;didn't collide
 JSR copy_position_restore              ;restore position
 LDA recent_collision_flag              ;collided previously?
 BNE reverse_up_two                     ;yes
 LDA #&07
 STA b_object_bounce_near               ;object bounce
 INC b_object_bounce_far                ;horizon movement
 INC recent_collision_flag
 INC sound_motion_blocked               ;motion blocked
 INC sound_bump                         ;bump sound
.reverse_up_two
 LDA #console_messages
 STA console_motion_blocked             ;"motion blocked by object"
 BNE reverse_up                         ;always
.clear_collision_flag                   ;clear recent collision
 LSR recent_collision_flag
.reverse_up
 INC sound_engine_rev_up
 RTS

.move_forward
 LDA tank_or_super_or_missile + &02,X   ;add movement delta to position
 CLC
 ADC movement_vector_x
 STA tank_or_super_or_missile + &02,X
 LDA tank_or_super_or_missile + &03,X
 ADC movement_vector_x + &01
 STA tank_or_super_or_missile + &03,X
 LDA tank_or_super_or_missile + &06,X
 CLC
 ADC movement_vector_z
 STA tank_or_super_or_missile + &06,X
 LDA tank_or_super_or_missile + &07,X
 ADC movement_vector_z + &01
 STA tank_or_super_or_missile + &07,X
 RTS

.move_backward
 LDA tank_or_super_or_missile + &02,X   ;subtract movement delta from position
 SEC
 SBC movement_vector_x
 STA tank_or_super_or_missile + &02,X
 LDA tank_or_super_or_missile + &03,X
 SBC movement_vector_x + &01
 STA tank_or_super_or_missile + &03,X
 LDA tank_or_super_or_missile + &06,X
 SEC
 SBC movement_vector_z
 STA tank_or_super_or_missile + &06,X
 LDA tank_or_super_or_missile + &07,X
 SBC movement_vector_z + &01
 STA tank_or_super_or_missile + &07,X
 RTS

.object_sine_cosines                    ;get sine/cosines for object x/y/z angles
 LDA z_object_rotation
 JSR sine_256
 STX z_sine
 STA z_sine + &01
 LDA z_object_rotation
 JSR cosine_256
 STX z_cosine
 STA z_cosine + &01
 LDA x_object_rotation
 JSR sine_256
 STX x_sine
 STA x_sine + &01
 LDA x_object_rotation
 JSR cosine_256
 STX x_cosine
 STA x_cosine + &01
.object_sine_cosines_game               ;game sine/cosines for object y angle
 LDA y_object_rotation
 JSR sine_256
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256
 STX y_cosine
 STA y_cosine + &01
 RTS

.object_info_display
 LDA object_relative_z + &01
 LDY #&0C
 JSR print_it_in_hex
 LDA x_object_rotation
 LDY #&1C
 JSR print_it_in_hex
 LDA y_object_rotation
 LDY #&24
 JSR print_it_in_hex
 LDA z_object_rotation
 LDY #&2C
 JSR print_it_in_hex
 LDA i_object_identity                  ;current object
 LDY #&3C
.print_it_in_hex
 STY prynt_hex + &03
 PHA
 LSR A
 LSR A
 LSR A
 LSR A
 JSR hex_convert
 STA hex_text
 PLA
 AND #&0F
 JSR hex_convert
 ORA #&80
 STA hex_text + &01
 LDX #LO(prynt_hex)
 LDY #HI(prynt_hex)
 JMP print

.hex_convert
 SED
 CMP #&0A
 ADC #&30
 CLD
 RTS

.prynt_hex
 EQUW hex_text
 EQUB &08
 EQUB &08
.hex_text
 EQUW &00

.model_display                          ;display defined objects
 JSR object_cycle                       ;cycle through objects
 JSR object_spin_radar
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_sine_cosines
 JSR object_info_display
 LDX i_object_identity
 JSR object_transfer_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 JSR object_track_flip
 JSR object_radar_tracks_16m
 LDA object_rotation_store              ;restore rotation
 STA y_object_rotation
 RTS

.object_spin_radar                      ;spin radar on top of standard tank
 LDA y_object_rotation                  ;save object rotation
 STA object_rotation_store
 LDA object_radar_rotation
 CLC
 ADC #object_radar_spin
 STA object_radar_rotation
 RTS

.object_radar_tracks_16m                ;place radar and tracks on tank
 LDA i_object_identity
 CMP #object_x04_tank
 BNE object_radar_tracks_exitg          ;exit if not standard tank
 LDA object_relative_z + &01
 CMP #radar_vanishing
 BCS object_radar_tracks_exitg          ;too far away then exit (radar and tracks)
 LDX #object_x0A_tank_radar
 JSR object_transfer_16                 ;get radar vertices
 LDA object_radar_rotation              ;internal radar rotation
 STA y_object_rotation
 JSR sine_256                           ;calculate radar y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_rotate_16g                  ;rotate radar about y axis
 LDA object_rotation_store
 STA y_object_rotation                  ;restore y rotation
 JSR sine_256                           ;calculate tracks y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_radar_translate_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 LDA object_relative_z + &01
 CMP #track_vanishing
 BCS object_restore_tank
 LDA tracks_active                       ;bit 7 for tank moving or not
 BMI object_restore_tank                 ;not moving so leave
 AND #&01
 STA tracks_active
 TAX
 INC track_index
 LDA track_index
 AND #&03
 ADC track_type,X                       ;c=0
 TAX
 JSR object_transfer_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
.object_restore_tank
 LDA #object_x04_tank                   ;restore tank
 STA i_object_identity
.object_radar_tracks_exitg
 RTS

.object_radar_tracks_16g                ;place radar and tracks on tank
 LDX i_object_identity
 CPX #object_x04_tank
 BNE object_radar_tracks_exitg          ;exit if not standard tank
 LDA object_relative_z + &01
 CMP #radar_vanishing
 BCS object_radar_tracks_exitg          ;too far away then exit (radar and tracks)
 LDX #object_x0A_tank_radar
 JSR object_transfer_16                 ;get radar vertices
 LDA y_object_rotation                  ;save y rotation
 PHA
 LDA object_radar_rotation              ;internal radar rotation
 CLC
 ADC #object_radar_spin
 STA object_radar_rotation
 STA y_object_rotation
 JSR sine_256                           ;calculate radar y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_rotate_16g                  ;rotate radar about y axis
 PLA
 STA y_object_rotation                  ;restore y rotation
 JSR sine_256                           ;calculate tracks y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_radar_translate_16
 JSR object_rotate_16g
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 LDA object_relative_z + &01            ;now test for tracks
 CMP #track_vanishing
 BCS object_radar_tracks_exitg
 LDA tracks_active                      ;bit 7 for tank moving or not
 BMI object_radar_tracks_exitg          ;not moving so leave
 AND #&01
 STA tracks_active
 TAX
 INC track_index
 LDA track_index
 AND #&03
 ADC track_type,X                       ;c=0
 TAX                                    ;x=track object
 JSR object_transfer_16
 JSR object_rotate_16g
 JSR object_view_transform_16
 JSR object_origin_16
 JMP object_draw_16

.object_track_flip
 DEC track_counter
 BNE object_track_flip_exit
 SEC
 ROR track_counter
 INC tracks_active                      ;next set of tracks
.object_track_flip_exit
 RTS

.object_radar_rotation
 EQUB &00
.object_rotation_store
 EQUB &00
.track_counter
 EQUB &80
.tracks_active
 EQUB &00
.track_index
 EQUB &00
.track_type
 EQUB object_x10_forward_track_00
 EQUB object_x14_reverse_tracks_00

.object_cycle                           ;cycle through defined objects
 BIT combined_arrow_up
 BPL not_combined_arrow_up
 LDA object_relative_y
 SEC
 SBC #LO(distance_step)
 STA object_relative_y
 LDA object_relative_y + &01
 SBC #HI(distance_step)
 STA object_relative_y + &01
.not_combined_arrow_up
 BIT combined_arrow_down
 BPL not_combined_arrow_down
 LDA object_relative_y
 CLC
 ADC #LO(distance_step)
 STA object_relative_y
 LDA object_relative_y + &01
 ADC #HI(distance_step)
 STA object_relative_y + &01
.not_combined_arrow_down
 BIT combined_arrow_left
 BPL not_combined_arrow_left
 LDA object_relative_x
 SEC
 SBC #LO(distance_step)
 STA object_relative_x
 LDA object_relative_x + &01
 SBC #HI(distance_step)
 STA object_relative_x + &01
.not_combined_arrow_left
 BIT combined_arrow_right
 BPL not_combined_arrow_right
 LDA object_relative_x
 CLC
 ADC #LO(distance_step)
 STA object_relative_x
 LDA object_relative_x + &01
 ADC #HI(distance_step)
 STA object_relative_x + &01
.not_combined_arrow_right
 BIT combined_m
 BPL not_combined_m
 LDA object_relative_z                  ;increase distance
 CLC
 ADC #LO(distance_step)
 TAX
 STA object_relative_z
 LDA object_relative_z + &01
 ADC #HI(distance_step)
 STA object_relative_z + &01
 CPX #LO(fustrum_far + &01)             ;test for far plane
 SBC #HI(fustrum_far + &01)
 BCC not_combined_m
 LDA #LO(fustrum_far + &01)             ;stop at far plane
 STA object_relative_z
 LDA #HI(fustrum_far + &01)
 STA object_relative_z + &01
.not_combined_m
 BIT combined_n
 BPL not_combined_n                     ;decrease distance
 LDA object_relative_z
 SEC
 SBC #LO(distance_step)
 TAX
 STA object_relative_z
 LDA object_relative_z + &01
 SBC #HI(distance_step)
 STA object_relative_z + &01
 CPX #LO(fustrum_near)                  ;test for near plane
 SBC #HI(fustrum_near)
 BCS not_combined_n
 LDA #LO(fustrum_near)                  ;stop at near plane
 STA object_relative_z
 LDA #HI(fustrum_near)
 STA object_relative_z + &01
.not_combined_n
 BIT combined_a                         ;rotate x-axis
 BPL not_combined_a
 INC x_object_rotation
.not_combined_a
 BIT combined_z                         ;rotate y-axis
 BPL not_combined_z
 INC y_object_rotation
.not_combined_z
 BIT combined_k                         ;rotate z-axis
 BPL not_combined_k
 INC z_object_rotation
.not_combined_k
 BIT combined_r                         ;change object
 BPL object_exit
 INC i_object_identity                  ;next object
 LDA i_object_identity
 CMP #last_object
 BCC no_wrap_object
 LDA #&00
.no_wrap_object
 STA i_object_identity
.object_exit
 RTS

.object_transfer_16                     ;x = object identity, set up counters and workspace
 LDA model_vertices_table_lo,X
 STA model_vertices_address
 LDA model_vertices_table_hi,X
 STA model_vertices_address + &01
 LDA model_segment_table_start_lo,X     ;model segment start vertice address
 STA model_segment_address
 LDA model_segment_table_start_hi,X
 STA model_segment_address + &01
 LDA model_vertices_number,X            ;model vertices number
 STA model_vertices_counter
 LDY model_vertices_index,X
 TAX
.transfer_vertices                      ;transfer vertices data
 LDA (model_vertices_address),Y
 STA vertices_z_msb,X
 DEY
 LDA (model_vertices_address),Y
 STA vertices_z_lsb,X
 DEY
 LDA (model_vertices_address),Y
 STA vertices_y_msb,X
 DEY
 LDA (model_vertices_address),Y
 STA vertices_y_lsb,X
 DEY
 LDA (model_vertices_address),Y
 STA vertices_x_msb,X
 DEY
 LDA (model_vertices_address),Y
 STA vertices_x_lsb,X
 DEY
 DEX
 BPL transfer_vertices
 STX model_segment_counter              ;model segment number &ff
 RTS

.object_radar_translate_16              ;move radar to left just above the tank
 LDX #(tank_radar_vertices_end - tank_radar_vertices) / &06 - &01
.object_translate_loop_16
 LDA vertices_z_lsb,X
 SEC
 SBC #LO(radar_x_adjust)
 STA vertices_z_lsb,X
 LDA vertices_z_msb,X
 SBC #HI(radar_x_adjust)
 STA vertices_z_msb,X
 DEX
 BPL object_translate_loop_16
 RTS

.object_rotate_16m                      ;model rotation around x/y/z
 LDX model_vertices_counter
 BIT mathbox_flag
 BMI object_rotate_16m_arm
.object_rotate_loop_16m
 STX model_vertices_work
 LDA vertices_x_lsb,X
 STA x_prime
 LDA vertices_x_msb,X
 STA x_prime + &01
 LDA vertices_y_lsb,X
 STA y_prime
 LDA vertices_y_msb,X
 STA y_prime + &01
 LDA vertices_z_lsb,X
 STA z_prime
 LDA vertices_z_msb,X
 STA z_prime + &01
 JSR object_x_rotation_16
 JSR object_y_rotation_16
 JSR object_z_rotation_16
 LDX model_vertices_work
 LDA x_prime
 STA vertices_x_lsb,X
 LDA x_prime + &01
 STA vertices_x_msb,X
 LDA y_prime
 STA vertices_y_lsb,X
 LDA y_prime + &01
 STA vertices_y_msb,X
 LDA z_prime
 STA vertices_z_lsb,X
 LDA z_prime + &01
 STA vertices_z_msb,X
 DEX
 BPL object_rotate_loop_16m
 RTS

.object_rotate_16m_arm                  ;mathbox rotate vertices
 STX model_vertices_work
 LDA vertices_x_lsb,X
 STA host_r0
 LDA vertices_x_msb,X
 STA host_r0 + &01
 LDA vertices_y_lsb,X
 STA host_r1
 LDA vertices_y_msb,X
 STA host_r1 + &01
 LDA vertices_z_lsb,X
 STA host_r2
 LDA vertices_z_msb,X
 STA host_r2 + &01
 LDA #mathbox_code_rotation_vertice16
 LDX #host_register_block
 JSR mathbox_function_ax
 LDX model_vertices_work
 LDA host_r0
 STA vertices_x_lsb,X
 LDA host_r0 + &01
 STA vertices_x_msb,X
 LDA host_r1
 STA vertices_y_lsb,X
 LDA host_r1 + &01
 STA vertices_y_msb,X
 LDA host_r2
 STA vertices_z_lsb,X
 LDA host_r2 + &01
 STA vertices_z_msb,X
 DEX
 BPL object_rotate_16m_arm
.object_draw_16_exit
 RTS

MACRO cs_clip_code_ymax y0, y_hi
 CPY y_hi
 LDA y0 + &01
 SBC y_hi + &01
 BVS no_eor_ymax
 EOR #&80
.no_eor_ymax                            ;n=0 when a<num, n=1 a>=num 
ENDMACRO

MACRO cs_clip_code_ymin y0, y_lo
 CPY y_lo
 LDA y0 + &01
 SBC y_lo + &01
 BVC no_eor_ymin_addr
 EOR #&80
.no_eor_ymin_addr                        ;n=1 when a < num, n=0 a>=num                                 
ENDMACRO

MACRO cs_clip_code_xmin x0, x_lo
 CPX x_lo
 LDA x0 + &01
 SBC x_lo + &01
 BVC no_eor_xmin
 EOR #&80
.no_eor_xmin                            ;n=1 when a<num, n=0 a>=num
ENDMACRO

MACRO cs_clip_code_xmax x0, x_hi
 CPX x_hi
 LDA x0 + &01
 SBC x_hi + &01
 BVS no_eor_xmax
 EOR #&80
.no_eor_xmax                            ;n=0 when a < num, n=1 a>=num
ENDMACRO

.object_draw_16_on_08
 JSR mathbox_line_draw08                ;inner window
 LDA graphic_temp                       ;restore original coordinates
 STA graphic_x_00
 LDA graphic_temp + &01
 STA graphic_x_00 + &01
 LDA graphic_temp + &02
 STA graphic_y_00
 LDA graphic_temp + &03
 STA graphic_y_00 + &01                 ;roll into routine below

.object_draw_16
 INC model_segment_counter              ;initialised at &ff
 LDY model_segment_counter
 LDA (model_segment_address),Y          ;get vertices number 0 -  31
 BMI object_draw_16_exit                ;bit 7=1, finished
 LSR A                                  ;c=1 plot a point
 BCS plot_point
 LSR A                                  ;c=1 draw else move
 TAX                                    ;vertices index 0 - 31
 BCS draw_position
 LDA vertices_x_lsb,X                   ;move vertices coordinates
 STA graphic_x_00
 STA graphic_temp
 LDA vertices_x_msb,X
 STA graphic_x_00 + &01
 STA graphic_temp + &01
 LDA vertices_y_lsb,X
 ADC b_object_bounce_near               ;c=0
 STA graphic_y_00
 STA graphic_temp + &02
 LDA vertices_y_msb,X
 ADC #&00
 STA graphic_y_00 + &01
 STA graphic_temp + &03
 JMP object_draw_16
.draw_position
 LDA vertices_x_lsb,X                   ;draw vertices coordinates
 STA graphic_x_01
 STA graphic_temp
 LDA vertices_x_msb,X
 STA graphic_x_01 + &01
 STA graphic_temp + &01
 LDA vertices_y_lsb,X
 CLC
 ADC b_object_bounce_near
 STA graphic_y_01
 STA graphic_temp + &02
 LDA vertices_y_msb,X
 ADC #&00
 STA graphic_y_01 + &01
 STA graphic_temp + &03
 JSR mathbox_line_clip16
 BCS object_draw_16_not_on              ;c=1 do not draw
 BEQ object_draw_16_on_08               ;z=1 inner window
 JSR mathbox_line_draw16c
.object_draw_16_not_on
 LDA graphic_temp                       ;restore original coordinates
 STA graphic_x_00
 LDA graphic_temp + &01
 STA graphic_x_00 + &01
 LDA graphic_temp + &02
 STA graphic_y_00
 LDA graphic_temp + &03
 STA graphic_y_00 + &01
 JMP object_draw_16

.plot_point                             ;plot a point
 TAX                                    ;no need to shift move/draw bit out as done within data
 LDA vertices_x_lsb,X                   ;move vertices coordinates
 STA graphic_x_00
 LDA vertices_x_msb,X
 STA graphic_x_00 + &01
 LDA vertices_y_lsb,X
 CLC
 ADC b_object_bounce_near
 STA graphic_y_00
 LDA vertices_y_msb,X
 ADC #&00
 STA graphic_y_00 + &01                 ;check graphic window
 LDX graphic_x_00
 cs_clip_code_xmax graphic_x_00, window_x_01
 BMI plot_point_exit
 cs_clip_code_xmin graphic_x_00, window_x_00
 BMI plot_point_exit
 LDY graphic_y_00
 cs_clip_code_ymax graphic_y_00, window_y_01
 BMI plot_point_exit
 cs_clip_code_ymin graphic_y_00, window_y_00
 BMI plot_point_exit                    ;exit x=graphic_x_00, y=graphic_y_00
 TXA                                    ;point within screen window so plot
 AND #&F8
 CLC
 ADC screen_access_y_lo,Y
 STA particle_address
 LDA screen_access_y_hi,Y
 ADC screen_hidden
 ADC graphic_x_00 + &01
 STA particle_address + &01
 TXA
 AND #&07
 TAX
 LDY #&00
 LDA pixel_mask,X
 ORA (particle_address),Y
 STA (particle_address),Y
.plot_point_exit
 JMP object_draw_16

.object_origin_16                       ;add in screen origin to vertices
 LDX model_vertices_counter
.object_origin_16_loop
 LDA vertices_x_lsb,X
 CLC
 ADC graphic_x_origin
 STA vertices_x_lsb,X
 LDA vertices_x_msb,X
 ADC graphic_x_origin + &01
 STA vertices_x_msb,X
 LDA vertices_y_lsb,X
 CLC
 ADC graphic_y_origin
 STA vertices_y_lsb,X
 LDA vertices_y_msb,X
 ADC graphic_y_origin + &01
 STA vertices_y_msb,X
 DEX
 BPL object_origin_16_loop
 RTS

.object_rotate_16g_arm                  ;mathbox rotation y axis only
 STX model_vertices_work
 LDA vertices_x_lsb,X
 STA host_r0
 LDA vertices_x_msb,X
 STA host_r0 + &01
 LDA vertices_z_lsb,X
 STA host_r2
 LDA vertices_z_msb,X
 STA host_r2 + &01
 LDA #mathbox_code_rotate16
 LDX #host_register_block               ;results
 JSR mathbox_function_ax
 LDX model_vertices_work
 LDA host_r0
 STA vertices_x_lsb,X
 LDA host_r0 + &01
 STA vertices_x_msb,X
 LDA host_r2
 STA vertices_z_lsb,X
 LDA host_r2 + &01
 STA vertices_z_msb,X
 DEX
 BPL object_rotate_16g_arm
 RTS

.object_rotate_16g                      ;game rotation y axis only
 LDX model_vertices_counter             ;x' = x * cos q - z * sin q
 BIT mathbox_flag                       ;y' = y
 BMI object_rotate_16g_arm              ;z' = x * sin q + z * cos q
.object_rotate_loop_16g
 STX model_vertices_work
 LDA vertices_x_lsb,X
 STA x_prime
 LDA vertices_x_msb,X
 STA x_prime + &01
 LDA vertices_z_lsb,X
 STA z_prime
 LDA vertices_z_msb,X
 STA z_prime + &01
 JSR object_y_rotation_16
 LDX model_vertices_work
 LDA x_prime
 STA vertices_x_lsb,X
 LDA x_prime + &01
 STA vertices_x_msb,X
 LDA z_prime
 STA vertices_z_lsb,X
 LDA z_prime + &01
 STA vertices_z_msb,X
 DEX
 BPL object_rotate_loop_16g
 RTS

 MACRO rotation p1, p2, sine, cosine
 LDA sine
 STA sine_a
 LDA sine + &01
 STA sine_a + &01
 LDA cosine
 STA cosine_a
 LDA cosine + &01
 STA cosine_a + &01
 LDA p1
 STA vertice_a
 LDA p1 + &01
 STA vertice_a + &01
 LDA p2
 STA vertice_b
 LDA p2 + &01
 STA vertice_b + &01
 JSR object_rotation16
 LDA vertice_a
 STA p1
 LDA vertice_a + &01
 STA p1 + &01
 LDA vertice_b
 STA p2
 LDA vertice_b + &01
 STA p2 + &01
 ENDMACRO

.object_x_rotation_16
; x' = x
; y' = y * cos q - z * sin q
; z' = y * sin q + z * cos q
 rotation y_prime, z_prime, x_sine, x_cosine
 RTS

.object_y_rotation_16
; x' = x * cos q - z * sin q
; y' = y
; z' = x * sin q + z * cos q
 rotation x_prime, z_prime, y_sine, y_cosine
 RTS

.object_z_rotation_16
; x' = x * cos q - y * sin q
; y' = x * sin q + y * cos q
; z' = z
 rotation x_prime, y_prime, z_sine, z_cosine
 RTS

.object_rotation16                      ;a' = a * cos q - b * sin q
 LDA sine_a                             ;b' = a * sin q + b * cos q
 STA multiplier_16
 LDA sine_a + &01
 STA multiplier_16 + &01
 LDA vertice_b
 STA multiplicand_16
 LDA vertice_b + &01
 STA multiplicand_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &01
 STA ta
 LDA product_16 + &02
 STA ta + &01
 LDA product_16 + &03
 STA ta + &02                           ;tx = b * sin q
 LDA cosine_a                           ;a * cos q
 STA multiplier_16
 LDA cosine_a + &01
 STA multiplier_16 + &01
 LDA vertice_a
 STA multiplicand_16
 LDA vertice_a + &01
 STA multiplicand_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &01
 SEC
 SBC ta
 STA ta
 LDA product_16 + &02
 SBC ta + &01
 STA ta + &01
 LDA product_16 + &03
 SBC ta + &02
 STA ta + &02
 LDA cosine_a
 STA multiplier_16
 LDA cosine_a + &01
 STA multiplier_16 + &01
 LDA vertice_b
 STA multiplicand_16
 LDA vertice_b + &01
 STA multiplicand_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &01
 STA tb
 LDA product_16 + &02
 STA tb + &01
 LDA product_16 + &03
 STA tb + &02                           ;ty = b * cos q
 LDX z_object_rotation
 LDA sine_a                             ;a * sin q
 STA multiplier_16
 LDA sine_a + &01
 STA multiplier_16 + &01
 LDA vertice_a
 STA multiplicand_16
 LDA vertice_a + &01
 STA multiplicand_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &01
 CLC
 ADC tb
 STA tb
 LDA product_16 + &02
 ADC tb + &01
 STA tb + &01
 LDA product_16 + &03
 ADC tb + &02
 STA tb + &02
 ASL tb
 LDA tb + &01
 ROL A
 STA vertice_b
 LDA tb + &02
 ROL A
 STA vertice_b + &01
 ASL ta
 LDA ta + &01
 ROL A
 STA vertice_a
 LDA ta + &02
 ROL A
 STA vertice_a + &01
 RTS

.object_view_rotate_angles              ;take tank rotation and calculate sine/cosine
 LDA m_tank_rotation
 LDX m_tank_rotation + &01
 JSR mathbox_sine1280
 STX y_sine_tank
 STA y_sine_tank + &01
 LDA m_tank_rotation
 LDX m_tank_rotation + &01
 JSR mathbox_cosine1280
 STX y_cosine_tank
 STA y_cosine_tank + &01
 RTS

.object_view_rotate_16                  ;calculate relative coordinates, c=0/1 visible/invisible
 LDA object_start + &02,X               ;calculate relative x
 SEC                                    ;relative x and z are in the ratio 2:1 to simulate foreground objects
 SBC m_tank_x                           ;rotating twice as fast as the landscape as per the arcade version
 STA x_prime                            ;this means z is x1 but x is x2, take into account
 LDA object_start + &03,X               ;when using workspace values elsewhere for radar spot/messages etc
 SBC m_tank_x + &01
 STA x_prime + &01
 LDA object_start + &06,X               ;calculate relative z
 SEC
 SBC m_tank_z
 STA z_prime                            ;z' = x * sin q + z * cos q
 STA multiplicand_16                    ;z * cos q
 LDA object_start + &07,X
 SBC m_tank_z + &01
 STA z_prime + &01
 STA multiplicand_16 + &01
 LDA y_cosine_tank
 STA multiplier_16
 LDA y_cosine_tank + &01
 STA multiplier_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &02
 STA ta
 LDA product_16 + &03
 STA ta + &01
 LDA x_prime                            ;x * sin q
 STA multiplicand_16
 LDA x_prime + &01
 STA multiplicand_16 + &01
 LDA y_sine_tank
 STA multiplier_16
 LDA y_sine_tank + &01
 STA multiplier_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &02
 CLC
 ADC ta
 STA vertice_a
 LDA product_16 + &03
 ADC ta + &01
 BPL z_position_in_view                 ;z +ve so perform rest of rotation
 LDX object_counter                     ;z -ve but perform rest of rotation if unit as use results later
 CPX #tank_or_super_or_missile - object_start
 BEQ z_position_in_view                 ;unit so calculate
 SEC                                    ;not unit and behind us so exit
 RTS
.z_position_in_view
 STA vertice_a + &01
 LDA z_prime                            ;x' = x * cos q - z * sin q
 STA multiplicand_16                    ;z * sin q
 LDA z_prime + &01
 STA multiplicand_16 + &01
 LDA y_sine_tank
 STA multiplier_16
 LDA y_sine_tank + &01
 STA multiplier_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &02
 STA vertice_b
 LDA product_16 + &03
 STA vertice_b + &01
 LDA x_prime                            ;x * cos q
 STA multiplicand_16
 LDA x_prime + &01
 STA multiplicand_16 + &01
 LDA y_cosine_tank
 STA multiplier_16
 LDA y_cosine_tank + &01
 STA multiplier_16 + &01
 JSR mathbox_multiplication16
 LDA product_16 + &02
 SEC
 SBC vertice_b
 STA vertice_b                          ;rotated x
 LDA product_16 + &03
 SBC vertice_b + &01
 ASL vertice_b
 ROL A
 STA vertice_b + &01
 LDA vertice_a + &01                    ;rotated z
 SEC                                    ;c=1 for exit/subtract
 BMI object_out_of_view                 ;test behind, c=1
 SBC #HI(fustrum_near) - &01            ;test far/near fustrum
 SBC #HI(fustrum_far) - HI(fustrum_near) + &01
 BCS object_out_of_view                 ;c=0 if in range n to m
 LDX vertice_b                          ;calculate absolute x
 LDY vertice_b + &01
 BPL x_absolute_is_postive
 TXA
 EOR #&FF
 ADC #&01                               ;c=0
 TAX
 TYA
 EOR #&FF
 ADC #&00
 TAY
.x_absolute_is_postive
 CPX vertice_a                          ;compare absolute x with z, z is positive
 TYA
 SBC vertice_a + &01                    ;c=0 in 90' view/c=1 out of view
.object_out_of_view
 RTS

.object_view_transform_16               ;model_x  = rotated_x + object_x
 LDX model_vertices_counter             ;model_z  = rotated_z + z_absolute
.object_view_transform_16_loop          ;model_y  = vertex_y  + object_y
 STX model_vertices_work                ;screen_x = model_x / model_z
 LDA vertices_z_lsb,X                   ;screen_y = model_y / model_z
 CLC                                    ;xs       = d * x / z
 ADC object_relative_z
 STA z_absolute
 STA divisor_24
 LDA vertices_z_msb,X
 ADC object_relative_z + &01
 STA z_absolute + &01
 STA divisor_24 + &01
 LDA vertices_x_lsb,X
 CLC
 ADC object_relative_x
 STA dividend_24 + &01
 LDA vertices_x_msb,X
 ADC object_relative_x + &01
 STA dividend_24 + &02
 ASL A                                  ;create lsb dividend
 LDA #&00
 ADC #&00
 EOR #&FF
 STA dividend_24
 JSR mathbox_division24
 LDX model_vertices_work
 LDA division_result_24
 STA vertices_x_lsb,X
 LDA division_result_24 + &01
 STA vertices_x_msb,X
 LDA vertices_y_lsb,X
 CLC
 ADC object_relative_y
 STA dividend_24 + &01
 LDA vertices_y_msb,X
 ADC object_relative_y + &01
 STA dividend_24 + &02
 ASL A                                  ;create lsb dividend
 LDA #&00
 ADC #&00
 EOR #&FF
 STA dividend_24
 LDA z_absolute
 STA divisor_24
 LDA z_absolute + &01
 STA divisor_24 + &01
 JSR mathbox_division24
 LDX model_vertices_work
 LDA division_result_24
 STA vertices_y_lsb,X
 LDA division_result_24 + &01
 STA vertices_y_msb,X
 DEX
 BPL object_view_transform_16_loop
 RTS

.object_test_visibility                 ;calculate relative x/y/z
 LDX #object_end - object_start - block_size
.test_visibility
 LDA object_start,X                     ;test status
 BMI test_next_visibility               ;inactive so next object
 CMP #object_x08_saucer                 ;check if saucer in view
 BNE not_saucer
 LDA saucer_dying                       ;check if saucer dying as need to flash it
 BEQ not_saucer
 SEC
 ROR A                                  ;test bit 0
 BCC not_saucer
 BMI test_next_visibility               ;always, flash the saucer on/off, bit 7 set
.not_saucer
 STX object_counter
 JSR object_view_rotate_16              ;exit c=0/1 visible/invisible
 LDX object_counter
 LDA vertice_b                          ;copy rotated x to workspace
 STA object_workspace + &02,X
 LDA vertice_b + &01
 STA object_workspace + &03,X
 LDA vertice_a                          ;copy rotated z to workspace
 STA object_workspace + &06,X
 LDA vertice_a + &01
 STA object_workspace + &07,X
 ROR A                                  ;rotate carry result into bit 7 then store
.test_next_visibility
 STA object_workspace,X                 ;store bit 7
 TXA
 SEC
 SBC #block_size                        ;size of object block
 TAX
 BCS test_visibility
 RTS

.object_render                          ;render all active objects
 LDY #object_end - object_start - block_size
.render_objects
 LDA object_workspace,Y                 ;in view flag?
 BMI test_next_object                   ;no
 STY object_counter
 LDX object_start,Y                     ;object identity
 LDA object_workspace + &02,Y
 STA object_relative_x
 LDA object_workspace + &03,Y
 STA object_relative_x + &01
 LDA object_start + &04,Y               ;y not calculated
 STA object_relative_y
 LDA object_start + &05,Y
 STA object_relative_y + &01
 LDA object_workspace + &06,Y
 STA object_relative_z
 LDA object_workspace + &07,Y
 STA object_relative_z + &01
 LDA m_tank_rotation_256                ;adjust object rotation
 SEC
 SBC object_start + &01,Y               ;object y rotation
 EOR #&80
 STA y_object_rotation
 STX i_object_identity
 JSR object_transfer_16                 ;transfer object to workspace
 JSR mathbox_rotation_angles            ;transfer object angles to mathbox, y axis only
 JSR object_sine_cosines_game
 JSR object_rotate_16g
 JSR object_view_transform_16
 JSR object_origin_16
 JSR object_draw_16
 JSR object_radar_tracks_16g            ;test for standard tank, place radar/tracks
 LDY object_counter
.test_next_object
 TYA
 SEC
 SBC #block_size                        ;size of object block
 TAY
 BCS render_objects
 RTS
