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
; 3d routines                     model  game
; object_transfer_16              x      x
; object_rotate_16g               -      x
; object_rotate_16m               x      -
; object_radar_translate_16       x      x
; object_view_rotate_16           x      x
; object_view_transform_16        x      x
; object_draw_16                  x      x
; object_radar_tracks_exhaust_16m x      -
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

 object_radar_spin                      = &08 << &02
 object_saucer_spin                     = &03 << &02

 fustrum_near                           = &03FF
 fustrum_far                            = &7AFF

 radar_vanishing                        = &10
 track_vanishing                        = &08

 distance_step                          = &80

.attract_counter
 EQUB &00

.attract_movement                       ;control my tank in attract mode
 LDA new_game_mode                      ;new game mode playing game?
 BEQ not_attract_movement               ;yes
 JSR calculate_movement_deltas_player   ;compute delta x/z
 BIT attract_counter                    ;attract_counter decides forward/backwards movement
 BVS go_backwards
 JSR move_player_forward
 JMP turn
.go_backwards
 JSR move_player_backward
.turn
 BIT tank_or_super_or_missile_z + &01
 BPL rotate_right
 BVC not_attract_movement               ;exit

.rotate_left
 LDA #&80
 CLC
 ADC m_tank_rotation_512
 STA m_tank_rotation_512
 LDA #&00
 ADC m_tank_rotation_512 + &01
 STA m_tank_rotation_512 + &01
 JMP calculate_rotation

.rotate_right
 LDA m_tank_rotation_512
 SEC
 SBC #&80
 STA m_tank_rotation_512
 LDA m_tank_rotation_512 + &01
 SBC #&00
 STA m_tank_rotation_512 + &01          ;roll into routine below

.calculate_rotation
 LDA m_tank_rotation_512                ;multiply by 2.5
 ASL A
 LDA m_tank_rotation_512 + &01
 ROL A
 STA m_tank_rotation
 ROL A
 AND #&01
 ASL m_tank_rotation
 ROL A
 TAX
 LDA m_tank_rotation_512 + &01          ;calculate 0.5 then add in
 CLC
 ADC m_tank_rotation
 STA m_tank_rotation
 TXA
 ADC #&00
 STA m_tank_rotation + &01              ;&00 - &4ff
.not_attract_movement
.test_no_keys
 RTS

.movement_keys                          ;if playing create a bit map of the four movement keys
 LDA m_tank_status                      ;are we dead?
 BMI test_no_keys                       ;yes so stay still
 ASL combined_a                         ;a = 0, a-z-k-m keys
 ROL A
 ASL combined_z                         ;8-4-2-1 bits
 ROL A                                  ;certain key combinations are invalid as on the
 ASL combined_k                         ;arcade machine they are mutually exclusive due to the nature
 ROL A                                  ;of the controls
 ASL combined_m
 ROL A
 BEQ movement_no_action                 ;no keys pressed so no action
 CMP #&0B
 BCS movement_no_action                 ;anything => then no action
 PHA                                    ;save key bit map
 LDA m_tank_x                           ;save player position
 STA unit_x_pos
 LDA m_tank_x + &01
 STA unit_x_pos + &01
 LDA m_tank_z
 STA unit_z_pos
 LDA m_tank_z + &01
 STA unit_z_pos + &01
 JSR calculate_movement_deltas_player   ;compute delta x/z
 PLA                                    ;retrieve key bit map
 TAX
 LDA key_pressed_table - &01,X          ;first value is for m key = &01
 STA movement_branch + &01
.movement_branch
 BNE movement_branch                    ;always, vector to movement code

.key_pressed_table
 EQUB pressed_m          - movement_branch - &02  ;m
 EQUB pressed_k          - movement_branch - &02  ;k
 EQUB movement_no_action - movement_branch - &02
 EQUB pressed_z          - movement_branch - &02  ;z
 EQUB pressed_z_m        - movement_branch - &02  ;z + m
 EQUB pressed_k_z        - movement_branch - &02  ;k + z
 EQUB movement_no_action - movement_branch - &02
 EQUB pressed_a          - movement_branch - &02  ;a
 EQUB pressed_a_m        - movement_branch - &02  ;m + a
 EQUB pressed_a_k        - movement_branch - &02  ;a + k

.movement_no_action
 LDA #&00
 STA sound_engine_pitch                 ;rev engine down
 LDA recent_collision_flag
 BEQ no_previous_collision
 LDA #console_messages                  ;keep putting out message even if still
 STA console_motion_blocked             ;"motion blocked by object"
.no_previous_collision
 RTS

.pressed_m
 JSR rotate_left
 JMP move_back_once

.pressed_k
 JSR rotate_right
 JSR move_player_forward
 JMP check_collision

.pressed_a
 JSR rotate_left
 JSR move_player_forward
 JMP check_collision

.pressed_z
 JSR rotate_right
 JSR move_player_backward
 JMP check_collision

.pressed_a_k
 JSR move_player_forward
 JSR move_player_forward
 JMP check_collision

.pressed_k_z
 LSR recent_collision_flag
 JSR rotate_right
 JMP rotate_right

.pressed_a_m
 LSR recent_collision_flag
 JSR rotate_left
 JMP rotate_left

.pressed_z_m
 JSR move_player_backward
.move_back_once
 JSR move_player_backward               ;roll into routine below

.check_collision
 LDX #block_size                        ;check object collision against my tank
 JSR object_collision_test
 BCC clear_collision_flag_engine        ;didn't collide
 BEQ not_collided_with_unit             ;c=1 collided with unit, &00 for obstacle, &ff for unit, encoded in z flag
 BIT missile_flag                       ;collided with missile?
 BMI clear_collision_flag               ;yes, let missile update destroy it
.not_collided_with_unit
 LDA unit_x_pos                         ;restore player position
 STA m_tank_x
 LDA unit_x_pos + &01
 STA m_tank_x + &01
 LDA unit_z_pos
 STA m_tank_z
 LDA unit_z_pos + &01
 STA m_tank_z + &01
 LDA recent_collision_flag              ;collided previously?
 BNE reverse_up                         ;yes
 LDA #&07
 STA b_object_bounce_near               ;object bounce
 LDA #&00
 STA sound_engine_pitch                 ;engine in idle
 INC b_object_bounce_far                ;horizon movement
 INC recent_collision_flag
 INC sound_bump                         ;bump sound
.reverse_up
 LDA #console_messages
 STA console_motion_blocked             ;"motion blocked by object"
 RTS
.clear_collision_flag_engine            ;clear recent collision
 INC sound_engine_pitch                 ;rev up engine
.clear_collision_flag
 LSR recent_collision_flag
 RTS

.move_player_forward
 LDA m_tank_x                           ;add movement delta to player position
 CLC
 ADC movement_vector_x
 STA m_tank_x
 LDA m_tank_x + &01
 ADC movement_vector_x + &01
 STA m_tank_x + &01
 LDA m_tank_z
 CLC
 ADC movement_vector_z
 STA m_tank_z
 LDA m_tank_z + &01
 ADC movement_vector_z + &01
 STA m_tank_z + &01
 RTS

.move_unit_forward
 LDA tank_or_super_or_missile_x         ;add movement delta to unit position
 CLC
 ADC movement_vector_x
 STA tank_or_super_or_missile_x
 LDA tank_or_super_or_missile_x + &01
 ADC movement_vector_x + &01
 STA tank_or_super_or_missile_x + &01
 LDA tank_or_super_or_missile_z
 CLC
 ADC movement_vector_z
 STA tank_or_super_or_missile_z
 LDA tank_or_super_or_missile_z + &01
 ADC movement_vector_z + &01
 STA tank_or_super_or_missile_z + &01
 RTS

.move_player_backward
 LDA m_tank_x                           ;subtract movement delta from player position
 SEC
 SBC movement_vector_x
 STA m_tank_x
 LDA m_tank_x + &01
 SBC movement_vector_x + &01
 STA m_tank_x + &01
 LDA m_tank_z
 SEC
 SBC movement_vector_z
 STA m_tank_z
 LDA m_tank_z + &01
 SBC movement_vector_z + &01
 STA m_tank_z + &01
 RTS

.object_sine_cosines                    ;get sine/cosines for object x/y/z angles
 LDA z_object_rotation
 JSR sine_256_16
 STX z_sine
 STA z_sine + &01
 LDA z_object_rotation
 JSR cosine_256_16
 STX z_cosine
 STA z_cosine + &01
 LDA x_object_rotation
 JSR sine_256_16
 STX x_sine
 STA x_sine + &01
 LDA x_object_rotation
 JSR cosine_256_16
 STX x_cosine
 STA x_cosine + &01
 LDA y_object_rotation
 JSR sine_256_16
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
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
 LDA model_identity
 LDY #&3C
.print_it_in_hex                        ;pass in a/y for direct printing
 STY prynt_hex + &03
 JSR hex_double
 STX hex_text
 ORA #&80
 STA hex_text + &01
 LDX #LO(prynt_hex)
 LDY #HI(prynt_hex)
 JMP print

.hex_double                             ;create two hex digits from accumulator, passed back in x/a
 SED
 PHA
 LSR A
 LSR A
 LSR A
 LSR A
 CMP #&0A
 ADC #&30
 TAX
 PLA
 AND #&0F
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
 LDA y_object_rotation                  ;save object rotation
 STA object_rotation_store
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_sine_cosines
 JSR object_info_display
 LDX i_object_identity
 JSR object_transfer_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_draw_16
 JSR object_track_flip
 JSR object_radar_tracks_exhaust_16m
 LDA object_rotation_store              ;restore rotation
 STA y_object_rotation
 RTS

.object_radar_tracks_exhaust_16m        ;place radar/tracks on tank or exhaust on missile
 LDA i_object_identity
 CMP #object_x04_tank
 BEQ object_place_extras                ;standard tank
 CMP #object_x07_missile
 BEQ object_place_exhaust               ;missile
 RTS

.object_place_exhaust
 LDA #object_x22_exhaust_03
 SEC
 SBC track_exhaust_index
 TAX
 JSR object_transfer_16                 ;get exhaust vertices
 LDA y_object_rotation
 JSR sine_256_16                        ;calculate exhaust y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_rotate_16m
 JSR object_view_transform_16
 JMP object_draw_16

.object_place_extras
 LDA object_relative_z + &01
 CMP #radar_vanishing
 BCS object_place_extras_exit           ;too far away then exit for radar and tracks
 LDX #object_x0A_tank_radar
 JSR object_transfer_16                 ;get radar vertices
 LDA object_radar_rotation              ;internal rotation
 STA y_object_rotation
 JSR sine_256_16                        ;calculate radar y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_rotate_16g                  ;rotate radar about y axis
 LDA object_rotation_store
 STA y_object_rotation                  ;restore y rotation
 JSR sine_256_16                        ;calculate tracks y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_radar_translate_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_draw_16
 LDA object_relative_z + &01
 CMP #track_vanishing
 BCS object_restore_tank
 LDX #object_x13_forward_track_03
 LDA tracks_active                      ;select forward\reverse tracks
 LSR A
 BCC first_tracks
 LDX #object_x17_reverse_tracks_03
.first_tracks
 TXA
 SEC
 SBC track_exhaust_index
 TAX
 JSR object_transfer_16
 JSR object_rotate_16m
 JSR object_view_transform_16
 JSR object_draw_16
.object_restore_tank
 LDA #object_x04_tank                   ;restore tank
 STA i_object_identity
.object_place_extras_exit
 RTS

.object_track_flip                      ;swap between forward/backward tracks
 DEC track_counter
 BNE object_track_flip_exit
 SEC
 ROR track_counter
 INC tracks_active                      ;next set of tracks for model, bit 7 is ignored in model view
.object_track_flip_exit
.object_radar_tracks_exitg
 RTS

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
 INC model_identity
.next_object
 INC i_object_identity                  ;next object
 LDX i_object_identity
 CPX #model_display_flag_end - model_display_flag
 BCC no_wrap_object
 LDX #&00
 STX model_identity
.no_wrap_object
 STX i_object_identity
 LDA model_display_flag,X
 BMI next_object                        ;don't display, try next object
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
.transfer_vertices                      ;transfer x/y/x vertices data and de-serialise
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
 STX model_segment_counter              ;model segment counter = &ff
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
 JSR object_y_rotation_16
 JSR object_x_rotation_16
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
.no_eor_ymin_addr                       ;n=1 when a < num, n=0 a>=num
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
 JSR mathbox_line_clip16                ;mathbox clip
 BCS object_draw_16_not_on              ;c=1 do not draw
 BEQ object_draw_16_on_08               ;z=1 inner window
 JSR mathbox_line_draw16
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
 STA volcano_address
 LDA screen_access_y_hi,Y
 ADC screen_hidden
 ADC graphic_x_00 + &01
 STA volcano_address + &01
 TXA
 AND #&07
 TAX
 LDY #&00
 LDA pixel_mask,X
 ORA (volcano_address),Y
 STA (volcano_address),Y
.plot_point_exit
 JMP object_draw_16

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

MACRO rotation p1, p2, sine, cosine, double
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

 IF double
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
   ASL tb                               ;x2 to correct vertices
   LDA tb + &01
   ROL A
   STA p2
   LDA tb + &02
   ROL A
   STA p2 + &01
   ASL ta
   LDA ta + &01
   ROL A
   STA p1
   LDA ta + &02
   ROL A
   STA p1 + &01
 ELSE
   LDA product_16 + &01                 ;x/z vertices doubled
   CLC
   ADC tb
   LDA product_16 + &02
   ADC tb + &01
   STA p2
   LDA product_16 + &03
   ADC tb + &02
   STA p2 + &01
   LDA ta + &01
   STA p1
   LDA ta + &02
   STA p1 + &01
 ENDIF
ENDMACRO

.object_x_rotation_16
; x' = x
; y' = y * cos q - z * sin q
; z' = y * sin q + z * cos q
 rotation y_prime, z_prime, x_sine, x_cosine, TRUE
 RTS

.object_y_rotation_16
; x' = x * cos q - z * sin q
; y' = y
; z' = x * sin q + z * cos q
 rotation x_prime, z_prime, y_sine, y_cosine, FALSE
 RTS

.object_z_rotation_16
; x' = x * cos q - y * sin q
; y' = x * sin q + y * cos q
; z' = z
 rotation x_prime, y_prime, z_sine, z_cosine, TRUE
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
 JSR multiply_16_signed
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
 JSR multiply_16_signed
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
 JSR multiply_16_signed
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
 JMP multiply_16_signed

.object_view_rotate_angles              ;take tank rotation and calculate sine/cosine
 LDA m_tank_rotation_512
 LDX m_tank_rotation_512 + &01
 JSR sine_512
 STY y_sine_tank
 STA y_sine_tank + &01
 LDA m_tank_rotation_512
 LDX m_tank_rotation_512 + &01
 JSR cosine_512
 STY y_cosine_tank
 STA y_cosine_tank + &01
 RTS

.mathbox_object_view_rotate_16
 LDA object_start + &02,Y               ;calculate relative x
 SEC
 SBC m_tank_x
 STA host_r0
 LDA object_start + &03,Y
 SBC m_tank_x + &01
 STA host_r0 + &01
 LDA object_start + &06,Y               ;calculate relative z
 SEC
 SBC m_tank_z
 STA host_r1
 LDA object_start + &07,Y
 SBC m_tank_z + &01
 STA host_r1 + &01
 LDA y_sine_tank
 STA host_r2
 LDA y_sine_tank + &01
 STA host_r2 + &01
 LDA y_cosine_tank
 STA host_r3
 LDA y_cosine_tank + &01
 STA host_r3 + &01
 LDA #mathbox_code_object_view_rotate16
 LDX #host_r0
 JSR mathbox_function_ax
 LDA host_r0
 STA vertice_x
 LDA host_r0 + &01
 STA vertice_x + &01
 LDA host_r1
 STA vertice_z
 LDA host_r1 + &01
 STA vertice_z + &01
 LSR host_flags                         ;c=0/1 visible/invisible
 RTS

.object_view_rotate_16                  ;calculate relative coordinates, c=0/1 visible/invisible
 BIT mathbox_flag
 BMI mathbox_object_view_rotate_16
 LDA object_start + &02,Y               ;calculate relative x
 SEC
 SBC m_tank_x
 STA x_prime
 LDA object_start + &03,Y
 SBC m_tank_x + &01
 STA x_prime + &01
 LDA object_start + &06,Y               ;calculate relative z
 SEC
 SBC m_tank_z
 STA z_prime                            ;z' = x * sin q + z * cos q
 STA multiplicand_16                    ;z * cos q
 LDA object_start + &07,Y
 SBC m_tank_z + &01
 STA z_prime + &01
 STA multiplicand_16 + &01
 LDA y_cosine_tank
 STA multiplier_16
 LDA y_cosine_tank + &01
 STA multiplier_16 + &01
 JSR multiply_16_signed
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
 JSR multiply_16_signed
 LDA product_16 + &02
 CLC
 ADC ta
 STA vertice_z
 LDA product_16 + &03
 ADC ta + &01
 BPL z_position_in_view                 ;z +ve so perform rest of rotation
 LDX object_counter                     ;z -ve but perform rest of rotation if unit as use results later
 CPX #tank_or_super_or_missile - object_start
 BEQ z_position_in_view                 ;unit index so calculate
 SEC                                    ;not unit and behind us so c=1 exit
 RTS
.z_position_in_view
 STA vertice_z + &01
 LDA z_prime                            ;x' = x * cos q - z * sin q
 STA multiplicand_16                    ;z * sin q
 LDA z_prime + &01
 STA multiplicand_16 + &01
 LDA y_sine_tank
 STA multiplier_16
 LDA y_sine_tank + &01
 STA multiplier_16 + &01
 JSR multiply_16_signed
 LDA product_16 + &02
 STA vertice_x
 LDA product_16 + &03
 STA vertice_x + &01
 LDA x_prime                            ;x * cos q
 STA multiplicand_16
 LDA x_prime + &01
 STA multiplicand_16 + &01
 LDA y_cosine_tank
 STA multiplier_16
 LDA y_cosine_tank + &01
 STA multiplier_16 + &01
 JSR multiply_16_signed
 LDA product_16 + &02
 SEC
 SBC vertice_x
 STA vertice_x                          ;rotated x
 LDA product_16 + &03
 SBC vertice_x + &01
 STA vertice_x + &01
 LDA vertice_z + &01                    ;rotated z
 SEC                                    ;c=1 for exit/subtract
 BMI object_out_of_view                 ;test behind, c=1
 SBC #HI(fustrum_near) - &01            ;test far/near fustrum
 SBC #HI(fustrum_far + &01) - HI(fustrum_near + &01) + &01
 BCS object_out_of_view                 ;c=0 if in range n to m
 LDY vertice_x + &01                    ;get result x, no need to correct by doubling as comparing like with like here
 BPL x_absolute_is_postive
 LDA vertice_x                          ;calculate absolute x
 EOR #&FF
 ADC #&01                               ;c=0
 TAX
 TYA
 EOR #&FF
 ADC #&00
 CPX vertice_z
 SBC vertice_z + &01                    ;c=0 in 90' (+-45') view/c=1 out of view
 RTS
.x_absolute_is_postive
 LDX vertice_x
 CPX vertice_z                          ;compare absolute x with z, z is positive
 TYA
 SBC vertice_z + &01                    ;c=0 in 90' (+-45') view/c=1 out of view
.object_out_of_view
 RTS

.mathbox_object_view_transform_16
 STX model_vertices_work
 LDA vertices_x_lsb,X
 CLC
 ADC object_relative_x
 STA host_r0
 LDA vertices_x_msb,X
 ADC object_relative_x + &01
 STA host_r0 + &01
 LDA vertices_y_lsb,X
 CLC
 ADC object_relative_y
 STA host_r1
 LDA vertices_y_msb,X
 ADC object_relative_y + &01
 STA host_r1 + &01
 LDA vertices_z_lsb,X
 CLC
 ADC object_relative_z
 STA host_r2
 LDA vertices_z_msb,X
 ADC object_relative_z + &01
 STA host_r2 + &01
 LDA #mathbox_code_object_view_transform16
 LDX #host_r0
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
 DEX
 BPL mathbox_object_view_transform_16
 RTS

.object_view_transform_16
 LDX model_vertices_counter
 BIT mathbox_flag
 BMI mathbox_object_view_transform_16
.object_view_transform_16_loop          ;model_x  = rotated_x + object_x
 STX model_vertices_work                ;model_z  = rotated_z + z_absolute
 LDA vertices_z_lsb,X                   ;model_y  = vertex_y  + object_y
 CLC                                    ;screen_x = model_x / model_z
 ADC object_relative_z                  ;screen_y = model_y / model_z
 PHA                                    ;xs       = d * x / z
 STA divisor_32
 LDA vertices_z_msb,X
 ADC object_relative_z + &01
 PHA
 STA divisor_32 + &01
 LDA vertices_x_lsb,X
 CLC
 ADC object_relative_x
 STA dividend_32 + &03
 LDA vertices_x_msb,X
 ADC object_relative_x + &01
 STA dividend_32
 JSR division_32_signed
 LDX model_vertices_work
 LDA division_quotient_32               ;add in screen origin x
 CLC
 ADC graphic_x_origin
 STA vertices_x_lsb,X
 LDA division_quotient_32 + &01
 ADC graphic_x_origin + &01
 STA vertices_x_msb,X
 LDA vertices_y_lsb,X
 CLC
 ADC object_relative_y
 STA dividend_32 + &03
 LDA vertices_y_msb,X
 ADC object_relative_y + &01
 STA dividend_32
 PLA
 STA divisor_32 + &01
 PLA
 STA divisor_32
 JSR division_32_signed
 LDX model_vertices_work
 LDA division_quotient_32               ;add in screen origin y
 CLC
 ADC graphic_y_origin
 STA vertices_y_lsb,X
 LDA division_quotient_32 + &01
 ADC graphic_y_origin + &01
 STA vertices_y_msb,X
 DEX
 BPL object_view_transform_16_loop
 RTS

.object_render                          ;render all active objects
 LDY #&00
 STY saucer_state                       ;clear saucer state
.render_objects
 SEC
 LDA object_start,Y                     ;active?
 BMI test_next_object                   ;no
 STY object_counter
 JSR object_view_rotate_16              ;exit c=0/1 visible/invisible
 LDY object_counter                     ;unit always calculated in view routine
 BCS test_next_object                   ;c=1 object not in view
 LDX object_relative_x                  ;multiply by 1.25 to correct view and make
 LDA object_relative_x + &01            ;foreground moves faster than background as per the arcade
 CMP #&80
 ROR object_relative_x + &01
 ROR object_relative_x
 CMP #&80
 ROR object_relative_x + &01
 ROR object_relative_x
 PHA
 TXA
 CLC
 ADC object_relative_x
 STA object_relative_x
 PLA
 ADC object_relative_x + &01
 STA object_relative_x + &01
 LDA object_start + &04,Y               ;y not calculated so just use
 STA object_relative_y                  ;z already set up
 LDA object_start + &05,Y
 STA object_relative_y + &01
 LDA m_tank_rotation_512 + &01          ;player facing
 SEC
 SBC object_start + &01,Y               ;subtract unit's facing
 STA y_object_rotation                  ;rotate object to this angle
 LDX object_start,Y                     ;object identity
 STX i_object_identity
 JSR object_transfer_16                 ;transfer object to workspace
 JSR mathbox_rotation_angles            ;transfer object angles to mathbox
 LDA y_object_rotation
 JSR sine_256_16
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR object_rotate_16g
 JSR object_view_transform_16
 JSR object_draw_16
 LDY object_counter
 CPY #saucer - object_start             ;saucer offset?
 BNE not_saucer_test                    ;no
 DEC saucer_state                       ;saucer in view, for sound generation
.not_saucer_test
 CLC                                    ;object was on
.test_next_object
 ROR A                                  ;put carry into bit 7 then x register
 TAX                                    ;for tracks/radar test when we finish the main loop
 TYA
 CLC
 ADC #block_size
 TAY
 BNE render_objects                     ;look at all 32 possible objects, 256 bytes
 LDA vertice_x                          ;last object is unit so save results for radar/messages
 STA tank_or_super_or_missile_workspace_x
 LDA vertice_x + &01                    ;copy rotated x/z to workspace
 STA tank_or_super_or_missile_workspace_x + &01
 LDA vertice_z
 STA tank_or_super_or_missile_workspace_z
 LDA vertice_z + &01
 STA tank_or_super_or_missile_workspace_z + &01
 TXA                                    ;unit in view?
 BMI object_render_exit                 ;no, so leave off radar/tracks
 LDA i_object_identity                  ;test for standard tank, place on radar/tracks
 LSR A                                  ;tank=&04/super=&05/missile=&07 bit 0=0 if standard unit
 BCS object_render_exit                 ;not standard so exit
 LDA object_relative_z + &01            ;radar and tracks placement
 CMP #radar_vanishing
 BCS object_render_exit                 ;too far away then exit (radar and tracks)
 LDX #object_x0A_tank_radar
 JSR object_transfer_16                 ;get radar vertices
 LDA y_object_rotation                  ;save y rotation
 PHA
 CLC
 ADC object_radar_rotation              ;internal rotation plus viewing rotation
 STA y_object_rotation
 JSR sine_256_16                        ;calculate radar y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_rotate_16g                  ;rotate radar about y axis
 PLA
 STA y_object_rotation                  ;restore y rotation
 JSR sine_256_16                        ;calculate tracks y rotation sine/cosine
 STX y_sine
 STA y_sine + &01
 LDA y_object_rotation
 JSR cosine_256_16
 STX y_cosine
 STA y_cosine + &01
 JSR mathbox_rotation_angles            ;transfer angles to mathbox
 JSR object_radar_translate_16
 JSR object_rotate_16g
 JSR object_view_transform_16
 JSR object_draw_16
 LDA object_relative_z + &01            ;now test for tracks
 CMP #track_vanishing
 BCS object_render_exit 
 LDA tracks_active
 LSR A
 LDA #object_x13_forward_track_03       ;select forward\reverse tracks
 BCC second_tracks
 LDA #object_x17_reverse_tracks_03
.second_tracks
 SEC
 SBC track_exhaust_index
 TAX                                    ;x=track object
 JSR object_transfer_16
 JSR object_rotate_16g
 JSR object_view_transform_16
 JMP object_draw_16
.object_render_exit
 RTS
