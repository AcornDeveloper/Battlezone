; page zero addresses
; tube addresses
; &00          - control block
; &12 - &13    - control block pointer
; &14          - bit 7 tube free
; &15          - claimant id
; &16 - &7F    - tube code etc
;
; normal workspace
; &00 - &8F    - language workspace
; &90 - &9F    - econet workspace
; &A0 - &A7    - nmi workspace
; &A8 - &AF    - os workspace
; &B0 - &BF    - file system scratch space
; &C0 - &CF    - file system scratch space
; &D0          - vdu status
; &D1          - byte mask for current graphics point
; &DA - &DF    - temporary workspace
; &E0 - &E1    - pointer to row multiplication table
; &E2          - cassette filing system status byte
; &E3          - cassette filing system options
; &E4 - &E6    - os workspace
; &E7          - auto repeat countdown byte
; &E8 - &E9    - osword &00 input pointer
; &EC          - last key press
; &ED          - penultimate key press
; &EE - &EF    - current program
; &F0          - x reg for most recent osbyte/osword
; &F1          - x reg for most recent osbyte/osword
; &F2 - &F3    - command line pointer/top of memory
; &F4          - paged rom
; &F5          - current logical speech phrom or rom filing system rom number
; &F6 - &F7    - data transfer address
; &F8 - &F9    - string pointer, osword control block
; &FA - &FB    - ctrl - osfile, osgbpb control block, prtext string pointer
; &FC          - interrupt accumulator
; &FD - &FE    - brk program counter
; &FF          - escape flag
;
; in general avoid using these zero page locations when using os calls
; &A0 - &AF
; &E0 - &EF
; &F0 - &F4
; &FA - &FC

; general
 stack                                  = &100
 language_work_space                    = &400

 page_zero                              = &00

 stack_address                          = &80
 clear_counter                          = &82
 clear_row_counter                      = &83
 list_access                            = &84

; multiply
 distance_dx                            = &90     ;<2>
 distance_dz                            = &92     ;<2>

 product_16                             = &C0     ;<4>
 multiplier_16                          = &C4     ;<2>
 multiplicand_16                        = &C6     ;<2>
 product_16_t1                          = &C8     ;<2>
 product_16_t2                          = &CA     ;<2>

 divisor_32                             = &C0     ;<2>
 dividend_32                            = divisor_32 + &02
 division_remainder_32                  = divisor_32 + &02
 division_quotient_32                   = divisor_32 + &04
 division_scratch_32                    = divisor_32 + &06
 division_carry_32                      = divisor_32 + &07

 dividend_24                            = &C0     ;<3>
 division_result_24                     = dividend_24
 division_remainder_24                  = &C3     ;<3>
 divisor_24                             = &C6     ;<3>

 square_address                         = &D0     ;<8>
 square1_lo                             = square_address
 square1_hi                             = square_address + &02
 square2_lo                             = square_address + &04
 square2_hi                             = square_address + &06

 cs_value_00                            = &C0
 cs_value_01                            = &C1

 m_tank_status                          = &CD
 m_tank_rotation                        = &CE     ;<2>

 graphic_x_00                           = host_r0 ;<2> 08/16 bit x0/y0 coordinate <--- host register mapped
 graphic_y_00                           = host_r1 ;<2>                            <--- host register mapped
 graphic_x_01                           = host_r2 ;<2> 08/16 bit x1/y1 coordinate <--- host register mapped
 graphic_y_01                           = host_r3 ;<2>                            <--- host register mapped

; line draw
 origin_address                         = &80     ;<2>

 graphic_x_origin                       = &B9     ;<2>
 graphic_y_origin                       = &BB     ;<2>

 graphic_window                         = &DC     ;<8> graphic window coordinates
 window_x_00                            = graphic_window          ;<2>
 window_y_00                            = graphic_window + &02    ;<2>
 window_x_01                            = graphic_window + &04    ;<2>
 window_y_01                            = graphic_window + &06    ;<2>

 graphic_dx                             = &C0     ;<2>
 graphic_dy                             = &C2     ;<2>
 graphic_video                          = &C4     ;<2>
 graphic_accumulator                    = &C6     ;<2>
 graphic_count                          = &C8
 graphic_store                          = &C9
 graphic_y_sign                         = &CA     ;<2>

; battlezone
 PRINT ""
 PRINT " page zero  : ", ~mathbox_workspace,             "  mathbox_workspace"
 PRINT "            : ", ~mathbox_save_state,            "  mathbox_save_state"
 PRINT "            : ", ~mathbox_save,                  "  mathbox_save"
 PRINT "            : ", ~mathbox_random,                " mathbox_random"
 PRINT "            : ", ~combined_block_start,          " combined_block_start"
 PRINT "            : ", ~combined_block_end,            " combined_block_end"
 PRINT
 PRINT "            : ", ~console_refresh,               " console_refresh"
 PRINT
 PRINT "            : ", ~console_print,                        " console_print"
 PRINT "            : ", ~console_press_start_etc,              " console_press_start_etc"
 PRINT "            : ", ~console_synchronise_message_flashing, " console_synchronise_message_flashing"
 PRINT "            : ", ~console_clear_space,                  " console_clear_space"
 PRINT "            : ", ~console_enemy_in_range,               " console_enemy_in_range"
 PRINT "            : ", ~console_motion_blocked,               " console_motion_blocked"
 PRINT "            : ", ~console_enemy_to_left,                " console_enemy_to_left"
 PRINT "            : ", ~console_enemy_to_right,               " console_enemy_to_right"
 PRINT "            : ", ~console_enemy_to_rear,                " console_enemy_to_rear"
 PRINT "            : ", ~clock_mode,                    " clock_mode"
 PRINT "            : ", ~clock_second,                  " clock_second"
 PRINT "            : ", ~clock_screen_update,           " clock_screen_update"
 PRINT "            : ", ~clock_divide_by_three,         " clock_divide_by_three"
 PRINT "            : ", ~clock_event_tick,              " clock_event_tick"
 PRINT "            : ", ~clock_frame_counter,           " clock_frame_counter"
 PRINT "            : ", ~clock_move_counter_reload,     " clock_move_counter_reload"
 PRINT "            : ", ~move_counter,                  " move_counter"
 PRINT "            : ", ~ai_rez_protect,                " ai_rez_protect"
 PRINT "            : ", ~x_sine,                        " x_sine"
 PRINT "            : ", ~x_cosine,                      " x_cosine"
 PRINT "            : ", ~y_sine,                        " y_sine"
 PRINT "            : ", ~y_cosine,                      " y_cosine"
 PRINT "            : ", ~z_sine,                        " z_sine"
 PRINT "            : ", ~z_cosine,                      " z_cosine"
 PRINT "            : ", ~y_sine_tank,                   " y_sine_tank"
 PRINT "            : ", ~y_cosine_tank,                 " y_cosine_tank"
 PRINT "            : ", ~game_mode,                     " game_mode"
 PRINT "            : ", ~new_game_mode,                 " new_game_mode"
 PRINT "            : ", ~player_score,                  " player_score"
 PRINT "            : ", ~enemy_score,                   " enemy_score"
 PRINT "            : ", ~recent_collision_flag,         " recent_collision_flag"
 PRINT "            : ", ~hundred_thousand,              " hundred_thousand"
 PRINT "            : ", ~b_object_bounce_near,          " b_object_bounce_near"
 PRINT "            : ", ~b_object_bounce_far,           " b_object_bounce_far"
 PRINT "            : ", ~game_number_of_tanks,          " game_number_of_tanks"
 PRINT "            : ", ~extra_tank,                    " extra_tank"
 PRINT "            : ", ~on_target,                     " on_target"
 PRINT "            : ", ~radar_arm_position,            " radar_arm_position"
 PRINT "            : ", ~m_tank_rotation_512,           " m_tank_rotation_512"
 PRINT "            : ", ~missile_flag,                  " missile_flag"
 PRINT "            : ", ~missile_hop_flag,              " missile_hop_flag"
 PRINT "            : ", ~missile_count,                 " missile_count"
 PRINT "            : ", ~missile_rand,                  " missile_rand"
 PRINT "            : ", ~missile_low_altitude,          " missile_low_altitude"
 PRINT "            : ", ~enemy_ang_delt_abs,            " enemy_ang_delt_abs"
 PRINT "            : ", ~object_counter,                " object_counter"
 PRINT "            : ", ~saucer_velocity_x,             " saucer_velocity_x"
 PRINT "            : ", ~saucer_velocity_z,             " saucer_velocity_z"
 PRINT "            : ", ~saucer_dying,                  " saucer_dying"
 PRINT "            : ", ~saucer_time_to_live,           " saucer_time_to_live"
 PRINT "            : ", ~m_shell_x_vector,              " m_shell_x_vector"
 PRINT "            : ", ~m_shell_z_vector,              " m_shell_z_vector"
 PRINT "            : ", ~m_shell_time_to_live,          " m_shell_time_to_live"
 PRINT "            : ", ~enemy_projectile_velocity_x,   " enemy_projectile_velocity_x"
 PRINT "            : ", ~enemy_projectile_velocity_z,   " enemy_projectile_velocity_z"
 PRINT "            : ", ~enemy_turn_to,                 " enemy_turn_to"
 PRINT "            : ", ~enemy_angle_adjustment,        " enemy_angle_adjustment"
 PRINT "            : ", ~enemy_rev_flags,               " enemy_rev_flags"
 PRINT "            : ", ~enemy_dist_hi,                 " enemy_dist_hi"
 PRINT "            : ", ~enemy_projectile_time_to_live, " enemy_projectile_time_to_live"
 PRINT "            : ", ~close_firing_angle,            " close_firing_angle"
 PRINT "            : ", ~page_zero_end,                 " page zero end"
 PRINT ""

 combined_block_start                   = &16                           ;<--- key start

 combined_x                             = combined_block_start + &00
 combined_t                             = combined_block_start + &01
 combined_r                             = combined_block_start + &02
 combined_b                             = combined_block_start + &03
 combined_c                             = combined_block_start + &04
 combined_d                             = combined_block_start + &05
 combined_n                             = combined_block_start + &06
 combined_arrow_up                      = combined_block_start + &07
 combined_arrow_down                    = combined_block_start + &08
 combined_arrow_left                    = combined_block_start + &09
 combined_arrow_right                   = combined_block_start + &0A
 combined_a                             = combined_block_start + &0B
 combined_z                             = combined_block_start + &0C
 combined_k                             = combined_block_start + &0D
 combined_m                             = combined_block_start + &0E
 combined_f                             = combined_block_start + &0F
 combined_escape                        = combined_block_start + &10
 combined_space                         = combined_block_start + &11

 combined_block_end                     = combined_block_start + &11    ;<--- key end

 console_refresh                        = combined_block_end + &01
 console_print                          = console_refresh + &00         ;print all to right of radar - panel, scores etc.
 console_press_start_etc                = console_refresh + &01         ;press start/sights flashing etc
 console_synchronise_message_flashing   = console_refresh + &02         ;synchronise message flashing
 console_clear_space                    = console_refresh + &03         ;clear top panel when killed
 console_enemy_in_range                 = console_refresh + &04         ;"enemy in range"
 console_motion_blocked                 = console_refresh + &05         ;"motion blocked by object"
 console_enemy_to_left                  = console_refresh + &06         ;"enemy to left"
 console_enemy_to_right                 = console_refresh + &07         ;"enemy to right"
 console_enemy_to_rear                  = console_refresh + &08         ;"enemy to rear"

 clock_mode                             = console_enemy_to_rear + &01
 clock_second                           = clock_mode + &01
 clock_screen_update                    = clock_second + &01
 clock_divide_by_three                  = clock_screen_update + &01
 clock_event_tick                       = clock_divide_by_three + &01
 clock_frame_counter                    = clock_event_tick + &01
 clock_move_counter_reload              = clock_frame_counter + &01
 move_counter                           = clock_move_counter_reload + &01
 ai_rez_protect                         = move_counter + &01
 x_sine                                 = ai_rez_protect + &01
 x_cosine                               = x_sine + &02
 y_sine                                 = x_cosine + &02
 y_cosine                               = y_sine + &02
 z_sine                                 = y_cosine + &02
 z_cosine                               = z_sine + &02
 y_sine_tank                            = z_cosine + &02
 y_cosine_tank                          = y_sine_tank + &02
 game_mode                              = y_cosine_tank + &02 ;0 main game     1 attract mode    2 high score table
                                                              ;3 service menu  4 new high score  5 battlezone text
                                                              ;6 model test
 new_game_mode                          = game_mode + &01
 player_score                           = new_game_mode + &01
 enemy_score                            = player_score + &02
 recent_collision_flag                  = enemy_score + &01
 hundred_thousand                       = recent_collision_flag + &01
 b_object_bounce_near                   = hundred_thousand + &01
 b_object_bounce_far                    = b_object_bounce_near + &01
 game_number_of_tanks                   = b_object_bounce_far + &01
 extra_tank                             = game_number_of_tanks + &01
 on_target                              = extra_tank + &01
 radar_arm_position                     = on_target + &01
 m_tank_rotation_512                    = radar_arm_position + &01
 missile_flag                           = m_tank_rotation_512 + &02
 missile_hop_flag                       = missile_flag + &01
 missile_count                          = missile_hop_flag + &01
 missile_rand                           = missile_count + &01
 missile_low_altitude                   = missile_rand + &01
 enemy_ang_delt_abs                     = missile_low_altitude + &01
 object_counter                         = enemy_ang_delt_abs + &01
 saucer_velocity_x                      = object_counter + &01
 saucer_velocity_z                      = saucer_velocity_x + &02
 saucer_dying                           = saucer_velocity_z + &02
 saucer_time_to_live                    = saucer_dying + &01
 m_shell_x_vector                       = saucer_time_to_live + &01
 m_shell_z_vector                       = m_shell_x_vector + &02
 m_shell_time_to_live                   = m_shell_z_vector + &02
 enemy_projectile_velocity_x            = m_shell_time_to_live + &01
 enemy_projectile_velocity_z            = enemy_projectile_velocity_x + &02
 enemy_turn_to                          = enemy_projectile_velocity_z + &02
 enemy_angle_adjustment                 = enemy_turn_to + &01
 enemy_rev_flags                        = enemy_angle_adjustment + &01
 enemy_dist_hi                          = enemy_rev_flags + &01
 enemy_projectile_time_to_live          = enemy_dist_hi + &01
 close_firing_angle                     = enemy_projectile_time_to_live + &01

 page_zero_end                          = close_firing_angle + &01

 vertical_sync                          = &8B
 frame_counter                          = &9E
 crack_counter                          = &9F

; render
 vertice_a                              = &75     ;<2>
 vertice_b                              = &77     ;<2>
 object_relative_x                      = &79     ;<2>
 object_relative_y                      = &7B     ;<2>
 object_relative_z                      = &7D     ;<2>
 saucer_state                           = &7F

 vertice_x                              = object_relative_x     ;<2>
 vertice_z                              = object_relative_z     ;<2>

 graphic_temp                           = &80

 i_object_identity                      = &90
 x_object_rotation                      = &91
 y_object_rotation                      = &92
 z_object_rotation                      = &93
 d_object_distance                      = &94     ;<2>

 model_vertices_address                 = &96     ;<2>
 model_segment_address                  = &98     ;<2>
 model_segment_counter                  = &9A
 model_vertices_counter                 = &9B
 model_vertices_work                    = &9C
 model_identity                         = &9D

 ta                                     = &80     ;<3>
 tb                                     = &83     ;<3>
 sine_a                                 = &86     ;<2>
 cosine_a                               = &88     ;<2>

 x_prime                                = &AA     ;<2>
 y_prime                                = &AC     ;<2>
 z_prime                                = &AE     ;<2>

 machine_flag                           = &8C
 mathbox_flag                           = &8D

 screen_work                            = &80

 small_address                          = &80
 workspace                              = &80

 destination                            = &82
 sprite_work                            = &82
 char_index                             = &82

 char_counter                           = &83

 read_rom                               = &84
 screen_index                           = &84

 convert_small                          = &85

 object_radar_rotation                  = &D8
 object_rotation_store                  = &D9

 track_counter                          = &F5
 track_exhaust_index                    = &F6
 tracks_active                          = &F7

; landscape
 landscape_segment_ix                   = &80
 landscape_result                       = &81

; tank sights
 sight_address_01                       = &80
 sight_address_02                       = &82
 sight_address_03                       = &84
 sight_address_04                       = &86

; moon
 moon_sprite_address                    = &80
 moon_sprite_store                      = &82
 moon_screen_address                    = &84
 moon_new_x_coor                        = &86
 moon_counter                           = &88

; volcano
 volcano_address                        = &80     ;<2>
 volcano_counter                        = &82
 volcano_x_store                        = &83     ;<2>
 volcano_y_store                        = &85
 volcano_work                           = &86     ;<2>

 radar_address                          = &80     ;<2>

; print characters
 print_block_address                    = &B0     ;<2>
 print_screen                           = &B2     ;<2>
 print_screen_work                      = &B4     ;<2>
 print_y_work                           = &B6     ;<2>
 print_y_reg                            = &B7
 print_character_height                 = &B8

; service menu
 service_box_left                       = &80
 service_box_right                      = &82
 service_box_top                        = &80
 service_box_bottom                     = &82
 service_diagonal_left                  = &80
 service_diagonal_right                 = &80
 service_mask                           = &82
 service_diagonal_counter               = &83

 service_transfer                       = &80     ;<2>

; tank
 tank_address                           = &C0     ;<2>
 tank_sprite_address                    = &C2     ;<2>

; video/swr
 screen_hidden                          = &8E
 found_a_slot                           = &8F

; os
 paged_rom                              = &F4
 interrupt_accumulator                  = &FC

; high score
 bcd_counter                            = &80

 radar_scr_a                            = &BD
 radar_scr_x                            = &BE
 radar_scr_y                            = &BF

 xdiff                                  = &90     ;<2>
 zdiff                                  = &92     ;<2>
 xdiff_abs                              = &94     ;<2>
 zdiff_abs                              = &96     ;<2>
 x_coor_tan_01                          = xdiff + &01
 z_coor_tan_01                          = zdiff + &01
 octant                                 = &98

; animate
 x_offset                               = &80
 z_offset                               = &82
 sine                                   = &84
 cosine                                 = &86

 unit_x_pos                             = &C0     ;<2>
 unit_z_pos                             = &C2     ;<2>
 movement_vector_x                      = &C4     ;<2>
 movement_vector_z                      = &C6     ;<2>
 general_x                              = &C8
 general_y                              = &C9
 general_store                          = &CA

; op codes
 bit_op                                 = &2C     ;bit
 cmp_op                                 = &CD     ;compare
 nop_op                                 = &EA     ;nop
 ora_op                                 = &11     ;ora
