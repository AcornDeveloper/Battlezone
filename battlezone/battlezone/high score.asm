; high score maintenance
;
; display high score table and entry screen for initials
; if score on table
;
; informational messages for player
; screen messages rows
; 0 enemy in range
; 1 motion blocked by object
; 2 enemy to left/right/rear
;
; text display on attract screen for different kinds of play message
;
; constants
 score                                  = &05
 high_score_address                     = screen_row * &03 + &FA
 game_over_address                      = screen_row * &0A + &78
 high_screen_address                    = screen_row * &03 + &C8

 enemy_in_range_screen                  = screen_row * &01 + &10
 motion_blocked_screen                  = screen_row * &02 + &10
 enemy_to_mess_screen                   = screen_row * &03 + &10

 radar_center_x                         = &9F
 radar_center_y                         = &17

 great_score_address                    = screen_row * &0B + &40
 select_address                         = great_score_address + screen_row * &02 + 16

; small message character space clear
MACRO small_message_clear screen_address_offset, number_of_characters
 LDX #LO(parameter)
 LDY #HI(parameter)
 JMP clear_cells

.parameter
 EQUW screen_address_offset
 EQUB number_of_characters * &08
ENDMACRO

; place small message on screen
MACRO small_message screen_address, sprite_address, number_of_characters
 LDX #LO(parameter)
 LDY #HI(parameter)
 JMP multiple_row_sprite

.parameter
 EQUW screen_address - &01
 EQUW sprite_address - &01
 EQUB &01
 EQUB number_of_characters * &08
ENDMACRO

; parameter block for fast sprites
MACRO fast_block screen, sprite, rows, bytes
 EQUW screen - &01
 EQUW sprite - &01
 EQUB rows
 EQUB bytes
ENDMACRO

; calculate an address in screen buffer
MACRO initialise_hidden page_zero, address
 LDA #LO(address)
 STA page_zero
 LDA #HI(address)
 IF high_score_address AND &FF00 < &800 ;test if can add/or in value
   ORA screen_hidden
 ELSE
   CLC
   ADC screen_hidden
 ENDIF
 STA page_zero + &01
ENDMACRO

.high_scores_save_start                 ;<--- high score block start

.high_score_top
 EQUW &9999                             ;high score stop, simplifies processing
.high_scores
 EQUW score                             ;highest score used for game screen
 EQUW score
 EQUW score
 EQUW score
 EQUW score
 EQUW score
 EQUW score
 EQUW score
 EQUW score
.bottom_high_score
 EQUW score
.high_scores_end

.high_score_names_top
 EQUS "XXX"
.high_score_names                       ;three character initials
 EQUS "EDR"
 EQUS "MPH"
 EQUS "JED"
 EQUS "DES"
 EQUS "TKE"
 EQUS "VKB"
 EQUS "EL "
 EQUS "HAD"
 EQUS "ORR"
.player_10
 EQUS "GJR"
.high_score_names_end

.high_scores_save_end
 EQUB &00                               ;<--- high score block end

.player_line
 EQUW high_score_build_line
 EQUB &68
 EQUB &00

.high_score_build_line
 EQUS "    000 "
.high_score_build_name
 EQUS "XXX"
.high_score_terminator
 EQUB bit_ascii_space
.high_score_build_line_tank
 EQUW &00                               ;small tank graphic >=100k points

.print_high_scores_table
 LDX #LO(player_high_score_header)      ;print table header and footer
 LDY #HI(player_high_score_header)
 JSR print
 LDX bonus_tank_index
 BEQ no_bonus_at_all
 LDA bonus_double_digits_low - &01,X
 STA table_double_digits + &01
 LDA bonus_double_digits_high - &01,X
 STA table_double_digits
 LDX #LO(player_table_footer_00)
 LDY #HI(player_table_footer_00)
 JSR print
.no_bonus_at_all
 LDA #&C0                               ;print each high score line
 STA player_line + &03
 LDX #&12
.player_high_score_line
 STX bcd_counter                        ;convert score from bcd to ascii hex 0-9
 LDA high_scores,X
 JSR hex_double
 STA high_score_build_line + &03
 STX high_score_build_line + &02
 LDX bcd_counter
 LDA #bit_ascii_space                   ;terminate string for now
 STA high_score_terminator
 LDA high_scores + &01,X                ;<100k tank graphic?
 BEQ less_than_100k                     ;yes
 LDY #ascii_space                       ;need a regular space between initials and tank
 STY high_score_terminator
 LDY #ascii_equals                      ;set tank to display, left tank
 STY high_score_build_line_tank         ;= followed by >, right tank
 LDY #bit_ascii_greater_than
 STY high_score_build_line_tank + &01
.less_than_100k
 JSR hex_double
 STA high_score_build_line + &01
 STX high_score_build_line
 LDX #&00
.scan_for_leading_zeroes
 LDA high_score_build_line,X
 CMP #ascii_00
 BNE suppressed_all                     ;suppress leading zeroes
 LDA #ascii_space
 STA high_score_build_line,X
 INX
 BNE scan_for_leading_zeroes
.suppressed_all
 LDA bcd_counter                        ;transfer initials
 LSR A                                  ;x1.5 to give index into 3 chars
 ADC bcd_counter                        ;c=0
 TAX
 LDA high_score_names,X
 STA high_score_build_name
 LDA high_score_names + &01,X
 STA high_score_build_name + &01
 LDA high_score_names + &02,X
 STA high_score_build_name + &02
 LDX #LO(player_line)                   ;print out the built line
 LDY #HI(player_line)
 JSR print
 LDA player_line + &03                  ;adjust printed y coordinate
 SEC
 SBC #&0C
 STA player_line + &03
 LDX bcd_counter
 DEX
 DEX
 BPL player_high_score_line
 RTS

.player_high_score_header
 EQUW high_scores_table
 EQUB &74
 EQUB &48
.high_scores_table
 EQUS "HIGH SCORE"
 EQUB bit_ascii_s

.player_table_footer_00
 EQUW table_footer_00
 EQUB &28
 EQUB &CC
.table_footer_00
 EQUS "BONUS TANK AT "
.table_double_digits
 EQUS "15000 AND 10000"
 EQUB bit_ascii_00

.bonus_double_digits_low
 EQUB LO(&3135)
 EQUB LO(&3235)
 EQUB LO(&3530)
.bonus_double_digits_high
 EQUB HI(&3135)
 EQUB HI(&3235)
 EQUB HI(&3530)

.great_score_parameters

 fast_block great_score_address, battlezone_sprites + great_score_offset, &02, 184

.select_parameters

 fast_block select_address, battlezone_sprites + select_offset, &01, 144

.three_number_set
 EQUW three_number_text
 EQUB &94
 EQUB &90

.increase_text
 INY
 CPY #27
 BNE text_okay
 LDY #&00
 BEQ text_okay                          ;always

.decrease_text
 DEY
 BPL text_okay
 LDY #26
.text_okay
 LDA service_char_text,Y
 LDX enter_text_index
 STA three_number_text,X
 STY valid_character_index
.new_high_exit
 RTS

.new_character
 LDA #&00
 STA valid_character_index
 LDA #ascii_upper_a
 STA three_number_text,X
 RTS
 
.three_number_text
 EQUD &00
.enter_text_index
 EQUB &00
.valid_character_index
 EQUB &00

.new_high_score_mode                    ;enter new high score
 LDX #LO(great_score_parameters)        ;great score message etc
 LDY #HI(great_score_parameters)
 JSR multiple_row_sprite
 LDX #LO(select_parameters)
 LDY #HI(select_parameters)
 JSR multiple_row_sprite
 LDX #LO(three_number_set)
 LDY #HI(three_number_set)
 JSR print
 BIT combined_escape
 BMI leave_high_score_from_new          ;leave without entering score
 LDY valid_character_index              ;y character entry ready for tests below
 BIT combined_k
 BMI increase_text
 BIT combined_m
 BMI decrease_text
 BIT combined_space
 BMI enter_character
 BIT clock_mode                         ;name entry timed out?
 BPL new_high_exit                      ;-ve edge exit
 JSR enter_score_on_time_out
 JMP switch_to_high_score

.leave_high_score_from_new              ;did not make the high score table
 JMP switch_to_attract

.enter_character
 INC enter_text_index
 LDX enter_text_index
 CPX #&03
 BNE new_character                      ;new characters else insert in table below

.enter_score_on_time_out                ;update entered with whatever present
 LDX #&02
.transfer_triple                        ;tidy up three characters to initials
 LDA three_number_text,X
 CMP #ascii_under_score
 BNE no_under_scores
 LDA #ascii_space                       ;store top bit set space
.no_under_scores
 STA three_number_text,X
 DEX
 BPL transfer_triple                    ;now insert into high score table
 LDX #bottom_high_score - high_score_top - &02
 LDY #player_10 - high_score_names_top - &03
.move_index_up
 LDA high_score_top,X                   ;compare score with current
 CMP player_score                       ;c=result
 LDA high_score_top + &01,X
 SBC player_score + &01                 ;find index to insert at
 BCS insert_row_here                    ;can go no higher up the table
 LDA high_score_top,X                   ;move score down one row
 STA high_score_top + &02,X
 LDA high_score_top + &01,X
 STA high_score_top + &03,X
 LDA high_score_names_top,Y             ;move initials down one row
 STA high_score_names_top + &03,Y
 LDA high_score_names_top + &01,Y
 STA high_score_names_top + &04,Y
 LDA high_score_names_top + &02,Y
 STA high_score_names_top + &05,Y
 DEY
 DEY
 DEY
 DEX                                    ;move pointers up table, two for score, three for initials
 DEX
 BPL move_index_up                      ;repeat until a row found or hit top stop score
.insert_row_here
 LDA player_score                       ;insert player score in table
 STA high_score_top + &02,X
 LDA player_score + &01
 STA high_score_top + &03,X
 LDX #&00                               ;now insert initials
.transfer_text
 LDA three_number_text,X
 STA high_score_names_top + &03,Y
 INY
 INX
 CPX #&03
 BCC transfer_text
 LDA #console_double                    ;refresh screen score
 STA console_print
 JMP switch_to_high_score

.leave_high_score_after_test            ;did not make the high score table
 JMP switch_to_attract

.test_for_new_high_score                ;test for new high score and if so enter into table
 LDX player_score                       ;test for equality first
 LDY player_score + &01
 CPX bottom_high_score
 BNE not_equal
 CPY bottom_high_score + &01
 BEQ leave_high_score_after_test        ;scores are equal so leave one on table as there first
.not_equal
 CPX bottom_high_score
 TYA
 SBC bottom_high_score + &01
 BCC not_on_table                       ;not greater than 10th score in table
 LDX #&05
.three_initial
 LDA high_initial_block,X
 STA three_number_text,X                ;clear text entry/index
 DEX
 BPL three_initial                      ;x=&ff
 STX sound_music                        ;enable music
 STX sound_gate                         ;disable sound
 JSR sound_flush_buffers
 JMP switch_to_new_high_score

.not_on_table                           ;did not make the high score table
 JMP switch_to_attract

.high_initial_block
 EQUB ascii_upper_a                     ;start with <a> <under score> <under score> for name entry
 EQUB ascii_under_score
 EQUB ascii_under_score
 EQUB bit_ascii_space                   ;stop character
 EQUW &00

.clear_message_top
 small_message_clear enemy_in_range_screen - &01, &08

.check_enemy_in_range
 LDA console_enemy_in_range
 BEQ common_exit_point                  ;no "enemy to range"
 INC console_enemy_in_range
 CMP #console_double - &01
 BCS clear_message_top
 LDA general_store
 BNE clear_message_top

 small_message enemy_in_range_screen, battlezone_sprites + enemy_in_range_offset, &08

.check_left_right_rear
 LDA console_enemy_to_left
 BEQ enemy_to_right                     ;no "enemy to left" so check "enemy to right"
 INC console_enemy_to_left
 CMP #console_double - &01
 BCS clear_message_bottom
 LDA general_store
 BNE clear_message_bottom

 small_message enemy_to_mess_screen, battlezone_sprites + enemy_to_left_offset, &08

.enemy_to_right
 LDA console_enemy_to_right
 BEQ enemy_to_rear
 INC console_enemy_to_right
 CMP #console_double - &01
 BCS clear_message_bottom
 LDA general_store
 BNE clear_message_bottom

 small_message enemy_to_mess_screen, battlezone_sprites + enemy_to_right_offset, &08

.enemy_to_rear
 LDA console_enemy_to_rear
 BEQ common_exit_point
 INC console_enemy_to_rear
 CMP #console_double - &01
 BCS clear_message_bottom
 LDA general_store
 BNE clear_message_bottom

 small_message enemy_to_mess_screen, battlezone_sprites + enemy_to_rear_offset, &08

.clear_message_bottom                   ;clear message and exit
 small_message_clear enemy_to_mess_screen - &01, &08

.common_exit_point
 RTS

.status_messages
 INC console_synchronise_message_flashing
 LDA console_synchronise_message_flashing
 AND #&02
 STA general_store
 JSR check_left_right_rear
 JSR check_enemy_in_range
 LDA console_motion_blocked             ;motion blocked
 BEQ common_exit_point
 INC console_motion_blocked
 CMP #console_double - &01
 BCS clear_motion_blocked
 LDA console_synchronise_message_flashing
 AND #&04
 BNE clear_motion_blocked

 small_message motion_blocked_screen, battlezone_sprites + motion_blocked_offset, &0E

.clear_motion_blocked
 INC sound_motion_blocked               ;motion blocked
 small_message_clear motion_blocked_screen - &01, &0E

.game_over_copyright_and_start
 LDX #LO(game_over)
 LDY #HI(game_over)
 JSR print_or
 LDX #LO(copyright)
 LDY #HI(copyright)
 JSR print
 LDX coins_amount                       ;coins required index
 BEQ press_to_start                     ;exit, no coins message
 LDA coins_added
 CMP coins_required,X
 BCS press_to_start
 LDX coins_amount                       ;coins required index
 LDY play_table_hi - &01,X
 LDA play_table_lo - &01,X
 TAX
 JSR print_or
 LDA console_press_start_etc
 AND #&02
 BNE common_exit_point                  ;flash text
 LDX #LO(insert_coin)                   ;insert coin
 LDY #HI(insert_coin)
 JMP print_or

.press_to_start
 LDA console_press_start_etc
 AND #&02
 BNE common_exit_point                  ;exit
 LDX #LO(press_start)
 LDY #HI(press_start)
 JMP print_or

.insert_coin
 EQUW insert_coin_text
 EQUB &70
 EQUB &96
.insert_coin_text
 EQUS "INSERT COI"
 EQUB bit_ascii_n

.game_over
 EQUW game_over_text
 EQUB &78
 EQUB &5A
.game_over_text
 EQUS "GAME OVE"
 EQUB bit_ascii_r

.play_table_lo
 EQUB LO(one_coin_for_two)
 EQUB LO(one_coin_for_one)
 EQUB LO(two_coins_for_one)

.play_table_hi
 EQUB HI(one_coin_for_two)
 EQUB HI(one_coin_for_one)
 EQUB HI(two_coins_for_one)

.one_coin_for_two
 EQUW one_coin_for_two_service_text
 EQUB &68
 EQUB &78

.one_coin_for_one
 EQUW one_coin_for_one_service_text
 EQUB &70
 EQUB &78

.two_coins_for_one
 EQUW two_coins_for_one_service_text
 EQUB &68
 EQUB &78

.copyright
 EQUW copyright_text
 EQUB &68
 EQUB &C8
.copyright_text
 EQUS ":;  ATARI 198"
 EQUB bit_ascii_00

.press_start
 EQUW press_start_text
 EQUB &70
 EQUB &77
.press_start_text
 EQUS "PRESS STAR"
 EQUB bit_ascii_t

.print_player_score
 BIT console_print                      ;print score
 BPL exit_small_number
 INC console_print
 JSR draw_tanks
 JSR high_score_text
 LDA player_score                       ;print large score
 JSR hex_double                         ;convert two bytes
 STA player_score_text + &03
 STX player_score_text + &02
 LDA player_score + &01
 JSR hex_double
 STA player_score_text + &01
 STX player_score_text
 LDY #&FF
.test_next_digit                        ;remove leading zeroes
 INY
 LDA player_score_text,Y
 EOR #ascii_00                          ;'0'
 BNE found_non_ascii_zero_digit
 LDA #ascii_space                       ;' '
 STA player_score_text,Y
 CPY #&02
 BNE test_next_digit
.found_non_ascii_zero_digit
 LDX #LO(player_score_screen)           ;print player score
 LDY #HI(player_score_screen)
 JMP print

.player_score_screen
 EQUW player_score_full
 EQUB &C8
 EQUB &0E
.player_score_full
 EQUS "SCORE "
.player_score_text
 EQUS "000000"
 EQUB bit_ascii_00

.right_hand_side
 LDA battlezone_sprites + small_numbers_offset - &02,X
 AND #&0F
 STA (small_address),Y
 DEX
 DEY
 DEC char_index
 BNE right_hand_side
 DEC char_counter
 BPL print_small_number
.exit_small_number
 RTS

.high_score_text                        ;small high score text
 LDX #LO(high_score_parameters)
 LDY #HI(high_score_parameters)
 JSR multiple_row_sprite                ;"highscore"
 LDA high_scores + &01
 TAX
 LSR A
 LSR A
 LSR A
 LSR A
 STA convert_small
 TXA
 AND #&0F
 STA convert_small + &01
 LDA high_scores
 TAX
 LSR A
 LSR A
 LSR A
 LSR A
 STA convert_small + &02
 TXA
 AND #&0F
 STA convert_small + &03
 LDX #&00
 STX convert_small + &04                ;trailing 0's
 STX convert_small + &05
 STX convert_small + &06
.find_more                              ;set top bit for leading 0's
 LDA convert_small,X
 BNE not_zero_digit
 SEC
 ROR convert_small,X
 INX
 BNE find_more
.not_zero_digit
 LDA #LO(high_score_address)            ;initialise screen address
 STA small_address
 LDA #HI(high_score_address)
 CLC
 ADC screen_hidden
 STA small_address + &01
 LDX #&06
 STX char_counter
 LDY #&1D
 STY screen_index
.print_small_number
 LDA #&05
 STA char_index
 LDX char_counter
 LDA convert_small,X
 BMI exit_small_number                  ;no more to print
 ASL A
 ASL A
 ADC convert_small,X                    ;*5, c=0
 ADC #&06                               ;+6
 TAX                                    ;index into character data
 LDY screen_index
 LDA char_counter
 LSR A
 BCS right_hand_side                    ;small number to right
.small_char_ora
 LDA battlezone_sprites + small_numbers_offset - &02,X
 AND #&F0
 ORA (small_address),Y
 STA (small_address),Y
 DEX
 DEY
 DEC char_index
 BNE small_char_ora
 DEY
 DEY
 DEY
 STY screen_index
 DEC char_counter
 BPL print_small_number
.exit_in_progress
 RTS

.high_score_parameters

 fast_block high_screen_address, battlezone_sprites + high_score_offset, &01, 48

.unit_in_distance
 SEC
 ROR radar_scr_a                        ;radar spot flag set to off
 LSR on_target                          ;clear sights flag, now off
 BIT missile_flag                       ;missile?
 BMI make_missile                       ;yes, make another
 JMP create_tank

.orientation_exit
 SEC
 ROR radar_scr_a                        ;radar spot flag set to off
 LSR on_target                          ;clear sights flag, now off
 BIT debris_last_chunk_to_hit_ground    ;explosion in progress?
 BPL exit_in_progress                   ;yes
 LDA game_mode                          ;attract mode?
 BNE make_tank                          ;yes, make tank
 LDX missile_appears_at_index           ;from service menu
 LDA player_score                       ;get low byte of score
 CMP missile_appears_at_score,X         ;compare to missile threshold
 BCC make_tank                          ;score too low so create a tank after all
 LDA mathbox_random
 CMP #&40                               ;adjusted value due to arcade using fast timer 256hz
 BCC make_missile                       ;not tired of missiles yet, make another
.make_tank
 JMP create_tank                        ;let's go back to tanks
.make_missile
 JMP create_missile

.orientation_off_radar
 ROR radar_scr_a
 LSR on_target                          ;clear sights flag, now off
 RTS

.orientation                            ;maintain messages/in sights flag/angle to player/radar spot
 BIT tank_or_super_or_missile
 BMI orientation_exit                   ;not on, messages will time out, no radar spot/on or target sights
 LDA tank_or_super_or_missile_workspace_x
 STA distance_dx
 LDA tank_or_super_or_missile_workspace_x + &01
 STA distance_dx + &01
 LDA tank_or_super_or_missile_workspace_z
 STA distance_dz
 LDA tank_or_super_or_missile_workspace_z + &01
 STA distance_dz + &01
 JSR distance_16                        ;exit a=d_object_distance + &01
 STA enemy_dist_hi                      ;store for use later by unit ai
 TAY                                    ;>=&80?
 BMI unit_in_distance                   ;out of range so exit, try and create a new unit
 CMP #&40
 BCS orientation_off_radar              ;no radar spot/messages but can still be seen
 LDA tank_or_super_or_missile_workspace_x + &01
 CMP #&80                               ;calculate radar x/y screen coordinate
 ROR A                                  ;from previously saved x/z coordinates
 CMP #&80                               ;signed divide by 4
 ROR A
 CLC
 ADC #radar_center_x                    ;radar spot x
 STA radar_scr_x
 LDA tank_or_super_or_missile_workspace_z + &01
 CMP #&80                               ;calculate radar y screen coordinate
 ROR A                                  ;signed divide by 4
 CMP #&80
 ROR A
 EOR #&FF                               ;invert
 CLC
 ADC #radar_center_y
 STA radar_scr_y                        ;radar spot y
 LDA tank_or_super_or_missile_workspace_z
 STA zdiff_abs
 LDA tank_or_super_or_missile_workspace_z + &01
 STA zdiff_abs + &01
 BPL zdiff_abs_value_this               ;value is negative so make positive
 LDA #&00
 SEC
 SBC zdiff_abs
 STA zdiff_abs
 LDA #&00
 SBC zdiff_abs + &01
 STA zdiff_abs + &01
.zdiff_abs_value_this
 LDA tank_or_super_or_missile_workspace_x
 STA xdiff_abs
 LDA tank_or_super_or_missile_workspace_x + &01
 STA xdiff_abs + &01
 BPL xdiff_abs_value_this
 LDA #&00                               ;value is negative so make positive
 SEC
 SBC xdiff_abs
 STA xdiff_abs
 LDA #&00
 SBC xdiff_abs + &01
 STA xdiff_abs + &01
.xdiff_abs_value_this
 LDX #&08                               ;scale xdiff/zdiff to increase accuracy
.keep_scaling_this
 ASL xdiff_abs
 ROL xdiff_abs + &01
 BMI scaling_done                       ;finished scaling
 ASL zdiff_abs
 ROL zdiff_abs + &01
 BMI scaling_done                       ;finished scaling
 ASL tank_or_super_or_missile_workspace_x
 ROL tank_or_super_or_missile_workspace_x + &01
 ASL tank_or_super_or_missile_workspace_z
 ROL tank_or_super_or_missile_workspace_z + &01
 DEX
 BNE keep_scaling_this
.scaling_done
 LDA tank_or_super_or_missile_workspace_x + &01
 STA x_coor_tan_01
 LDA tank_or_super_or_missile_workspace_z + &01
 STA z_coor_tan_01
 JSR arctangent                         ;exit a = 0-255 angle between unit and player
 LDX #&00
 CLC
 ADC #&40                               ;adjust to compare with radar arm
 TAY
 LSR A                                  ;place radar spot on
 LSR A
 LSR A
 LSR A                                  ;bring into radar counter range 0-15
 CMP radar_arm_position
 BNE place_radar_spot
 BIT m_tank_status                      ;check here as still require this processing to take place
 BMI no_radar_sound                     ;even though my tank has been destroyed
 INC sound_enemy_radar                  ;radar ping sound
.no_radar_sound
 LDX #&0F                               ;large radar spot
.place_radar_spot
 STX radar_scr_a
 LDX #console_messages                  ;enemy on radar so "enemy in range"
 STX console_enemy_in_range             ;x = message counter
 TYA                                    ;to move/attack the player
 CMP #&20
 BCC check_sights
 CMP #&E0
 BCS check_sights_set                   ;check sights if in front 45' view so no left/right/rear messages
 LDY #&00                               ;message zero page index
 CMP #&A0
 BCS target_message                     ;"enemy to left"
 INY
 CMP #&60
 BCC target_message                     ;"enemy to right"
 INY                                    ;"enemy to rear"
.target_message
 STX console_enemy_to_left,Y
 RTS
.check_sights
 SEC
.check_sights_set
 SBC #&02
 CMP #LO(-&04)
 ROR on_target                          ;in sights flag updated
 RTS
