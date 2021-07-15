; service menu
;
; a service screen mode as such does not exist on the original machine due to
; in cabinet dip switches being used to control this behaviour
;
; a static self-test screen, indicating full working order, is displayed as per the
; original over which are super-imposed a number of entries controlling initial number
; of tanks etc
;
; the values selected here will be used for the duration of
; any game played on the computer if not saved as per below
;
; service : escape - exit to attract mode
;           x      - cycle bonus tanks score
;           t      - cycle tank number
;           b      - cycle missile appears at score
;           c      - cycle coin play
;           n      - cycle coin(s) bonus  
;           r      - cycle exit options if present
;           0 leave         - use screen values
;           1 save settings - save settings to disc
;           2 save scores   - save high scores to disc
;                    3 reset         - load default settings
;
; high score save is a current add on board for original machines
; http://www.brasington.org/arcade/products/hs/bzone/
;
; specific dip-switch settings for battlezone taken from
; https://www.arcade-museum.com/dipswitch-settings/7059.html
;
; service settings and high scores are persisted to disc, under user control,
; as if they were hardware dip switches
;
; top 8 switch dip
; 87654321
; --------
; ......11   free play
; ......10   1 coin for 2 plays
; ......01   1 coin for 1 play
; ......00   2 coins for 1 play
; ....11..   right coin mech . 1
; ....10..   right coin mech . 4
; ....01..   right coin mech . 5
; ....00..   right coin mech . 6
; ...1....   center (or left) coin mech . 1
; ...0....   center (or left) coin mech . 2
; 111.....   no bonus coin
; 110.....   for every 2 coins inserted, game logic adds 1 more
; 101.....   for every 4 coins inserted, game logic adds 1 more
; 100.....   for every 4 coins inserted, game logic adds 2 more
; 011.....   for every 5 coins inserted, game logic adds 1 more
;
; bottom 8 switch dip, # = default settings
; 87654321
; --------
; ......11   game starts with 2 tanks
; ......10   game starts with 3 tanks #
; ......01   game starts with 4 tanks
; ......00   game starts with 5 tanks
; ....11..   missile appears after 5,000 points
; ....10..   missile appears after 10,000 points #
; ....01..   missile appears after 20,000 points
; ....00..   missile appears after 30,000 points
; ..11....   no bonus tank
; ..10....   bonus tanks at 15,000 and 100,000 points #
; ..01....   bonus tanks at 20,000 and 100,000 points
; ..00....   bonus tanks at 50,000 and 100,000 points
;
; 4 switch dip
; 4321
; ----
; ..11       all coin mechanisms register on one coin counter
; ..01       left and center coin mechanisms on one coin counter, right on second
; ..10       center and right coin mechanisms on one coin counter, left on second
; ..00       each coin mechanism has it's own counter

; constants
 service_graphic_left_x                 = &00
 service_graphic_bottom_y               = &00
 service_graphic_right_x                = screen_row - &01
 service_graphic_top_y                  = &FF

.output_text
 LDA mathbox_flag                       ;mathbox activated
 BPL no_mathbox_activated
 LSR A                                  ;check speed of arm processor
 BCS mathbox_arm_fast
 LDA #bit_ascii_s
 EQUB bit_op
.mathbox_arm_fast
 LDA #bit_ascii_f
 STA fast_slow
 LDX #LO(arm_mathbox)
 LDY #HI(arm_mathbox)
 JSR print_or
.no_mathbox_activated
 LDX #service_print_hi - service_print_lo - &01
.print_service                          ;block print all indicators
 TXA
 PHA
 LDY service_print_hi,X
 LDA service_print_lo,X
 TAX
 JSR print_or
 PLA
 TAX
 DEX
 BPL print_service
 LDX coins_amount
 LDY play_table_service_hi,X
 LDA play_table_service_lo,X
 TAX
 JSR print_or
 LDX #LO(coin_bonus)
 LDY #HI(coin_bonus)
 LDA bonus_coins_index
 BNE print_bonus
 LDX #LO(no_coins_bonus)
 LDY #HI(no_coins_bonus)
.print_bonus
 JMP print_or

.service_print_lo
 EQUB LO(service_char_set)
 EQUB LO(service_number_set)
 EQUB LO(tank_char_set)
 EQUB LO(bonus_tank_char_set)
 EQUB LO(missile_char_set)
 EQUB LO(version)
.service_print_hi
 EQUB HI(service_char_set)
 EQUB HI(service_number_set)
 EQUB HI(tank_char_set)
 EQUB HI(bonus_tank_char_set)
 EQUB HI(missile_char_set)
 EQUB HI(version)

.service_menu
 TSX
 STX stack_store
 JSR draw_service_box
 JSR fill_in_values
 JSR action_print
 JSR output_text                        ;roll into routine below

.check_service_keys
 BIT combined_escape
 BMI change_mode_attract_mode
 BIT combined_t
 BMI increase_tanks
 BIT combined_x
 BMI increase_bonus
 BIT combined_c
 BMI increase_coins
 BIT combined_b
 BMI increase_missile_appears_at
 BIT combined_d
 BMI increase_coins_index
.cycle_actions_exit
 RTS

.change_mode_attract_mode
 INC sound_key_click
 JSR actions_on_exit                    ;actions exit default 0, do nothing
 JMP switch_to_attract

.action_print
 BIT machine_flag                       ;disk enabled?
 BVC cycle_actions_exit                 ;no
 BIT combined_r
 BPL print_actions
 INC sound_key_click
 LDX actions_index
 INX                                    ;next action
 TXA
 AND #&03
 STA actions_index                      ;perform action
.print_actions
 LDX actions_index
 LDA action_on_exit_table_lsb,X
 STA action_text_address
 LDA action_on_exit_table_msb,X
 STA action_text_address + &01
 LDA text_center,X
 STA action_text_center
 LDX #LO(action_text_address)
 LDY #HI(action_text_address)
 JMP print_or

.increase_tanks
 INC sound_key_click
 LDX service_number_of_tanks
 INX
 CPX #&06
 BNE no_tank_limit
 LDX #&02
.no_tank_limit
 STX service_number_of_tanks
 RTS

.increase_bonus
 INC sound_key_click
 LDX bonus_tank_index
 INX
 TXA
 AND #&03
 STA bonus_tank_index
 RTS

.increase_coins
 INC sound_key_click
 LDX coins_amount
 INX
 TXA
 AND #&03
 STA coins_amount
 RTS

.increase_missile_appears_at
 INC sound_key_click
 LDX missile_appears_at_index
 INX
 TXA
 AND #&03
 STA missile_appears_at_index
 RTS

.increase_coins_index
 INC sound_key_click
 LDA #&00                               ;clear tab as bonus changed
 STA bonus_coins_tab
 LDX bonus_coins_index
 INX
 CPX #&05
 BNE no_coin_limit
 TAX
.no_coin_limit
 STX bonus_coins_index
 RTS

.service_char_set
 EQUW service_char_text
 EQUB &30
 EQUB &F0
.service_char_text
 EQUS "ABCDEFGHIJKLMNOPQRSTUVWXYZ "
 EQUB bit_ascii_less_than
.service_number_set
 EQUW service_number_text
 EQUB &70
 EQUB &E0
.service_number_text
 EQUS "0123456789:"
 EQUB bit_ascii_semi_colon

.tank_char_set
 EQUW tank_char_text
 EQUB &88
 EQUB &A8
.tank_char_text
 EQUS "TANKS "
 EQUB bit_ascii_03

.bonus_tank_char_set
 EQUW &00
 EQUB &00
 EQUB &78

.missile_char_set
 EQUW missile_at
 EQUB &58
 EQUB &90

.missile_at
 EQUS "MISSILES FROM "
.missile_two_digits
 EQUS "XX00"
 EQUB bit_ascii_00

.missile_digits_first
 EQUS " 123"
.missile_digits_second
 EQUS "5000"

.bonus_tank_table_lo
 EQUB LO(bonus_tanks_100000)
 EQUB LO(bonus_tanks_15000)
 EQUB LO(bonus_tanks_25000)
 EQUB LO(bonus_tanks_50000)
.bonus_tank_table_hi
 EQUB HI(bonus_tanks_100000)
 EQUB HI(bonus_tanks_15000)
 EQUB HI(bonus_tanks_25000)
 EQUB HI(bonus_tanks_50000)
.bonus_tank_x
 EQUB &A0 - 06 * &08
 EQUB &A0 - 11 * &08
 EQUB &A0 - 11 * &08
 EQUB &A0 - 11 * &08

.bonus_tanks_100000
 EQUS "NO BONUS TAN"
 EQUB bit_ascii_k
.bonus_tanks_15000
 EQUS "TANKS 15000 AND 10000"
 EQUB bit_ascii_00
.bonus_tanks_25000
 EQUS "TANKS 25000 AND 10000"
 EQUB bit_ascii_00
.bonus_tanks_50000
 EQUS "TANKS 50000 AND 10000"
 EQUB bit_ascii_00

.fill_in_values
 LDA service_number_of_tanks
 ORA #&B0                               ;convert to ascii and top bit set
 STA tank_char_text + &06
 LDX bonus_tank_index                   ;get two bcd digits
 LDA bonus_tank_table_lo,X
 STA bonus_tank_char_set
 LDA bonus_tank_table_hi,X
 STA bonus_tank_char_set + &01
 LDA bonus_tank_x,X
 STA bonus_tank_char_set + &02
 LDX missile_appears_at_index           ;get two bcd digits
 LDA missile_digits_first,X
 STA missile_two_digits
 LDA missile_digits_second,X
 STA missile_two_digits + &01
 LDX bonus_coins_index                  ;every added
 LDA coins_every,X
 STA coins_start
 LDA coins_added_to,X
 STA coins_end
 RTS

.coins_every
 EQUS "02445"
.coins_added_to
 EQUB bit_ascii_00
 EQUB bit_ascii_01
 EQUB bit_ascii_01
 EQUB bit_ascii_02
 EQUB bit_ascii_01
.missile_appears_at_score
 EQUB &05
 EQUB &10
 EQUB &20
 EQUB &30

.draw_left_diagonals
 LDA #&06
 STA service_diagonal_counter
.draw_left_loop
 LDY service_diagonal_counter
 LDX diagonal,Y
 LDA left_diagonal_offsets_lo,Y
 STA service_diagonal_left
 LDA left_diagonal_offsets_hi,Y
 CLC
 ADC screen_hidden
 STA service_diagonal_left + &01
.another_left_diagonal
 LDY #&07
 LDA #&01
 STA service_mask
.left_diagonal
 LDA (service_diagonal_left),Y
 ORA service_mask
 STA (service_diagonal_left),Y
 ASL service_mask
 DEY
 BPL left_diagonal
 LDA service_diagonal_left
 ADC #LO(screen_row + &07)              ;c=1 from last asl
 STA service_diagonal_left
 LDA service_diagonal_left + &01
 ADC #HI(screen_row + &07)
 STA service_diagonal_left + &01
 DEX
 BNE another_left_diagonal
 DEC service_diagonal_counter
 BPL draw_left_loop
 RTS

.left_diagonal_offsets_lo
 EQUB LO(32)
 EQUB LO(96)
 EQUB LO(160)
 EQUB LO(224)
 EQUB LO(320 *  8 + 32)
 EQUB LO(320 * 16 + 32)
 EQUB LO(320 * 24 + 32)
.left_diagonal_offsets_hi
 EQUB HI(32)
 EQUB HI(96)
 EQUB HI(160)
 EQUB HI(224)
 EQUB HI(320 *  8 + 32)
 EQUB HI(320 * 16 + 32)
 EQUB HI(320 * 24 + 32)

.draw_service_box
 JSR clear_all_screen
 JSR box_bound
 JSR draw_left_diagonals                ;roll into routine below

.draw_right_diagonals
 LDA #&06
 STA service_diagonal_counter
.draw_right_loop
 LDY service_diagonal_counter
 LDX diagonal,Y
 LDA right_diagonal_offsets_lo,Y
 STA service_diagonal_left
 LDA right_diagonal_offsets_hi,Y
 CLC
 ADC screen_hidden
 STA service_diagonal_left + &01
.another_right_diagonal
 LDY #&07
 LDA #&80
 STA service_mask
.right_diagonal
 LDA (service_diagonal_right),Y
 ORA service_mask
 STA (service_diagonal_right),Y
 LSR service_mask
 DEY
 BPL right_diagonal
 LDA service_diagonal_right
 ADC #LO(screen_row - &09)              ;c=1 from last lsr
 STA service_diagonal_right
 LDA service_diagonal_right + &01
 ADC #HI(screen_row - &09)
 STA service_diagonal_right + &01
 DEX
 BNE another_right_diagonal
 DEC service_diagonal_counter
 BPL draw_right_loop
 RTS

.right_diagonal_offsets_lo
 EQUB LO(280)
 EQUB LO(216)
 EQUB LO(152)
 EQUB LO(88)
 EQUB LO(320 * 9 - 40)
 EQUB LO(320 * 17 - 40)
 EQUB LO(320 * 25 - 40)
.right_diagonal_offsets_hi
 EQUB HI(280)
 EQUB HI(216)
 EQUB HI(152)
 EQUB HI(88)
 EQUB HI(320 * 9 - 40)
 EQUB HI(320 * 17 - 40)
 EQUB HI(320 * 25 - 40)
.diagonal
 EQUB &20
 EQUB &18
 EQUB &10
 EQUB &08
 EQUB &18
 EQUB &10
 EQUB &08

.cross_hatch
 EQUW &20
 EQUW &00
 EQUW &11F
 EQUW &00
 EQUW &11F
 EQUW &FF
 EQUW &20
 EQUW &FF
 EQUW &20
 EQUW &00
.cross_hatch_end

.box_bound
 LDY #&00
.cross_draw
 LDX #&00
.transfer_coordinates
 LDA cross_hatch,Y
 STA graphic_x_00,X
 INX
 INY
 CPX #&08
 BNE transfer_coordinates
 TYA
 PHA
 JSR mathbox_line_draw08
 PLA
 SEC
 SBC #&04
 TAY
 CPY #cross_hatch_end - cross_hatch - &04
 BNE cross_draw
 RTS

.play_table_service_lo
 EQUB LO(free_play_service)
 EQUB LO(one_coin_for_two_service)
 EQUB LO(one_coin_for_one_service)
 EQUB LO(two_coins_for_one_service)

.play_table_service_hi
 EQUB HI(free_play_service)
 EQUB HI(one_coin_for_two_service)
 EQUB HI(one_coin_for_one_service)
 EQUB HI(two_coins_for_one_service)

.free_play_service
 EQUW free_text_service
 EQUB &80
 EQUB &48
.free_text_service
 EQUS "FREE PLA"
 EQUB bit_ascii_y

.one_coin_for_two_service
 EQUW one_coin_for_two_service_text
 EQUB &58
 EQUB &48
.one_coin_for_two_service_text
 EQUS "1 COIN FOR 2 PLAY"
 EQUB bit_ascii_s

.one_coin_for_one_service
 EQUW one_coin_for_one_service_text
 EQUB &60
 EQUB &48
.one_coin_for_one_service_text
 EQUS "1 COIN FOR 1 PLA"
 EQUB bit_ascii_y

.two_coins_for_one_service
 EQUW two_coins_for_one_service_text
 EQUB &58
 EQUB &48
.two_coins_for_one_service_text
 EQUS "2 COINS FOR 1 PLA"
 EQUB bit_ascii_y

.arm_mathbox
 EQUW activated
 EQUB &50
 EQUB &30
.activated
 EQUS "ARM MATHBOX SPEED "
 EQUB ascii_pling
 EQUS " "
.fast_slow
 EQUB &00

.save_high_scores_block
 EQUW high_score_name
.save_high_score_reset
 EQUD &00
 EQUD &00
 EQUD high_scores_save_start
 EQUD high_scores_save_end

.high_score_name
 EQUS "bz2", &0D

.save_service_block                     ;service settings saved to disc
 EQUW service_name
.save_service_reset
 EQUD service_block_start + host_addr
 EQUD service_block_start + host_addr
 EQUD service_block_start + host_addr
 EQUD service_block_end   + host_addr

.service_name
 EQUS "bz0", &0D

.load_bz_file_parameters                ;bz0 & 1 service and backup
 EQUW load_bz_name                      ;bz2 & 3 high score and backup
 EQUD &00
 EQUD &01
 EQUD &00
 EQUD &00
.load_bz_name
 EQUS "bz0", &0D

.load_bz_file
 STA load_bz_name + &02                 ;file to load
 LDA #&FF
 LDX #LO(load_bz_file_parameters)
 LDY #HI(load_bz_file_parameters)
 JMP osfile

.actions_on_exit
 LDA brkv                               ;claim break vector
 STA service_brkv_store
 LDA brkv + &01
 STA service_brkv_store + &01
 LDA #LO(brk_error_message)
 STA brkv
 LDA #HI(brk_error_message)
 STA brkv + &01
 JSR action_decide                      ;decide which action to take
.restore_brk_vector
 LDA service_brkv_store                 ;restore break vector
 STA brkv
 LDA service_brkv_store + &01
 STA brkv + &01
 RTS

.brk_error_message                      ;error number byte at stack + &01
 JSR select_swr_ram_slot
 JSR restore_brk_vector
 LDA stack + &01                        ;create hex error number
 PHA
 AND #&0F
 JSR hex_convert
 STA disc_error + &01
 PLA
 LSR A
 LSR A
 LSR A
 LSR A
 JSR hex_convert
 STA disc_error
 LDA #&02                               ;display for a few seconds
 STA mode_clock
.brk_fail_message
 SEC
 ROR sound_key_click
 JSR mathbox_toggle_activated
 JSR sound_control
 JSR flip_screen
 LDX #LO(brk_error_address)
 LDY #HI(brk_error_address)
 JSR print_or
 LDA mode_clock                         ;check clock
 BNE brk_fail_message
 LDX stack_store
 TXS
 JMP switch_to_attract

.brk_error_address
 EQUW brk_message
 EQUB &70
 EQUB &D0
.brk_message
 EQUS "! CODE "
.disc_error
 EQUW &00
 EQUB ascii_space
 EQUB bit_ascii_pling

.action_decide
 LDX actions_index                      ;which action?
 LDA actions_table,X
 STA actions_branch + &01
.actions_branch
 BNE actions_branch                     ;always except zero action code
.action_just_exit
 RTS

.actions_table
 EQUB action_just_exit     - actions_branch - &02
 EQUB action_save_settings - actions_branch - &02
 EQUB action_save_scores   - actions_branch - &02
 EQUB action_reset_all     - actions_branch - &02

.action_reset_all
 LDA #ascii_01                          ;load reset settings
 JSR load_bz_file
 LDA #ascii_03                          ;load reset scores
 JSR load_bz_file
 JSR action_save_scores                 ;save scores then save settings below

.action_save_settings                   ;save settings to disc
 LDA #&00
 LDX #LO(save_service_block)
 LDY #HI(save_service_block)
 JMP osfile

.action_save_scores                     ;save high scores to disc
 LDA #&00
 LDX #LO(save_high_scores_block)
 LDY #HI(save_high_scores_block)
 JMP osfile

.action_text_address
 EQUW text_save_settings
.action_text_center
 EQUB &50
 EQUB &C0

.action_on_exit_table_lsb
 EQUB LO(text_abandon)
 EQUB LO(text_save_settings)
 EQUB LO(text_save_high_score)
 EQUB LO(text_reset)
.action_on_exit_table_msb
 EQUB HI(text_abandon)
 EQUB HI(text_save_settings)
 EQUB HI(text_save_high_score)
 EQUB HI(text_reset)

.text_save_settings
 EQUS "EXIT ! SAVE SETTING"
 EQUB bit_ascii_s
.text_save_high_score
 EQUS "EXIT ! SAVE SCORE"
 EQUB bit_ascii_s
.text_reset
 EQUS "EXIT ! RESE"
 EQUB bit_ascii_t
.text_abandon
 EQUS "EXIT ! LEAV"
 EQUB bit_ascii_e

.text_center
 EQUB &A0 - 6  * &08                    ;leave
 EQUB &A0 - 10 * &08                    ;save settings
 EQUB &A0 - 9  * &08                    ;save scores
 EQUB &A0 - 6  * &08                    ;reset

.no_coins_bonus
 EQUW no_coins_bonus_text
 EQUB &70
 EQUB &60
.no_coins_bonus_text
 EQUS "NO BONUS COI"
 EQUB bit_ascii_n

.coin_bonus
 EQUW coin_bonus_text
 EQUB &58
 EQUB &60
.coin_bonus_text
 EQUS "EVERY "
.coins_start
 EQUS "0 COINS ADD "
.coins_end
 EQUB bit_ascii_00

.version
 EQUW version_string
 EQUB &30
 EQUB &E0
.version_string
 EQUS "V1#"
 EQUB bit_ascii_00

.service_block_start                    ;<--- save block start

.actions_index
 EQUB &00
.bonus_tank_index
 EQUB &01
.service_number_of_tanks
 EQUB &03
.missile_appears_at_index
 EQUB &01
.coins_added                            ;coins added through pressing 'c'
 EQUB &00
.coins_amount                           ;required coins to play
 EQUB &00
.bonus_coins_index                      ;coins added at bonus rate
 EQUB &00
.bonus_coins_tab                        ;keep tabs on coins added for bonus coin addition
 EQUB &00

.service_block_end
 EQUB &00                               ;<--- service block end

.start_coins                            ;add coins to store
 BIT combined_c
 BPL no_coin_increase
 LDA coins_added                        ;check wallet full
 BMI no_bonus_coins                     ;enough
 INC sound_key_click
 JSR increment_coins                    ;/2 = number of coins
 INC bonus_coins_tab                    ;bonus coins tabulation
.no_coin_increase
 LDX bonus_coins_index                  ;check to add bonus coins
 BEQ no_bonus_coins
 LDA coins_every,X
 SEC
 SBC #ascii_00                          ;convert to decimal
 CMP bonus_coins_tab                    ;got a bonus coin?
 BCS no_bonus_coins                     ;not yet
 CPX #&03                               ;just add one bonus coin?
 BNE just_add_one_coin                  ;yes
 JSR increment_coins
.just_add_one_coin
 LDA #&00                               ;bonus added so clear down tab
 STA bonus_coins_tab
.increment_coins                        ;add two coins
 INC coins_added
 INC coins_added
.no_bonus_coins
 RTS
