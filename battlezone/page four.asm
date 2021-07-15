; constants
 block_size                             = &08
 vertices_number                        = &1A

; addresses
 PRINT " page one   : ", ~vertices_table,   "vertice start"
 PRINT "            : ", ~page_one_end,     "page one end"
 PRINT ""
 PRINT " page four  : ", ~object_workspace, "object workspace"
 PRINT "            : ", ~object_start,     "object start"
 PRINT "            : ", ~page_four_end,    "page four end"
 PRINT ""

 vertices_table                         = &100
 vertices_x_lsb                         = vertices_table
 vertices_x_msb                         = vertices_table + vertices_number * &01
 vertices_y_lsb                         = vertices_table + vertices_number * &02
 vertices_y_msb                         = vertices_table + vertices_number * &03
 vertices_z_lsb                         = vertices_table + vertices_number * &04
 vertices_z_msb                         = vertices_table + vertices_number * &05
 vertices_end                           = vertices_table + vertices_number * &06
 page_one_end                           = vertices_end

MACRO object id, angle, xcr, ycr, zcr
 EQUB id                                ;+00 identity
 EQUB angle                             ;+01 object y axis rotation
 EQUW xcr                               ;+02 object x coordinate
 EQUW ycr                               ;+04 object y coordinate
 EQUW zcr                               ;+06 object z coordinate
ENDMACRO

MACRO object_workspace visible, xwork, ywork, zwork
 EQUW visible                           ;+00 visible
 EQUW xwork                             ;+02 object x work
 EQUW ywork                             ;+04 object y work
 EQUW zwork                             ;+06 object z work
ENDMACRO

.object_workspace                       ;object workspace
 object_workspace &FF, &00, &FF, &00    ;x +&02/&03 rotated around my tank, full value
 object_workspace &FF, &00, &FF, &00    ;z +&06/&07 rotated around my tank, half value
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00

.object_workspace_collision
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00

.object_workspace_animated              ;object_animated_workspace
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00    ;<--- debris #0 / tank shell
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00
 object_workspace &FF, &00, &FF, &00    ;<--- debris #5 / missile exhaust

.object_saucer_workspace
 object_workspace &FF, &00, &FF, &00

.object_explosion_workspace
 object_workspace &FF, &00, &FF, &00

.object_tank_or_super_or_missile_workspace
 object_workspace &FF, &00, &FF, &00

.object_start                           ;short cubes are not checked by my shot/enemy shot collision
 object object_x02_short_cube,     &10, obstacle_x_01, &00, obstacle_z_01
 object object_x02_short_cube,     &40, obstacle_x_03, &00, obstacle_z_03
 object object_x02_short_cube,     &48, obstacle_x_09, &00, obstacle_z_09
 object object_x02_short_cube,     &68, obstacle_x_0D, &00, obstacle_z_0D
 object object_x02_short_cube,     &88, obstacle_x_11, &00, obstacle_z_11

.object_shot_collision_start            ;these geometrical objects tall enough for my shot/enemy shot collision
 object object_x00_narrow_pyramid, &28, obstacle_x_05, &00, obstacle_z_05
 object object_x00_narrow_pyramid, &38, obstacle_x_07, &00, obstacle_z_07
 object object_x00_narrow_pyramid, &58, obstacle_x_0B, &00, obstacle_z_0B
 object object_x00_narrow_pyramid, &78, obstacle_x_0F, &00, obstacle_z_0F
 object object_x00_narrow_pyramid, &98, obstacle_x_13, &00, obstacle_z_13
 object object_x01_tall_cube,      &30, obstacle_x_06, &00, obstacle_z_06
 object object_x01_tall_cube,      &40, obstacle_x_08, &00, obstacle_z_08
 object object_x01_tall_cube,      &60, obstacle_x_0C, &00, obstacle_z_0C
 object object_x01_tall_cube,      &80, obstacle_x_10, &00, obstacle_z_10
 object object_x01_tall_cube,      &A0, obstacle_x_14, &00, obstacle_z_14
 object object_x03_wide_pyramid,   &00, obstacle_x_00, &00, obstacle_z_00
 object object_x03_wide_pyramid,   &18, obstacle_x_04, &00, obstacle_z_04
 object object_x03_wide_pyramid,   &20, obstacle_x_02, &00, obstacle_z_02
 object object_x03_wide_pyramid,   &50, obstacle_x_0A, &00, obstacle_z_0A
 object object_x03_wide_pyramid,   &70, obstacle_x_0E, &00, obstacle_z_0E
 object object_x03_wide_pyramid,   &90, obstacle_x_12, &00, obstacle_z_12
.object_shot_collision_end

.animated_object_start                  ;<--- start block to reset
.m_shell                                ;my shell and unit shells reversed compared with
 EQUW &FF                               ;my tank and unit for sub-routine indexing
.m_shell_x
 EQUW &00
.m_shell_y
 EQUW &00
.m_shell_z
 EQUW &00

.tank_shell                             ;index x = &08 tank shell, x = &00 my tank
 EQUW &FF
.tank_shell_x
 EQUW &00
.tank_shell_y
 EQUW &00
.tank_shell_z
 EQUW &00

.debris_start                           ;debris block x6
 object &FF, &00, &00, &00, &00
 object &FF, &00, &00, &00, &00
 object &FF, &00, &00, &00, &00
 object &FF, &00, &00, &00, &00         ;<--- last chunk to hit the ground
 object &FF, &00, &00, &00, &00
 object &FF, &00, &00, &00, &00         ;<--- reused for missile exhaust
                                        missile_offset    = debris_start + block_size * &05
                                        missile_exhaust   = missile_offset
                                        missile_exhaust_x = missile_offset + &02
                                        missile_exhaust_y = missile_offset + &04
                                        missile_exhaust_z = missile_offset + &06
.debris_end

.saucer                                 ;flying saucer
 EQUW &FF
.saucer_x
 EQUW &00
.saucer_y
 EQUW &00
.saucer_z
 EQUW &00

.explosion                              ;exploding stars, x4 frames of animation
 EQUW &FF
.explosion_x
 EQUW &00
.explosion_y
 EQUW &00
.explosion_z
 EQUW &00

.tank_or_super_or_missile               ;opponent placed here to index in for movement with my tank below
 EQUW &FF
.tank_or_super_or_missile_x
 EQUW &00
.tank_or_super_or_missile_y
 EQUW &00
.tank_or_super_or_missile_z
 EQUW &00

.object_end                             ;<--- end block to reset

.m_tank                                 ;index x = &00 tank/super/missile, x = &08 my tank
 EQUW &00                               ;my tank placed here to index in for movement
.m_tank_x
 EQUW &00
.m_tank_y
 EQUW &00
.m_tank_z
 EQUW &00

.reset_refresh                          ;reset console messages and keyboard block
 LDX #&06
 LDA #console_double
.refresh_all
 STA console_refresh - &01,X
 DEX
 BNE refresh_all
 LDY #&04                               ;x=0
.zero_combat_messages
 STX console_refresh + &06,Y            ;turn off combat messages
 DEY
 BPL zero_combat_messages
.reset_keyboard_block
 LDY #combined_block_end - combined_block_start
.clear_combined_block                   ;clear keys pressed
 STX combined_block_start,Y
 DEY
 BPL clear_combined_block
 RTS

.clear_all_screen                       ;full screen clear
 LDY #&28
 BNE clear_area_of_memory
 
.clear_rest_of_space                    ;clear off score, high score and tanks
 LDY #&09                               ;roll into routine below
 
.clear_area_of_memory                   ;y = number of pages to clear from screen start
 LDA screen_hidden
 STA clear_a_page + &02
 LDA #&00
 TAX
.clear_a_page
 STA dummy_address,X
 DEX
 BNE clear_a_page
 INC clear_a_page + &02
 DEY
 BNE clear_a_page
 RTS

.divide_rotation
 LDA m_tank_rotation + &01              ;entry number in range 0-&4ff
 LSR A                                  ;/4
 STA result_shifted + &01
 STA rotation_divide + &01
 LDA m_tank_rotation
 ROR A
 LSR result_shifted + &01
 LSR rotation_divide + &01
 ROR A
 TAX
 LSR result_shifted + &01               ;/16
 ROR A
 LSR A
 STA result_shifted
 TXA                                    ;- 1/16
 SEC
 SBC result_shifted
 LSR result_shifted                     ;/64
 LSR result_shifted
 CLC                                    ;+ 1/64
 ADC result_shifted
 LSR result_shifted                     ;/256
 LSR result_shifted
 SEC                                    ;- 1/256
 SBC result_shifted
 LSR result_shifted                     ;/1024
 LSR result_shifted
 CLC                                    ;+ 1/1024
 ADC result_shifted
 STA m_tank_rotation_256                ;sum in range 0-&ff
 STA m_tank + &01                       ;facing angle
 RTS

.score_increase                         ;a = opponent scoring
 SED
 CLC
 ADC player_score                       ;add to player score
 STA player_score
 LDA player_score + &01
 ADC #&00
 STA player_score + &01
 CLD                                    ;exit decimal mode
 LDY #console_double                    ;refresh screen score
 STY console_print
 BIT extra_tank                         ;had extra tank?
 BMI test_for_music                     ;yes, so lets check score for music
 LDY bonus_tank_index                   ;any bonus tank?
 BEQ test_for_music                     ;bonus tank not set up
 LDA player_score
 CMP bonus_tank_scores - &01,Y
 LDA player_score + &01
 SBC #&00
 BCC no_bonus_tank_yet                  ;no bonus tank yet
 DEC extra_tank                         ;extra tank given, set flag for given
 INC sound_extra_life                   ;extra life sound
 BNE increment_tanks                    ;always
.test_for_music
 BIT hundred_thousand                   ;check for 100,000 score
 BMI already_past_hundred_thousand      ;bit 7 = 1 music played
 TAX                                    ;a = high byte, reached score?
 BEQ not_reached_hundred_thousand
 DEC hundred_thousand                   ;set flag to &ff for 100,000 reached
 LDA #&80
 STA sound_music                        ;enable music
 STA sound_mute                         ;disable sound
 JSR sound_flush_buffers
 LDY bonus_tank_index                   ;any bonus tanks?
 BEQ not_reached_hundred_thousand       ;none
.increment_tanks
 INC game_number_of_tanks               ;+1 tank
.no_bonus_tank_yet
.not_reached_hundred_thousand
.already_past_hundred_thousand
 RTS

.bonus_tank_scores
 EQUB &15
 EQUB &25
 EQUB &50

.clear_cells                            ;use a list to clear screen objects
 STX list_access                        ;list pointer
 STY list_access + &01
 LDY #&00
.more_blocks
 LDA (list_access),Y
 STA clear_access + &01                 ;self modify address for speed
 INY
 LDA (list_access),Y
 CLC
 ADC screen_hidden                      ;hidden screen address
 STA clear_access + &02
 INY
 LDA (list_access),Y
 TAX                                    ;number of bytes to clear
 INY
 LDA #&00                               ;write byte
.clear_access
 STA clear_access,X
 DEX
 BNE clear_access
 RTS

.select_swr_ram_slot
 SEI
 LDX found_a_slot
 BIT machine_flag
 BMI select_electron
 STX bbc_romsel
 BPL paged                              ;always
.select_electron
 CPX #&08
 BCS just_select_swr
 LDA #&0C                               ;de-select basic
 STA paged_rom
 STA electron_romsel
.just_select_swr
 STX electron_romsel
.paged
 STX paged_rom
 CLI
 RTS

.cosine_256                             ;cosine, entry a = angle 0-&ff
 CLC
 ADC #&40                               ;roll into routine below

.sine_256                               ;sine, entry a = angle 0-&ff
 TAY
 BPL sine_positive
 EOR #&80                               ;bring into +ve range
 TAY
 LDA #&00                               ;invert table value
 SEC
 SBC sine_table_128_lsb,Y
 TAX
 LDA #&00
 SBC sine_table_128_msb,Y
 RTS                                    ;exit x/a lo/hi trig value
.sine_positive
 LDX sine_table_128_lsb,Y
 LDA sine_table_128_msb,Y
 RTS

.draw_tanks
 LDA game_number_of_tanks               ;check for zero tanks/in attract mode
 BEQ no_draw_tanks                      ;no tanks
 STA tank_working
 LDA #&07
 STA tank_screen_x
.next_tank
 LDA tank_screen_x                      ;screen address
 AND #&F8
 CLC
 ADC #LO(tank_screen_offset)
 STA tank_address
 LDA #HI(tank_screen_offset)
 ADC screen_hidden
 STA tank_address + &01
 LDA tank_screen_x
 AND #&07
 STA tank_rotate_bits
 LDX #&07
.tank_column
 LDA battlezone_sprites + tank_offset + &08,X
 STA tank_workspace
 LDA battlezone_sprites + tank_offset + &10,X
 STA tank_workspace + &01
 LDA battlezone_sprites + tank_offset,X
 LDY tank_rotate_bits
 BEQ no_tank_rotate
.rotate_tank_bits
 LSR A
 ROR tank_workspace
 ROR tank_workspace + &01
 DEY
 BNE rotate_tank_bits
.no_tank_rotate
 ORA (tank_address),Y                   ;or three bytes with screen
 STA (tank_address),Y
 LDY #&08
 LDA tank_workspace
 STA (tank_address),Y
 LDY #&10
 LDA tank_workspace + &01
 STA (tank_address),Y
 DEC tank_address
 DEX
 BPL tank_column
 LDA tank_screen_x
 CLC
 ADC #&13
 STA tank_screen_x
 DEC tank_working
 BNE next_tank
.no_draw_tanks
 RTS
 
.timer_interrupt
 LDA user_via_timer_1_latch_lo          ;only source user via interrupt timer 1, clear interrupt
 LDA #&B5                               ;logical colour 1 to physical colour 2 (green)
 STA sheila + &21
 LDA #&A5
 STA sheila + &21
 LDA #&95
 STA sheila + &21
 LDA #&85
 STA sheila + &21
 LDA #&F5
 STA sheila + &21
 LDA #&E5
 STA sheila + &21
 LDA #&D5
 STA sheila + &21
 LDA #&C5
 STA sheila + &21
 LDA interrupt_accumulator
 RTI

.game_initialisation                    ;initialise machine at start
 LDA #mode_01_attract_mode
 STA game_mode
 STA new_game_mode
 BIT machine_flag
 BPL no_colour_change                   ;running on bbc
 LDA #&FB                               ;change electron colour to green
 STA sheila + &08
 LDA #&FF
 STA sheila + &09
.no_colour_change
 LDA #14                                ;enable vertical sync event
 LDX #&04
 JSR osbyte
 JMP change_mode                        ;start up in attract mode

.print_variables
 RTS
; LDY #&00
; LDA hexx + &03
; JSR print_it_in_hex
; LDY #&08
; LDA hexx + &02
; JSR print_it_in_hex
; LDY #&10
; LDA hexx + &01
; JSR print_it_in_hex
; LDY #&18
; LDA hexx
; JMP print_it_in_hex
;
;.hexx EQUD &FFFFFFFF

.page_four_end
