; ******************************************************************************
; *                                                                            *
; *  atari battlezone                                                          *
; *  copyright 1980 atari, inc.                                                *
; *                                                                            *
; *  by ed rotberg, jed margolin, harry jenkins, roger hector, howard delman,  *
; *  mike albaugh, dan pliskin, doug snyder, owen rubin, and morgan hoff       *
; *                                                                            *
; *  bbc/electron source code for battlezone conversion                        *
; *  target machines : electron/bbc b/b+ with 16k swr/bbc master 128 with arm  *
; *                    tdmi second processor or faster                         *
; *  program         : atari battlezone 1980                                   *
; *                                                                            *
; ******************************************************************************
;
; electron keyboard
; -----------------
; keyboard is mapped to rom number 8 or 9
; column  address bit 0   bit 1    bit 2   bit 3
; 0       &bffe   right   copy     nc      space
; 1       &bffd   left    down     return  delete
; 2       &bffb   -       up       :       nc
; 3       &bff7   0       p        ;       /
; 4       &bfef   9       o        l       .
; 5       &bfdf   8       i        k       ,
; 6       &bfbf   7       u        j       m
; 7       &bf7f   6       y        h       n
; 8       &beff   5       t        g       b
; 9       &bdff   4       r        f       v
; a       &bbff   3       e        d       c
; b       &b7ff   2       w        s       x
; c       &afff   1       q        a       z
; d       &9fff   escape  caps lck ctrl    shift
;
; constants
; ticks per second
 hertz_03                               = &03
 hertz_16                               = &10

; timers for mode changes etc, all in seconds
 timer_slacking                         = &40
 timer_attract                          = &10
 timer_high_score                       = &08
 timer_music_in_game                    = &80
 timer_new_high_score                   = &3C
 timer_swerving                         = &40

 model_z_coordinate                     = &3FF
 screen_row                             = &140
 counter_refresh                        = &02

 mode_00_main_game                      = &00
 mode_01_attract_mode                   = &01
 mode_02_high_score_table               = &02
 mode_03_service_menu                   = &03
 mode_04_new_high_score                 = &04
 mode_05_battlezone_text                = &05
 mode_06_model_test                     = &06

 end_the_crack                          = &08
 dfs_page                               = &E00

 text_initial_y                         = &280
 text_initial_z                         = &140

 sine_peak                              = &7FFF
 sine_full_512                          = &200
 sine_quarter_512                       = sine_full_512 / 4

 score_tank                             = &01
 score_missile                          = &02
 score_super_tank                       = &03
 score_saucer                           = &05

 random_seed                            = &AA

; console variables action on current hidden screen
; &00 - do nothing
; &FF - clear the message
; &FE - clear the message
; &FD - display message *                          <--- start value for rhs
; &FC - start and display message, initial value * <--- start value for lhs
; * dependant on bit 0 of console_refresh + &03, middle row uses
; bit invert to synchronise message flashing

 console_messages                       = &FC   ;value controls messages
 console_score_entry                    = &FD   ;value controls flashing text
 console_double                         = &FE   ;value controls pane clear
 console_treble                         = &FD   ;value for score, tanks and high score print

 dummy_address                          = &1000 ;16 bit address with zero lsb

 bbc_a_key                              = 65    ;make inkey value positive and subtract 1
 bbc_b_key                              = 100
 bbc_c_key                              = 82
 bbc_d_key                              = 50
 bbc_k_key                              = 70
 bbc_m_key                              = 101
 bbc_n_key                              = 85
 bbc_r_key                              = 51
 bbc_t_key                              = 35
 bbc_x_key                              = 66
 bbc_z_key                              = 97
 bbc_escape                             = 112
 bbc_space                              = 98
 bbc_f_key                              = 67
 bbc_arrow_up                           = 57
 bbc_arrow_down                         = 41
 bbc_arrow_left                         = 25
 bbc_arrow_right                        = 121

 column_00                              = &BFFE
 column_01                              = &BFFD
 column_02                              = &BFFB
 column_03                              = &BFF7
 column_04                              = &BFEF
 column_05                              = &BFDF
 column_06                              = &BFBF
 column_07                              = &BF7F
 column_08                              = &BEFF
 column_09                              = &BDFF
 column_0A                              = &BBFF
 column_0B                              = &B7FF
 column_0C                              = &AFFF
 column_0D                              = &9FFF

 game_timer                             = &1200
 tank_screen_offset                     = &C7

 text_frames                            = &84

 host_addr                              = &30000 ;set bits to force load to host processor

 bzone0_loaded_at_host                  = &2000 + host_addr
 bzone2_loaded_at                       = &2000
 bzone2_loaded_at_host                  = &2000 + host_addr
 bzone2_relocated_to                    = &8000
 bzone3_loaded_at                       = &2000
 bzone3_loaded_at_host                  = &2000 + host_addr
 bzone3_relocated_to                    = &0E00
 bzone5_loaded_at                       = &2000
 bzone5_loaded_at_host                  = &2000 + host_addr
 bzone5_relocated_to                    = &0400

; addresses
 screen_start                           = &3000
 electron_romsel                        = &FE05

 INCLUDE "operating system.asm"         ;data files for main assembly
 INCLUDE "mathbox variables.asm"        ;mathbox usage
 INCLUDE "page zero.asm"                ;zp declarations
 INCLUDE "ascii.asm"                    ;ascii values etc
 INCLUDE "binary/battlezone sprites.bin.info"

; debounce bbc key press
MACRO debounce_bbc_key key_address
 BPL clear_key                          ;y=&ff
 BIT key_address                        ;stored key address
 BMI clear_key                          ;if pressed last time clear bit 7
 BVS debounce_end                       ;debounce
 STY key_address                        ;set key pressed flag and debounce flag
 BVC debounce_end                       ;always
.clear_key
 LSR key_address                        ;key not pressed, second pass clears debounce flag
.debounce_end
ENDMACRO

; debounce electron key press
MACRO debounce_electron_key key_address
 BEQ clear_key                          ;y=&ff
 BIT key_address
 BMI clear_key                          ;if pressed last time clear bit 7
 BVS debounce_end                       ;debounce
 STY key_address                        ;set key pressed flag and debounce flag
 BVC debounce_end                       ;always
.clear_key
 LSR key_address                        ;key not pressed, second pass clears debounce flag
.debounce_end
ENDMACRO

; direct key press on the bbc
MACRO read_key_bbc key_value
 LDA #key_value
 STA system_via_ora_ira_no_hand
 LDA system_via_ora_ira_no_hand         ;n flag = key pressed
ENDMACRO

; store unbounced key press result on bbc
MACRO store_unbounced_bbc unbounced_address
 STA unbounced_address
ENDMACRO

; direct read of key press on the electron
MACRO read_electron_key rom_address, key_bit_mask
 LDA rom_address
 AND #key_bit_mask                      ;z flag = key pressed
ENDMACRO

; store unbounced key press result on the electron
MACRO store_unbounced_electron unbounced_address
 BEQ electron_not_pressed
 TYA
.electron_not_pressed
 STA unbounced_address
ENDMACRO

; loader and code from disc, sequence is:-
; bz     - &0a00
;  > run bzone0, logo etc
;  > test for swr and abort if not present
; bzone0 - &2000
;  > test for arm second processor and set flag if present
;  > load arm parasite code
;  > one-off game set up code
;  > play atari logo
; bzone1 - &0a00
; bzone2 - &2000
; bzone3 - &3000
; bzone4 - &9000 - arm code
; bzone5 - &0400
; bz0/1  - settings
; bz2/3  - scores
;
; addresses available outside of normal workspace
; &0400 - &07ff language workspace/second processor code
; &0a00 - &0aff rs232/cassette buffer
; &0b00 - &0bff function keys
; &0c00 - &0cff expanded character set
;
; game memory map
; &2000 - &2fff initialisation code and atari logo
;
; &0400 - &07ff general workspace/code
; &0a00 - &0cff loader/workspace
;
; &0e00 - &2fff main code/data
; &3000 - &7fff double buffered mode 4
; &8000 - &bfff swr code/data

 ORG   &0A00
 CLEAR &0A00, &0CFF
 GUARD &0D00
 CPU   0

.bz                                     ;battlezone loader
 LDX #&C0                               ;temporary lowering of stack for loader while second processor initialised etc
 TXS
 LDA #ascii_00                          ;load bzone0 initialise environment/atari logo/find swr
 STA bzone_name + &05
 LDA #&FF
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 JSR bzone0                             ;run bzone0 logo
 LDX #&FF                               ;flatten stack for rest of loader
 TXS
 JSR randomise
 LDA #ascii_02                          ;load bzone2
 STA bzone_name + &05
 LDA #&FF                               ;load file to &2000
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 LDX #&40                               ;transfer to swr &8000
 LDY #&00
.transfer_bzone2_00
 LDA bzone2_loaded_at,Y
.transfer_bzone2_01
 STA bzone2_relocated_to,Y
 DEY
 BNE transfer_bzone2_00
 INC transfer_bzone2_00 + &02
 INC transfer_bzone2_01 + &02
 DEX
 BNE transfer_bzone2_00
 LDA #ascii_05                          ;load bzone5
 STA bzone_name + &05
 LDA #&FF
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 LDY #&00                               ;transfer to language workspace &400
.transfer_bzone5
 LDA bzone5_loaded_at,Y
 STA language_work_space,Y
 LDA bzone5_loaded_at + &100,Y
 STA language_work_space + &100,Y
 LDA bzone5_loaded_at + &200,Y
 STA language_work_space + &200,Y
 LDA bzone5_loaded_at + &300,Y
 STA language_work_space + &300,Y
 DEY
 BNE transfer_bzone5
 LDA #ascii_03                          ;load bzone3
 STA bzone_name + &05
 LDA #&FF
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 LDA #ascii_01                          ;*run bzone1
 STA bzone_name + &05
 LDA #&04
 LDX #LO(bzone_name)
 LDY #HI(bzone_name)
 JMP (fscv)

.load_bzone_file
 EQUW bzone_name
 EQUD &00
 EQUD &FF
 EQUD &00
 EQUD &00
.bzone_name
 EQUS "bzone0", &0D

.randomise                              ;initialise random number generator
 LDA #random_seed
 STA mathbox_random
 STA mathbox_random + &01
 STA mathbox_random + &02
 STA mathbox_random + &03
 RTS

.bz_end
 SAVE "bz", bz, &0CFF, bz + host_addr, bz + host_addr

 ORG   &0A00
 CLEAR &0A00, &0CFF
 GUARD &0D00

.bzone1
 INCLUDE "mathbox.asm"

.bzone1_end
 SAVE "bzone1", bzone1, &0CFF, bzone1_execute + host_addr, bzone1 + host_addr

 ORG   &0E00
 CLEAR &0E00, &2FFF
 GUARD &3000

.bzone3
 INCLUDE "multiply.asm"                 ;must be aligned on a page boundary
 INCLUDE "service.asm"
 INCLUDE "high score.asm"
 INCLUDE "radar.asm"

.electron_columns_lsb
 EQUB LO(column_0B - &FF)               ;subtract &ff as y loaded with &ff
 EQUB LO(column_08 - &FF)
 EQUB LO(column_09 - &FF)
 EQUB LO(column_08 - &FF)
 EQUB LO(column_0A - &FF)
 EQUB LO(column_0A - &FF)
 EQUB LO(column_07 - &FF)
 EQUB LO(column_02 - &FF)
 EQUB LO(column_01 - &FF)
 EQUB LO(column_01 - &FF)
 EQUB LO(column_00 - &FF)

.electron_columns_msb
 EQUB HI(column_0B - &FF)
 EQUB HI(column_08 - &FF)
 EQUB HI(column_09 - &FF)
 EQUB HI(column_08 - &FF)
 EQUB HI(column_0A - &FF)
 EQUB HI(column_0A - &FF)
 EQUB HI(column_07 - &FF)
 EQUB HI(column_02 - &FF)
 EQUB HI(column_01 - &FF)
 EQUB HI(column_01 - &FF)
 EQUB HI(column_00 - &FF)

.electron_key_mask
 EQUB (&08 << &01)                      ;x
 EQUB (&02 << &01)                      ;t
 EQUB (&02 << &01)                      ;r
 EQUB (&08 << &01)                      ;b
 EQUB (&08 << &01)                      ;c
 EQUB (&04 << &01)                      ;d
 EQUB (&08 << &01) + &01                ;n
 EQUB (&02 << &01) + &01                ;up
 EQUB (&02 << &01) + &01                ;down
 EQUB (&01 << &01) + &01                ;left
 EQUB (&01 << &01) + &01                ;right

.not_electron_new_high_score

 read_electron_key column_05, &04       ;k
 store_unbounced_electron combined_k
 read_electron_key column_06, &08       ;m
 store_unbounced_electron combined_m

 LDA #&0C                               ;deselect basic
 STA electron_romsel
 LDA paged_rom                          ;restore sideways ram
 STA electron_romsel
 PLP                                    ;restore irq status
 RTS

.read_rest_electron_keyboard            ;read individually for speed

 read_electron_key column_0C, &04       ;a
 store_unbounced_electron combined_a
 read_electron_key column_0C, &08       ;z
 store_unbounced_electron combined_z
 read_electron_key column_0D, &01       ;escape
 store_unbounced_electron combined_escape
 read_electron_key column_00, &08       ;space
 debounce_electron_key combined_space

 LDA game_mode                          ;debounce only for new high score
 CMP #mode_04_new_high_score
 BNE not_electron_new_high_score

 read_electron_key column_05, &04       ;k
 debounce_electron_key combined_k
 read_electron_key column_06, &08       ;m
 debounce_electron_key combined_m

 LDA #&0C                               ;deselect basic
 STA electron_romsel
 LDA paged_rom                          ;restore sideways ram
 STA electron_romsel
 PLP                                    ;restore irq status
 RTS

.electron_keyboard
 LDA #&08                               ;select keyboard rom 8
 STA electron_romsel
 LDA game_mode
 BEQ read_rest_electron_keyboard
 LDX #electron_columns_msb - electron_columns_lsb - &01
.electron_read_keys
 LDA electron_columns_lsb,X
 STA workspace
 LDA electron_columns_msb,X
 STA workspace + &01
 LDA electron_key_mask,X                ;bit 0 = debounce into carry
 LSR A
 AND (workspace),Y
 BEQ clear_electron_key                 ;y=&ff
 LDA combined_block_start,X
 BCS no_electron_debounce
 BMI clear_electron_key                 ;if pressed last time then not pressed
 ASL A                                  ;check debounce flag
 BMI electron_debounce_end
.no_electron_debounce
 STY combined_block_start,X             ;set key pressed/debounce flags
 DEX
 BPL electron_read_keys
 JMP read_rest_electron_keyboard
.clear_electron_key
 LSR combined_block_start,X             ;key not pressed, second pass clears debounce flag
.electron_debounce_end
 DEX
 BPL electron_read_keys
 JMP read_rest_electron_keyboard

.read_keyboard
 LDY #&FF                               ;key reset, y=&ff
 PHP
 SEI
 BIT machine_flag
 BMI electron_keyboard
 LDA #&7F                               ;set port a for input on bit 7 others outputs
 STA system_via_ddr_a
 LDA #&03                               ;stop keyboard auto-scan
 STA system_via_orb_irb
 LDA game_mode                          ;game mode
 BEQ bbc_00_main_game                   ;read all keys when playing main game
 LDX #bbc_key_values_end - bbc_key_values - &01
.bbc_read_keys
 LDA bbc_key_values,X                   ;bit 0 = debounce into carry
 LSR A
 STA system_via_ora_ira_no_hand
 LDA system_via_ora_ira_no_hand
 BPL clear_key
 LDA combined_block_start,X
 BCS no_bbc_debounce                    ;c=1 do not debounce key
 BMI clear_key                          ;if pressed last time clear bit 7
 ASL A                                  ;check bit 6
 BMI debounce_end                       ;debounce
.no_bbc_debounce
 STY combined_block_start,X             ;set key pressed flag and debounce flag
 DEX
 BPL bbc_read_keys
 BMI bbc_00_main_game                   ;always
.clear_key
 LSR combined_block_start,X             ;key not pressed, second pass clears debounce flag
.debounce_end
 DEX
 BPL bbc_read_keys

.bbc_00_main_game                       ;read individually for speed
 read_key_bbc bbc_a_key
 store_unbounced_bbc combined_a
 read_key_bbc bbc_z_key
 store_unbounced_bbc combined_z
 read_key_bbc bbc_escape
 store_unbounced_bbc combined_escape
 read_key_bbc bbc_space
 debounce_bbc_key combined_space
 read_key_bbc bbc_f_key
 debounce_bbc_key combined_f

 LDA game_mode                          ;debounce only for new high score for text entry
 CMP #mode_04_new_high_score
 BNE not_bbc_new_high_score

 read_key_bbc bbc_k_key
 debounce_bbc_key combined_k
 read_key_bbc bbc_m_key
 debounce_bbc_key combined_m

 LDA #&0B                               ;select auto scan of keyboard
 STA system_via_orb_irb
 PLP                                    ;restore irq status
 RTS

.not_bbc_new_high_score
 read_key_bbc bbc_k_key
 store_unbounced_bbc combined_k
 read_key_bbc bbc_m_key
 store_unbounced_bbc combined_m

 LDA #&0B                               ;select auto scan of keyboard
 STA system_via_orb_irb
 PLP                                    ;restore irq status
 RTS

.bbc_key_values
 EQUB bbc_x_key        << &01
 EQUB bbc_t_key        << &01
 EQUB bbc_r_key        << &01
 EQUB bbc_b_key        << &01
 EQUB bbc_c_key        << &01
 EQUB bbc_d_key        << &01
 EQUB (bbc_n_key       << &01) + &01    ;do not debounce keys with bit 0 = 1
 EQUB (bbc_arrow_up    << &01) + &01
 EQUB (bbc_arrow_down  << &01) + &01
 EQUB (bbc_arrow_left  << &01) + &01
 EQUB (bbc_arrow_right << &01) + &01
.bbc_key_values_end

.game_wait_event
 PHP                                    ;only vertical sync event 4 enabled
 PHA
 BIT machine_flag
 BMI maintain_counters                  ;electron so just maintain counters
 LDA #&B6                               ;logical colour 1 to physical colour 1 (red)
 STA sheila + &21
 LDA #&A6
 STA sheila + &21
 LDA #&96
 STA sheila + &21
 LDA #&86
 STA sheila + &21
 LDA #&F6
 STA sheila + &21
 LDA #&E6
 STA sheila + &21
 LDA #&D6
 STA sheila + &21
 LDA #&C6
 STA sheila + &21
 LDA #LO(game_timer)                    ;write lo latch
 STA user_via_timer_2_latch_lo
 LDA #HI(game_timer)                    ;write hi latch, initiate timer
 STA user_via_timer_2_latch_hi
.maintain_counters
 DEC vertical_sync                      ;tell foreground to switch if waiting
 DEC clock_divide_by_three              ;divide 50hz clock / 3 for 16hz clock
 BNE event_exit_timer                   ;simulates 16hz clock in arcade game used in ai
 LDA #hertz_03                          ;reset wait for three ticks
 STA clock_divide_by_three              ;rest of clocks updated at 16hz
 LDA ai_rez_protect                     ;at top?
 CMP #&FF                               ;updated at 16hz
 BEQ no_rez_inc
 INC ai_rez_protect                     ;update protect from aggression counter
.no_rez_inc
 LDA move_counter                       ;decrement move counter at ~16hz if > &00
 BEQ move_at_zero
 DEC move_counter
.move_at_zero
 INC clock_frame_counter                ;increment number of 16hz clock ticks for previous frame, actual update rate
 DEC clock_second                       ;decrement 16hz clock
 BNE event_swerve_timer
 LDA #hertz_16                          ;reload 16hz clock
 STA clock_second
 DEC clock_mode                         ;update mode change/slacker code seconds clock, triggered on -ve
 INC attract_counter                    ;used in attract mode for movement
.event_swerve_timer
 DEC clock_event_tick                   ;4 second clock, updated at 16hz used for missile swerving
 BPL event_exit_timer
 LDA #timer_swerving
 STA clock_event_tick
.event_exit_timer
 DEC clock_screen_update                ;update screen clock, change triggered on -ve value update for radar/messages
 PLA
 PLP
 RTS

.timer_interrupt
 LDA user_via_timer_2_latch_lo          ;clear interrupt by reading timer 2 low latch
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

.battlezone_sprites
 INCBIN  "binary/battlezone sprites.bin"

.bzone3_end
 SAVE "bzone3", bzone3,                 &3000,                bzone3 + host_addr,                 bzone3_loaded_at_host
 SAVE "bz0",    service_block_start,    service_block_end,    service_block_start    + host_addr, service_block_start    + host_addr
 SAVE "bz1",    service_block_start,    service_block_end,    service_block_start    + host_addr, service_block_start    + host_addr
 SAVE "bz2",    high_scores_save_start, high_scores_save_end, high_scores_save_start + host_addr, high_scores_save_start + host_addr
 SAVE "bz3",    high_scores_save_start, high_scores_save_end, high_scores_save_start + host_addr, high_scores_save_start + host_addr

 ORG   &8000
 CLEAR &8000, &BFFF
 GUARD &C000

.bzone2

.sine_table_256_lsb                     ;<--- aligned on a page boundary for access speed
 FOR angle, 0, &7F                      ;sine table &00 - &7f, half wave
   EQUB LO(sine_peak * SIN(angle * 2 * PI / 256))
 NEXT

.sine_table_256_msb
 FOR angle, 0, &7F                      ;sine table &00 - &7f, half wave
   EQUB HI(sine_peak * SIN(angle * 2 * PI / 256))
 NEXT

.sine_table_512_lsb                     ;<--- aligned on a page boundary for access speed
 FOR angle, 0, &7F                      ;sine table &00 - &7f, quarter wave
   EQUB LO(sine_peak * SIN(angle * 2 * PI / 512))
 NEXT

.sine_table_512_msb
 FOR angle, 0, &7F                     ;sine table &00 - &7f, quarter wave
   EQUB HI(sine_peak * SIN(angle * 2 * PI / 512))
 NEXT

; atan(2^(x/32))*128/pi
.atan_tab
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00
 EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &00 : EQUB &01 : EQUB &01 : EQUB &01
 EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01
 EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01
 EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01
 EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &01 : EQUB &02 : EQUB &02 : EQUB &02
 EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02
 EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02 : EQUB &02
 EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03
 EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &03 : EQUB &04 : EQUB &04 : EQUB &04
 EQUB &04 : EQUB &04 : EQUB &04 : EQUB &04 : EQUB &04 : EQUB &04 : EQUB &04 : EQUB &04
 EQUB &05 : EQUB &05 : EQUB &05 : EQUB &05 : EQUB &05 : EQUB &05 : EQUB &05 : EQUB &05
 EQUB &06 : EQUB &06 : EQUB &06 : EQUB &06 : EQUB &06 : EQUB &06 : EQUB &06 : EQUB &06
 EQUB &07 : EQUB &07 : EQUB &07 : EQUB &07 : EQUB &07 : EQUB &07 : EQUB &08 : EQUB &08
 EQUB &08 : EQUB &08 : EQUB &08 : EQUB &08 : EQUB &09 : EQUB &09 : EQUB &09 : EQUB &09
 EQUB &09 : EQUB &0A : EQUB &0A : EQUB &0A : EQUB &0A : EQUB &0B : EQUB &0B : EQUB &0B
 EQUB &0B : EQUB &0C : EQUB &0C : EQUB &0C : EQUB &0C : EQUB &0D : EQUB &0D : EQUB &0D
 EQUB &0D : EQUB &0E : EQUB &0E : EQUB &0E : EQUB &0E : EQUB &0F : EQUB &0F : EQUB &0F
 EQUB &10 : EQUB &10 : EQUB &10 : EQUB &11 : EQUB &11 : EQUB &11 : EQUB &12 : EQUB &12
 EQUB &12 : EQUB &13 : EQUB &13 : EQUB &13 : EQUB &14 : EQUB &14 : EQUB &15 : EQUB &15
 EQUB &15 : EQUB &16 : EQUB &16 : EQUB &17 : EQUB &17 : EQUB &17 : EQUB &18 : EQUB &18
 EQUB &19 : EQUB &19 : EQUB &19 : EQUB &1A : EQUB &1A : EQUB &1B : EQUB &1B : EQUB &1C
 EQUB &1C : EQUB &1C : EQUB &1D : EQUB &1D : EQUB &1E : EQUB &1E : EQUB &1F : EQUB &1F

; log2(x)*32
.log2_tab
 EQUB &00 : EQUB &00 : EQUB &20 : EQUB &32 : EQUB &40 : EQUB &4A : EQUB &52 : EQUB &59
 EQUB &60 : EQUB &65 : EQUB &6A : EQUB &6E : EQUB &72 : EQUB &76 : EQUB &79 : EQUB &7D
 EQUB &80 : EQUB &82 : EQUB &85 : EQUB &87 : EQUB &8A : EQUB &8C : EQUB &8E : EQUB &90
 EQUB &92 : EQUB &94 : EQUB &96 : EQUB &98 : EQUB &99 : EQUB &9B : EQUB &9D : EQUB &9E
 EQUB &A0 : EQUB &A1 : EQUB &A2 : EQUB &A4 : EQUB &A5 : EQUB &A6 : EQUB &A7 : EQUB &A9
 EQUB &AA : EQUB &AB : EQUB &AC : EQUB &AD : EQUB &AE : EQUB &AF : EQUB &B0 : EQUB &B1
 EQUB &B2 : EQUB &B3 : EQUB &B4 : EQUB &B5 : EQUB &B6 : EQUB &B7 : EQUB &B8 : EQUB &B9
 EQUB &B9 : EQUB &BA : EQUB &BB : EQUB &BC : EQUB &BD : EQUB &BD : EQUB &BE : EQUB &BF
 EQUB &C0 : EQUB &C0 : EQUB &C1 : EQUB &C2 : EQUB &C2 : EQUB &C3 : EQUB &C4 : EQUB &C4
 EQUB &C5 : EQUB &C6 : EQUB &C6 : EQUB &C7 : EQUB &C7 : EQUB &C8 : EQUB &C9 : EQUB &C9
 EQUB &CA : EQUB &CA : EQUB &CB : EQUB &CC : EQUB &CC : EQUB &CD : EQUB &CD : EQUB &CE
 EQUB &CE : EQUB &CF : EQUB &CF : EQUB &D0 : EQUB &D0 : EQUB &D1 : EQUB &D1 : EQUB &D2
 EQUB &D2 : EQUB &D3 : EQUB &D3 : EQUB &D4 : EQUB &D4 : EQUB &D5 : EQUB &D5 : EQUB &D5
 EQUB &D6 : EQUB &D6 : EQUB &D7 : EQUB &D7 : EQUB &D8 : EQUB &D8 : EQUB &D9 : EQUB &D9
 EQUB &D9 : EQUB &DA : EQUB &DA : EQUB &DB : EQUB &DB : EQUB &DB : EQUB &DC : EQUB &DC
 EQUB &DD : EQUB &DD : EQUB &DD : EQUB &DE : EQUB &DE : EQUB &DE : EQUB &DF : EQUB &DF
 EQUB &DF : EQUB &E0 : EQUB &E0 : EQUB &E1 : EQUB &E1 : EQUB &E1 : EQUB &E2 : EQUB &E2
 EQUB &E2 : EQUB &E3 : EQUB &E3 : EQUB &E3 : EQUB &E4 : EQUB &E4 : EQUB &E4 : EQUB &E5
 EQUB &E5 : EQUB &E5 : EQUB &E6 : EQUB &E6 : EQUB &E6 : EQUB &E7 : EQUB &E7 : EQUB &E7
 EQUB &E7 : EQUB &E8 : EQUB &E8 : EQUB &E8 : EQUB &E9 : EQUB &E9 : EQUB &E9 : EQUB &EA
 EQUB &EA : EQUB &EA : EQUB &EA : EQUB &EB : EQUB &EB : EQUB &EB : EQUB &EC : EQUB &EC
 EQUB &EC : EQUB &EC : EQUB &ED : EQUB &ED : EQUB &ED : EQUB &ED : EQUB &EE : EQUB &EE
 EQUB &EE : EQUB &EE : EQUB &EF : EQUB &EF : EQUB &EF : EQUB &EF : EQUB &F0 : EQUB &F0
 EQUB &F0 : EQUB &F1 : EQUB &F1 : EQUB &F1 : EQUB &F1 : EQUB &F1 : EQUB &F2 : EQUB &F2
 EQUB &F2 : EQUB &F2 : EQUB &F3 : EQUB &F3 : EQUB &F3 : EQUB &F3 : EQUB &F4 : EQUB &F4
 EQUB &F4 : EQUB &F4 : EQUB &F5 : EQUB &F5 : EQUB &F5 : EQUB &F5 : EQUB &F5 : EQUB &F6
 EQUB &F6 : EQUB &F6 : EQUB &F6 : EQUB &F7 : EQUB &F7 : EQUB &F7 : EQUB &F7 : EQUB &F7
 EQUB &F8 : EQUB &F8 : EQUB &F8 : EQUB &F8 : EQUB &F9 : EQUB &F9 : EQUB &F9 : EQUB &F9
 EQUB &F9 : EQUB &FA : EQUB &FA : EQUB &FA : EQUB &FA : EQUB &FA : EQUB &FB : EQUB &FB
 EQUB &FB : EQUB &FB : EQUB &FB : EQUB &FC : EQUB &FC : EQUB &FC : EQUB &FC : EQUB &FC
 EQUB &FD : EQUB &FD : EQUB &FD : EQUB &FD : EQUB &FD : EQUB &FD : EQUB &FE : EQUB &FE
 EQUB &FE : EQUB &FE : EQUB &FE : EQUB &FF : EQUB &FF : EQUB &FF : EQUB &FF : EQUB &FF

.octant_adjust
 EQUB &3F                               ;00111111 x+,y+,|x|>|y|
 EQUB &00                               ;00000000 x+,y+,|x|<|y|
 EQUB &C0                               ;11000000 x+,y-,|x|>|y|
 EQUB &FF                               ;11111111 x+,y-,|x|<|y|
 EQUB &40                               ;01000000 x-,y+,|x|>|y|
 EQUB &7F                               ;01111111 x-,y+,|x|<|y|
 EQUB &BF                               ;10111111 x-,y-,|x|>|y|
 EQUB &80                               ;10000000 x-,y-,|x|<|y|

 INCLUDE "sound.asm"
 INCLUDE "division.asm"

.address_pointer
 EQUW square1_lo_16
 EQUW square1_hi_16
 EQUW square2_lo_16
 EQUW square2_hi_16

.square_table                           ;quarter square table used for multiplication
 LDX #&07
.load_table
 LDA address_pointer,X
 STA square_address,X
 DEX
 BPL load_table
 RTS

.cosine_512                             ;entry :- x/a 9-bit angle, x = top 8 bits/a = bottom bit in bit 7
 ASL A                                  ;exit  :- y/a lsb/msb trig value
 TXA                                    ;put top bit y into bottom bit x
 ROL A
 TAX
 ROL A
 AND #&01
 TAY                                    ;x/y - full 9 bit value
 TXA                                    ;add in quarter value for cosine
 CLC
 ADC #LO(sine_quarter_512)
 TAX
 TYA
 ADC #&00
 BEQ first_half_512                     ;still in range
 TAY
 TXA
 SEC
 SBC #LO(sine_full_512)                 ;bring into range
 TAX
 TYA
 SBC #HI(sine_full_512)
 BNE second_half_512
.first_half_512
 TXA                                    ;if in second quarter then invert into first
 BPL not_negative_512_00
 EOR #&FF
 TAX
.not_negative_512_00
 LDA sine_table_512_msb,X
 LDY sine_table_512_lsb,X
 RTS

.sine_512                               ;entry :- x/y 9-bit angle, x = top 8 bits/y = bottom bit in bit 7
 ASL A                                  ;exit  :- y/a lsb/msb trig value
 TXA                                    ;put top bit y into bottom bit x
 ROL A
 TAX
 BCC first_half_512                     ;carry contains top bit
.second_half_512
 TXA                                    ;if in second quarter then invert into first
 BPL not_negative_512_01
 EOR #&FF
 TAX
.not_negative_512_01
 LDA #&00
 SEC
 SBC sine_table_512_lsb,X
 TAY
 LDA #&00
 SBC sine_table_512_msb,X
 RTS

; common sprite routine for multiple row based sprite operations
; + 00 screen address offset
; + 02 sprite address
; + 04 number of rows
; + 05 number of bytes

.multiple_row_sprite                    ;works at row level
 STX sprite_work
 STY sprite_work + &01
 LDY #&00
 LDA (sprite_work),Y                    ;screen address
 STA fast_store + &01
 INY
 LDA (sprite_work),Y
 CLC
 ADC screen_hidden                      ;add in screen address
 STA fast_store + &02
 INY
 LDA (sprite_work),Y                    ;sprite address
 STA fast_load + &01
 INY
 LDA (sprite_work),Y
 STA fast_load + &02
 INY
 LDA (sprite_work),Y                    ;number of rows
 TAX
 INY
 LDA (sprite_work),Y                    ;number of bytes in row
 STA fast_add + &01
 TAY
 STY fast_bytes + &01
.fast_bytes
 LDY #&00
.fast_load
 LDA fast_load,Y
.fast_store
 STA fast_store,Y
 DEY
 BNE fast_load
 LDA fast_store + &01                   ;next screen row
 CLC
 ADC #LO(screen_row)
 STA fast_store + &01
 LDA fast_store + &02
 ADC #HI(screen_row)
 STA fast_store + &02
 LDA fast_load + &01
.fast_add
 ADC #&00
 STA fast_load + &01
 BCC fast_no_inc
 INC fast_load + &02
.fast_no_inc
 DEX
 BNE fast_bytes
 RTS

.that_game_mode
 LDA new_game_mode
 ASL A
 BCC now_game_mode                      ;always

.this_game_mode
 LDA game_mode                          ;vector to mode routine
 ASL A                                  ;c=0
.now_game_mode
 ADC #LO(game_mode_select)
 STA game_vector + &01
.game_vector
 JMP (game_mode_select)

.game_mode_select
 EQUW main_game_mode
 EQUW main_attract_mode
 EQUW high_score_mode
 EQUW service_menu
 EQUW new_high_score_mode
 EQUW battlezone_text_mode
 EQUW model_test_mode
.game_mode_select_end

 IF (game_mode_select >> 8) <> (game_mode_select_end >> 8)
   ERROR ">>>> game vector table across two pages"
 ENDIF

.main_program
 JSR read_keyboard                      ;read keys according to mode

 mathbox_random_number
                                        ;<--- game start block
 LDA clock_frame_counter                ;get number of 16hz clock ticks for previous frame, actual update rate
 STA clock_move_counter_reload          ;store for move counter multiplication adjustment
 LDA #&00                               ;clear clock frame counter for this frame
 STA clock_frame_counter
 STA b_object_bounce_far                ;settle the landscape after a collision etc
 LSR b_object_bounce_near               ;>> 1 settle the objects
                                        ;<--- game end block
 JSR sound_tchaikovsky                  ;play music
 JSR this_game_mode                     ;all screen render
 JSR mathbox_toggle_activated           ;last thing before screen flip
 JSR sound_control                      ;scan for sounds to make
 JSR flip_screen
 JSR change_mode_now

 BIT clock_screen_update                ;clock used to smooth on screen effects
 BPL main_program                       ;not yet
                                        ;<--- timer start block
 BIT tracks_active                      ;is unit/missile moving?, bit7=1 stop frame update
 BMI no_track_move                      ;no
 DEC track_exhaust_index
 LDA track_exhaust_index
 AND #&03
 STA track_exhaust_index
.no_track_move
 LDA object_radar_rotation              ;update tank radar internal rotation
 ADC #object_radar_spin                 ;c=0, usually
 STA object_radar_rotation
 LDX explosion                          ;animate explosion
 BMI no_explosion                       ;not exploding
 INX
 STX explosion                          ;next explosion frame
 CPX #object_x1E_explosion_03 + &01
 BCC no_explosion
 ROR explosion                          ;c=1, this explosion now done so stop
.no_explosion
 INC radar_arm_position                 ;update radar arm
 LDA radar_arm_position
 AND #(number_of_sectors - &01)
 STA radar_arm_position
 INC console_press_start_etc            ;maintain flashing attract text/sights
 LDA #counter_refresh                   ;ready for next time
 STA clock_screen_update
                                        ;<--- timer end block
 BNE main_program                       ;always

.flip_screen
 LDA #&00                               ;wait for vertical sync
 STA vertical_sync
.wait_for_sync
 BIT vertical_sync
 BPL wait_for_sync
.flip_screen_loader                     ;flip without waiting
 LDA screen_hidden
 EOR #&68
 STA screen_hidden
 EOR #&68
 LSR A
 BIT machine_flag
 BPL flip_screen_bbc
 STA sheila + &03                       ;electron ula
.exit_function
 RTS

.flip_screen_bbc                        ;screen change
 LDX #x6845_r12
 STX sheila
 LSR A
 LSR A
 STA sheila + &01
 BIT mathbox_flag                       ;mathbox present?
 BVC exit_function
 LDA screen_hidden
 STA host_r0                            ;ready for mathbox
 LDA #mathbox_code_screen_address       ;write screen address to mathbox
 JMP mathbox_function_a_only

.check_for_slacking                     ;also render those elements required when alive
 JSR status_messages
 JSR radar                              ;render radar display
 JSR radar_spot                         ;render radar spot
 BIT clock_mode                         ;check main clock, gone -ve so slacking?
 BPL exit_function                      ;no
 BIT missile_flag                       ;missile active?
 BMI exit_function                      ;yes, so leave
 JMP create_missile                     ;throw one at player

.in_playing_game
 JSR my_projectile
 JSR enemy_projectile
 JSR object_render
 JSR orientation                        ;maintain messages
 JSR sound_continuous                   ;check for those continuous sounds tank/missile
 BIT m_tank_status                      ;player dead?
 BPL check_for_slacking                 ;no
 JSR clear_rest_of_space                ;player dying, clear radar/score/high score/tanks
 JSR crack_screen_open
 BIT m_shell                            ;my shot in flight? (check for my shot hit unit)
 BPL exit_function                      ;yes
 BIT debris_last_chunk_to_hit_ground    ;debris still flying?
 BPL exit_function                      ;yes
 LDA crack_counter
 CMP #end_the_crack
 BCC exit_function                      ;minimum delay before leaving cracked screen
 DEC game_number_of_tanks
 BEQ last_tank_gone                     ;no tanks left
 LDX #&00                               ;reinitialise for new tank
 STX crack_counter
 STX m_tank_status
 STX on_target
 JSR place_in_landscape
 JSR reset_refresh
 JMP create_tank                        ;create tank after being shot

.last_tank_gone
 JMP test_for_new_high_score            ;check if a high score

.main_game_mode                         ;play game
 JSR movement_keys
 BIT combined_escape                    ;check for escape, exit to attract
 BPL main_attract_mode                  ;not pressed
 JSR quick_switch_to_attract
.main_attract_mode                      ;common code for main game/attract mode
 JSR object_view_rotate_angles          ;player rotation angles
 JSR clear_play_area
 JSR moon
 JSR tank_sights
 JSR landscape
 JSR horizon
 JSR update_volcano
 JSR update_debris
 JSR update_saucer
 JSR update_enemy_unit
 JSR update_exhaust                     ;after update enemy unit to copy over coordinates
 JSR print_player_score
 LDA game_mode
 BEQ in_playing_game                    ;playing game, different code path
 LDA new_game_mode                      ;check new mode
 BEQ in_playing_game                    ;branch if switching

 JSR game_over_copyright_and_start      ;attract mode from here
 JSR attract_movement
 JSR start_coins                        ;add coins
 JSR object_render
 JSR orientation                        ;maintain messages
 JSR status_messages
 JSR radar                              ;render radar display
 JSR radar_spot                         ;render radar spot
 BIT clock_mode                         ;time to switch?
 BMI switch_to_high_score
.test_exit_keys
 BIT combined_b                         ;test keys while in attract mode
 BMI check_coins                        ;check enough coins to play game
 BIT combined_r
 BMI switch_to_model                    ;enter model mode
 BIT combined_x
 BMI switch_to_service                  ;enter service mode
.exit_routine
 RTS

.battlezone_text_mode                   ;battlezone text mode
 JSR clear_play_area
 JSR moon
 JSR landscape
 JSR horizon
 JSR radar                              ;render radar display, no spot
 JSR update_volcano
 JSR print_player_score
 LDX #LO(copyright)
 LDY #HI(copyright)
 JSR print
 JSR start_coins                        ;add coins
 BIT frame_counter                      ;pause before text displayed
 BMI no_start_text_yet
 JSR battlezone_text
.no_start_text_yet
 DEC frame_counter
 BNE test_exit_keys                     ;timed section not finished, if so switch to attract below

.switch_to_attract                      ;switch tree, all game mode switching occurs here
 LDA #mode_01_attract_mode
 BNE switch_to_new_mode
.switch_to_game
 LDA #mode_00_main_game
 BEQ switch_to_new_mode
.switch_to_high_score
 LDA #mode_02_high_score_table
 BNE switch_to_new_mode
.switch_to_service
 LDA #mode_03_service_menu
 BNE switch_to_new_mode
.switch_to_new_high_score
 LDA #mode_04_new_high_score
 BNE switch_to_new_mode
.switch_to_battlezone_text
 LDA #mode_05_battlezone_text
 BNE switch_to_new_mode
.switch_to_model
 LDA #mode_06_model_test
.switch_to_new_mode
 STA new_game_mode
 RTS

.model_test_mode
 BIT combined_escape
 BMI switch_to_battlezone_text
 JSR clear_play_area
 JMP model_display

.high_score_mode                        ;display high score table
 JSR clear_play_area
 JSR print_player_score
 JSR print_high_scores_table
 JSR start_coins                        ;add coins
 BIT clock_mode
 BMI switch_to_battlezone_text
 BPL test_exit_keys                     ;always

.check_coins                            ;enough coins to play?
 LDX coins_amount
 LDA coins_added
 SEC
 SBC coins_required,X
 BCC exit_routine                       ;insufficent funds
 STA coins_added                        ;use coins for game
 BCS switch_to_game                     ;always

.coins_required
 EQUB &00
 EQUB &01
 EQUB &02
 EQUB &04

.change_mode_now
 LDA game_mode
 EOR new_game_mode
 BNE change_mode
 RTS

.change_mode
 JSR reset_refresh
 JSR new_game_switch                    ;any initialisation to be done
 JSR clear_all_screen
 JSR set_up_timers
 JSR that_game_mode
 LDA new_game_mode
 STA game_mode
 JSR mathbox_toggle_activated           ;last thing before screen flip
 JSR flip_screen
 JSR sound_gate_mode                    ;sound/music control per mode/set music counter
 JMP clear_all_screen

.set_up_timers                          ;game timers set up on mode change
 PHP
 SEI                                    ;set timer
 LDA #hertz_16                          ;16hz timer
 STA clock_second
 LDX new_game_mode
 LDA time_out,X                         ;timer for mode shift etc
 STA clock_mode
 PLP                                    ;restore irq status
.exit_change                            ;exit here if no initialisation
 RTS

.new_game_switch                        ;vector to change routine
 LDX new_game_mode
 LDA change_type,X
 STA change_vector + &01
.change_vector
 BNE change_vector                      ;always

.change_type
 EQUB change_00   - change_vector - &02 ;0 main game
 EQUB change_01   - change_vector - &02 ;1 attract mode
 EQUB exit_change - change_vector - &02 ;2 high score table
 EQUB change_04   - change_vector - &02 ;3 service menu
 EQUB change_04   - change_vector - &02 ;4 new high score
 EQUB change_05   - change_vector - &02 ;5 battlezone text
 EQUB change_06   - change_vector - &02 ;6 model test

.change_00                              ;00 main game
 LDA service_number_of_tanks
 STA game_number_of_tanks
 JSR variables
 LDX #&00
 STX bonus_coins_tab                    ;clear bonus coins tabs as playing game
 STX m_tank_status
 STX m_tank_rotation_512 + &01          ;initialise player facing, low byte semi-random
 STX m_tank_x + &01                     ;place tank at roughly 0,0 don't zero low bytes
 STX m_tank_z + &01
 JSR calculate_rotation
.create_new_tank
 JSR create_tank                        ;spawn a tank
 JMP graphics_origin_game

.change_01                              ;01 attract mode
 LDA #&00
 STA game_number_of_tanks
 JSR variables
 LDA tank_or_super_or_missile           ;going into attract from game as finished?
 ORA missile_flag                       ;or missile then replace with standard tank
 BPL no_create_new_tank                 ;not present in arcade as no way to abort a game
 JSR create_tank                        ;spawn a tank
.no_create_new_tank
 JSR sound_flush_buffers
 JMP graphics_origin_game

.change_04
 SEC                                    ;turn music on
 ROR sound_music
 RTS

.change_05                              ;05 battlezone text
 LDA #&00
 STA object_relative_x
 STA object_relative_x + &01
 STA object_relative_z
 STA game_number_of_tanks
 STA player_score
 STA player_score + &01
 LDA #HI(text_initial_z)
 STA object_relative_z + &01
 LDA #LO(text_initial_y)
 STA object_relative_y
 LDA #HI(text_initial_y)
 STA object_relative_y + &01
 JSR variables
 LDA #text_frames                       ;frame counter
 STA frame_counter
 JSR calculate_rotation
 JMP graphics_origin_text

.change_06                              ;06 model test
 LDA #&70                               ;set up model x/y/z rotations
 STA y_object_rotation
 LDA #&00
 STA tracks_active
 STA x_object_rotation
 STA z_object_rotation
 STA object_relative_x
 STA object_relative_x + &01
 STA object_relative_y
 STA object_relative_y + &01
 STA model_identity
 LDA #LO(model_z_coordinate)
 STA object_relative_z
 LDA #HI(model_z_coordinate)
 STA object_relative_z + &01
 LDA #object_x00_narrow_pyramid         ;initial object
 STA i_object_identity
 JMP graphics_origin_game

.quick_switch_to_attract                ;load up attract mode from game, to avoid screen flicker/jumping
 JSR sound_flush_buffers                ;clear up any remaining sounds from the game
 LDA #mode_01_attract_mode
 STA game_mode
 STA new_game_mode                      ;roll into attract change
 LDA #console_treble                    ;refresh screen score
 STA console_print
 JSR set_up_timers
 JSR sound_gate_mode                    ;sound/music control per mode/set music counter
 JMP change_01

.variables                              ;common variables set up here
 PHP                                    ;stop interrupts since some counters are updated on events
 SEI
 LDA #&FF
 STA missile_count                      ;&ff = no missiles launched yet
 STA m_shell                            ;my shot/unit shot off
 STA unit_shot
 STA saucer                             ;saucer off
 STA explosion                          ;explosion off
 STA debris_start                       ;clear any debris
 STA debris_start + block_size
 STA debris_start + block_size * &02
 STA debris_start + block_size * &03
 STA debris_start + block_size * &04
 STA debris_start + block_size * &05
 STA radar_scr_a                        ;radar spot off
 LDA #&00                               ;clear workspace
 STA tank_or_super_or_missile_workspace_x
 STA tank_or_super_or_missile_workspace_x + &01
 STA tank_or_super_or_missile_workspace_z
 STA tank_or_super_or_missile_workspace_z + &01
 STA attract_counter
 LDY #variable_end - variable_start - &01
.setup_variables                        ;a=0
 LDX variable_start,Y
 STA page_zero,X
 DEY
 BPL setup_variables
 PLP                                    ;restore irq status
 RTS

.variable_start                         ;zero page addresses, set to zero
 EQUB clock_divide_by_three             ;counter for ~16hz timer
 EQUB clock_event_tick                  ;missile swerve counter
 EQUB clock_frame_counter               ;frame counter
 EQUB saucer_time_to_live
 EQUB move_counter
 EQUB saucer_dying
 EQUB player_score
 EQUB player_score + &01
 EQUB enemy_score
 EQUB clock_screen_update               ;initialise on game entry
 EQUB b_object_bounce_near
 EQUB b_object_bounce_far
 EQUB m_tank_status
 EQUB on_target
 EQUB crack_counter
 EQUB extra_tank
 EQUB hundred_thousand
 EQUB ai_rez_protect                    ;protect from aggression at game start
 EQUB recent_collision_flag
 EQUB track_exhaust_index
 EQUB missile_rand
.variable_end

.time_out
 EQUB timer_slacking                    ;main game        = 0  64 seconds, used to chase slackers with missiles
 EQUB timer_attract                     ;attract mode     = 1  16 seconds, move around for this long
 EQUB timer_high_score                  ;high score table = 2  08 seconds, show the latest scores
 EQUB &00                               ;service menu     = 3   0  unused, set up for use in disc error message
 EQUB timer_new_high_score              ;new high score   = 4  60 seconds
 EQUB &00                               ;battlezone text  = 5   0  unused, frame counter used instead
 EQUB &00                               ;test models      = 6   0  unused

.place_in_landscape
 LDA mathbox_random + &01               ;place view point in battlefield with random rotation in the landscape
 STA m_tank_rotation_512 + &01
 JSR calculate_rotation
.collided_try_again

 mathbox_random_number

 LDA mathbox_random
 STA enemy_turn_to
 STA m_tank_z
 LDA mathbox_random + &01
 STA m_tank_x
 LDA mathbox_random + &02
 AND #&3F
 STA m_tank_z + &01
 LDA mathbox_random + &03
 STA m_tank_x + &01
 LDX #block_size                        ;check object collision against my tank
 JSR object_collision_test
 BCS collided_try_again
 BIT missile_flag
 BPL death_by_tank
 JMP create_tank                        ;create tank if killed by missile
.death_by_tank
 RTS

 INCLUDE "models.asm"
 INCLUDE "reticule.asm"
 INCLUDE "linedraw.asm"
 INCLUDE "render.asm"
 INCLUDE "animate.asm"
 INCLUDE "landscape.asm"

.bzone2_end
 SAVE "bzone2", bzone2, &C000, bzone2 + host_addr, bzone2_loaded_at_host

; system via on the bbc has multiple interrupts suppressed in order
; to make the machine as fast as possible on the atari logo display
; to prevent colour bleeds
;
; configuration
; bit 0 = 1 a key has been pressed
; bit 1 = 1 vertical synchronisation has occurred on the video system (a 50hz time signal)
; bit 2 = 1 the system via shift register times out
; bit 3 = 1 a light pen strobe off the screen has occurred
; bit 4 = 1 the analogue converter has finished a conversion
; bit 5 = 1 timer 2 has timed out used for the speech system
; bit 6 = 1 timer 1 has timed out this timer provides the 100hz signal for the internal clocks
; bit 7 = 1 the system via was the source of the interrupt

 INCLUDE "atari logo.asm"

 ORG   &9000
 CLEAR &9000, &EFFF
 GUARD &F000

.bzone4                                 ;import file to save it to disk
 INCBIN  "bzarm,10d24-10d24"

.bzone4_end
 SAVE "bzone4", bzone4, bzone4_end, bzone4, bzone4

 ORG   &0400
 CLEAR &0400, &07FF
 GUARD &0800

.bzone5
 INCLUDE "page four.asm"                ;variable declarations/code for pages &04-&07

.bzone5_end
 SAVE "bzone5", bzone5, &07FF, bzone5 + host_addr, bzone5_loaded_at_host

 PUTTEXT "documents/credits.txt", "credits", &0000, &0000
 PUTTEXT "documents/notes.txt"  , "notes"  , &0000, &0000
 PUTTEXT "documents/readme.txt" , "readme" , &0000, &0000
 PUTTEXT "documents/version.txt", "version", &0000, &0000

 bz_limit     = &0B00
 bzone0_limit = &3000
 bzone1_limit = &0D00
 bzone2_limit = &C000
 bzone3_limit = &3000
 bzone4_limit = &D000
 bzone5_limit = &0800

 PRINT " page one   : ", ~vertices_table_start, "vertices table start"
 PRINT "            : ", ~vertices_table_end,   "vertices table end"
 PRINT "            : ", ~volcano_end,          "volcano end"
 PRINT " page four  : ", ~object_start,         "object start"
 PRINT "            : ", ~object_end,           "object end"
 PRINT "            : ", ~page_four_end,        "page four end"
 PRINT ""

 total_free_space = (bzone1_limit - bzone1_end) + (bzone2_limit - bzone2_end) + (bzone3_limit - bzone3_end) + (bzone5_limit - bzone5_end)

 PRINT "          >      <      |      ><"
 PRINT " bz     :", ~bz    , " ", ~bz_end    , " ", ~bz_limit
 PRINT " bzone0 :", ~bzone0, "" , ~bzone0_end, "" , ~bzone0_limit
 PRINT " bzone1 :", ~bzone1, " ", ~bzone1_end, " ", ~bzone1_limit, " " , ~bzone1_limit - bzone1_end
 PRINT " bzone2 :", ~bzone2, "" , ~bzone2_end, "" , ~bzone2_limit, ""  , ~bzone2_limit - bzone2_end
 PRINT " bzone3 :", ~bzone3, " ", ~bzone3_end, "" , ~bzone3_limit, ""  , ~bzone3_limit - bzone3_end
 PRINT " bzone4 :", ~bzone4, "" , ~bzone4_end, "" , ~bzone4_limit, ""
 PRINT " bzone5 :", ~bzone5, " ", ~bzone5_end, " ", ~bzone5_limit, " " , ~bzone5_limit - bzone5_end
 PRINT "                              ",  ~total_free_space
 PRINT "            >     <"
 PRINT " scores   :",  ~high_scores_save_start, ~high_scores_save_end
 PRINT " settings :",  ~service_block_start,    ~service_block_end
