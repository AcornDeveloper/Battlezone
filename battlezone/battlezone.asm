; ******************************************************************************
; *                                                                            *
; *  battlezone                                                                *
; *  copyright 1980 atari, inc.                                                *
; *                                                                            *
; *  by ed rotberg, jed margolin, harry jenkins, roger hector, howard delman,  *
; *  mike albaugh, dan pliskin, doug snyder, owen rubin, and morgan hoff       *
; *                                                                            *
; *  bbc/electron source/object code for battlezone conversion                 *
; *  target machines : electron, bbc b/b+ with 16k swr or bbc master 128,      *
; *                    arm tdmi second processor or faster                     *
; *  program         : atari battlezone 1980                                   *
; *                                                                            *
; ******************************************************************************
;
; cosmetic changes from the original arcade machine
; -------------------------------------------------
; service menu    - dip switch setting replaced using keys and values displayed
;                   settings persisted to disc as per later third-party add-on board
; general         - some centering of on screen text etc
; model display   - in built testing for the wire frame objects and routines
;
; arm mathbox
; -----------
; code is assembled at &9000 to be within the file system capability
; of loading straight from disc into second processor memory space
;
; view game instructions
; ----------------------
; change to mode 0, ctrl-n to scroll lock and then *type readme
; and use return key to page through
;
; atari logo
; ----------
; achieved with four frames of animation moving the horizontal
; tubes down screen with screen interrupts held in lock step to
; keep their colour as they move
;
; gap of four scan lines to accomodate colour change timing interrupt
; 256 frames of moving tubes
;
; logical colour, lc, colour value then program all possible
; values x, palette bits being the physical colour eor all bits set
;
; mode       bit 7  bit 6  bit 5  bit 4
; -------------------------------------
; 02 colour  lc     x      x      x
;            bit 0
; -------------------------------------
; 04 colour  lc     x      lc     x
;            bit 1         bit 0
; -------------------------------------
; 16 colour  lc     lc     lc     lc
;            bit 3  bit 2  bit 1  bit 0
; -------------------------------------
; lc = logical colour
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
; build variables

 atari_display                          = FALSE  ;loop control, use to display full logo
 debug                                  = FALSE  ;use to display loading etc

; atari constants
 atari_colour_copy                      = &90    ;&00 - &7F used by second processor initially
 atari_interrupt_flag                   = &91
 atari_invert                           = &92
 atari_work                             = &93

; constants
 model_z_coordinate                     = &500

 screen_row                             = &140
 black                                  = &00
 red                                    = &01
 green                                  = &02
 yellow                                 = &03
 blue                                   = &04
 magenta                                = &05
 cyan                                   = &06
 white                                  = &07
 atari_row                              = &1F0
 atari_timer                            = &EE8
 atari_logo_screen_address              = screen_row * 8 + &68
 atari_text_screen_address              = screen_row * 21 + &60
 atari_claim_id                         = &28

 counter_refresh                        = &03

 mode_00_main_game                      = &00
 mode_01_attract_mode                   = &01
 mode_02_high_score_table               = &02
 mode_03_service_menu                   = &03
 mode_04_new_high_score                 = &04
 mode_05_battlezone_text                = &05
 mode_06_model_test                     = &06

 end_the_crack                          = &10
 page                                   = &E00

 tank_facing                            = &477

 text_initial_y                         = &280
 text_initial_z                         = &140

 sine_peak                              = &7FFF
 sine_full_1280                         = &500
 sine_half_1280                         = sine_full_1280 / 2
 sine_quarter_1280                      = sine_full_1280 / 4

 tank_score                             = &01
 missile_score                          = &02
 super_tank_score                       = &03
 saucer_score                           = &05

; console variables action on current hidden screen
; &00 - do nothing
; &ff - clear the message
; &fe - clear the message
; &fd - display message *                          <--- start value for rhs
; &fc - display message *
; &fb - start and display message, initial value * <--- start value for lhs
; * dependant on bit 0 of console_refresh + &05, middle row uses bit invert

 console_messages                       = &FB   ;value controls messages
 console_score_entry                    = &FD   ;value controls flashing text
 console_double                         = &FE   ;value controls score, tanks and high score print, radar

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

 electron_plus_1_rom                    = &2AC

 game_timer                             = &1200
 tank_screen_offset                     = screen_row + &C7

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
 INCLUDE "page zero.asm"                ;all declarations in here to prevent duplication
 INCLUDE "battlezone sprites.bin.info"  ;sprite info
 INCLUDE "bzatri sprites.bin.info"

; envelope data
MACRO envelope n, t, pi1, p12, pi3, pn1, pn2, pn3, aa, ad, as, ar, ala, ald
 EQUB n
 EQUB t
 EQUB pi1
 EQUB p12
 EQUB pi3
 EQUB pn1
 EQUB pn2
 EQUB pn3
 EQUB aa
 EQUB ad
 EQUB as
 EQUB ar
 EQUB ala
 EQUB ald
ENDMACRO

; debounce bbc key press
MACRO debounce_bbc_key key_address
 BPL clear_key                          ;y=&ff
 BIT key_address
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
 STA sheila + &4F
 LDA sheila + &4F                       ;n flag = key pressed
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

; colour bytes for 6845
MACRO mode_colour_values_2_bits_per_pixel logical_colour, physical_colour
 IF logical_colour = 0
  byte = &00
 ELSE
  byte = &80
 ENDIF
 LDA #byte + &00 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &10 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &20 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &30 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &40 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &50 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &60 + (physical_colour EOR &07)
 STA sheila + &21
 LDA #byte + &70 + (physical_colour EOR &07)
 STA sheila + &21
 LDA interrupt_accumulator
 RTI
ENDMACRO

; loader and code from disc, sequence is:-
; bz     - &0a00
;  > run bzone0, logo etc
; bzone0 - &2000
;  > test for swr and abort if not present
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

.bz                                     ;battlezone loader
 LDX #&C0                               ;temporary stack for loader while second processor initialised etc
 TXS
 LDA #ascii_00                          ;load bzone0 initialise environment/atari logo
 STA bzone_name + &05
 LDA #&FF
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 JSR bzone0                             ;run bzone0 logo
 LDX #&FF                               ;flatten stack for rest of loader
 TXS
 JSR square_table                       ;multiplication square table
 LDA #ascii_02                          ;load bzone2
 STA bzone_name + &05
 LDA #&FF                               ;load file to &2000
 LDX #LO(load_bzone_file)
 LDY #HI(load_bzone_file)
 JSR osfile
 LDX #&40                               ;transfer to swr
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
 LDX #&04                               ;transfer to language workspace
 LDY #&00
.transfer_bzone5_00
 LDA bzone5_loaded_at,Y
.transfer_bzone5_01
 STA bzone5_relocated_to,Y
 DEY
 BNE transfer_bzone5_00
 INC transfer_bzone5_00 + &02
 INC transfer_bzone5_01 + &02
 DEX
 BNE transfer_bzone5_00
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
 STX mathbox_random                     ;initialise random number generator
 STX mathbox_random + &01
 STX mathbox_random + &02
 STX mathbox_random + &03
 STX mathbox_random + &04
 STX mathbox_random + &05
 RTS

.bz_end
 SAVE "bz", bz, &0CFF, bz + host_addr, bz + host_addr

 ORG   &0A00
 CLEAR &0A00, &0CFF
 GUARD &0D00

.bzone1
 INCLUDE "mathbox.asm"

.bzone1_free_space                      ;from here to &0cff to reuse as code is executed and discarded

.bzone1_execute
 JSR relocate_bzone3                    ;move code into place at page &0e00
 JSR game_event_and_user_via_vector     ;patch in irq and event processing
 BIT machine_flag                       ;test for disk enabled v=1
 BVC disk_inactive
 LDA #ascii_00                          ;load service
 JSR load_bz_file
 LDA #ascii_02                          ;load high scores
 JSR load_bz_file
.disk_inactive
 JSR select_swr_ram_slot

 IF NOT(debug)
   JSR clear_all_screen
   JSR flip_screen
   JSR clear_all_screen
 ENDIF

 JMP main_program_start

.relocate_bzone3
 LDY #&00
 LDX #&22
.transfer_bzone3_00
 LDA bzone3_loaded_at,Y
.transfer_bzone3_01
 STA bzone3_relocated_to,Y
 DEY
 BNE transfer_bzone3_00
 INC transfer_bzone3_00 + &02
 INC transfer_bzone3_01 + &02
 DEX
 BNE transfer_bzone3_00
 RTS

.game_event_and_user_via_vector
 SEI
 BIT machine_flag
 BMI no_user_via                        ;no user via for the electron
 LDA #LO(bbc_game_wait_event)
 STA eventv
 LDA #HI(bbc_game_wait_event)
 STA eventv + &01
 LDA #LO(timer_interrupt)
 STA irq2v
 LDA #HI(timer_interrupt)
 STA irq2v + &01
 CLI
 RTS

.no_user_via
 LDA #LO(electron_game_wait_event)
 STA eventv
 LDA #HI(electron_game_wait_event)
 STA eventv + &01
 CLI
 RTS

.bzone1_end
 SAVE "bzone1", bzone1, &0CFF, bzone1_execute + host_addr, bzone1 + host_addr

 ORG   &0E00
 CLEAR &0E00, &2FFF
 GUARD &3000

.bzone3
 INCLUDE "multiply.asm"                 ;must be aligned on a page boundary
 INCLUDE "service.asm"
 INCLUDE "high score.asm"
 INCLUDE "sound.asm"

.bbc_game_wait_event
 PHP                                    ;only vertical sync event 4 enabled
 PHA
 LDA game_mode                          ;no screen interrupts for model and service modes
 CMP #mode_03_service_menu
 BEQ maintain_counters
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
 LDA #user_via_aux_timer_1_one_shot     ;auxillary register set for timer 1 one shot
 STA user_via_aux_reg
 LDA #HI(game_timer)                    ;set up one shot timer
 STA user_via_timer_1_latch_hi          ;don't bother with latch low
 LDA #user_via_ier_timer_1              ;enable user via timer 1 interrupt
 STA user_via_ier_reg
 BNE maintain_counters                  ;always

.electron_game_wait_event
 PHP                                    ;only vertical sync event 4 enabled
 PHA
.maintain_counters
 DEC motion_counter                     ;countdown, when -ve trigger update
 DEC seconds                            ;decrement 50hz counter
 BPL event_exit_timer
 LDA #49                                ;reload seconds
 STA seconds
 LDA ai_clock                           ;update ai clock
 BEQ check_rez_protect
 DEC ai_clock
.check_rez_protect
 LDA ai_rez_protect                     ;update agression protect
 CMP #&11                               ;maximum 17 seconds peace and quiet
 BEQ check_mode_clock
 INC ai_rez_protect
.check_mode_clock
 LDA mode_clock                         ;update mode clock
 BEQ event_exit_timer
 DEC mode_clock
.event_exit_timer
 PLA
 PLP
 RTS

.electron_columns_lsb
 EQUB LO(column_0B - &FF)               ;subtract &ff as y loaded with this
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
 BPL electron_debounce_end              ;always
.clear_electron_key
 LSR combined_block_start,X             ;key not pressed, second pass clears debounce flag
.electron_debounce_end
 DEX
 BPL electron_read_keys

.read_rest_electron_keyboard            ;read individually for speed
 read_electron_key column_0C, &04       ;a
 store_unbounced_electron combined_a
 read_electron_key column_0C, &08       ;z
 store_unbounced_electron combined_z
 read_electron_key column_05, &04       ;k
 store_unbounced_electron combined_k
 read_electron_key column_06, &08       ;m
 store_unbounced_electron combined_m
 read_electron_key column_0D, &01       ;escape
 store_unbounced_electron combined_escape
 read_electron_key column_00, &08       ;space
 store_unbounced_electron combined_space

 LDA #&0C                               ;deselect basic
 STA electron_romsel
 LDA paged_rom                          ;restore sideways ram
 STA electron_romsel
 CLI
 RTS

.read_keyboard
 LDY #&FF                               ;key reset, y=&ff
 SEI
 BIT machine_flag
 BMI electron_keyboard
 LDA #&7F                               ;system via port a data direction register a
 STA sheila + &43                       ;bottom 7 bits are outputs and the top bit is an input
 LDA #&0F
 STA sheila + &42                       ;allow write to addressable latch
 LDA #&03
 STA sheila + &40                       ;set bit 3 to 0
 LDA game_mode                          ;game mode
 BEQ bbc_00_main_game                   ;read all keys after here
 LDX #bbc_key_values_end - bbc_key_values - &01
.bbc_read_keys
 LDA bbc_key_values,X                   ;bit 0 = debounce into carry
 LSR A
 STA sheila + &4F
 LDA sheila + &4F
 BPL clear_key
 LDA combined_block_start,X
 BCS no_bbc_debounce                    ;c=1 do not debounce key
 BMI clear_key                          ;if pressed last time clear bit 7
 ASL A                                  ;check bit 6
 BMI debounce_end                       ;debounce
.no_bbc_debounce
 STY combined_block_start,X             ;set key pressed flag and debounce flag
 BPL debounce_end                       ;always
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
 read_key_bbc bbc_k_key
 store_unbounced_bbc combined_k
 read_key_bbc bbc_m_key
 store_unbounced_bbc combined_m
 read_key_bbc bbc_escape
 store_unbounced_bbc combined_escape
 read_key_bbc bbc_space
 store_unbounced_bbc combined_space
 read_key_bbc bbc_f_key
 debounce_bbc_key combined_f

 CLI
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

.battlezone_sprites
 INCBIN  "battlezone sprites.bin"

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

.sine_table_128_lsb                     ;<--- aligned on a page boundary for access speed
 FOR angle, 0, &7F
   EQUB LO(sine_peak * SIN(angle * 2 * PI / 256))
 NEXT

.sine_table_128_msb
 FOR angle, 0, &7F
   EQUB HI(sine_peak * SIN(angle * 2 * PI / 256))
 NEXT

 INCLUDE "radar.asm"                    ;<--- aligned on a page boundary for access speed

.sine_table_320                         ;sine table 0 - &140, half wave interpolated 0 - &4ff
 FOR angle, 0, &140                     ;extra entry to cater for last index value having bit 0 set
   EQUW (sine_peak * SIN(angle * 2 * PI / 640))
 NEXT

.cosine_1280_6502                       ;cosine table x/a = angle required 0-&4ff
 CLC                                    ;a = lsb, x = msb
 ADC #LO(sine_quarter_1280)
 TAY                                    ;save a
 TXA
 ADC #HI(sine_quarter_1280)
 TAX
 TYA                                    ;restore a
 CPX #HI(sine_full_1280)
 BCC sine_1280_6502                     ;still in range
 TXA
 SEC                                    ;bring into range
 SBC #HI(sine_full_1280)
 TAX
 TYA
.sine_1280_6502                         ;sine table x/a = angle required 0-&4ff
 STA angle_address
 STX angle_address + &01
 SEC
 SBC #LO(sine_half_1280)                ;which half is angle in?
 TAY                                    ;save result
 TXA
 SBC #HI(sine_half_1280)
 ROR angle_bits                         ;save carry bit, used for result sign later
 BPL angle_in_range                     ;c into n, c=0 first half, c=1 second half
 STY angle_address                      ;update angle brought into range
 STA angle_address + &01
.angle_in_range
 LDA angle_address                      ;interpolation bit
 LSR A
 ROR angle_bits                         ;save carry bit
 ASL A                                  ;c=0, put back with bottom bit clear for index into table
 ADC #LO(sine_table_320)                ;offset into table
 STA angle_address
 LDA angle_address + &01
 ADC #HI(sine_table_320)
 STA angle_address + &01
 LDY #&00
 LDA (angle_address),Y
 STA angle_index
 INY
 LDA (angle_address),Y
 STA angle_index + &01                  ;a = hi trig
 BIT angle_bits                         ;interpolate?
 BPL do_not_interpolate
 INY                                    ;odd angle so needs average
 LDA angle_index
 ADC (angle_address),Y                  ;c=0 from addition above
 STA angle_index
 INY
 LDA angle_index + &01                  ;a = hi trig
 ADC (angle_address),Y
 LSR A                                  ;divide by 2 to get average
 STA angle_index + &01
 ROR angle_index
 BIT angle_bits                         ;re-test bit 6
.do_not_interpolate
 BVC do_not_negate                      ;angle in first half
 LDA #&00                               ;negate as in second half
 SEC
 SBC angle_index
 TAX
 LDA #&00
 SBC angle_index + &01                  ;exit x/a lo/hi trig value
 RTS
.do_not_negate
 LDX angle_index                        ;exit x/a lo/hi trig value
 RTS

; common sprite routine for multiple row based sprite operations
; + 00 screen address offset
; + 02 sprite address
; + 04 number of rows
; + 05 number of bytes

.multiple_row_sprite
 STX sprite_work
 STY sprite_work + &01
 LDY #&00
 LDA (sprite_work),Y                    ;screen address
 STA fast_store + &01
 INY
 LDA (sprite_work),Y
 CLC
 ADC screen_hidden
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
 EQUB bit_op
.this_game_mode
 LDA game_mode                          ;vector to mode routine
 ASL A                                  ;c=0
 ADC #LO(game_mode_select)
 STA game_vector + &01
.game_vector
 JMP (game_mode_select)

.game_mode_select
 EQUW main_game_mode
 EQUW attract_mode
 EQUW high_score_mode
 EQUW service_menu
 EQUW new_high_score_mode
 EQUW battlezone_text_mode
 EQUW model_test_mode
.game_mode_select_end

 IF (game_mode_select >> 8) <> (game_mode_select_end >> 8)
   ERROR ">>>> game vector table across two pages"
 ENDIF

.main_program_start
 JSR mathbox_claim_tube                 ;if present claim the tube
 JSR game_initialisation                ;set up
.game_loop
 JSR mathbox_random_number
 JSR sound_tchaikovsky                  ;music if required
 JSR sound_control                      ;scan for sounds to make
 JSR read_keyboard
 JSR this_game_mode
 JSR mathbox_toggle_activated
 JSR flip_screen
 JSR change_mode_now
 LSR b_object_bounce_near               ;divide near bounce offset by 2
 LSR b_object_bounce_far                ;clear far bounce offset
 BIT motion_counter                     ;timer used to smooth on screen effects
 BPL game_loop                          ;not yet
;                                       <--- timer start block
 INC radar_arm_position                 ;update radar arm
 LDA radar_arm_position
 AND #(number_of_sectors - &01)
 STA radar_arm_position
 LDA #console_double                    ;update radar
 STA console_radar
 INC crack_counter                      ;increment screen crack
 DEC console_sights_flashing            ;maintain flashing sights
 DEC console_press_start_etc            ;maintain flashing attract text
 LDA #counter_refresh                   ;maintain motion counter
 STA motion_counter
;                                       <--- timer end block
 BNE game_loop                          ;always

.flip_screen
 JSR print_variables
 LDA #19
 JSR osbyte
 LDA screen_hidden
 EOR #&68
 STA screen_hidden
 EOR #&68
 LSR A
 BIT machine_flag
 BPL bbc_screen_flip
 STA sheila + &03
.exit_function
 RTS

.bbc_screen_flip                        ;screen change
 LDX #&0C
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

.dead_or_alive
 JSR object_view_rotate_angles          ;tank rotation angles
 JSR orientation                        ;maintain messages and radar spot
 JSR status_messages
 JSR control_saucer                     ;saucer

 BIT m_tank_status                      ;if +ve player okay
 BMI player_dead
 JSR movement_keys                      ;player alive
.player_alive_no_keys
 JSR clear_play_area
 JSR moon
 JSR tank_sights
 JSR landscape
 JSR horizon
 JSR radar
 JSR particle_maintenance
 JSR print_player_score
 JSR animate_debris
 JSR animate_explosion_exhaust
 JSR object_test_visibility
 JMP object_render

.player_dead
 JSR clear_rest_of_space                ;clear off radar, score, high score and tanks
 JSR clear_play_area
 JSR moon
 JSR landscape
 JSR horizon
 JSR particle_maintenance
 JSR animate_debris
 JSR animate_explosion_exhaust
 JSR object_test_visibility
 JSR object_render
 JSR crack_screen_open
 LDX game_mode                          ;in attract mode?
 DEX
 BEQ exit_routine                       ;yes
 LDA crack_counter
 CMP #end_the_crack
 BCC exit_routine                       ;delay before leaving crack
 DEC game_number_of_tanks
 BEQ last_tank_gone                     ;no tanks left
 LDA #&00                               ;reinitialise for new tank
 STA crack_counter
 STA m_tank_status
 JSR reset_refresh
 JSR particle_system_reset
 JSR clear_all_screen
 JSR flip_screen
 JSR clear_all_screen
 JMP player_alive_no_keys
.last_tank_gone
 JMP test_for_new_high_score            ;check if a high score

.attract_mode                           ;attract mode
 JSR divide_rotation                    ;my tank rotation &04FF -> &FF
 JSR object_view_rotate_angles          ;tank rotation angles
 JSR player_alive_no_keys               ;different code legs
 JSR orientation                        ;maintain messages and radar spot
 JSR status_messages
 JSR control_saucer                     ;saucer
 JSR game_over_copyright_and_start
 JSR attract_movement
 JSR start_coins                        ;add coins
 LDA mode_clock
 BEQ switch_to_high_score
.test_exit_keys
 BIT combined_b                         ;test keys while in attract mode
 BMI check_coins                        ;see if enough to play game
 BIT combined_r
 BMI switch_to_model
 BIT combined_x
 BMI switch_to_service                  ;enter service mode
.exit_routine
 RTS

.check_coins                            ;enough coins to play?
 LDX coins_amount
 LDA coins_added
 SEC
 SBC coins_required,X
 BCC exit_routine                       ;insufficent funds
 STA coins_added                        ;add new coins
 BCS switch_to_game                     ;always

.coins_required
 EQUB &00
 EQUB &01
 EQUB &02
 EQUB &04

.battlezone_text_mode                   ;battlezone text mode
 JSR start_coins                        ;add coins
 JSR clear_play_area
 JSR moon
 JSR landscape
 JSR horizon
 JSR radar
 JSR particle_maintenance
 JSR battlezone_text
 DEC text_counter
 BNE test_exit_keys                     ;timed section not finished, if so switch to attract

.switch_to_attract                      ;switch tree, all game mode switching occurs here
 LDA #mode_01_attract_mode
 EQUB bit_op
.switch_to_game
 LDA #mode_00_main_game
 EQUB bit_op
.switch_to_high_score
 LDA #mode_02_high_score_table
 EQUB bit_op
.switch_to_service
 LDA #mode_03_service_menu
 EQUB bit_op
.switch_to_new_high_score
 LDA #mode_04_new_high_score
 EQUB bit_op
.switch_to_battlezone_text
 LDA #mode_05_battlezone_text
 EQUB bit_op
.switch_to_model
 LDA #mode_06_model_test
 STA new_game_mode
 RTS

.model_test_mode
 BIT combined_escape
 BMI switch_to_attract
 JSR start_coins                        ;add coins
 JSR clear_all_screen
 JMP model_display

.high_score_mode                        ;display high score table
 JSR start_coins                        ;add coins
 JSR clear_play_area
 JSR print_player_score
 JSR print_high_scores_table
 LDA mode_clock
 BEQ switch_to_battlezone_text
 BNE test_exit_keys                     ;always

.main_game_mode                         ;play game
 BIT combined_escape                    ;check for escape, exit to attract
 BMI switch_to_attract
 JSR divide_rotation                    ;my tank rotation &04FF -> &FF
 JSR dead_or_alive                      ;different code paths
 JMP my_projectile                      ;my shot maintenance

.change_mode_now
 LDA game_mode
 EOR new_game_mode
 BNE change_mode
.exit_change                            ;exit here if no initialisation
 RTS

.change_mode
 JSR reset_keyboard_block
 JSR particle_system_reset
 JSR reset_refresh
 JSR new_game_switch                    ;any initialisation to be done
 JSR game_window
 JSR clear_all_screen
 SEI                                    ;set timer
 LDX new_game_mode
 LDA #49                                ;timer in seconds
 STA seconds
 LDA time_out,X                         ;timer for mode shift etc
 STA mode_clock
 CLI
 JSR that_game_mode
 LDA new_game_mode
 STA game_mode
 JSR mathbox_toggle_activated
 JSR flip_screen
 JSR sound_mute_mode                    ;set sound enabled/disabled according to mode
 JMP clear_all_screen

.change_type
 EQUW change_00                         ;0 main game
 EQUW change_01                         ;1 attract mode
 EQUW exit_change                       ;2 high score table
 EQUW exit_change                       ;3 service menu
 EQUW exit_change                       ;4 new high score
 EQUW change_05                         ;5 battlezone text
 EQUW change_06                         ;6 model test
.change_type_end

.new_game_switch
 LDA new_game_mode                      ;vector to change routine
 ASL A                                  ;c=0
 ADC #LO(change_type)
 STA change_vector + &01
.change_vector
 JMP (change_type)

 IF (change_type >> 8) <> (change_type_end >> 8)
    PRINT ~change_type, ~change_type_end
    ERROR ">>>> change vector table across two pages"
 ENDIF

.change_00                              ;00 main game
 JSR animated_object_reset_all
 LDA service_number_of_tanks            ;set up game variables
 STA game_number_of_tanks
 LDX #&00
 STX bonus_coins_tab                    ;clear bonus coins tabs as playing game
 STX m_tank_status
 STX player_score                       ;clear scores
 STX player_score + &01
 STX enemy_score
 STX motion_counter
 STX crack_counter
 STX extra_tank
 STX hundred_thousand
 STX on_target
 STX missile_flag
 STX ai_rez_protect                     ;protect from aggression at game start for 17 seconds
 DEX
 STX missile_count
 STX debris_present
 JSR tank_random
 JSR unit_creation
 LDA #LO(tank_facing)                   ;place tank facing towards moon
 STA m_tank_rotation
 LDA #HI(tank_facing)
 STA m_tank_rotation + &01
 JSR divide_rotation                    ;my tank rotation &04ff -> &ff
 JMP graphics_origin_game

.change_01                              ;01 attract mode
 LDX #&FF                               ;x = &ff
 STX debris_present
 INX                                    ;x = 0
 STX game_number_of_tanks               ;don't display tanks on panel
 STX player_score
 STX player_score + &01
 STX motion_counter
 STX b_object_bounce_near
 STX b_object_bounce_far
 STX on_target
 STX missile_flag
 STX radar_arm_position
 STX frame_counter                      ;attract change counter
 INX                                    ;x = 1
 STX m_tank_speed_of_rotation
 STX m_tank_status
 JSR tank_random
 JSR unit_creation
 JMP graphics_origin_game

.change_05                              ;05 battlezone text
 LDX #&00
 STX object_relative_x
 STX object_relative_x + &01
 STX object_relative_z
 LDA #HI(text_initial_z)
 STA object_relative_z + &01
 LDA #LO(text_initial_y)
 STA object_relative_y
 LDA #HI(text_initial_y)
 STA object_relative_y + &01
 LDA #&80                               ;frame counter
 STA text_counter
 JSR tank_random
 JSR divide_rotation                    ;done once as landscape static
 JMP graphics_origin_text

.change_06                              ;06 model test
 LDA #&70                               ;set up model x/y/z rotations
 STA y_object_rotation
 LDA #&00
 STA x_object_rotation
 STA z_object_rotation
 STA object_relative_x
 STA object_relative_x + &01
 STA object_relative_y
 STA object_relative_y + &01
 STA object_relative_z
 LDA #HI(model_z_coordinate)
 STA object_relative_z + &01
 LDA #object_x00_narrow_pyramid         ;initial object
 STA i_object_identity
 JMP graphics_origin_game

.tank_random                            ;place somewhere in battlefield with random rotation in the landscape
 JSR mathbox_random_number
 LDA mathbox_random                     ;x/z low populate
 STA m_tank_x
 LDA mathbox_random + &01
 STA m_tank_z
 LDA mathbox_random + &02               ;x/z high populate
 STA m_tank_x + &01
 LDA mathbox_random + &03
 AND #&3F                               ;limit z axis
 STA m_tank_z + &01
 LDX #block_size                        ;check my tank not inside a geometrical object
 JSR object_collision_test
 BCS tank_random                        ;try again
 LDA #&00
 STA recent_collision_flag              ;clear collision flag
 LDA mathbox_random + &04
 LSR A
 AND #&03
 ADC #&00
 STA m_tank_rotation + &01              ;generate value between 0-&4ff
 RTS

.time_out
 EQUB &00                               ;main game        = 0   0 not used from here, ai/rez counter used
 EQUB &11                               ;attract mode     = 1  17 seconds
 EQUB &09                               ;high score table = 2  09 seconds
 EQUB &00                               ;service menu     = 3   0 not used, set up for use in disc error message
 EQUB &3C                               ;new high score   = 4  60 seconds
 EQUB &00                               ;battlezone text  = 5   0 not used, frame counter used instead
 EQUB &00                               ;test models      = 6   0 not used

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
; to prevent colour jumps
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

 ORG   &2000
 CLEAR &2000, &2FFF
 GUARD &3000

.bzone0
 JSR bzatri_machine_test
 JSR atari_clear_screens
 JSR atari_set_mode_four_and_cursor_off
 JSR bzatri_flush_sound_buffers
 JSR bzatri_common                      ;common to bbc/electron set up calls
 JSR bzatri_bbc_or_electron
 JSR bzatri_set_screen_hidden           ;game specific set up <--- start
 JSR bzatri_set_up_envelopes            ;game specific set up <--- end
 JSR bzatri_atari_logo_demo
 JSR bzatri_find_swr_ram_slot
 SEI
 LDX found_a_slot
 BIT machine_flag
 BMI bzatri_select_electron
 STX paged_rom
 STX bbc_romsel
 CLI
 RTS
.bzatri_select_electron
 CPX #&08
 BCS bzatri_just_select_swr
 LDA #&0C                               ;de-select basic
 STA paged_rom
 STA electron_romsel
.bzatri_just_select_swr
 STX paged_rom
 STX electron_romsel
 CLI
 RTS

 EQUS "BBC B,B+, Master 128/ARM TDMI/1Ghz Pi Copro/Electron - Battlezone © 1980 Atari Inc."
 EQUS "This version assembled on "
 EQUS TIME$

.bzatri_set_screen_hidden
 LDA #&30                               ;screen address
 STA screen_hidden
 RTS

.atari_set_mode_four_and_cursor_off
 LDA #&90                               ;turn interlace on
 LDX #&00
 LDY #&00
 JSR osbyte
 LDX #&00
.vdu_bytes
 LDA vdu_codes,X
 JSR oswrch
 INX
 CPX #vdu_codes_end - vdu_codes
 BNE vdu_bytes
 RTS

.vdu_codes
 EQUB 22                                ;set mode 4
 EQUB 4
 EQUB 23                                ;turn cursor off
 EQUB 1
 EQUD &00
 EQUD &00
.vdu_codes_end

.atari_clear_screens
 LDA #HI(screen_start)
 STA clear_screen + &02
 LDA #&00
 TAX
.clear_screen
 STA screen_start,X
 DEX
 BNE clear_screen
 INC clear_screen + &02
 BPL clear_screen
 RTS

.bzatri_bbc_or_electron
 BIT machine_flag
 BMI bzatri_electron_only               ;bbc specific
 LDA #16                                ;adc off
 LDX #&00
 LDY #&00
 JSR osbyte
 LDA #181                               ;rs423 off
 LDX #&00
 LDY #&00
 JSR osbyte
 LDA #201                               ;disable keyboard
 LDX #&00
 LDY #&00
 JMP osbyte

.bzatri_electron_only                   ;electron specific
 LDA #163                               ;disable printer/adc on plus 1
 LDX #&80
 LDY #&01
 JSR osbyte
 LDA #&00                               ;disable plus 1 rom in table
 STA electron_plus_1_rom
 RTS

.bzatri_common
 LDA #&09                               ;flashing colour 0
 LDX #&00
 LDY #&00
 JSR osbyte
 LDA #&0A                               ;flashing colour 1
 LDX #&00
 LDY #&00
 JMP osbyte

; some return values for machine type
; x = &00 bbc a/b with os 0.10
; x = &01 acorn electron os
; x = &f4 master 128 mos 3.26
; x = &f5 master compact mos 5
; x = &fb bbc b+ 64/128 (os 2.00)
; x = &fc bbc micro (west german mos)
; x = &fd master 128 mos 3.20/3.50
; x = &fe bbc micro (american os a1.0)
; x = &ff bbc micro os 1.00/1.20/1.23

.bzatri_machine_test
 LDA #&81                               ;which machine are we running on?
 LDX #&00
 LDY #&FF
 STX machine_flag                       ;clear machine type flag
 JSR osbyte
 CPX #&01                               ;if x=1 electron else other machines
 BNE bzatri_not_electron
 SEC                                    ;ignore tube flag as this is an electron
 ROR machine_flag                       ;set bit 7 for electron
.bzatri_not_electron
 LDA #&83                               ;read os high water mark
 JSR osbyte
 CPY #HI(page)                          ;page must be at &0e00 to activate disk use for save/load
 BNE atari_no_disk_access               ;primarily for &0e00 dfs disk systems
 LDA machine_flag                       ;bit 7 machine type/bit 6 disk enabled
 ORA #&40                               ;enable disk use
 STA machine_flag
.atari_no_disk_access
 RTS

.bzatri_find_swr_ram_slot
 BIT machine_flag
 BMI bzatri_find_electron_swr_ram_slot
 LDX #&0D
.bzatri_bbc_swr_loop
 STX bbc_romsel
 LDA bzone2
 INC bzone2
 CMP bzone2
 BEQ bzatri_next_slot
 LDY #swr_test_end - swr_self_write_test - &01
.transfer_test
 LDA swr_self_write_test,Y
 STA bzone2,Y
 DEY
 BPL transfer_test
 JSR bzone2                             ;z = result
 BNE bzatri_found_a_swr_slot
.bzatri_next_slot
 DEX
 BPL bzatri_bbc_swr_loop
 LDA paged_rom                          ;restore basic
 STA bbc_romsel
.bzatri_brk
 BRK
 EQUB &FF
 EQUS "Sideways RAM bank not found.", &00
.bzatri_found_a_swr_slot
 STX found_a_slot
 RTS

.swr_self_write_test                    ;try to increment memory
 INC bzone2 + (swr_test_location - swr_self_write_test)
 LDA bzone2 + (swr_test_location - swr_self_write_test)
 RTS                                    ;z = 1 no change z = 0 can use

.swr_test_location
 EQUB &00
.swr_test_end

.bzatri_find_electron_swr_ram_slot
 LDX #&0D
.bzatri_swr_electron_loop
 CPX #&08
 BCS bzatri_just_select_rom
 LDA #&0C                               ;de-select basic
 STA electron_romsel
.bzatri_just_select_rom
 STX electron_romsel                    ;now select rom
 LDA bzone2
 INC bzone2
 CMP bzone2
 BNE bzatri_found_a_swr_slot
 DEX
 BPL bzatri_swr_electron_loop
 LDA paged_rom                          ;restore basic
 STA electron_romsel
 JMP bzatri_brk

.bzatri_flush_sound_buffers
 LDX #&04                               ;clear out all sounds
.bzatri_flush_all_sounds
 TXA
 PHA
 LDA #21
 JSR osbyte
 PLA
 TAX
 INX
 CPX #&08
 BNE bzatri_flush_all_sounds
 RTS

.bzatri_set_up_envelopes                ;sound envelopes
 LDY envelope_index
 LDX envelope_data_address_start,Y      ;set up x/y address of envelope data
 LDA envelope_data_address_start + &01,Y
 INY
 INY
 STY envelope_index
 TAY
 LDA #&08                               ;define an envelope
 JSR osword
 DEC envelope_counter
 BNE bzatri_set_up_envelopes
 RTS

.envelope_counter
 EQUB (envelope_data_address_end - envelope_data_address_start) DIV 2
.envelope_index
 EQUB &00
.envelope_data_address_start
 EQUW envelope_01
 EQUW envelope_02
 EQUW envelope_03
 EQUW envelope_04
 EQUW envelope_05
 EQUW envelope_06
 EQUW envelope_07
 EQUW envelope_08
 EQUW envelope_09
 EQUW envelope_10
 EQUW envelope_11
 EQUW envelope_12
 EQUW envelope_13
 EQUW envelope_14
 EQUW envelope_15
.envelope_data_address_end

.envelope_01
 envelope 1,0,0,0,0,0,0,0,0,0,0,0,0,0                      ;enemy alert
.envelope_02
 envelope 2,1,0,0,0,0,0,0,126,-16,0,0,126,0                ;enemy radar
.envelope_03
 envelope 3,0,0,0,0,0,0,0,0,0,0,0,0,0                      ;missile hum
.envelope_04
 envelope 4,1,0,0,0,2,16,2,127,-3,0,0,99,0                 ;bump
.envelope_05
 envelope 5,0,0,0,0,0,0,0,0,0,0,0,0,0                      ;engine rev up
.envelope_06
 envelope 6,0,0,0,0,0,0,0,0,0,0,0,0,0                      ;engine rev down
.envelope_07
 envelope 7,1,0,0,0,0,0,0,126,-1,0,-20,126,30              ;tank soft shot
.envelope_08
 envelope 8,1,0,0,0,0,0,0,126,-1,0,-20,126,30              ;tank loud shot
.envelope_09
 envelope 9,0,0,0,0,0,0,0,0,0,0,0,0,0                      ;explosion soft
.envelope_10
 envelope 10,0,0,0,0,0,0,0,0,0,0,0,0,0                     ;explosion loud
.envelope_11
 envelope 11,0,0,0,0,0,0,0,0,0,0,0,0,0                     ;motion blocked
.envelope_12
 envelope 12,1,8,4,-12,4,4,4,100,0,0,0,100,100             ;saucer in motion
.envelope_13
 envelope 13,1,5,-9,7,3,3,3,126,0,0,-126,127,127           ;saucer shot
.envelope_14
 envelope 14,0,0,0,0,0,0,0,0,0,0,0,0,0                     ;extra life
.envelope_15
 envelope 15,1,0,0,0,0,0,0,126,-16,0,0,126,0               ;key click

.bzatri_atari_logo_demo
 JSR atari_mathbox
 JSR atari_irq_and_event_vector_set_up
 JSR atari_enable_vertical_event
.atari_logo
 JSR atari_flip_screen
 JSR atari_logo_animation
.atari_wait_for_it
 LDA atari_wait_counter                 ;even out animation
 CMP #&02
 BCC atari_wait_for_it
 LDA #&00
 STA atari_wait_counter
 DEC atari_frames_displayed             ;number of animation frames

 IF atari_display                       ;only display logo on full game
   BNE atari_logo
 ENDIF

 JSR atari_disable_vertical_event
 LDA #19                                ;need to let all interrupts finish
 JSR osbyte

 IF NOT(debug)
   JSR atari_colour_off                 ;roll into routine below
 ENDIF

.atari_restore_interrupt_vector
 SEI
 BIT machine_flag
 BMI atari_electron_restore_event
 LDA atari_irq_vector
 STA irq1v
 LDA atari_irq_vector + &01
 STA irq1v + &01
 LDA #&C2                               ;restore system via, enable timer 1 and vertical sync
 STA system_via_ier_reg
.atari_electron_restore_event
 LDA atari_event_vector                 ;restore event vector
 STA eventv
 LDA atari_event_vector + &01
 STA eventv + &01
 CLI
 RTS

.atari_colour_off                       ;logical colour 1 to physical colour 0
 LDX #&00
.atari_colour_bytes
 LDA colour_one,X
 JSR oswrch
 INX
 CPX #colour_one_end - colour_one
 BNE atari_colour_bytes
 RTS

.colour_one
 EQUB &13
 EQUB &01
 EQUD &00
.colour_one_end

.atari_print_logo
 LDA atari_frames_displayed
 BEQ atari_logo_exit
 CMP #&E0
 BCS atari_logo_exit
 LDX atari_frame_counter
 INX
 TXA
 AND #&03
 ASL A
 TAX
 LDA bzatri_logo_addresses,X
 STA bzatri_sprite_address_01
 STA bzatri_sprite_address_02
 LDA bzatri_logo_addresses + &01,X
 STA bzatri_sprite_address_01 + &01
 STA bzatri_sprite_address_02 + &01
 LDA #LO(atari_logo_screen_address)
 STA bzatri_sprite_block_01
 LDA #LO(atari_logo_screen_address + 56)
 STA bzatri_sprite_block_02
 LDA #HI(atari_logo_screen_address)
 STA bzatri_sprite_block_01 + &01
 LDA #HI(atari_logo_screen_address + 56)
 STA bzatri_sprite_block_02 + &01
 LDA atari_bars
 CMP #&0B << &02
 BCS atari_no_inc_bars
 INC atari_bars
.atari_no_inc_bars
 AND #&FC
 LSR A
 LSR A
 TAX
.atari_print
 TXA
 PHA
 LDX #LO(bzatri_sprite_block_01)
 LDY #HI(bzatri_sprite_block_01)
 JSR bzatri_multiple_row_sprite
 LDX #LO(bzatri_sprite_block_02)
 LDY #HI(bzatri_sprite_block_02)
 JSR bzatri_multiple_row_sprite
 LDA bzatri_sprite_block_01
 CLC
 ADC #LO(screen_row)
 STA bzatri_sprite_block_01
 LDA bzatri_sprite_block_01 + &01
 ADC #HI(screen_row)
 STA bzatri_sprite_block_01 + &01
 LDA bzatri_sprite_block_02
 CLC
 ADC #LO(screen_row)
 STA bzatri_sprite_block_02
 LDA bzatri_sprite_block_02 + &01
 ADC #HI(screen_row)
 STA bzatri_sprite_block_02 + &01
 PLA
 TAX
 DEX
 BPL atari_print
.atari_logo_exit
 RTS

.bzatri_sprite_block_01
 EQUW atari_logo_screen_address
.bzatri_sprite_address_01
 EQUW bzatri_sprites + atari_logo_00_offset
 EQUB &01
 EQUB 56

.bzatri_sprite_block_02
 EQUW atari_logo_screen_address + 56
.bzatri_sprite_address_02
 EQUW bzatri_sprites + atari_logo_00_offset
 EQUB &01
 EQUB 56

.bzatri_logo_addresses
 EQUW bzatri_sprites + atari_logo_00_offset
 EQUW bzatri_sprites + atari_logo_01_offset
 EQUW bzatri_sprites + atari_logo_02_offset
 EQUW bzatri_sprites + atari_logo_03_offset

.atari_text_parameters
 EQUW atari_text_screen_address
 EQUW bzatri_sprites + atari_text_large_offset
 EQUB &05
 EQUB &78

.atari_logo_mask                        ;mask display with atari logo
 LDA #LO(atari_logo_screen_address + screen_row * &06)
 STA screen_work
 LDA #HI(atari_logo_screen_address + screen_row * &06)
 CLC
 ADC screen_hidden
 STA screen_work + &01
 LDA #LO(bzatri_sprites + atari_logo_02_a_offset)
 STA sprite_work
 LDA #HI(bzatri_sprites + atari_logo_02_a_offset)
 STA sprite_work + &01
 LDX #&06
.atari_logo_loop_01
 LDY #111
.atari_logo_row_01
 LDA (screen_work),Y
 AND (sprite_work),Y
 STA (screen_work),Y
 DEY
 BPL atari_logo_row_01
 LDA screen_work
 CLC
 ADC #LO(screen_row)
 STA screen_work
 LDA screen_work + &01
 ADC #HI(screen_row)
 STA screen_work + &01
 LDA sprite_work
 CLC
 ADC #112
 STA sprite_work
 BCC atari_logo_no_inc_01
 INC sprite_work + &01
.atari_logo_no_inc_01
 DEX
 BNE atari_logo_loop_01
 LDA #LO(atari_logo_screen_address + 32)
 STA screen_work
 LDA #HI(atari_logo_screen_address + 32)
 CLC
 ADC screen_hidden
 STA screen_work + &01
 LDA #LO(bzatri_sprites + atari_logo_01_a_offset)
 STA sprite_work
 LDA #HI(bzatri_sprites + atari_logo_01_a_offset)
 STA sprite_work + &01
 LDX #&06
.atari_logo_loop_02
 LDY #47
.atari_logo_row_02
 LDA (screen_work),Y
 AND (sprite_work),Y
 STA (screen_work),Y
 DEY
 BPL atari_logo_row_02
 LDA screen_work
 CLC
 ADC #LO(screen_row)
 STA screen_work
 LDA screen_work + &01
 ADC #HI(screen_row)
 STA screen_work + &01
 LDA sprite_work
 CLC
 ADC #48
 STA sprite_work
 BCC atari_logo_no_inc_02
 INC sprite_work + &01
.atari_logo_no_inc_02
 DEX
 BNE atari_logo_loop_02
 RTS

.atari_clear_cells                      ;use a list to clear screen objects
 STA clear_counter                      ;number of entries to clear
 STX list_access                        ;list pointer
 STY list_access + &01
 LDY #&00
.atari_more_blocks
 LDA (list_access),Y
 STA atari_access + &01                 ;self modify address for speed
 INY
 LDA (list_access),Y
 CLC
 ADC screen_hidden                      ;hidden screen address
 STA atari_access + &02
 INY
 LDA (list_access),Y
 TAX                                    ;number of bytes to clear
 INY
 LDA #&00                               ;write byte
.atari_access
 STA atari_access,X
 DEX
 BNE atari_access
 DEC clear_counter                      ;number of blocks to clear
 BNE atari_more_blocks
 RTS

.atari_offset_lo
 EQUB LO(atari_timer)
 EQUB LO(atari_timer + (atari_row * 0.25))
 EQUB LO(atari_timer + (atari_row * 0.50))
 EQUB LO(atari_timer + (atari_row * 0.75))
.atari_offset_hi
 EQUB HI(atari_timer)
 EQUB HI(atari_timer + (atari_row * 0.25))
 EQUB HI(atari_timer + (atari_row * 0.50))
 EQUB HI(atari_timer + (atari_row * 0.75))

.atari_irq_and_event_vector_set_up
 SEI
 BIT machine_flag
 BMI bzatri_electron_set_up
 LDA #&7D                               ;disable all system via interrupts except vertical sync
 STA system_via_ier_reg
 LDA eventv
 STA atari_event_vector
 LDA eventv + &01
 STA atari_event_vector + &01
 LDA #LO(atari_wait_event)
 STA eventv
 LDA #HI(atari_wait_event)
 STA eventv + &01
 LDA irq1v
 STA atari_irq_vector
 LDA irq1v + &01
 STA atari_irq_vector + &01
 LDA #LO(atari_timer_interrupt)
 STA irq1v
 LDA #HI(atari_timer_interrupt)
 STA irq1v + &01
 CLI
 RTS

.bzatri_electron_set_up                 ;only events set up for electron
 LDA eventv
 STA atari_event_vector
 LDA eventv + &01
 STA atari_event_vector + &01
 LDA #LO(atari_electron_wait_event)
 STA eventv
 LDA #HI(atari_electron_wait_event)
 STA eventv + &01
 CLI
 RTS

.atari_wait_event
 PHP
 CMP #event_vertical_sync
 BNE atari_exit_event
 PHA
 TXA
 PHA
 LDA #user_via_aux_timer_1_one_shot     ;auxillary register set for timer 1 one shot
 STA user_via_aux_reg
 LDX atari_frame_counter
 LDA atari_offset_lo,X
 STA user_via_timer_1_latch_lo
 LDA atari_offset_hi,X
 STA user_via_timer_1_latch_hi
 LDA #user_via_ier_timer_1              ;enable user via timer interrupt
 STA user_via_ier_reg
 LDA #&00                               ;flag to differentiate interrupts
 STA atari_interrupt_flag
 LDA #&11                               ;number of interrupts down screen required
 STA atari_number_free_running
 INC atari_wait_counter
 PLA
 TAX
 PLA
 PLP
 RTS
.atari_exit_event
 PLP
 JMP (atari_event_vector)

.atari_timer_interrupt
 BIT user_via_ifr_reg                   ;bit 7 is set if interrupt was from user 6522
 BPL exit_atari_timer_interrupt         ;only source of user via interrupts is timer 1
 BIT atari_interrupt_flag               ;test if free running
 BMI atari_free_running_interrupt
 DEC atari_interrupt_flag               ;now set up free running interrupts
 LDA #user_via_aux_timer_1_continuous   ;auxillary register set for timer 1 continuous
 STA user_via_aux_reg
 LDA #LO(atari_row)
 STA user_via_timer_1_latch_lo          ;clear interrupt
 LDA #HI(atari_row)
 STA user_via_timer_1_latch_hi
 LDA #user_via_ier_timer_1              ;enable user via timer interrupt
 STA user_via_ier_reg
 LDA atari_colour_start                 ;colour table start
 STA atari_colour_copy                  ;for use by rolling interrupt
 LDA interrupt_accumulator
 RTI
.exit_atari_timer_interrupt
 JMP (atari_irq_vector)

.atari_free_running_interrupt
 TXA
 PHA
 LDA user_via_timer_1_latch_lo          ;clear interrupt
 DEC atari_number_free_running          ;last interrupt?
 BEQ exit_turn_to_last
 LDX atari_colour_copy                  ;increment for next interrupt
 INX
 CPX #&08
 BCC atari_no_black
 LDX #red
.atari_no_black
 STX atari_colour_copy
 LDA colour_table_lo,X                  ;get the address
 STA atari_work
 LDA colour_table_hi,X
 STA atari_work + &01
 PLA
 TAX
 JMP (atari_work)                       ;change colour
.exit_turn_to_last
 LDA #user_via_aux_clear                ;disable user via timer interrupt
 STA user_via_aux_reg
 PLA
 TAX
 LDA interrupt_accumulator
 RTI

.atari_stack_store
 EQUB &00
.atari_irq_vector
 EQUW &00
.atari_event_vector
 EQUW &00
.atari_colour_start
 EQUB &01
.atari_number_free_running
 EQUB &00
.atari_frame_counter
 EQUB &00
.atari_frames_displayed
 EQUB &00
.atari_bars
 EQUB &03
.atari_prynt_text
 EQUB &02
.atari_wait_counter
 EQUB &00

.colour_table_lo
 EQUB LO(black_colour)
 EQUB LO(red_colour)
 EQUB LO(green_colour)
 EQUB LO(blue_colour)
 EQUB LO(yellow_colour)
 EQUB LO(magenta_colour)
 EQUB LO(cyan_colour)
 EQUB LO(white_colour)

.colour_table_hi
 EQUB HI(black_colour)
 EQUB HI(red_colour)
 EQUB HI(green_colour)
 EQUB HI(blue_colour)
 EQUB HI(yellow_colour)
 EQUB HI(magenta_colour)
 EQUB HI(cyan_colour)
 EQUB HI(white_colour)

.black_colour
 mode_colour_values_2_bits_per_pixel 1,black
.red_colour
 mode_colour_values_2_bits_per_pixel 1,red
.green_colour
 mode_colour_values_2_bits_per_pixel 1,green
.yellow_colour
 mode_colour_values_2_bits_per_pixel 1,yellow
.blue_colour
 mode_colour_values_2_bits_per_pixel 1,blue
.magenta_colour
 mode_colour_values_2_bits_per_pixel 1,magenta
.cyan_colour
 mode_colour_values_2_bits_per_pixel 1,cyan
.white_colour
 mode_colour_values_2_bits_per_pixel 1,white

.atari_flip_screen
 LDX atari_frame_counter                ;next logo frame
 INX
 TXA
 AND #&03
 STA atari_frame_counter
 BNE atari_no_start_inc                 ;every four frames pull colour back one
 LDX atari_colour_start
 DEX
 BNE atari_start
 DEX
.atari_start
 TXA
 AND #&07
 STA atari_colour_start
.atari_no_start_inc
 LDA #19
 JSR osbyte
 LDA screen_hidden
 EOR #&68
 STA screen_hidden
 EOR #&68
 LSR A
 BIT machine_flag
 BMI atari_electron_screen_flip
 LDX #&0C
 STX sheila
 LSR A
 LSR A
 STA sheila + &01
 RTS
.atari_electron_screen_flip
 STA sheila + &03
.atari_exit_routine
 RTS

.atari_blank
 EQUW atari_logo_screen_address - &01
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &01 - &01
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &02 - &01
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &03 - &01
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &04 - &01
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &05 - &01
 EQUB &20
 EQUW atari_logo_screen_address - &01 + 80
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &01 - &01 + 80
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &02 - &01 + 80
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &03 - &01 + 80
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &04 - &01 + 80
 EQUB &20
 EQUW atari_logo_screen_address + screen_row * &05 - &01 + 80
 EQUB &20
.atari_blank_end

.atari_logo_animation
 JSR atari_print_logo
 JSR atari_logo_mask
 LDA #(atari_blank_end - atari_blank) DIV 3
 LDX #LO(atari_blank)
 LDY #HI(atari_blank)
 JSR atari_clear_cells
.atari_text                             ;display text just twice
 LDA atari_prynt_text
 BEQ atari_exit_routine
 DEC atari_prynt_text
 LDX #LO(atari_text_parameters)
 LDY #HI(atari_text_parameters)         ;roll into routine below

.bzatri_multiple_row_sprite
 STX sprite_work
 STY sprite_work + &01
 LDY #&00
 LDA (sprite_work),Y                    ;screen address
 STA bzatri_fast_store + &01
 INY
 LDA (sprite_work),Y
 CLC
 ADC screen_hidden
 STA bzatri_fast_store + &02
 INY
 LDA (sprite_work),Y                    ;sprite address
 STA bzatri_fast_load + &01
 INY
 LDA (sprite_work),Y
 STA bzatri_fast_load + &02
 INY
 LDA (sprite_work),Y                    ;number of rows
 TAX
 INY
 LDA (sprite_work),Y                    ;number of bytes in row
 STA bzatri_fast_add + &01
 TAY
 DEY
 STY bzatri_fast_bytes + &01
.bzatri_fast_bytes
 LDY #&00
.bzatri_fast_load
 LDA bzatri_fast_load,Y
.bzatri_fast_store
 STA bzatri_fast_store,Y
 DEY
 BPL bzatri_fast_load
 LDA bzatri_fast_store + &01            ;next screen row
 CLC
 ADC #LO(screen_row)
 STA bzatri_fast_store + &01
 LDA bzatri_fast_store + &02
 ADC #HI(screen_row)
 STA bzatri_fast_store + &02
 LDA bzatri_fast_load + &01
 CLC
.bzatri_fast_add
 ADC #&00
 STA bzatri_fast_load + &01
 BCC bzatri_fast_no_inc
 INC bzatri_fast_load + &02
.bzatri_fast_no_inc
 DEX
 BNE bzatri_fast_bytes
 RTS

.atari_enable_vertical_event
 LDA #&0E                               ;enable vertical sync event
 LDX #&04
 JMP osbyte

.atari_disable_vertical_event
 LDA #&0D                               ;disable vertical sync event
 LDX #&04
 JMP osbyte

.atari_electron_wait_event
 PHP
 PHA
 TXA
 PHA
 LDA atari_invert
 EOR #&80
 STA atari_invert
 BMI atari_no_flip
 LDX atari_colour_start
 LDA bzatri_electron_colour_08,X        ;set logical colour 0 black, colour 1 red - white
 STA sheila + &08
 LDA bzatri_electron_colour_09,X
 STA sheila + &09
.atari_no_flip
 INC atari_wait_counter
 PLA
 TAX
 PLA
 PLP
 RTS

; two colour mode palette
;       d7  d6  d5  d4  d3  d2  d1  d0
; &fe08 x   b1  x   b0  x   g1  x   x
; &fe09 x   x   x   g0  x   r1  x   r0

.bzatri_electron_colour_08              ;reverse the logic with eor
 EQUB &00 EOR &FF                       ;black
 EQUB &00 EOR &FF                       ;red
 EQUB &04 EOR &FF                       ;green
 EQUB &40 EOR &FF                       ;blue
 EQUB &04 EOR &FF                       ;yellow
 EQUB &40 EOR &FF                       ;magenta
 EQUB &44 EOR &FF                       ;cyan
 EQUB &44 EOR &FF                       ;white

.bzatri_electron_colour_09
 EQUB &00 EOR &FF                       ;black
 EQUB &04 EOR &FF                       ;red
 EQUB &00 EOR &FF                       ;green
 EQUB &00 EOR &FF                       ;blue
 EQUB &04 EOR &FF                       ;yellow
 EQUB &04 EOR &FF                       ;magenta
 EQUB &00 EOR &FF                       ;cyan
 EQUB &04 EOR &FF                       ;white

.bzatri_sprites
 INCBIN  "bzatri sprites.bin"

.atari_mathbox
 LDA #&EA                               ;read second processor presence
 LDX #&00
 LDY #&FF
 STX mathbox_flag                       ;mathbox absent/mathbox speed
 JSR osbyte
 TXA                                    ;&00 = off, &ff = on
 BEQ tube_status_off                    ;tube enabled
 TSX                                    ;store stack for return
 STX atari_stack_store
 LDX #&FF                               ;flatten stack for second processor initialisation
 TXS
 TXA                                    ;load bzone4, arm mathbox
 LDX #LO(atari_load_bzone_file)
 LDY #HI(atari_load_bzone_file)
 JSR osfile
 LDA eventv                             ;save event vector
 STA atari_event_vector
 LDA eventv + &01
 STA atari_event_vector + &01
 SEI
 LDA #LO(atari_timed_out)               ;hook event vector to interval timer code
 STA eventv
 LDA #HI(atari_timed_out)
 STA eventv + &01
 CLI
 LDA #&04                               ;write interval timer, three seconds duration
 LDX #LO(atari_timer_block)
 LDY #HI(atari_timer_block)
 JSR osword
 LDA #&0E                               ;enable interval timer
 LDX #event_interval_timer
 LDY #&00
 STY host_function_code                 ;clear function code
 JSR osbyte
.atari_claim_tube                       ;claim the tube
 LDA #&C0 + atari_claim_id
 JSR tube_code_entry_point
 BCC atari_claim_tube
 LDX #LO(atari_transfer_block)          ;execute arm code in parasite
 LDY #HI(atari_transfer_block)
 LDA #tube_reason_code_04
 JMP tube_code_entry_point              ;implied tube release, does not return here so jump
.tube_status_off
 RTS

.atari_transfer_block
 EQUD mathbox_execute_address

.atari_timed_out
 PHP
 CMP #event_interval_timer
 BNE atari_not_timer                    ;not interval timer event
.atari_disable_event
 LDA #&0D                               ;disable interval timer
 LDX #event_interval_timer
 LDY #&00
 JSR osbyte
 LDA atari_event_vector                 ;restore vector
 STA eventv
 LDA atari_event_vector + &01
 STA eventv + &01
 LDX atari_stack_store                  ;restore temporary stack (ignore push)
 TXS
 CLI
 RTS                                    ;return from mathbox setup
.atari_not_timer
 PLP
 JMP (atari_event_vector)

.atari_load_bzone_file
 EQUW atari_bzone_name
 EQUD &00
 EQUD &FF
 EQUD &00
 EQUD &00
.atari_bzone_name
 EQUS "bzone4", &0D

.atari_timer_block                      ;wait three seconds
 EQUB -300
 EQUD -1

.bzone0_end
 SAVE "bzone0", bzone0, &3000, bzone0 + host_addr, bzone0 + host_addr

 ORG   &9000
 CLEAR &9000, &EFFF
 GUARD &F000

.bzone4                                 ;import file to save it to disk
 INCBIN "C:\Program Files (x86)\RPCEmu\hostfs\Mathbox\bzarm,11e28-11e28"

.bzone4_end
 SAVE "bzone4", bzone4, bzone4_end, bzone4, bzone4

 ORG   &0400
 CLEAR &0400, &07FF
 GUARD &0800

.bzone5
 INCLUDE "page four.asm"                ;variable declarations/code for pages &03-&07

.bzone5_end
 SAVE "bzone5", bzone5, &07FF, bzone5 + host_addr, bzone5_loaded_at_host

 PUTTEXT "credits.txt", "credits", &0000, &0000
 PUTTEXT "notes.txt",   "notes",   &0000, &0000
 PUTTEXT "readme.txt",  "readme",  &0000, &0000

 bz_limit     = &0B00
 bzone0_limit = &3000
 bzone1_limit = &0D00
 bzone2_limit = &C000
 bzone3_limit = &3000
 bzone4_limit = &D000
 bzone5_limit = &0800

 total_free_space = (bzone1_limit - bzone1_end) + (bzone2_limit - bzone2_end) + (bzone3_limit - bzone3_end) + (bzone5_limit - bzone5_end)

 PRINT "          >      <      |      ><"
 PRINT " bz     :", ~bz    , " ", ~bz_end    , " ", ~bz_limit
 PRINT " bzone0 :", ~bzone0, "" , ~bzone0_end, "" , ~bzone0_limit
 PRINT " bzone1 :", ~bzone1, " ", ~bzone1_end, " ", ~bzone1_limit, " " , bzone1_limit - bzone1_end
 PRINT " bzone2 :", ~bzone2, "" , ~bzone2_end, "" , ~bzone2_limit, ""  , bzone2_limit - bzone2_end
 PRINT " bzone3 :", ~bzone3, " ", ~bzone3_end, "" , ~bzone3_limit, ""  , bzone3_limit - bzone3_end
 PRINT " bzone4 :", ~bzone4, "" , ~bzone4_end, "" , ~bzone4_limit, ""
 PRINT " bzone5 :", ~bzone5, " ", ~bzone5_end, " ", ~bzone5_limit, " " , bzone5_limit - bzone5_end
 PRINT "                              ",  total_free_space
 PRINT "            >     <"
 PRINT " scores   :",  ~high_scores_save_start, ~high_scores_save_end
 PRINT " settings :",  ~service_block_start,    ~service_block_end
