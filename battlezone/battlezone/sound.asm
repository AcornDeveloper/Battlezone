; sound
; maintain sound channels to provide full use for multiple effects, original sounds
; have been re-created, as close as possible, by using the data from the roms
;
; n, t, pi1, p12, pi3, pn1, pn2, pn3, aa, ad, as, ar, ala, ald
;
; n is the envelope number
; t is the length of each step in 1/100ths of a second
; pi1, pi2 and pi3 are the changes in pitch per step during sections 1, 2 and 3
; pnl, pn2 and pn3 are the number of steps in sections 1, 2 and 3 respectively
; aa is the rate of change of amplitude during the attack phase
; ad is the rate of change of amplitude during the decay phase
; as is the rate of change of amplitude during the sustain phase
; ar is the rate of change of amplitude during the release phase
; ala is the target amplitude for the attack phase
; ald is the target amplitude for the decay phase
;
; bbc noise channel
; p = 0 periodic noise high pitch
; p = 1 periodic noise medium pitch
; p = 2 periodic noise low pitch
; p = 3 periodic noise related to channel 1 pitch
; p = 4 white noise high pitch
; p = 5 white noise medium pitch
; p = 6 white noise low pitch
; p = 7 white noise related to channel 1 pitch
;
.sound_flag_block                       ;increment/place bit pattern in to activate

.sound_enemy_alert                      EQUB &00
.sound_enemy_radar                      EQUB &00
.sound_bump                             EQUB &00
.sound_tank_shot_soft                   EQUB &00
.sound_tank_shot_loud                   EQUB &00
.sound_explosion_soft                   EQUB &00
.sound_explosion_loud                   EQUB &00
.sound_motion_blocked                   EQUB &00
.sound_saucer_in_view                   EQUB &00
.sound_saucer_shot                      EQUB &00
.sound_extra_life                       EQUB &00
.sound_key_click                        EQUB &00
.sound_missile                          EQUB &00
.sound_engine                           EQUB &00

.sound_flag_block_end

.sound_channel
 EQUB &13                               ;01 enemy alert
 EQUB &03                               ;02 enemy radar
 EQUB &12                               ;03 bump
 EQUB &10                               ;04 tank shot soft
 EQUB &10                               ;05 tank shot loud
 EQUB &10                               ;06 explosion soft
 EQUB &10                               ;07 explosion loud
 EQUB &10                               ;08 motion blocked
 EQUB &12                               ;09 saucer in view
 EQUB &12                               ;0A saucer shot
 EQUB &03                               ;0B extra life
 EQUB &03                               ;0C key click
 EQUB &10                               ;0D missile
 EQUB &11                               ;0E engine

.sound_amplitude_envelope
 EQUB &01                               ;01 enemy alert
 EQUB &02                               ;02 enemy radar
 EQUB &03                               ;03 bump
 EQUB &04                               ;04 tank shot soft
 EQUB &05                               ;05 tank shot loud
 EQUB &06                               ;06 explosion soft
 EQUB &07                               ;07 explosion loud
 EQUB &F6                               ;08 motion blocked
 EQUB &08                               ;09 saucer in view
 EQUB &09                               ;0A saucer shot
 EQUB &F8                               ;0B extra life
 EQUB &F8                               ;0C key click
.sound_missile_amplitude
 EQUB &F0                               ;0D missile
 EQUB &0A                               ;0E engine

.sound_data_pitch
 EQUB &75                               ;01 enemy alert
 EQUB &89                               ;02 enemy radar
 EQUB &00                               ;03 bump
 EQUB &06                               ;04 tank shot soft
 EQUB &06                               ;05 tank shot loud
 EQUB &06                               ;06 explosion soft
 EQUB &06                               ;07 explosion loud
 EQUB &01                               ;08 motion blocked
 EQUB &A0                               ;09 saucer in view
 EQUB &A0                               ;0A saucer shot
 EQUB &BD                               ;0B extra life
 EQUB &A0                               ;0C key click
 EQUB &02                               ;0D missile
.sound_engine_pitch
 EQUB &00                               ;0E engine

.sound_data_duration
 EQUB &0C                               ;01 enemy alert
 EQUB &04                               ;02 enemy radar
 EQUB &10                               ;03 bump
 EQUB &0C                               ;04 tank shot soft
 EQUB &0C                               ;05 tank shot loud
 EQUB &10                               ;06 explosion soft
 EQUB &10                               ;07 explosion loud
 EQUB &01                               ;08 motion blocked
 EQUB &10                               ;09 saucer in view
 EQUB &14                               ;0A saucer shot
 EQUB &03                               ;0B extra life
 EQUB &04                               ;0C key click
 EQUB &10                               ;0D missile
 EQUB &10                               ;0E engine

.sound_flush_buffers                    ;flush all sounds and reset sound flags
 LDX #&04                               ;flush sound buffers 4 to 7
.flush_all_sounds
 STX general_x
 LDA #21
 JSR osbyte
 LDX general_x
 INX
 CPX #&08
 BNE flush_all_sounds
 LDX #sound_flag_block_end - sound_flag_block - &01
 LDA #&00                               ;clear sound control block
.clear_sound_control
 STA sound_flag_block,X                 ;clear all sound flags
 DEX
 BPL clear_sound_control
 RTS

.sound_continuous                       ;check for those continous sounds engine/missile/saucer
 BIT sound_gate                         ;if muted sound then exit, = &ff
 BMI sound_leave                        ;sound off then leave engine/saucer generation/missile
 BIT m_tank_status                      ;player alive?
 BMI sound_leave                        ;no, so leave all other sounds alone
 BIT saucer_state                       ;saucer in view? only makes sound when in view
 BPL sound_saucer_absent                ;no
 LDA sound_saucer_in_view               ;sound already playing?
 BNE sound_saucer_absent                ;yes, so leave
 LDA saucer_dying                       ;saucer been shot?
 BNE sound_saucer_absent                ;yes, so leave
 LDA #%00000011                         ;set up saucer in view sound
 STA sound_saucer_in_view
.sound_saucer_absent
 BIT machine_flag                       ;no engine sound for electron as only one channel to play with
 BMI sound_no_engine
 INC sound_engine                       ;make the sound
.sound_no_engine
 LDA sound_engine_pitch                 ;get current engine pitch and clip
 CMP #&10
 BCC sound_missile_sound_control
 DEC sound_engine_pitch                 ;pull it back down
.sound_missile_sound_control            ;missile active
 LDA tank_or_super_or_missile           ;branch if missile active as this requires priority over engine sound
 CMP #object_x07_missile                ;(do not use missile_flag as it's preserved until new unit created)
 BNE sound_leave                        ;missile not in play
 LDA enemy_dist_hi                      ;missile distance
 LSR A
 LSR A
 ORA #&F0
 STA sound_missile_amplitude
 INC sound_missile
.sound_leave
 RTS

.sound_when_dead                        ;call when my tank destroyed
 JSR sound_flush_buffers                ;flush all sounds/clear flags
 DEC m_tank_status                      ;kill player
 INC enemy_score                        ;enemy score +1
 INC sound_explosion_loud               ;make large explosion sound
 LDA #console_double                    ;clear space above
 STA console_clear_space
 LDA #&07
 STA b_object_bounce_near               ;object bounce
 INC b_object_bounce_far                ;horizon movement, fall into sound routine below

.sound_control                          ;scan sound flags and action any requests
 LDX #sound_flag_block_end - sound_flag_block - &01
.sound_control_loop
 LSR sound_flag_block,X                 ;this will clear the sound bit, except for repeating sounds
 BCC sound_next                         ;probably zero but don't make sound yet as c=0, dependant on bit pattern loaded
 BIT sound_gate                         ;if muted sound then exit, = &ff
 BMI sound_next                         ;tested here because we still want to clear down the sound flags
 STX general_x                          ;in non-game play modes
 LDA sound_channel,X                    ;channel to use
 STA sound_envelope_channel
 LDA sound_amplitude_envelope,X         ;amplitude/envelope
 STA sound_envelope_adsr
 LDA sound_data_pitch,X                 ;pitch
 STA sound_envelope_pitch
 LDA sound_data_duration,X              ;duration
 STA sound_envelope_duration
 LDX #LO(sound_osword)
 LDY #HI(sound_osword)
 LDA #&07
 JSR osword
 LDX general_x
.sound_next
 DEX
 BPL sound_control_loop
 RTS

.sound_osword                           ;parameter block for general sounds

.sound_envelope_channel
 EQUW &00
.sound_envelope_adsr
 EQUW &00
.sound_envelope_pitch
 EQUW &00
.sound_envelope_duration
 EQUW &00

.sound_music                            ;bit 7 = 1 music on,  = 0 music off
 EQUB &00
.sound_gate                             ;bit 7 = 1 sound off, = 0 sound on
 EQUB &00
.sound_music_index                      ;sound note counter 0 to 8
 EQUB &00

.sound_tchaikovsky                      ;play tchaikovsky's 1812 overture e flat major
 BIT sound_music                        ;&FB (-&05) sound output buffer 0
 BPL sound_tchaikovsky_leave            ;&FA (-&06) sound output buffer 1
.sound_read_buffer                      ;&F9 (-&07) sound output buffer 2
 LDA #&80                               ;&F8 (-&08) sound output buffer 3
 LDX #LO(-&07)                          ;read sound buffer channel 2, 2&3 synchronised
 JSR osbyte
 CPX #&04                               ;each sound 3 bytes in buffer, buffer size 16 bytes, 5 requests total
 BCC sound_tchaikovsky_leave            ;buffer full, come back next time
 LDY sound_music_index
 LDA sound_music_channel_02_notes,Y
 BEQ sound_music_finished               ;all notes played, turn off music
 STA sound_music_channel_02_pitch
 LDA sound_music_channel_03_notes,Y
 STA sound_music_channel_03_pitch
 LDA sound_music_length,Y               ;sound duration
 STA sound_music_channel_02_duration
 STA sound_music_channel_03_duration
 LDX #LO(sound_music_channel_02_osword) ;add notes to play
 LDY #HI(sound_music_channel_02_osword)
 LDA #&07
 JSR osword
 BIT machine_flag
 BMI sound_no_music
 LDX #LO(sound_music_channel_03_osword)
 LDY #HI(sound_music_channel_03_osword)
 LDA #&07
 JSR osword
.sound_no_music
 INC sound_music_index                  ;onto next note
 BNE sound_read_buffer                  ;always, check to see if more space in buffer/finished

.sound_music_finished                   ;reset counter etc for next use, a=&00 on entry
 LDY clock_mode                         ;get music timer
 LDX game_mode                          ;which mode is game in?
 BNE sound_check_new_high_score
 CPY #timer_music_in_game - &02         ;in game music
 BCS sound_tchaikovsky_leave
 BCC sound_make_explosion               ;always
.sound_check_new_high_score             ;music in new high score entry screen
 CPY #timer_new_high_score - &02
 BCS sound_tchaikovsky_leave
.sound_make_explosion
 STA sound_music_index                  ;reset music for next time
 STA sound_music                        ;turn music off
 STA sound_gate                         ;turn sound on
 INC sound_explosion_loud               ;music over so loud explosion sound required
.sound_tchaikovsky_leave
 RTS

.sound_music_channel_02_osword          ;parameter block for channel 02

.sound_music_channel_02_channel         ;sound &hsfn, h=hold current sound, s=synchronise, f=flush, n=channel
 EQUW &02
.sound_music_channel_02_adsr            ;use envelope 11
 EQUW &0B
.sound_music_channel_02_pitch
 EQUW &00
.sound_music_channel_02_duration
 EQUW &00

.sound_music_channel_03_osword          ;parameter block for channel 03

.sound_music_channel_03_channel
 EQUW &03
.sound_music_channel_03_adsr
 EQUW &0B
.sound_music_channel_03_pitch
 EQUW &00
.sound_music_channel_03_duration
 EQUW &00

; bbc sound note/value table
; note 	octave
;       1    2    3    4    5    6    7
; b 	1 	 49   97   145  193  241
; a# 	0 	 45   93   141  189  237
; a 	  	 41   89   137  185  233
; g# 	  	 37   85   133  181  229
; g 	  	 33   81   129  177  225
; f# 	  	 29   77   125  173  221
; f 	  	 25   73   121  169  217
; e 	  	 21   69   117  165  213
; d# 	  	 17   65   113  161  209
; d 	  	 13   61   109  157  205  253
; c# 	  	  9   57   105  153  201  249
; c 	  	  5   53   101  149  197  245
;
; atari sound note/value table
; note            octave
;                 3    4    5    6    7
; c               &F3  &79  &3C  &1E  &0E
; c#              &E6  &72  &39  &1C
; d               &D9  &6C  &35  &1A
; d#              &CC  &66  &32  &19
; e               &C1  &60  &2F  &17
; f               &B6  &5B  &2D  &16
; f#              &AC  &55  &2A  &15
; g               &A2  &51  &28  &13
; g#              &99  &4C  &25  &12
; a               &90  &48  &23  &11
; a#              &88  &44  &21  &10
; b               &80  &40  &1F  &0F

.sound_music_channel_02_notes
 EQUB 061                               ;pokey D - &D9  bbc 061
 EQUB 081                               ;      G - &A2      081
 EQUB 089                               ;      A - &90      089
 EQUB 097                               ;      B - &80      097
 EQUB 089                               ;      A - &90      089
 EQUB 081                               ;      G - &A2      081
 EQUB 089                               ;      A - &90      089
 EQUB 097                               ;      B - &80      097
 EQUB 081                               ;      G - &A2      081
 EQUB &00                               ;end music flag

.sound_music_channel_03_notes
 EQUB 109                               ;pokey D - &6C  bbc 109
 EQUB 129                               ;      G - &51      129
 EQUB 137                               ;      A - &48      137
 EQUB 145                               ;      B - &40      145
 EQUB 137                               ;      A - &48      137
 EQUB 129                               ;      G - &51      129
 EQUB 137                               ;      A - &48      137
 EQUB 145                               ;      B - &40      145
 EQUB 129                               ;      G - &51      129

.sound_music_length
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &08
 EQUB &10

.sound_gate_mode                        ;sound/music control per mode/set music counter
 LDX game_mode
 LDA #%01110110                         ;sound bit mask, bit 7 = 0/1 sound on/sound off
;             + mode 00 main game
;            +- mode 01 attract mode
;           +-- mode 02 high score table
;          +--- mode 03 service menu
;         +---- mode 04 new high score
;        +----- mode 05 battlezone text
;       +------ mode 06 model test
;      +------- unused
.sound_get_flag                         ;shift right into carry
 LSR A
 DEX
 BPL sound_get_flag                     ;on exit carry has sound status for mode
 ROR sound_gate                         ;put carry in bit 7 of sound flag
 INX                                    ;x=&00
 STX sound_music_index                  ;set music index = &00
 LDA game_mode
 CMP #mode_04_new_high_score            ;entering new high score?
 BEQ sound_gate_mode_exit               ;yes, don't set music off
 STX sound_music                        ;set music off
.sound_gate_mode_exit
 RTS
