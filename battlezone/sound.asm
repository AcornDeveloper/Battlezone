; sound
; maintain sound channels to provide full use for multiple effects, original sounds
; have been re-created, as close as possible, by using the data from the roms
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
; from andy mcfadden's site https://6502disassembly.com/ ...
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; pokey audio has four channels, with two 8-bit i/o locations per channel (audfn
; and audcn) the sound effects defined by this data are played on channels 1
; and 2
;
; the audfn setting determines frequency, larger value == lower pitch
;
; the audcn value is nnnfvvvv, where
; n is a noise / distortion setting
; f is "forced volume-only output" enable
; v is the volume level
;
; the sound specified by the value is played until the duration reaches zero
; if the repetition count is non-zero, the value is increased or decreased by the
; increment, and the duration is reset
; when the repetition count reaches zero the next chunk is loaded
; if the chunk has the value $00, the sequence ends the counters are updated by the 250hz nmi
;
; because audfn and audcn are specified by different chunks, care must be taken
; to ensure the durations run out at the same time
;
; initiates a sound effect on audio channel 1 and/or 2
;
; &01: channel 1     : radar ping
; &02: channel 1     : collided with object
; &04: channel 2     : quiet "merp"
; &08: channel 2     : extra life notification (4 high-pitched beeps)
; &10: channel 2     : new enemy alert (three boops)
; &20: channel 1     : saucer hit (played in a loop while saucer fades out)
; &40: channel 1     : saucer sound (played in a loop while saucer alive)
; &80: channel 1 & 2 : nine notes from 1812 overture
;
; in the sound table below, each chunk has 4 values
;  +00 initial value
;  +01 duration
;  +03 increment
;  +04 repetition count
;
; >> sound table, reproduced below <<
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; sfx_audio_data EQUB &00
; .bulk   $40,$02,$ff,$18   ;sound $10 F2 (three-boop new enemy alert)
; .bulk   $40,$02,$ff,$18   ;($02 * $18) * $03 = $90
; .bulk   $40,$02,$ff,$18
; .bulk   $00,$00
; .bulk   $a3,$30,$00,$03   ;sound $10 C2 ($30 * $03 = $90)
; .bulk   $00,$00
; .bulk   $23,$10,$00,$01   ;sound $01 F2 (radar ping)
; .bulk   $00,$00
; .bulk   $a3,$10,$00,$01   ;sound $01 C2
; .bulk   $00,$00
; .bulk   $10,$01,$00,$20   ;sound $04 F1 (quiet "merp")
; .bulk   $00,$00
; .bulk   $c1,$10,$ff,$02   ;sound $04 C1
; .bulk   $00,$00
; .bulk   $c0,$01,$f6,$06   ;sound $02 F1 (vehicular collision)
; .bulk   $84,$01,$09,$0c
; .bulk   $f0,$01,$f8,$0c
; .bulk   $90,$01,$07,$0c
; .bulk   $e4,$01,$fa,$0c
; .bulk   $9c,$01,$05,$0c
; .bulk   $d8,$01,$fc,$0c
; .bulk   $a8,$01,$03,$0c
; .bulk   $cc,$01,$fe,$0c
; .bulk   $b4,$01,$01,$0c
; .bulk   $00,$00
; .bulk   $ab,$04,$ff,$09   ;sound $02 C1
; .bulk   $a2,$27,$ff,$02
; .bulk   $00,$00
; .bulk   $10,$70,$00,$02   ;sound $08 F2 (four-beep extra life)
; .bulk   $00,$00
; .bulk   $a2,$20,$00,$01   ;sound $08 C2
; .bulk   $a0,$20,$00,$01
; .bulk   $a2,$20,$00,$01
; .bulk   $a0,$20,$00,$01
; .bulk   $a2,$20,$00,$01
; .bulk   $a0,$20,$00,$01
; .bulk   $a2,$20,$00,$01
; .bulk   $00,$00
; .bulk   $30,$01,$fc,$0c   ;sound $20 F1 (saucer hit)
; .bulk   $30,$01,$fc,$0c
; .bulk   $00,$00
; .bulk   $a3,$02,$00,$0c   ;sound $20 C2
; .bulk   $00,$00
; .bulk   $40,$01,$fe,$10   ;sound $40 F1 (saucer alive)
; .bulk   $20,$01,$02,$10
; .bulk   $40,$01,$fe,$10
; .bulk   $20,$01,$02,$10
; .bulk   $00,$00
; .bulk   $a1,$10,$00,$04   ;sound $40 C1
; .bulk   $00,$00
; .bulk   $d9,$30,$00,$01   ;sound $80 F1 (1812 Overture)
; .bulk   $a2,$30,$00,$01
; .bulk   $90,$30,$00,$01
; .bulk   $80,$30,$00,$01
; .bulk   $90,$30,$00,$01
; .bulk   $a2,$30,$00,$01
; .bulk   $90,$30,$00,$01
; .bulk   $80,$30,$00,$02
; .bulk   $a2,$30,$00,$04
; .bulk   $00,$00
; .bulk   $a7,$30,$00,$0d   ;sound $80 C1/C2 (13 steps, same as F1/F2)
; .bulk   $00,$00
; .bulk   $6c,$30,$00,$01   ;sound $80 F2
; .bulk   $51,$30,$00,$01
; .bulk   $48,$30,$00,$01
; .bulk   $40,$30,$00,$01
; .bulk   $48,$30,$00,$01
; .bulk   $51,$30,$00,$01
; .bulk   $48,$30,$00,$01
; .bulk   $40,$30,$00,$02
; .bulk   $51,$30,$00,$04
; .bulk   $00,$00
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; bbc sound note/value table
; name 	octave
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
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; atari sound note/value table
; name  octave
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
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; constants
; bit 7   = use envelope else sound amplitude
; bit 2-6 = envelope number 1-16/amplitude 0-15 (negated in program)
; bit 0-1 = preferred channel number 0-3
;                                         envelope/sound flag
; format                                            envelope number/sound amplitude
;                                                              sound channel to use
  bit_flags_enemy_alert                 = %0 << 7 + &01 << 2 + &03 ;enemy alert       ;
  bit_flags_enemy_radar                 = %1 << 7 + &02 << 2 + &02 ;enemy radar       ;*
  bit_flags_missile_hum                 = %0 << 7 + &03 << 2 + &00 ;missile hum       ;
  bit_flags_bump                        = %0 << 7 + &04 << 2 + &02 ;bump              ;*
  bit_flags_engine_rev_up               = %0 << 7 + &05 << 2 + &00 ;engine rev up     ;
  bit_flags_engine_rev_down             = %0 << 7 + &06 << 2 + &00 ;engine rev down   ;
  bit_flags_tank_shot_soft              = %0 << 7 + &07 << 2 + &00 ;tank shot soft    ;
  bit_flags_tank_shot_loud              = %0 << 7 + &08 << 2 + &00 ;tank shot loud    ;*
  bit_flags_explosion_soft              = %0 << 7 + &09 << 2 + &00 ;explosion soft    ;
  bit_flags_explosion_loud              = %0 << 7 + &0A << 2 + &00 ;explosion loud    ;
  bit_flags_motion_blocked              = %0 << 7 + &0B << 2 + &00 ;motion blocked    ;
  bit_flags_saucer_in_motion            = %0 << 7 + &0C << 2 + &02 ;saucer in motion  ;
  bit_flags_saucer_shot                 = %0 << 7 + &0D << 2 + &02 ;saucer shot       ;
  bit_flags_extra_life                  = %0 << 7 + &0E << 2 + &03 ;extra life        ;
  bit_flags_key_click                   = %0 << 7 + &0F << 2 + &01 ;key click         ;
;
; to create a sound:-
;
; increment flag i.e. inc sound_enemy_alert
; for sound effects such as explosions then two incs are required, one for channel 0, one for channel 1
; all sounds played immediately, flush sound channel used
;

.sound_flag_block

.sound_enemy_alert                      EQUB &00                 ;initialise to 0, increment to activate
.sound_enemy_radar                      EQUB &00
.sound_missile_hum                      EQUB &00
.sound_bump                             EQUB &00
.sound_engine_rev_up                    EQUB &00
.sound_engine_rev_down                  EQUB &00
.sound_tank_shot_soft                   EQUB &00
.sound_tank_shot_loud                   EQUB &00
.sound_explosion_soft                   EQUB &00
.sound_explosion_loud                   EQUB &00
.sound_motion_blocked                   EQUB &00
.sound_saucer_in_motion                 EQUB &00
.sound_saucer_shot                      EQUB &00
.sound_extra_life                       EQUB &00
.sound_key_click                        EQUB &00

.sound_control_end

.sound_control_type                     ;bit flags for each sound required
 EQUB bit_flags_enemy_alert
 EQUB bit_flags_enemy_radar
 EQUB bit_flags_missile_hum
 EQUB bit_flags_bump
 EQUB bit_flags_engine_rev_up
 EQUB bit_flags_engine_rev_down
 EQUB bit_flags_tank_shot_soft
 EQUB bit_flags_tank_shot_loud
 EQUB bit_flags_explosion_soft
 EQUB bit_flags_explosion_loud
 EQUB bit_flags_motion_blocked
 EQUB bit_flags_saucer_in_motion
 EQUB bit_flags_saucer_shot
 EQUB bit_flags_extra_life
 EQUB bit_flags_key_click

.sound_channel_flip                     ;sound effects flip channel
 EQUB &02                               ;arcade not used so can cut sounds off mid-stream
                                        ;not perfect but a step up
.sound_flush_buffers                    ;clear all sounds and reset sound flags
 LDX #&04
.flush_all_sounds
 STX workspace
 LDA #21
 JSR osbyte
 LDX workspace
 INX
 CPX #&08
 BNE flush_all_sounds
.sound_clear_flags
 LDX #sound_control_end - sound_flag_block - &01
 LDA #&00                               ;clear sound control block
.clear_sound_control
 STA sound_flag_block,X                 ;clear flags
 DEX
 BPL clear_sound_control
 RTS

.sound_when_dead                        ;call when my tank destroyed
 JSR sound_flush_buffers                ;flush all sounds/clear flags
 INC sound_explosion_loud
 SEC                                    ;prevent any other sounds from being made
 ROR sound_mute
 BNE sound_control_bypass               ;always

.sound_control                          ;scan sound flags and action any requests
 LDA sound_mute                         ;if muted sound and music playing then exit to clear block
 ORA sound_music
 BMI sound_clear_flags                  ;clear sound flags/counters
.sound_control_bypass
 LDX #sound_control_end - sound_flag_block - &01
.sound_control_loop
 LDA sound_flag_block,X                 ;x = index into sound
 BEQ sound_next                         ;no sound required
 STX workspace                          ;save index
 LDA #&00                               ;clear sound flag
 STA sound_flag_block,X
 LDY sound_control_type,X
 TYA
 AND #&02
 BEQ sound_bypass_flip                  ;bypass as 0 or 1
 LDA sound_channel_flip                 ;flip channel 2/3 for effect
 EOR #&01
 STA sound_channel_flip
 BNE sound_add_flush_control            ;always
.sound_bypass_flip
 TYA
 AND #&01                               ;channel 0/1
.sound_add_flush_control
 ORA #&10                               ;add flush control to channel
 STA sound_envelope_channel             ;store channel to use
 TYA                                    ;reload sound bit map
 LSR A
 LSR A
 AND #&1F                               ;get envelope/amplitude
 CPY #&00                               ;test bit 7
 BMI sound_use_envelope
 EOR #&FF                               ;negate to get volume
 CLC
 ADC #&01
.sound_use_envelope
 STA sound_envelope_adsr                ;store envelope number/amplitude
 LDA sound_control_type,X
 LDA sound_data_pitch,X                 ;pitch
 STA sound_envelope_pitch
 LDA sound_data_duration,X              ;duration
 STA sound_envelope_duration
 JSR sound_this_envelope
 LDX workspace
.sound_next
 DEX
 BPL sound_control_loop
 RTS

.sound_music                            ;bit 7 = 1 music = 0 off
 EQUB &00
.sound_mute
 EQUB &00
.sound_note_counter                     ;sound note counter 8 to 0
 EQUB sound_block_end - sound_block_start - &01

; atari sound duration resolution is 1/250 or 0.004 seconds
; bbc   sound duration resolution is 1/20  or 0.050 seconds on non-envelope sounds
;                                    1/100 or 0.010 seconds on envelope sounds

.sound_data_pitch
 EQUB &00                               ;#01 enemy alert
 EQUB &89                               ;#02 enemy radar
 EQUB &00                               ;#03 missile hum
 EQUB &00                               ;#04 bump
 EQUB &00                               ;#05 engine rev up
 EQUB &00                               ;#06 engine rev down
 EQUB &00                               ;#07 tank shot soft
 EQUB &06                               ;#08 tank shot loud
 EQUB &00                               ;#09 explosion soft
 EQUB &00                               ;#0A explosion loud
 EQUB &50                               ;#0B motion blocked
 EQUB &00                               ;#0C saucer in motion
 EQUB &A0                               ;#0D saucer shot
 EQUB &00                               ;#0E extra life
 EQUB &A0                               ;#0F key click

.sound_data_duration
 EQUB &00                               ;#01 enemy alert
 EQUB &14                               ;#02 enemy radar
 EQUB &00                               ;#03 missile hum
 EQUB &10                               ;#04 bump
 EQUB &00                               ;#05 engine rev up
 EQUB &00                               ;#06 engine rev down
 EQUB &00                               ;#07 tank shot soft
 EQUB &0C                               ;#08 tank shot loud
 EQUB &00                               ;#09 explosion soft
 EQUB &00                               ;#0A explosion loud
 EQUB &14                               ;#0B motion blocked
 EQUB &00                               ;#0C saucer in motion
 EQUB &14                               ;#0D saucer shot
 EQUB &00                               ;#0E extra life
 EQUB &14                               ;#0F key click

.sound_tchaikovsky                      ;play tchaikovsky's 1812 overture e flat major
 BIT sound_music
 BPL sound_tchaikovsky_leave            ;no music playing
 LDA #&80                               ;read sound buffer status
 LDX #&FA                               ;check channel 1 and 2 buffer status
.sound_check
 STX workspace
 JSR osbyte
 CPX #&04
 BCC sound_tchaikovsky_leave            ;buffer full
 LDX workspace
 DEX
 CPX #&F8
 BNE sound_check                        ;check both buffers
 LDX sound_note_counter
 BMI sound_music_finished               ;all notes played, turn off music
 DEC sound_note_counter                 ;onto next note
 LDA #&02
 STA sound_envelope_adsr
 LDA #&01                               ;channel 1
 STA sound_envelope_channel
 LDA sound_music_pitch,X
 STA sound_envelope_pitch
 LDA sound_music_duration,X
 STA sound_envelope_duration
.sound_this_envelope
 LDX #LO(sound_envelope_channel)
 LDY #HI(sound_envelope_channel)
 LDA #&07
 JMP osword
.sound_music_finished                   ;reset counter for next use
 LDA #sound_block_end - sound_block_start - &01
 STA sound_note_counter
 ASL sound_music                        ;clear bit 7, music now finished, started as &80
 ROR sound_explosion_loud               ;rotate carry in, music over so loud explosion sound required
.sound_tchaikovsky_leave
 RTS

.sound_envelope_channel
 EQUW &00
.sound_envelope_adsr
 EQUW &00
.sound_envelope_pitch
 EQUW &00
.sound_envelope_duration
 EQUW &00

.sound_block_start

.sound_music_pitch                      ;notes in reverse order
 EQUB 129                               ;pokey  G - &51 217  bbc 129
 EQUB 145                               ;       B - &40 162      145
 EQUB 137                               ;       A - &48 144      137
 EQUB 129                               ;       G - &51 128      129
 EQUB 137                               ;       A - &48 144      137
 EQUB 145                               ;       B - &40 162      145
 EQUB 137                               ;       A - &48 144      137
 EQUB 129                               ;       G - &51 128      129
 EQUB 109                               ;       D - &6C 162      109

.sound_block_end

.sound_music_duration
 EQUB &0C
 EQUB &08
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04
 EQUB &04

.sound_mute_mode                        ;sound control per mode
 LDX game_mode
 LDA sound_status,X
 STA sound_mute                         ;music and note counter set up
 CLC
 ROR sound_music
 LDA #sound_block_end - sound_block_start - &01
 STA sound_note_counter
 RTS

.sound_status                           ;bit 7 = 1 sound muted
 EQUB &00                               ;mode 00 main game
 EQUB &80                               ;mode 01 attract mode
 EQUB &80                               ;mode 02 high score table
 EQUB &00                               ;mode 03 service menu
 EQUB &00                               ;mode 04 new high score
 EQUB &80                               ;mode 05 battlezone text
 EQUB &80                               ;mode 06 model test
