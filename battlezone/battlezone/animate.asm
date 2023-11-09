; animate
; object coordinates
; object positions represented by signed 16 bits
;
; calculate distance
; fustrum near &03ff, far &7aff
;
; unit debris table
;               acorn                          atari
;               object_x0A_tank_radar          &06, &13, &1D
;               object_x0B_ship_chunk_00       &10, &15, &1A, &1C
;               object_x0C_ship_chunk_01       &11, &14, &18
;               object_x0D_ship_chunk_02       &12
;               object_x0E_ship_chunk_04       &19, &1D
;               object_x0F_ship_chunk_05       &1B
;
; standard tank &0B, &0C, &0D, &0A, &0C, &0B   &10 - &15
; super tank    &0B, &0C, &0D, &0C, &0C, &0B   &10 - &12, &14 x 2, &15
; missile       &0C, &0E, &0B, &0F, &0B, &0E   &18 - &1D
;
; constants
 general_bounding_box                   = &800
 debris_gravity                         = &28
 radar_x_adjust                         = &400 * bz_xz_scale_factor

 radius_my_tank                         = &480
 radius_tank                            = &780
 radius_missile                         = &380
 radius_saucer                          = &480

 unit_diameter                          = &480
 player_diameter                        = &480

 missile_shot_ceiling                   = &200
 missile_fly_low_altitude               = &200
 missile_fly_high_altitude              = -&1000
 saucer_dying_counter                   = &20 / &02

 diameter_narrow_pyramid                = &28
 diameter_tall_cube                     = &58
 diameter_short_cube                    = &00
 diameter_wide_pyramid                  = &56

 radius_narrow_pyramid                  = &340
 radius_tall_cube                       = &340
 radius_short_cube                      = &3C0
 radius_wide_pyramid                    = &400

 player_time_to_live                    = &06
 unit_time_to_live                      = &04

.debris_x_velocity                      ;pre-determined x/z debris velocities
 EQUW -120
 EQUW -120
 EQUW   20
 EQUW  200
 EQUW    0
 EQUW -160

.debris_y_velocity
 EQUW &00
 EQUW &00
 EQUW &00
 EQUW &00
 EQUW &00
 EQUW &00

.debris_z_velocity
 EQUW  120
 EQUW    0
 EQUW  -20
 EQUW  200
 EQUW -160
 EQUW -160

.debris_y_velocity_initial              ;single byte x4 calculated in arcade game, double byte and pre-shifted here
 EQUW &37 << &02
 EQUW &28 << &02
 EQUW &46 << &02
 EQUW &58 << &02
 EQUW &28 << &02
 EQUW &42 << &02

.debris_parts
.debris_tank                            ;x3 sets of debris
 EQUB object_x0B_ship_chunk_00
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0D_ship_chunk_02
 EQUB object_x0A_tank_radar
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0B_ship_chunk_00
.debris_super_tank
 EQUB object_x0B_ship_chunk_00
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0D_ship_chunk_02
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0B_ship_chunk_00
.debris_missile
 EQUB object_x0C_ship_chunk_01
 EQUB object_x0E_ship_chunk_04
 EQUB object_x0B_ship_chunk_00
 EQUB object_x0F_ship_chunk_05
 EQUB object_x0B_ship_chunk_00
 EQUB object_x0B_ship_chunk_00

.initialise_unit_chunks                 ;initialise debris positions
 LDX #block_size * &06 - block_size
 SEC
.init_unit_chunks_loop
 LDA tank_or_super_or_missile_x         ;initialise debris enemy x/y/z world coordinates
 STA debris_start + &02,X
 LDA tank_or_super_or_missile_x + &01
 STA debris_start + &03,X
 LDA #&00                               ;initial height
 STA debris_start + &04,X
 STA debris_start + &05,X
 LDA tank_or_super_or_missile_z
 STA debris_start + &06,X
 LDA tank_or_super_or_missile_z + &01
 STA debris_start + &07,X
 TXA
 SBC #block_size
 TAX
 BCS init_unit_chunks_loop
 LDY #&0B                               ;initialise y velocities
.initialise_debris_y_velocity
 LDA debris_y_velocity_initial,Y
 STA debris_y_velocity,Y
 DEY
 BPL initialise_debris_y_velocity
 INC sound_explosion_soft               ;unit explosion
 LDX #debris_missile - debris_parts + &05
 BIT missile_flag                       ;missile?
 BMI debris_set                         ;yes
 LDX #debris_tank - debris_parts + &05
 LDA tank_or_super_or_missile
 CMP #object_x04_tank                   ;standard tank?
 BEQ debris_set
 LDX #debris_super_tank - debris_parts + &05
.debris_set                             ;set up debris objects
 LSR missile_flag                       ;clear missile flag, used to suppress missiles at a distance
 LDY #block_size * &06 - block_size
 SEC
.initialise_debris_id
 LDA debris_parts,X                     ;set up debris object ids
 STA debris_start,Y
 DEX
 TYA
 SBC #block_size
 TAY
 BCS initialise_debris_id
 STA tank_or_super_or_missile           ;remove destroyed unit/my shell, a=&f8, top bit set
 STA m_shell
 RTS

.obstacle_radius_lsb
 EQUB LO(radius_narrow_pyramid)         ;narrow pyramid
 EQUB LO(radius_tall_cube)              ;tall cube
 EQUB LO(radius_short_cube)             ;short cube (not used)
 EQUB LO(radius_wide_pyramid)           ;wide pyramid

.obstacle_radius_msb
 EQUB HI(radius_narrow_pyramid)
 EQUB HI(radius_tall_cube)
 EQUB HI(radius_short_cube)
 EQUB HI(radius_wide_pyramid)

.obstacle_diameter
 EQUB diameter_narrow_pyramid           ;narrow pyramid
 EQUB diameter_tall_cube                ;tall cube
 EQUB diameter_short_cube               ;short cube (not used)
 EQUB diameter_wide_pyramid             ;wide pyramid

.object_collision_test                  ;collision test of objects/my tank/unit
 STX general_x                          ;exit c=0/1 miss/collision
 LDY #(object_shot_collision_end - object_start) - block_size
.object_collide_loop
 STY general_y
 LDA object_start + &02,Y               ;calculate delta x distance to object
 SEC
 SBC tank_or_super_or_missile_x,X
 STA distance_dx
 LDA object_start + &03,Y
 SBC tank_or_super_or_missile_x + &01,X
 STA distance_dx + &01
 BPL quick_check_x                      ;quick check for x bounding
 EOR #&FF
 CLC
 ADC #&01
.quick_check_x
 CMP #HI(general_bounding_box)
 BCS no_object_collision
 LDA object_start + &06,Y               ;object z
 SEC
 SBC tank_or_super_or_missile_z,X
 STA distance_dz
 LDA object_start + &07,Y               ;calculate delta z distance to object
 SBC tank_or_super_or_missile_z + &01,X
 STA distance_dz + &01
 BPL quick_check_z                      ;quick check for z bounding
 EOR #&FF
 CLC
 ADC #&01
.quick_check_z
 CMP #HI(general_bounding_box)
 BCS no_object_collision                ;outside bounding box
 JSR distance_16
 LDY general_y                          ;restore x/y
 LDX general_x                          ;enemy or my tank we are checking?
 BEQ enemy_unit                         ;enemy we're looking at
 LDA #LO(radius_my_tank)                ;use my tank's radius
 CMP d_object_distance
 LDA #HI(radius_my_tank)
 BNE subtract_distance                  ;always

.enemy_unit
 LDX object_start,Y                     ;get object type
 BIT missile_flag
 BPL not_the_missile
 CMP #&80                               ;a = d_object_distance + &01
 ROR A
 PHA
 LDA d_object_distance
 ROR A
 CLC
 ADC d_object_distance
 STA d_object_distance
 PLA
 ADC d_object_distance + &01
 STA d_object_distance + &01
 BCC not_the_missile                    ;c=0 not overflowed so test against objects
 LDX general_x                          ;restore x
 BCS no_object_collision                ;always, carry set we are too far away

.not_the_missile
 LDA obstacle_radius_lsb,X
 CMP d_object_distance
 LDA obstacle_radius_msb,X
 LDX general_x                          ;restore x
.subtract_distance
 SBC d_object_distance + &01
 BCS hit_obstacle_return
 SEC
.no_object_collision
 TYA                                    ;next object block
 SBC #block_size
 TAY
 BCS object_collide_loop
 LDA tank_or_super_or_missile           ;geometrical shapes now done so check my tank/unit
 ORA m_tank_status                      ;if either dead then exit
 BMI no_hit_unit_return                 ;no collision as either is off, c=0
 TXA                                    ;flip x to other unit and pass to y
 EOR #block_size
 TAY
 SEC
 LDA tank_or_super_or_missile_x,X       ;calculate delta x distance between units
 SBC tank_or_super_or_missile_x,Y
 STA distance_dx
 LDA tank_or_super_or_missile_x + &01,X
 SBC tank_or_super_or_missile_x + &01,Y
 STA distance_dx + &01
 SEC
 LDA tank_or_super_or_missile_z,X       ;calculate delta z distance between units
 SBC tank_or_super_or_missile_z,Y
 STA distance_dz
 LDA tank_or_super_or_missile_z + &01,X
 SBC tank_or_super_or_missile_z + &01,Y
 STA distance_dz + &01
 JSR distance_16
 LDA #HI(radius_tank)                   ;check tank/missile
 BIT missile_flag
 BPL not_missile_again
 LDA #HI(radius_missile)
.not_missile_again
 CMP d_object_distance + &01            ;c=0/1
.no_hit_unit_return
 LDA #&FF                               ;flag &ff for unit
 RTS
.hit_obstacle_return
 LDA #&00                               ;flag &00 for obstacle
 RTS

.update_exhaust                         ;exhaust objects
 BIT debris_last_chunk_to_hit_ground    ;if debris in flight then no missile exhaust
 BPL update_exhaust_exit                ;in flight
 BIT missile_flag                       ;missile opponent?
 BPL update_exhaust_exit                ;no
 BIT tank_or_super_or_missile           ;active?
 BMI update_exhaust_exit                ;no
 LDA #object_x22_exhaust_03
 SEC
 SBC track_exhaust_index
 STA missile_exhaust
 LDA #&00                               ;clear missile exhaust y
 STA missile_exhaust_y
 STA missile_exhaust_y + &01
 LDA tank_or_super_or_missile_x         ;copy missile x/z to exhaust x/z to keep pace
 STA missile_exhaust_x
 LDA tank_or_super_or_missile_x + &01
 STA missile_exhaust_x + &01
 LDA tank_or_super_or_missile_z
 STA missile_exhaust_z
 LDA tank_or_super_or_missile_z + &01
 STA missile_exhaust_z + &01
.update_exhaust_exit
 RTS

.update_debris                          ;maintain debris
 BIT debris_last_chunk_to_hit_ground
 BMI unit_creation                      ;last chunk inactive so create a unit
 LDX #block_size * &06 - block_size     ;point to last debris entry
.update_debris_loop
 LDA debris_start,X                     ;test if debris present
 BMI debris_next
 TXA                                    ;divide by 4 to get index into x/y/z velocity
 LSR A
 LSR A
 TAY
 LDA debris_start + &04,X               ;update y position
 SBC debris_y_velocity,Y
 STA debris_start + &04,X
 LDA debris_start + &05,X
 SBC debris_y_velocity + &01,Y
 STA debris_start + &05,X
 BMI debris_still_active
 SEC                                    ;remove debris
 ROR debris_start,X
 BMI debris_next                        ;always
.debris_still_active
 LDA debris_start + &02,X               ;update x position
 ADC debris_z_velocity,Y
 STA debris_start + &02,X
 LDA debris_start + &03,X
 ADC debris_z_velocity + &01,Y
 STA debris_start + &03,X
 LDA debris_start + &06,X               ;update z position
 ADC debris_x_velocity,Y
 STA debris_start + &06,X
 LDA debris_start + &07,X
 ADC debris_x_velocity + &01,Y
 STA debris_start + &07,X
 LDA debris_y_velocity,Y
 SBC #LO(debris_gravity)
 STA debris_y_velocity,Y                ;update y position
 LDA debris_y_velocity + &01,Y
 SBC #HI(debris_gravity)
 STA debris_y_velocity + &01,Y
 LDA debris_y_velocity,Y                ;rotate chunk
 AND #&3F
 ADC debris_start + &01,X
 STA debris_start + &01,X
.debris_next
 TXA
 SEC
 SBC #block_size
 TAX
 BCS update_debris_loop
.unit_not_required
 RTS

.unit_creation                          ;spawn enemy according to previous events
 BIT tank_or_super_or_missile           ;already active?
 BPL unit_not_required                  ;yes
 LDA player_score + &01                 ;score >= 100K?
 BNE maybe_missile                      ;yes, think about a missile
 LDX missile_appears_at_index           ;from service menu
 LDA player_score                       ;get low byte of score
 CMP missile_appears_at_score,X         ;compare to missile threshold
 BCC create_tank                        ;score too low so create a tank after all
.maybe_missile
 LDY mathbox_random                     ;get a random number
 TYA
 EOR missile_rand                       ;xor with previous random value
 STY missile_rand                       ;save new random value
 LSR A                                  ;shift low bit into carry
 BCC create_tank                        ;and create tank if clear
 LDY #&02
 JSR eight_bit_unsigned                 ;init clock used for missed-missile retry timeout
 JMP create_missile

.create_tank
 INC sound_enemy_alert                  ;unit spawned
 LDY #&30
 JSR eight_bit_unsigned
 LDA #&00                               ;clear reverse flags/unit stationary
 STA enemy_rev_flags                    ;is &80 in arcade but nothing tests for it
 JSR get_tank_type                      ;standard/super tank
 STA tank_or_super_or_missile
 LDA mathbox_random + &02
 STA enemy_turn_to                      ;random heading
 LDX #&00
 STX missile_flag                       ;not a missile
 STX tank_or_super_or_missile_y         ;correct altitude, previous unit may have been a missile
 STX tank_or_super_or_missile_y + &01
 LDA game_mode                          ;are we playing the game?
 BNE be_nice                            ;no
 LDA player_score + &01                 ;over 100K points?
 BNE be_mean                            ;yes
 LDA player_score
 SEC                                    ;compare player score/enemy score
 SBC enemy_score
 BCC be_nice
 BEQ be_nice
 CMP #&07                               ;is player_score - enemy_score < 7000?
 BCC be_sort_of_nice                    ;yes, be sort of nice
.be_mean                                ;get value from &00-78 that affects the angle at which the enemy unit is placed
 LDA #&07                               ;smaller values place the enemy closer to where the player is facing
.be_sort_of_nice                        ;so we use those when playing nice
 LSR A                                  ;1-7 becomes 0-3
 BEQ be_nice                            ;player barely ahead of enemy
 TAX                                    ;x is now left-shift counter (1-3)
 LDA #&0F
.rotate
 SEC
 ROL A
 DEX
 BNE rotate
 BEQ create_common                      ;always
.be_nice
 LDA #&0F
 BNE create_common                      ;always

.create_missile
 LDA #object_x07_missile                ;set type to missile
 STA tank_or_super_or_missile
 INC missile_count                      ;increment missile counter
 LDA #HI(missile_fly_high_altitude)     ;set high altitude
 STA tank_or_super_or_missile_y + &01
 LDX #&00
 STX tank_or_super_or_missile_y         ;set low altitude
 STX tracks_active                      ;enable exhaust animation
 STX sound_missile_amplitude            ;quiet sound
 DEX                                    ;x=&ff
 STX saucer                             ;remove saucer
 STX unit_shot                          ;remove unit shot
 STX missile_flag                       ;missile now active4
 STX enemy_dist_hi                      ;initial distance
 LDA mathbox_random + &02               ;get a random value for angle adjustment
 AND #&0F                               ;reduce to 0-15, roll into routine below

.create_common                          ;create common features of the opponent
 STA enemy_angle_adjustment             ;pass an angle adjustment in and add to the player's facing
 LDA mathbox_random + &03               ;to determine the angle at which the new unit will appear
 AND enemy_angle_adjustment             ;small values will put enemy unit to the front, large to side/rear
 BIT mathbox_random + &03               ;reduce angle adjustment value by zeroing random bits
 BVS no_invert                          ;test bit 6
 EOR #&FF                               ;flip the bits
.no_invert
 CLC
 ADC m_tank_rotation_512 + &01          ;add to player facing
 STA enemy_angle_adjustment             ;compute the z coordinate using cos(enemy_angle_adjustment)
 JSR cosine_256_16                      ;compute cos(angle_adjustment)
 STX z_offset                           ;value from &0000-&7fff
 STX cosine
 STA cosine + &01
 CMP #&80                               ;divide by 4 with sign extension
 ROR A                                  ;gives +/- 8191
 ROR z_offset
 CMP #&80
 ROR A
 ROR z_offset
 STA z_offset + &01
 TXA                                    ;compute z_offset = cos(angle_adjustment) - z_offset
 SEC                                    ;which leaves us with 3/4 cos(angle_adjustment)
 SBC z_offset                           ;about &6000 units away
 STA z_offset
 LDA cosine + &01
 SBC z_offset + &01
 BIT missile_flag                       ;is this a missile?
 BMI max_range_z                        ;yes, use max range
 CMP #&80                               ;divide by 4 bringing the enemy closer
 ROR A
 ROR z_offset
 CMP #&80
 ROR A
 ROR z_offset
.max_range_z
 STA z_offset + &01
 LDA enemy_angle_adjustment             ;repeat the process for sin(angle_adjustment) and the x coordinate
 JSR sine_256_16                        ;compute sin(angle_adjustment)
 STX x_offset                           ;this is a value from &0000-&7fff
 STX sine
 STA sine + &01
 CMP #&80                               ;divide by 4 with sign extension
 ROR A                                  ;yields essentially +/- 8191
 ROR x_offset
 CMP #&80
 ROR A
 ROR x_offset
 STA x_offset + &01
 SEC                                    ;compute x_offset = sin(angle_adjustment) - z_offset
 LDA sine                               ;which leaves us with 3/4 sin(angle_adjustment)
 SBC x_offset
 STA x_offset
 LDA sine + &01
 SBC x_offset + &01
 BIT missile_flag                       ;are we creating a missile?
 BMI max_range_x
 CMP #&80                               ;divide by 4 bringing the enemy closer
 ROR A
 ROR x_offset
 CMP #&80
 ROR A
.max_range_x
 STA x_offset + &01                     ;use x/z offsets from my tank position
 LDA m_tank_z                           ;add z_offset to player z position
 CLC
 ADC z_offset
 STA tank_or_super_or_missile_z         ;save as enemy z position
 LDA m_tank_z + &01
 ADC z_offset + &01
 STA tank_or_super_or_missile_z + &01
 LDA m_tank_x                           ;same for x
 CLC
 ADC x_offset
 STA tank_or_super_or_missile_x
 LDA m_tank_x + &01
 ADC x_offset + &01
 STA tank_or_super_or_missile_x + &01
 BIT missile_flag                       ;spawning missile?
 BPL not_a_missile                      ;no
 JSR calc_angle_to_player               ;point the missile at the player
 STA tank_or_super_or_missile + &01     ;set enemy facing
 STA enemy_turn_to                      ;set desired heading the same

.not_a_missile
 LDX #&00                               ;x=&00
 STX ai_rez_protect                     ;suppress aggression after spawning unit
 DEX                                    ;x=&ff
 STX missile_exhaust                    ;exhaust off
 PHP
 SEI
 LDA #hertz_16                          ;16hz timer
 STA clock_second
 LDA #timer_slacking                    ;set slacking timer
 STA clock_mode
 PLP                                    ;restore irq status
 RTS

.get_tank_type
 LDA missile_count                      ;check number of missiles launched
 BMI normal_tank                        ;&80-ff, want slow tanks
 CMP #&05                               ;super tank if it's &05 <= n < &80
 BCC normal_tank_clear
 LDA #object_x05_super_tank
 RTS
.normal_tank
 CLC
.normal_tank_clear
 LDA #object_x04_tank
 RTS                                    ;a = tank type c=0/1 standard tank/super tank

.my_projectile                          ;maintain my shell
 BIT m_shell                            ;in flight?
 BPL my_projectile_in_flight            ;yes
 BIT m_tank_status                      ;player alive?
 BMI tank_cannot_fire                   ;explosions are used by tank/unit so cannot wait to finish
 BIT combined_space                     ;fire shot?
 BMI take_a_shot                        ;yes
.tank_cannot_fire
 RTS

.take_a_shot
 BIT missile_flag                       ;missile active?
 BMI no_shot_sound
 INC sound_tank_shot_loud
.no_shot_sound
 LDA m_tank_rotation_512
 LDX m_tank_rotation_512 + &01
 JSR sine_512
 STY workspace
 STA m_shell_x_vector
 ASL A                                  ;sign extend x
 LDA #&00
 ADC #&FF
 EOR #&FF
 ASL workspace
 ROL m_shell_x_vector
 ROL A
 STA m_shell_x_vector + &01
 LDA m_tank_rotation_512
 LDX m_tank_rotation_512 + &01
 JSR cosine_512
 STY workspace
 STA m_shell_z_vector
 ASL A                                  ;sign extend z
 LDA #&00
 ADC #&FF
 EOR #&FF
 ASL workspace
 ROL m_shell_z_vector
 ROL A
 STA m_shell_z_vector + &01
 LDA #object_x09_m_shell                ;spawn my shell
 STA m_shell
 LDA m_tank_rotation_512 + &01          ;my shell facing angle
 STA m_shell + &01
 LDA m_tank_x                           ;initialise x coordinate
 STA m_shell + &02
 LDA m_tank_x + &01
 STA m_shell + &03
 LDA m_tank_z                           ;initialise z coordinate
 STA m_shell + &06
 LDA m_tank_z + &01
 STA m_shell + &07
 LDA #&7F / player_time_to_live         ;time to live counter
 STA m_shell_time_to_live
 RTS

.my_projectile_in_flight                ;move my shot and test for objects/unit/saucer collision
 DEC m_shell_time_to_live
 BEQ m_shell_deactivate
 LDX #&04                               ;test movement x4
 STX object_counter
.move_my_shell
 LDA m_shell_x
 CLC
 ADC m_shell_x_vector
 STA m_shell_x
 LDA m_shell_x + &01
 ADC m_shell_x_vector + &01
 STA m_shell_x + &01
 LDA m_shell_z
 CLC
 ADC m_shell_z_vector
 STA m_shell_z
 LDA m_shell_z + &01
 ADC m_shell_z_vector + &01
 STA m_shell_z + &01                    ;test if hit anything
 LDX #m_shell - object_shot_collision_start
 JSR projectile_object_collision_test   ;test collision my shot ---> object
 BCS my_shell_hit_geometrical_object
 LDX #block_size
 JSR projectile_saucer_collision_test   ;test collision my shot ---> saucer
 BCS my_shell_hit_saucer
 LDX #zero_size
 LDY #block_size
 JSR projectile_collision_test          ;test collision my shot ---> unit
 BCS my_shell_hit_unit
 DEC object_counter
 BNE move_my_shell
 RTS

.my_shell_hit_unit
 LDA #timer_slacking                    ;set slacking timer
 STA clock_mode
 LDA #score_missile                     ;now calculate score to add in
 BIT missile_flag                       ;hit missile?
 BMI standard_tank_or_missile
 LDA #score_tank
 LDX tank_or_super_or_missile           ;hit standard tank?
 CPX #object_x04_tank
 BEQ standard_tank_or_missile
 LDA #score_super_tank                  ;hit super tank
.standard_tank_or_missile
 JSR score_increase
 JSR initialise_unit_chunks             ;blow unit up, now fall through and do exploding stars etc.

.my_shell_hit_geometrical_object
 INC sound_explosion_soft               ;shot hit object
 LDA #object_x1B_explosion_00           ;set up explosion at my shot coordinates
 STA explosion
 LDA m_tank_rotation_512 + &01          ;explosion facing
 STA explosion + &01
 LDX #&05
.transfer_shot_to_explosion             ;transfer x/y/z
 LDA m_shell_x,X
 STA explosion_x,X
 DEX
 BPL transfer_shot_to_explosion
.m_shell_deactivate
 SEC                                    ;reset my shot
 ROR m_shell
 RTS

.my_shell_hit_saucer
 INC sound_saucer_shot
 LDA #score_saucer
 JSR score_increase
 LDA #saucer_dying_counter
 STA saucer_dying
 BNE my_shell_hit_geometrical_object    ;always

.update_saucer                          ;spawn/move saucer
 BIT saucer                             ;saucer active?
 BMI saucer_inactive                    ;no, see if we want to spawn one
 LDA saucer_dying                       ;saucer dying?
 BEQ saucer_not_dying                   ;no
 DEC saucer_dying
 BNE saucer_return                      ;still dying so don't move and exit
 LDA mathbox_random                     ;random delay until new saucer appears
 STA saucer_time_to_live
 SEC                                    ;set saucer to inactive and exit
 ROR saucer
.saucer_return
 RTS

.saucer_inactive
 BIT missile_flag                       ;missile active?
 BMI saucer_return                      ;yes so don't try to spawn a saucer
 LDA player_score + &01                 ;score >= 100,000 then check to spawn a saucer
 BNE saucer_check
 LDA player_score                       ;get current score
 LSR A                                  ;divide by 2
 BEQ saucer_return
.saucer_check
 LDA saucer_time_to_live                ;if low byte score >= 2000, see if we want to add a saucer, is it time?
 BNE saucer_not_yet                     ;not yet
 LDA mathbox_random                     ;spawn saucer in random location
 STA saucer_z + &01                     ;saucer z
 LDA mathbox_random + &01
 STA saucer_x + &01                     ;saucer x
 LDA #object_x08_saucer
 STA saucer
 LDA #&00
 STA saucer_dying                       ;saucer not dying
.saucer_randomize                       ;saucer random direction
 LDA mathbox_random + &02               ;set low byte of z velocity to a random value
 STA saucer_velocity_z
 ASL A                                  ;sign-extend into z high byte
 LDA #&00
 ADC #&FF
 EOR #&FF
 STA saucer_velocity_z + &01
 LDA mathbox_random + &03               ;set low byte of x velocity to random value
 STA saucer_velocity_x
 ASL A                                  ;sign-extend into x high byte
 LDA #&00
 ADC #&FF
 EOR #&FF
 STA saucer_velocity_x + &01
 LDA mathbox_random + &03               ;random time until direction change
 LSR A                                  ;0-127
 STA saucer_time_to_live
.saucer_exit
 RTS

.saucer_not_dying
 LDA saucer + &01                       ;update facing angle so saucer spins
 ADC #object_saucer_spin                ;saucer spin speed
 STA saucer + &01
 LDA saucer_time_to_live                ;time to change direction?
 BEQ saucer_randomize                   ;yes
 LDA saucer_velocity_z                  ;add x/z velocity to current position
 ADC saucer_z
 STA saucer_z
 LDA saucer_z + &01
 ADC saucer_velocity_z + &01
 STA saucer_z + &01
 LDA saucer_x
 ADC saucer_velocity_x
 STA saucer_x
 LDA saucer_x + &01
 ADC saucer_velocity_x + &01
 STA saucer_x + &01
.saucer_not_yet
 DEC saucer_time_to_live                ;decrement time-to-live / time-until-velocity-changes
.no_update_enemy_unit
 RTS

.update_enemy_unit                      ;update one of the active three units
 BIT tank_or_super_or_missile           ;is enemy alive?
 BMI no_update_enemy_unit               ;no, exit
 BIT missile_flag                       ;is it a missile?
 BPL update_tank                        ;no, do tank update
 JMP update_missile                     ;yes, do missile update

.update_tank
 LDA #&80                               ;forward tracks always active, bit7=1 stop frame update
 STA tracks_active                      ;in case we are stationary and are rotating on the spot
 LDA tank_or_super_or_missile_x         ;save unit position
 STA unit_x_pos
 LDA tank_or_super_or_missile_x + &01
 STA unit_x_pos + &01
 LDA tank_or_super_or_missile_z
 STA unit_z_pos
 LDA tank_or_super_or_missile_z + &01
 STA unit_z_pos + &01
 LDA enemy_rev_flags                    ;check the reverse flags
 LSR A                                  ;bit 0 = 0/1 forward/reverse
 BCC unit_go_forward                    ;bit 1 = 0/1 turn left/right
 LSR A                                  ;shift once more to get turn direction
 PHP                                    ;save carry flag
 JSR calculate_movement_deltas_unit     ;compute delta x/z
 LDA tank_or_super_or_missile_x         ;subtract movement delta x from unit, move backward one step
 SEC
 SBC movement_vector_x
 STA tank_or_super_or_missile_x
 LDA tank_or_super_or_missile_x + &01
 SBC movement_vector_x + &01
 STA tank_or_super_or_missile_x + &01
 LDA tank_or_super_or_missile_z         ;subtract movement delta z from unit
 SEC
 SBC movement_vector_z
 STA tank_or_super_or_missile_z
 LDA tank_or_super_or_missile_z + &01
 SBC movement_vector_z + &01
 STA tank_or_super_or_missile_z + &01
 PLP                                    ;restore carry flag giving reverse direction
 BCC back_right
 INC tank_or_super_or_missile + &01     ;increase angle, rotate anticlockwise
 BCS back_common                        ;always

.unit_go_forward
 LDA move_counter                       ;time to update heading?
 BNE continue_move                      ;not yet, continue
 JMP set_tank_turn_to

.back_right
 DEC tank_or_super_or_missile + &01     ;decrease angle, rotate clockwise
.back_common
 LDA #&01                               ;tracks going backwards
 STA tracks_active
 LDA move_counter                       ;still going in reverse?
 BEQ reverse_done                       ;no
.reverse_exit
 RTS

.reverse_done                           ;reversed out of trouble
 LDA enemy_rev_flags
 AND %11111100                          ;clear reverse/turn flag &01=reverse/&02=direction
 STA enemy_rev_flags                    ;now move forward
 LDA tank_or_super_or_missile + &01     ;set desired direction to current direction
 STA enemy_turn_to
 LDY #&34
 JMP eight_bit_unsigned

.continue_move                          ;rotate toward facing, move forward if not too close, if within
 LDA tank_or_super_or_missile + &01     ;a few degrees of player then fire
 SEC                                    ;difference between current and desired facing
 SBC enemy_turn_to
 TAY                                    ;copy to y
 BPL continue_move_is_positive          ;get absolute value
 EOR #&FF
 CLC
 ADC #&01
.continue_move_is_positive
 CMP close_firing_angle                 ;are we close to the correct angle?
 BCC small_angle                        ;yes, turn and move
 TYA                                    ;signed angle delta from y, angle too large rotate without moving forward
 BPL turn_right_multi                   ;sign says to turn right
 INC tank_or_super_or_missile + &01     ;increase angle
 JSR try_shoot_player
 INC tank_or_super_or_missile + &01     ;increase angle
 JSR try_shoot_player
 JSR get_tank_type
 BCC reverse_exit                       ;c=0 standard tank
 INC tank_or_super_or_missile + &01     ;turn left fast, increase angle, super tanks rotate at 2x
 JSR try_shoot_player
 INC tank_or_super_or_missile + &01     ;increase angle
 JMP try_shoot_player

.turn_right_multi
 DEC tank_or_super_or_missile + &01     ;decrease angle
 JSR try_shoot_player
 DEC tank_or_super_or_missile + &01     ;decrease angle again
 JSR try_shoot_player
 JSR get_tank_type
 BCC reverse_exit                       ;c=0 standard tank
 DEC tank_or_super_or_missile + &01     ;decrease angle, super tanks rotate at 2x
 JSR try_shoot_player
 DEC tank_or_super_or_missile + &01     ;decrease angle again
 JMP try_shoot_player

.small_angle                            ;angle nearly correct, turn slowly/move forward if we're not right in player's face
 CMP #&00                               ;are we lined up on the player?
 BEQ small_angle_try_shoot              ;yes, try to shoot
 TYA                                    ;no, rotate one step then try to shoot
 BPL go_right
 INC tank_or_super_or_missile + &01     ;increase angle
 JMP small_angle_try_shoot

.go_right
 DEC tank_or_super_or_missile + &01     ;decrease angle
.small_angle_try_shoot
 JSR try_shoot_player
 JSR get_tank_type
 LDA enemy_dist_hi                      ;get high byte of distance
 BCC this_slow_tank                     ;c=0 standard tank
 CMP #&08                               ;is distance >= &800
 BCS go_forward                         ;yes, move
 RTS

.this_slow_tank
 CMP #&05                               ;is distance >= &500?
 BCC go_forward_exit                    ;no
.go_forward                             ;move towards my tank
 JSR calculate_movement_deltas_unit     ;compute delta x/z
 JSR get_tank_type
 BCC move_slow                          ;c=0 standard tank
 ASL movement_vector_x                  ;super tank so x2 movement rate
 ROL movement_vector_x + &01
 ASL movement_vector_z
 ROL movement_vector_z + &01
.move_slow
 JSR move_unit_forward                  ;update unit position
 LDX #zero_size
 JSR object_collision_test              ;did unit drive into something?
 BCS hit_something                      ;yes, handle it
 LDA #&00                               ;move tracks forward
 STA tracks_active
 LDA tank_or_super_or_missile + &01     ;are we moving without turning?
 CMP enemy_turn_to
 BEQ move_again                         ;yes, both treads forward, move again
.go_forward_exit
 RTS

.move_again
 JSR move_unit_forward                  ;update unit position
 LDX #zero_size
 JSR object_collision_test              ;did unit drive into something?
 BCC go_forward_exit                    ;no

.hit_something
 LDX game_mode                          ;are we playing now?
 BNE not_playing                        ;no, branch with a = 0 (don't reverse)
 ORA #&00                               ;set flags for a (&00 = obstacle, &ff = unit)
 BMI hit_player
 LDA mathbox_random                     ;get random value
 AND #&02                               ;&00 or &02 (determines direction turned)
 ORA #&01                               ;&01 or &03 (&01 = reversing)

.not_playing
 ORA enemy_rev_flags
 STA enemy_rev_flags                    ;set flags
 LDY #&30
 JSR eight_bit_unsigned
.hit_player                             ;restore unit position
 LDA unit_x_pos
 STA tank_or_super_or_missile_x
 LDA unit_x_pos + &01
 STA tank_or_super_or_missile_x + &01
 LDA unit_z_pos
 STA tank_or_super_or_missile_z
 LDA unit_z_pos + &01
 STA tank_or_super_or_missile_z + &01
 RTS

.set_tank_turn_to                       ;expired move counter
 LDA game_mode                          ;are we playing the game?
 BNE go_mild                            ;then attract mode
 LDA ai_rez_protect
 CMP #&FF                               ;has summoning sickness worn off?
 BEQ go_hard                            ;yes, don't be nice
 LDA mathbox_random + &01               ;get a random number
 LSR A                                  ;shift low bit into carry flag
 BCC go_hard                            ;50/50 chance
 LDA player_score + &01                 ;score >= 100K?
 BNE go_hard                            ;yes
 LDA player_score                       ;compare player score to enemy score to gauge enemy ai
 CMP enemy_score                        ;decimal mode not required as only adc/sbc affected by d flag
 BEQ go_medium
 BCC go_mild
.go_hard                                ;head directly at my tank
 JSR calc_angle_to_player               ;get angle to player
 STA enemy_turn_to                      ;use that
 LDA #&00
 STA enemy_rev_flags                    ;clear reverse flag
 JMP update_move_control_and_angle

.go_medium                              ;after a recent rez the player is winning
 LDA mathbox_random + &03               ;but just barely, pick off-target angle
 AND #&07                               ;0-7
 BNE seven_of_eight                     ;7 out of 8 times, branch
 LDA #&01                               ;set the move-backward flag
 ORA enemy_rev_flags
 STA enemy_rev_flags
 BNE set_counter                        ;always

.seven_of_eight
 LDA #&00
 STA enemy_rev_flags                    ;clear reverse flag
 JSR calc_angle_to_player               ;get angle to player
 EOR #&40                               ;pick a direction 90 degrees off (&80, &40)
 STA enemy_turn_to                      ;head that way
.set_counter
 LDY #&40
 JSR eight_bit_unsigned                 ;move this way
.try_to_shoot_exit
 RTS

.go_mild                                ;enemy out scoring my tank so go easy
 LDA mathbox_random + &02               ;get random value for angle offset
 AND #&1F                               ;reduce to 45 degrees
 BIT mathbox_random + &03
 BMI is_negative
 SBC enemy_turn_to                      ;offset previous turn-to
 BNE turn_common_mild
.is_negative
 ADC enemy_turn_to
.turn_common_mild
 STA enemy_turn_to                      ;set new direction
 LDA #&00
 STA enemy_rev_flags                    ;clear reverse flag
 JMP update_move_control_and_angle

.try_shoot_player                       ;try shoot player, wait when player newly spawned, not correct angle
 LDA ai_rez_protect                     ;and behave mildy if relative scores mean unit is in front
 CMP #&20                               ;check time since player or unit spawn
 BCC try_to_shoot_exit                  ;< 2 seconds then exit
 CMP #&FF
 BEQ shoot_okay                         ;> 17 seconds then go for it
 LDA player_score + &01                 ;score >= 100K points?
 BNE shoot_okay                         ;yes
 LDA player_score
 LSR A                                  ;score >= 2000 points?
 BNE shoot_okay                         ;yes
 LDA enemy_ang_delt_abs                 ;check angle to player
 CMP #&20                               ;within 45 degrees?
 BCS return_tank                        ;no, don't shoot
 LDA enemy_dist_hi                      ;are they somewhat close?
 CMP #&24
 BCS return_tank                        ;no, don't shoot
.shoot_okay
 JSR calc_angle_to_player               ;get angle to player
 SEC                                    ;compute difference of enemy facing and angle
 SBC tank_or_super_or_missile + &01
 BPL is_pos_tank
 CLC                                    ;negative value, invert
 EOR #&FF
 ADC #&02                               ;add 2 instead of 1
.is_pos_tank
 CMP #&02                               ;off by >= 2 angle units?
 BCS return_tank                        ;yes, don't fire
 BIT unit_shot                          ;are we ready to fire?
 BPL return_tank                        ;no, our projectile is active
 BIT m_tank_status                      ;player alive?
 BMI return_tank                        ;no
 LDA #&7F / unit_time_to_live           ;init time to live counter, shoot at my tank
 STA enemy_projectile_time_to_live
 INC sound_tank_shot_soft
 LDA tank_or_super_or_missile + &01     ;get enemy tank's facing angle
 JSR cosine_256_16                      ;compute cosine
 STX general_store                      ;save low byte
 STA enemy_projectile_velocity_z        ;save high byte in low byte z velocity
 ASL A
 LDA #&00
 ADC #&FF
 EOR #&FF
 ASL general_store                      ;multiply by 2 to get >> 7 overall
 ROL enemy_projectile_velocity_z
 ROL A
 STA enemy_projectile_velocity_z + &01
 LDA tank_or_super_or_missile + &01
 JSR sine_256_16
 STX general_store
 STA enemy_projectile_velocity_x
 ASL A
 LDA #&00
 ADC #&FF
 EOR #&FF
 ASL general_store
 ROL enemy_projectile_velocity_x
 ROL A
 STA enemy_projectile_velocity_x + &01
 LDA #object_x06_tank_shot              ;enable unit shot
 STA unit_shot
 LDA tank_or_super_or_missile + &01     ;copy facing
 STA unit_shot + &01
 LDA tank_or_super_or_missile_x         ;copy unit position to unit projectile
 STA unit_shot_x
 LDA tank_or_super_or_missile_x + &01
 STA unit_shot_x + &01
 LDA tank_or_super_or_missile_z
 STA unit_shot_z
 LDA tank_or_super_or_missile_z + &01
 STA unit_shot_z + &01
.return_tank
 RTS

.update_move_control_and_angle
 LDX game_mode                          ;are we playing?
 BEQ do_play                            ;playing
 LDY #&30
 JSR eight_bit_unsigned                 ;no, just use basic movement
 JMP use_ten                            ;always

.do_play
 CMP #&05                               ;a = &00 or (usually) &80
 BCS greater_than_five
 LDY #&40                               ;&40
 JSR eight_bit_unsigned
 JMP long
.greater_than_five
 LDY #&04
 JSR eight_bit_unsigned                 ;move &04 steps then re-evaluate
.long
 BIT close_firing_angle                 ;high bit set?
 BMI use_ten
 LDA #&0A                               ;value = &0A - value
 SEC
 SBC close_firing_angle
 BCS do_shift_one                       ;if value was &02, a holds &08
 LDA #&01                               ;value was &10
 BNE do_shift_one                       ;always

.use_ten
 LDA #&0A
.do_shift_one
 ASL A                                  ;multiply by 2
 STA close_firing_angle
 RTS

.update_missile
 LDA tank_or_super_or_missile_y + &01   ;is the altitude < &200?
 CLC                                    ;get absolute value
 EOR #&FF
 ADC #&01
 STA general_store
 LDA #HI(missile_fly_low_altitude)
 CMP general_store                      ;carry is set if near ground
 LDA #&00
 ROL A
 STA missile_low_altitude               ;set to &01 for low altitude
 AND missile_hop_flag                   ;clear it if we're hopping
 BEQ no_hop                             ;not hopping, do regular stuff
 DEC tank_or_super_or_missile_y + &01   ;low altitude, hopping, fly up, decrement high byte y
 RTS

.no_hop
 LDA m_tank_rotation_512 + &01          ;compute the difference between the direction
 SEC                                    ;player is facing and the direction we are
 SBC enemy_turn_to                      ;heading (should be close to &80)
 STA workspace                          ;save it
 BPL is_pos_missile
 CLC                                    ;get absolute value
 EOR #&FF
 ADC #&01
.is_pos_missile
 CMP #&40                               ;>= 90 degrees?
 BCS big_angle                          ;yes
 BIT workspace                          ;check sign
 BPL turn_right_two                     ;turn right or left
 BMI turn_left_two                      ;always

.big_angle
 JSR calc_angle_to_player               ;get angle from missile to player
 STA workspace
 LDA enemy_turn_to                      ;get direction we're currently heading
 SEC
 SBC workspace                          ;subtract angle
 BEQ turn_common                        ;headed straight at player
 BPL big_pos
 CMP #&FD                               ;turn left once or twice depending on angle
 BCC turn_left_one
.turn_left_two
 INC enemy_turn_to
.turn_left_one
 INC enemy_turn_to
 JMP turn_common

.big_pos
 CMP #&03                               ;turn right once or twice
 BCC turn_right_one
.turn_right_two
 DEC enemy_turn_to
.turn_right_one
 DEC enemy_turn_to
.turn_common
 LDA missile_count                      ;first missile ever?
 BEQ final_turn                         ;yes, fly straight in
 LDA player_score + &01                 ;over 100k points? check score 25k from initial missile threshold
 BNE harsh                              ;it determines how far away it stops swerving
 LDX missile_appears_at_index           ;get the missiles first appear at value
 LDA missile_appears_at_score,X
 CLC
 SED
 ADC #&25                               ;add bcd 25 ---> 30/35/45/55
 CLD
 SEC
 SBC player_score                       ;subtract player score
 BMI harsh                              ;score is higher than second threshold so branch
 CMP #&08                               ;is it getting close?
 BCS sub_harsh                          ;no, use score diff as distance threshold
.harsh
 LDA #&08                               ;use distance threshold of &0800 - very close
.sub_harsh
 CMP enemy_dist_hi                      ;are we within threshold?
 BCC not_point_blank                    ;no, still far away
.final_turn
 LDA enemy_turn_to                      ;head at player
 JMP move_missile

.not_point_blank                        ;make the missile swerve
 LDA clock_event_tick                   ;timer swerving, carry flag will be set for 2 sec, clear for 2 sec
 CMP #(timer_swerving >> 1)             ;this determines whether we swerve left or right
 AND #&1F
 STA general_store                      ;keep the 5 bits
 LDA enemy_turn_to                      ;get desired facing
 BCS desired_facing
 ADC general_store                      ;c=0 add the bits
 ADC general_store                      ;add again to subtract below
.desired_facing
 SBC general_store                      ;c=1 subtract the bits
.move_missile                           ;move missile forward in facing direction
 STA tank_or_super_or_missile + &01     ;set the facing direction
 JSR sine_256_16                        ;compute sine
 STA movement_vector_x                  ;save the high byte
 ASL A                                  ;move the missile forward in the direction it's currently facing
 LDA #&00                               ;take the sin/cos values and right-shift them 6x with sign extension
 ADC #&FF                               ;(+/- 512) done by shifting a byte right and then 2 bits left
 EOR #&FF
 STA movement_vector_x + &01
 TXA                                    ;low sine byte
 ASL A                                  ;shift left
 ROL movement_vector_x                  ;roll the bit in
 ROL movement_vector_x + &01
 ASL A
 ROL movement_vector_x
 ROL movement_vector_x + &01
 LDA tank_or_super_or_missile + &01
 JSR cosine_256_16                      ;compute cosine
 STA movement_vector_z                  ;save the high byte
 ASL A
 LDA #&00
 ADC #&FF
 EOR #&FF
 STA movement_vector_z + &01
 TXA                                    ;low cosine byte
 ASL A
 ROL movement_vector_z
 ROL movement_vector_z + &01
 ASL A
 ROL movement_vector_z
 ROL movement_vector_z + &01
 LDA tank_or_super_or_missile_x         ;save unit position
 STA unit_x_pos
 LDA tank_or_super_or_missile_x + &01
 STA unit_x_pos + &01
 LDA tank_or_super_or_missile_z
 STA unit_z_pos
 LDA tank_or_super_or_missile_z + &01
 STA unit_z_pos + &01
 JSR move_unit_forward                  ;update unit position
 LDX #zero_size
 LDY #block_size
 JSR projectile_collision_test          ;test collision my shot ---> unit
 BCS missile_return                     ;yes, blow up soon
 LDX #zero_size
 JSR object_collision_test              ;did unit drive into something?
 BCC no_collision
 LDX missile_low_altitude               ;at low altitude?
 BNE handle_collision                   ;yes, we didn't fly over, handle it
 RTS                                    ;no, we flew over it

.no_collision
 LDA #&00
 STA missile_hop_flag                   ;no longer colliding, clear hop flag
 LDA tank_or_super_or_missile_y         ;check the current altitude
 ORA tank_or_super_or_missile_y + &01
 BEQ missile_return                     ;at ground level so leave
 INC tank_or_super_or_missile_y + &01   ;increment high byte
.missile_return
 RTS

.handle_collision
 ORA #&00                               ;set flags for a &00=obstacle, &ff=player
 BMI player_really_dead
 LDA unit_x_pos                         ;restore unit position to pre-collision with geometrical object
 STA tank_or_super_or_missile_x
 LDA unit_x_pos + &01
 STA tank_or_super_or_missile_x + &01
 LDA unit_z_pos
 STA tank_or_super_or_missile_z
 LDA unit_z_pos + &01
 STA tank_or_super_or_missile_z + &01
 INC missile_hop_flag                   ;set hop flag
 RTS

.player_really_dead                     ;missile and player collided
 JSR initialise_unit_chunks             ;blow up missile
 SEC
 ROR tank_or_super_or_missile           ;missile dead
 JSR sound_when_dead
 CLC
 RTS

.projectile_object_collision_test       ;projectile collision with geometrical objects
 LDY #(object_shot_collision_end - object_shot_collision_start) - block_size
.projectile_collide_loop                ;x = index my shot/unit shot
 SEC                                    ;calculate delta x distance to object
 LDA object_shot_collision_start + &02,Y
 SBC object_shot_collision_start + &02,X
 STA distance_dx
 LDA object_shot_collision_start + &03,Y
 SBC object_shot_collision_start + &03,X
 STA distance_dx + &01
 BPL projectile_quick_check_x           ;quick check for x bounding
 EOR #&FF
 CLC
 ADC #&01
.projectile_quick_check_x
 CMP #HI(general_bounding_box)
 BCS no_projectile_collision            ;not in x range
 SEC
 LDA object_shot_collision_start + &06,Y
 SBC object_shot_collision_start + &06,X
 STA distance_dz                        ;calculate delta z distance to object
 LDA object_shot_collision_start + &07,Y
 SBC object_shot_collision_start + &07,X
 STA distance_dz + &01
 BPL projectile_quick_check_z           ;quick check for z bounding
 EOR #&FF
 CLC
 ADC #&01
.projectile_quick_check_z
 CMP #HI(general_bounding_box)
 BCS no_projectile_collision            ;not in z range
 STX general_x
 STY general_y
 JSR distance_16                        ;exit a=d_object_distance + &01
 LDY general_y                          ;restore y
 CMP #&80                               ;divide distance by 4
 ROR A
 ROR d_object_distance
 CMP #&80
 ROR A
 ROR d_object_distance
 CMP #&00
 BNE projectile_reload_x                ;too far, branch
 LDX object_shot_collision_start,Y      ;geometrical object id
 LDA obstacle_diameter,X                ;get the object diameter
 CMP d_object_distance
 BCS projectile_collision_exit          ;hit an object
.projectile_reload_x
 LDX general_x                          ;restore x
 SEC
.no_projectile_collision
 TYA                                    ;next object block
 SBC #block_size
 TAY
 BCS projectile_collide_loop
.projectile_collision_exit
 RTS

.projectile_collision_test              ;projectile collision, my shot ---> unit/unit shot ---> my tank
 LDA tank_or_super_or_missile,X         ;player x = &00, y = &08, unit x = &08, y = &00
 BMI clear_result_return                ;opposing unit on? no, don't test
 LDA unit_shot_x,Y                      ;delta x
 SEC
 SBC tank_or_super_or_missile_x,X
 STA distance_dx
 LDA unit_shot_x + &01,Y
 SBC tank_or_super_or_missile_x + &01,X
 STA distance_dx + &01
 BPL quick_check_x_unit                 ;quick check for x bounding
 EOR #&FF
 CLC
 ADC #&01
.quick_check_x_unit
 CMP #HI(general_bounding_box)
 BCS clear_result_return
 LDA unit_shot_z,Y                      ;delta z
 SEC
 SBC tank_or_super_or_missile_z,X
 STA distance_dz
 LDA unit_shot_z + &01,Y
 SBC tank_or_super_or_missile_z + &01,X
 STA distance_dz + &01
 BPL quick_check_z_unit                 ;quick check for z bounding
 EOR #&FF
 CLC
 ADC #&01
.quick_check_z_unit
 CMP #HI(general_bounding_box)
 BCS clear_result_return
 JSR distance_16                        ;divide distance by 4
 BMI clear_result_return                ;missed it
 LSR A                                  ;distance / 2
 ROR d_object_distance
 LSR A
 BNE clear_result_return                ;missed it
 ROR d_object_distance                  ;distance / 4
 BIT missile_flag                       ;bypass if not missile
 BPL not_missile_here
 LDA tank_or_super_or_missile_y + &01   ;test missile height
 EOR #&FF
 CLC
 ADC #&01
 STA workspace
 LDA #HI(missile_shot_ceiling)          ;is the missile <= &200 units?
 CMP workspace
 BCC unit_return                        ;c=0 missed
.not_missile_here
 LDA m_tank_rotation_512 + &01          ;player facing
 SEC
 SBC tank_or_super_or_missile + &01     ;subtract unit facing
 ASL A
 BPL double_is_postive_unit
 EOR #&FF
 CLC
 ADC #&01
.double_is_postive_unit
 LSR A
 LSR A
 BIT missile_flag                       ;missile?
 BMI is_missile
 LSR A
 BPL common                             ;always
.is_missile                             ;adjust as missile
 CLC
 ADC #24
.common
 STA workspace                          ;multiply by 1.5
 LSR A
 CLC
 ADC workspace
 ADC #56
 CMP d_object_distance                  ;test against distance
.unit_return
 RTS
.clear_result_return
 CLC
 RTS

.projectile_saucer_collision_test       ;test collision my shot/unit shot --> saucer
 BIT saucer                             ;projectile owner, x = &00 unit, x = &08 my tank
 BMI clear_result_return
 LDA saucer_dying                       ;if saucer dying then ignore
 BNE clear_result_return
 SEC
 LDA unit_shot_x,X                      ;calculate delta x distance to saucer
 SBC saucer_x
 STA distance_dx
 LDA unit_shot_x + &01,X
 SBC saucer_x + &01
 STA distance_dx + &01
 BPL saucer_quick_check_x               ;quick check for x bounding
 EOR #&FF
 CLC
 ADC #&01
.saucer_quick_check_x
 CMP #HI(general_bounding_box)
 BCS clear_result_return                ;not in x range
 SEC
 LDA unit_shot_z,X
 SBC saucer_z
 STA distance_dz                        ;calculate delta z distance to saucer
 LDA unit_shot_z + &01,X
 SBC saucer_z + &01
 STA distance_dz + &01
 BPL saucer_quick_check_z               ;quick check for z bounding
 EOR #&FF
 CLC
 ADC #&01
.saucer_quick_check_z
 CMP #HI(general_bounding_box)
 BCS clear_result_return                ;not in z range
 JSR distance_16
 LDA #LO(radius_saucer)
 CMP d_object_distance
 LDA #HI(radius_saucer)
 SBC d_object_distance + &01            ;c=0/1
 RTS

.enemy_projectile                       ;maintain enemy projectile
 BIT unit_shot
 BMI unit_shot_not_in_flight_exit
 DEC enemy_projectile_time_to_live
 BEQ enemy_shell_deactivate
 LDX #&04                               ;test movement x4
 STX object_counter
.move_enemy_projectile
 LDA unit_shot_x                        ;advance unit shot
 CLC
 ADC enemy_projectile_velocity_x
 STA unit_shot_x
 LDA unit_shot_x + &01
 ADC enemy_projectile_velocity_x + &01
 STA unit_shot_x + &01
 LDA unit_shot_z
 CLC
 ADC enemy_projectile_velocity_z
 STA unit_shot_z
 LDA unit_shot_z + &01
 ADC enemy_projectile_velocity_z + &01
 STA unit_shot_z + &01
 LDX #unit_shot - object_shot_collision_start
 JSR projectile_object_collision_test   ;test collision unit shot ---> object
 BCS enemy_projectile_hit_geometrical_object
 LDX #block_size
 LDY #zero_size
 JSR projectile_collision_test          ;test collision unit shot ---> player
 BCS enemy_projectile_hit_my_tank
 LDX #zero_size
 JSR projectile_saucer_collision_test   ;test collision unit shot ---> saucer
 BCS enemy_projectile_hit_saucer
 DEC object_counter
 BNE move_enemy_projectile
.unit_shot_not_in_flight_exit
 RTS

.enemy_projectile_hit_my_tank
 JSR sound_when_dead
.enemy_shell_deactivate
 SEC                                    ;reset unit shot
 ROR unit_shot
 RTS

.enemy_projectile_hit_saucer
 INC sound_saucer_shot
 LDA #saucer_dying_counter
 STA saucer_dying
 SEC                                    ;reset unit shot
 ROR unit_shot
 RTS

.enemy_projectile_hit_geometrical_object
 INC sound_explosion_soft               ;shot hit object
 LDA #object_x1B_explosion_00           ;set up explosion at enemy shot coordinates
 STA explosion
 LDA m_tank_rotation_512 + &01          ;correct rotation that is rendered later on
 STA explosion + &01
 LDX #&05
.transfer_enemy_shot_to_explosion
 LDA unit_shot_x,X
 STA explosion_x,X
 DEX
 BPL transfer_enemy_shot_to_explosion
 STX unit_shot                          ;reset unit shot, x=&ff
 RTS
