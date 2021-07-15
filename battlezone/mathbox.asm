; mathbox mathbox
; invokes mathbox arm v2 second processor or above to leverage the power of a co-processor
;
; the functions provided by the original atari mathbox were uncomplicated, essentially
; providing access to multiplication/division/rotations etc, something a later generation of
; microprocessors to the 6502 would provide with ease
;
; host code will check for presence of a second processor, load mathbox code into the
; memory space and execute causing the second processor, if an arm v2 or greater, to return a flag to
; the host indicating presence/speed, it then enters a service loop waiting
; for function requests from the host
;
; portions of the tube control code are taken from the bbc master 6502 dnfs rom and adapted
; to serve the limited requirements of the mathbox and also free up the host language work space
; residing at &0400 - &07ff for user programs and multiple zero page locations
;
; in general the mathbox function sequence is:-
; host     - populate host register(s) with data/function code
; host     - send all host registers and function code to mathbox
; mathbox  - executes function and populates arm mathbox register(s) with result(s)
; mathbox  - enters service loop polling for the next function request
; host     - retrieve result(s) from arm mathbox register(s) to host register(s)/zero page address
;            optionally wait for results if function invoked is time consuming
;            generally the mathbox will return results in the time it takes the 6502 to
;            start reading and will return results as fast as can be read from the tube
; host     - uses result(s) passed back either in situ or moved to a memory location
;
; mathbox_flag  7 6 5 4 3 2 1 0
;               active
;                 presence
;                             speed
; note: there is mapping of variables onto host registers to minimise transfers to load
; these registers up so that calls are done as efficently as possible

.mathbox_retrieve_data_length           ;transfer bytes mathbox indexed by function code/top bit set wait for results
 EQUB &02                               ;01 16 bit signed division
 EQUB &04                               ;02 16 bit signed multiplication
 EQUB &01                               ;03 16 bit square root
 EQUB &00                               ;04 16 bit rotation angles x/y/z
 EQUB &06                               ;05 16 bit rotation around x/y/z
 EQUB &06                               ;06 16 bit rotation
 EQUB &80                               ;07 16 bit line draw
 EQUB &02                               ;08 24 bit signed division
 EQUB &00                               ;09 08 bit screen address
 EQUB &02                               ;0A 16 bit distance
 EQUB &00                               ;0B 16 bit graphic window x0/y0/x1/y1
 EQUB &89                               ;0C 16 bit line clip
 EQUB &02                               ;0D 32 bit signed division
 EQUB &02                               ;0E 16 bit sine
 EQUB &02                               ;0F 16 bit cosine

.mathbox_command_code
 EQUB &86                               ;parasite to host bytes
 EQUB &88                               ;host to parasite bytes

.mathbox_vectors                        ;<--- vector table start

.mathbox_vector_division_16             ;initialised with 6502 vectors
 EQUW division_16_signed_6502
.mathbox_vector_multiplication_16
 EQUW multiply_16_signed_6502
.mathbox_vector_square_root_16
 EQUW square_root_16_6502
.mathbox_vector_line_draw_08
 EQUW line_draw_08_6502
.mathbox_vector_line_draw_16
 EQUW line_draw_16_6502
.mathbox_vector_line_draw_16c
 EQUW line_draw_16c_6502
.mathbox_vector_division_24
 EQUW division_24_signed_6502
.mathbox_vector_distance_16
 EQUW distance_16_6502
.mathbox_vector_line_clip_16
 EQUW line_clip_16_6502
.mathbox_vector_division_32
 EQUW division_32_signed_6502
.mathbox_vector_sine_16
 EQUW sine_1280_6502
.mathbox_vector_cosine_16
 EQUW cosine_1280_6502

.mathbox_vectors_end                    ;<--- vector table end

.mathbox_arm_vector_table               ;arm vectors
 EQUW mathbox_division_16_signed
 EQUW mathbox_multiply_16_signed
 EQUW mathbox_square_root
 EQUW mathbox_line_draw_16
 EQUW mathbox_line_draw_16
 EQUW mathbox_line_draw_16
 EQUW mathbox_division_24_signed
 EQUW mathbox_distance_16
 EQUW mathbox_line_clip_16
 EQUW mathbox_division_32_signed
 EQUW mathbox_sine_1280
 EQUW mathbox_cosine_1280

.mathbox_division16                     ;mathbox vectors
 JMP (mathbox_vector_division_16)
.mathbox_multiplication16
 JMP (mathbox_vector_multiplication_16)
.mathbox_square_root16
 JMP (mathbox_vector_square_root_16)
.mathbox_line_draw08
 JMP (mathbox_vector_line_draw_08)
.mathbox_line_draw16
 JMP (mathbox_vector_line_draw_16)
.mathbox_line_draw16c
 JMP (mathbox_vector_line_draw_16c)
.mathbox_division24
 JMP (mathbox_vector_division_24)
.mathbox_distance16
 JMP (mathbox_vector_distance_16)
.mathbox_line_clip16
 JMP (mathbox_vector_line_clip_16)
.mathbox_division32
 JMP (mathbox_vector_division_32)
.mathbox_sine1280
 JMP (mathbox_vector_sine_16)
.mathbox_cosine1280
 JMP (mathbox_vector_cosine_16)

.mathbox_transfer_block                 ;host where to send bytes in mathbox
 EQUD mathbox_register_block            ;pointer to parasite registers/flag/function code

.mathbox_function_code_block            ;host where to receive bytes in mathbox
 EQUD mathbox_function_code             ;pointer to parasite function code

.mathbox_line_block                     ;host where to receive bytes in mathbox
 EQUD mathbox_screen_address            ;pointer to parasite line data

.mathbox_toggle_activated               ;toggle mathbox status if activated
 BIT mathbox_flag
 BVC no_mathbox                         ;no mathbox at all
 LDA #LO(mathbox_toggle)
 STA mathbox_workspace
 LDA screen_hidden
 CLC
 ADC #HI(mathbox_toggle)
 STA mathbox_workspace + &01
 BIT combined_f                         ;f key
 BPL display_mathbox
 LDA mathbox_flag                       ;toggle mathbox status
 EOR #&80
 STA mathbox_flag
 LDY #mathbox_vectors_end - mathbox_vectors - &01
.mathbox_populate_table                 ;copy vector table
 LDX mathbox_arm_vector_table,Y         ;swap over 6502/arm vectors
 LDA mathbox_vectors,Y
 STA mathbox_arm_vector_table,Y
 TXA
 STA mathbox_vectors,Y
 DEY
 BPL mathbox_populate_table
.display_mathbox
 LDY #&00
 BIT mathbox_flag
 BPL mathbox_clear_indicator            ;mathbox off
 LDA #&18
 BNE mathbox_square_on_screen
.mathbox_clear_indicator
 TYA
.mathbox_square_on_screen
 STA (mathbox_workspace),Y
 INY
 STA (mathbox_workspace),Y
.no_mathbox
 RTS

.mathbox_command_tube_direct
 STY host_control_block_pointer + &01   ;control block pointer
 STX host_control_block_pointer
 STA host_data_register_04              ;send action code using r4 to parasite
 TAX                                    ;save action code
 LDA host_control_block_claim_id        ;send tube id using r4
 STA host_data_register_04
 LDY #&03                               ;send control block
.mathbox_send_control_block
 LDA (host_control_block_pointer),Y
 STA host_data_register_04
 DEY
 BPL mathbox_send_control_block
 LDY #&18
 STY host_status_register_01            ;disable fifo/nmi, set sr1
 LDA mathbox_command_code,X             ;get action code and set sr1
 STA host_status_register_01
 LSR A
 LSR A
 BCC mathbox_no_wait
 BIT host_data_register_03              ;delay
.mathbox_no_wait
 STA host_data_register_04              ;send flag synchronise using r4
 BCC mathbox_command_tube_direct_exit
 LSR A
 BCC mathbox_command_tube_direct_exit
 LDY #&88
 STY host_status_register_01
.mathbox_command_tube_direct_exit
 RTS

.mathbox_claim_tube                     ;claim tube
 BIT mathbox_flag
 BVC mathbox_return
 LDA #&C0 + mathbox_claim_id
.mathbox_keep_claiming
 JSR mathbox_claim_tube_direct
 BCC mathbox_keep_claiming
.mathbox_return
 RTS

.mathbox_claim_tube_direct              ;claim tube directly
 ASL host_control_block_tube_flag
 BCS mathbox_claim_it
 CMP host_control_block_claim_id
 BEQ mathbox_claim_tube_direct_exit     ;already claimed
 CLC                                    ;can't claim it yet
.mathbox_claim_tube_direct_exit
 RTS
.mathbox_claim_it
 STA host_control_block_claim_id
.mathbox_exit
 RTS

.mathbox_line_clip_16                   ;clip line segment to game window
 LDA #mathbox_code_line_clip16
 LDX #host_register_block               ;results back
 JSR mathbox_function_ax
 LSR host_flags
 RTS

.mathbox_division_32_signed             ;32bit division, note this is optimised for a 16 bit result
 LDA division_n_32 + &04
 STA host_r0 + &00
 LDA division_n_32 + &05
 STA host_r0 + &01
 LDA division_n_32 + &02
 STA host_r1 + &00
 LDA division_n_32 + &03
 STA host_r1 + &01
 LDA divisor_32
 STA host_r2 + &00
 LDA divisor_32 + &01
 STA host_r2 + &01
 LDA #mathbox_code_signed_division32
 LDX #division_result_32
 BNE mathbox_function_ax

.mathbox_window_16                      ;x/y point to graphics window
 STX mathbox_workspace
 STY mathbox_workspace + &01
 LDY #&07
 LDX #&07
.mathbox_window_setup
 LDA (mathbox_workspace),Y
 STA host_r0,X
 STA graphic_window,X
 DEX
 DEY
 BPL mathbox_window_setup
 BIT mathbox_flag
 BVC mathbox_exit                       ;update mathbox regardless if present
 LDA #mathbox_code_window16             ;result destination not required
 BNE mathbox_function_a_only            ;always

.mathbox_division_16_signed
 LDA dividend_16
 STA host_r0
 LDA dividend_16 + &01
 STA host_r0 + &01
 LDA divisor_16
 STA host_r1
 LDA divisor_16 + &01
 STA host_r1 + &01
 LDA #mathbox_code_signed_division16
 LDX #division_result_16                ;result destination
 BNE mathbox_function_ax                ;always

.mathbox_square_root
 LDA #mathbox_code_square_root16
 LDX #squared_16                        ;result destination
 BNE mathbox_function_ax                ;always

.mathbox_rotation_angles                ;copy object rotation angles to mathbox
 BIT mathbox_flag
 BVC mathbox_nearest_exit
 LDA x_object_rotation                  ;store x/y/z rotation angles
 STA host_r0
 LDA y_object_rotation
 STA host_r1
 LDA z_object_rotation
 STA host_r2
 LDA #mathbox_code_rotation_angles08    ;result destination not required
 BNE mathbox_function_a_only            ;always

.mathbox_distance_16
 LDA #mathbox_code_distance16
 LDX #d_object_distance                 ;result destination
 BNE mathbox_function_ax

.mathbox_multiply_16_signed
 LDA multiplier_16
 STA host_r0
 LDA multiplier_16 + &01
 STA host_r0 + &01
 LDA multiplicand_16
 STA host_r1
 LDA multiplicand_16 + &01
 STA host_r1 + &01
 LDA #mathbox_code_signed_multiply16
 LDX #product_16                        ;result destination

.mathbox_function_ax
 STX mathbox_workspace                  ;x = results destination
.mathbox_function_a_only
 STA host_function_code                 ;a = function required
 LDX #LO(mathbox_transfer_block)
 LDY #HI(mathbox_transfer_block)
 LDA #tube_reason_code_01               ;command for host ---> parasite - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDX #LO(-&0A)
.mathbox_send_data_command_01_loop      ;send r0-r3 registers, flag and function code
 LDA host_register_block + &0A,X        ;use zero page wrap-around for index and counter
 STA host_data_register_03
 INX
 BNE mathbox_send_data_command_01_loop
 LDX host_function_code                 ;get function code and index into bytes to bring back
 LDA mathbox_retrieve_data_length - &01,X
 BEQ mathbox_nearest_exit               ;nothing to retrieve, call only so exit
 PHA                                    ;save length
 BPL mathbox_no_wait_required           ;no need to wait for result
.mathbox_wait_for_host_flag             ;wait until results are available from the mathbox
 LDX #LO(mathbox_function_code_block)
 LDY #HI(mathbox_function_code_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDA host_data_register_03              ;read function code
 BPL mathbox_wait_for_host_flag         ;not ready yet, still working on it
.mathbox_no_wait_required
 LDX #LO(mathbox_transfer_block)
 LDY #HI(mathbox_transfer_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 PLA                                    ;retrieve length and clear any top bit
 AND #&7F
 BEQ mathbox_nearest_exit               ;check wait for results only ie &80
 TAY
 LDX mathbox_workspace
.mathbox_retrieve_data_02
 LDA host_data_register_03
 STA page_zero,X
 INX
 DEY
 BNE mathbox_retrieve_data_02
.mathbox_nearest_exit
 RTS

.mathbox_division_24_signed             ;24bit division, note this is optimised for a 16 bit result
 LDA dividend_24
 STA host_r0 + &00
 LDA dividend_24 + &01
 STA host_r0 + &01
 LDA dividend_24 + &02
 STA host_r1
 LDA divisor_24
 STA host_r2
 LDA divisor_24 + &01
 STA host_r2 + &01
 LDA #mathbox_code_signed_division24
 LDX #division_result_24                ;result destination
 BNE mathbox_function_ax                ;always

.mathbox_line_draw_16
 LDA #mathbox_code_line_draw16
 JSR mathbox_function_a_only
 LDX #LO(mathbox_line_block)            ;retrieve line data
 LDY #HI(mathbox_line_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDA #&00
 STA mathbox_workspace
.mathbox_transfer_line
 LDY host_data_register_03              ;4 screen low address
 LDA host_data_register_03              ;4 screen high address
 BMI mathbox_function_exit              ;2 screen high address bit 7 = 1 then exit
 STA mathbox_workspace + &01            ;3
 LDA host_data_register_03              ;4 screen byte
 ORA (mathbox_workspace),Y              ;5
 STA (mathbox_workspace),Y              ;5
 JMP mathbox_transfer_line              ;3 = 30 cycles = 66.7kps/533.3kps min/max approx. best case

.mathbox_sine_1280                      ;get 1280 sine
 STA host_r0 + &00                      ;a = lsb, x = msb
 STX host_r0 + &01
 LDA #mathbox_code_sine1280
 LDX #host_register_block
 JSR mathbox_function_ax
 LDX host_r0                            ;x = lsb, a = msb
 LDA host_r0 + &01
.mathbox_function_exit
 RTS

.mathbox_cosine_1280                    ;get 1280 cosine
 STA host_r0 + &00                      ;a = lsb, x = msb
 STX host_r0 + &01
 LDA #mathbox_code_cosine1280
 LDX #host_register_block
 JSR mathbox_function_ax
 LDX host_r0                            ;x = lsb, a = msb
 LDA host_r0 + &01
 RTS

.mathbox_random_number
 ASL mathbox_random                     ;provides six random number bytes as pokey is accessed several
 ROL mathbox_random + &01               ;times for random numbers in one routine and changes value every
 ROL mathbox_random + &02               ;machine cycle, something that cannot be done here
 ROL mathbox_random + &03               ;placed here as part of general mathbox capabilities for games
 ROL mathbox_random + &04
 ROL mathbox_random + &05
 BCC mathbox_nofeedback
 LDA mathbox_random
 EOR #&B7
 STA mathbox_random
 LDA mathbox_random + &01
 EOR #&1D
 STA mathbox_random + &01
 LDA mathbox_random + &02
 EOR #&C1
 STA mathbox_random + &02
 LDA mathbox_random + &03
 EOR #&04
 STA mathbox_random + &03
.mathbox_nofeedback
.mathbox_flush_video_exit
 RTS

.mathbox_flush_video                    ;clear screen using screen addresses for master and arm only
 LDX #LO(host_r0)                       ;retrieve line addresses from mathbox buffer to erase
 LDY #HI(host_r0)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDA #&00
 STA mathbox_workspace                  ;clear workspace low byte/set a = 0
.mathbox_transfer_address
 LDY host_data_register_03              ;4 screen low address
 LDX host_data_register_03              ;4 screen high address
 BMI mathbox_flush_video_exit           ;2 screen high address bit 7 = 1 then exit
 STX mathbox_workspace + &01            ;3
 STA (mathbox_workspace),Y              ;5 now clear byte
 BPL mathbox_transfer_address           ;3 = 21 cycles = 95.2kbs, always

.mathbox_release_tube_direct            ;to release tube if required/fully tested and working, not used here
 BIT mathbox_flag
 BVC mathbox_release_exit_routine
 LDA #&C0 + mathbox_claim_id            ;release the tube
 CMP host_control_block_claim_id        ;compare release to claim
 BNE mathbox_release_exit_routine       ;exit as different
 PHP
 SEI
 LDA #&05
 STA host_data_register_04
 LDA host_control_block_claim_id
 STA host_data_register_04
 PLP                                    ;restore irq status
 LDA #&80
 STA host_control_block_claim_id        ;store tube id
 STA host_control_block_tube_flag       ;set tube status
.mathbox_release_exit_routine
 RTS
