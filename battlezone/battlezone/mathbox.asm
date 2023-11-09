; mathbox
; invokes mathbox arm v2 second processor or above to leverage the power of a co-processor
;
; the functions provided by the original atari mathbox were uncomplicated, essentially
; providing access to line drawing/transforms/rotations etc, something a later generation of
; microprocessors to the 6502 would provide with ease
;
; host code will check for presence of a second processor, load mathbox code into parasite
; memory space and execute causing the second processor, if an arm v2 or greater, to return a flag
; byte to the host indicating presence and general speed, it then enters a service loop waiting
; for function requests from the host
;
; if mathbox detected on load some mathbox calls are still made even though the user has
; selected not to use, this is to faciliate correct switching at any point in the game
;
; portions of the tube control code are taken from the bbc master 6502 dnfs rom and adapted
; to serve the limited requirements of the mathbox and also free up the host language work space
; residing at &0400 - &07ff for user programs as well as multiple zero page locations
; from &00 - &70 approx on the host processor
;
; due to the tube call overhead only functions that require significant processing power are passed across
;
; in general the mathbox function sequence is:-
;
; host          - populate host register(s) with data/function code
; host          - send all host registers and function code to mathbox
; mathbox       - executes function and populates arm mathbox register(s) with result(s)
; mathbox       - enters service loop polling for the next function request
; host          - retrieve result(s) from arm mathbox register(s) to host register(s)/zero page address
;                 optional wait for results if function invoked is time consuming
;                 generally even a slow mathbox will return results in the time it takes the 6502 to
;                 start reading and will return results as fast as can be read from the tube with no wait states
; host          - uses result(s) passed back either in situ or moved to an address in zero page
;
; mathbox_flag  7 6 5 4 3 2 1 0
;               | |           |speed
;               | |presence
;               |active
;
; note: some page zero variables are mapped directly onto host registers to minimise data transfers

.mathbox_retrieve_data_length           ;transfer count from mathbox indexed by function code
                                        ;bit 7 = 1 wait for results by polling function code until -ve
                                        ;bit 6 = 1 function is expected to take time, exit to perform other host work
                                        ;          then return and wait for results by polling function code until -ve
 EQUB &00                               ;01 16 bit rotation angles x/y/z
 EQUB &06                               ;02 16 bit rotation around x/y/z
 EQUB &06                               ;03 16 bit rotation
 EQUB &80                               ;04 16 bit line draw
 EQUB &00                               ;05 16 bit graphic origin
 EQUB &00                               ;06 08 bit screen address
 EQUB &00                               ;07 16 bit graphic window x0/y0/x1/y1
 EQUB &89                               ;08 16 bit line clip
 EQUB &04                               ;09 16 bit object view transform
 EQUB &09                               ;0A 16 bit object view rotate

.mathbox_command_code
 EQUB &86                               ;parasite to host bytes
 EQUB &88                               ;host to parasite bytes

.mathbox_vectors                        ;<--- vector table start

.mathbox_vector_line_draw_08            ;initialised with 6502 vectors
 EQUW line_draw_08_6502
.mathbox_vector_line_draw_16
 EQUW line_draw_16_6502
.mathbox_vector_line_clip_16
 EQUW line_clip_16_6502

.mathbox_vectors_end                    ;<--- vector table end

.mathbox_arm_vector_table               ;arm/6502 vectors
 EQUW mathbox_line_draw_16
 EQUW mathbox_line_draw_16
 EQUW mathbox_line_clip_16

.mathbox_line_draw08                    ;mathbox vectors
 JMP (mathbox_vector_line_draw_08)
.mathbox_line_draw16
 JMP (mathbox_vector_line_draw_16)
.mathbox_line_clip16
 JMP (mathbox_vector_line_clip_16)

.mathbox_transfer_block                 ;host where to send bytes in mathbox
 EQUD mathbox_register_block            ;pointer to parasite registers/flag/function code

.mathbox_function_code_block            ;host where to receive bytes in mathbox
 EQUD mathbox_function_code             ;pointer to parasite function code

.mathbox_line_block                     ;host where to receive bytes in mathbox
 EQUD mathbox_screen_address            ;pointer to parasite line data

.mathbox_toggle_activated               ;toggle mathbox status if activated
 BIT mathbox_flag
 BVC no_mathbox                         ;no mathbox at all so exit
 BIT combined_f
 BMI mathbox_swap
 LDA #LO(mathbox_toggle)
 STA mathbox_workspace
 LDA screen_hidden
 CLC
 ADC #HI(mathbox_toggle)
 STA mathbox_workspace + &01
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

.mathbox_swap
 INC sound_key_click
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
 RTS

.mathbox_command_tube_direct
 STY host_control_block_pointer + &01   ;control block pointer
 STX host_control_block_pointer
 STA host_data_register_04              ;send action code using r4 to parasite
 TAX                                    ;save action code
 LDA host_control_block_claim_id        ;send tube id using r4
 STA host_data_register_04
 LDY #&03                               ;send control block
 LDA (host_control_block_pointer),Y
 STA host_data_register_04
 DEY
 LDA (host_control_block_pointer),Y
 STA host_data_register_04
 DEY
 LDA (host_control_block_pointer),Y
 STA host_data_register_04
 DEY
 LDA (host_control_block_pointer),Y
 STA host_data_register_04
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

.mathbox_claim_tube                     ;claim tube, used only once at game start
 BIT mathbox_flag
 BVC mathbox_claim_tube_direct_exit
 LDA #&C0 + mathbox_claim_id
.mathbox_keep_claiming
 ASL host_control_block_tube_flag       ;repeat until claimed
 BCS mathbox_claim_it
 CMP host_control_block_claim_id
 BEQ mathbox_claim_tube_direct_exit     ;already claimed
 BNE mathbox_keep_claiming              ;can't claim it yet
.mathbox_claim_it
 STA host_control_block_claim_id
.mathbox_claim_tube_direct_exit
 RTS

MACRO math_line_clip_window x0, y0, xmin, xmax, ymin, ymax, result
 LDX x0
 LDY y0
 math_clip_code_ymax_address y0, ymax, result
 math_clip_code_ymin_address y0, ymin, result
 math_clip_code_xmax_address x0, xmax, result
 math_clip_code_xmin_address x0, xmin, result
ENDMACRO

MACRO math_clip_code_ymax_address y0, y_hi, result
 CPY y_hi
 LDA y0 + &01
 SBC y_hi + &01
 BVS no_eor_ymax
 EOR #&80
.no_eor_ymax
 ASL A                                  ;c=0 when a<num, c=1 a>=num
 ROL result                             ;bit 3
ENDMACRO

MACRO math_clip_code_ymin_address y0, y_lo, result
 CPY y_lo
 LDA y0 + &01
 SBC y_lo + &01
 BVC no_eor_ymin_addr
 EOR #&80
.no_eor_ymin_addr
 ASL A                                  ;c=1 when a < num, c=0 a>=num
 ROL result                             ;bit 2
ENDMACRO

MACRO math_clip_code_xmax_address x0, x_hi, result
 CPX x_hi
 LDA x0 + &01
 SBC x_hi + &01
 BVS no_eor_xmax
 EOR #&80
.no_eor_xmax
 ASL A                                  ;c=0 when a < num, c=1 a>=num
 ROL result                             ;bit 1
ENDMACRO

MACRO math_clip_code_xmin_address x0, x_lo, result
 CPX x_lo
 LDA x0 + &01
 SBC x_lo + &01
 BVC no_eor_xmin
 EOR #&80
.no_eor_xmin
 ASL A                                  ;c=1 when a<num, c=0 a>=num
 ROL result                             ;bit 0
ENDMACRO

.mathbox_line_clip_16                   ;clip line segment, check it needs clipping before call
 LDA #&00
 STA cs_value_00                        ;generate clip codes
 STA cs_value_01

 math_line_clip_window graphic_x_00, graphic_y_00, window_x_00, window_x_01, window_y_00, window_y_01, cs_value_00
 math_line_clip_window graphic_x_01, graphic_y_01, window_x_00, window_x_01, window_y_00, window_y_01, cs_value_01

 LDA cs_value_00
 ORA cs_value_01
 BEQ mathbox_line_segment_visible       ;line within viewport
 LDA cs_value_00
 AND cs_value_01
 BNE mathbox_line_segment_invisible     ;line outside of viewport
 LDA #mathbox_code_line_clip16
 LDX #host_register_block               ;results back
 JSR mathbox_function_ax
 LSR host_flags                         ;c=visibility flag
 RTS
.mathbox_line_segment_visible
 CLC
 RTS
.mathbox_line_segment_invisible
 SEC
.mathbox_exit
 RTS

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

.mathbox_rotation_angles                ;copy object rotation angles to mathbox
 BIT mathbox_flag
 BVC mathbox_exit
 LDA x_object_rotation                  ;store x/y/z rotation angles
 STA host_r0
 LDA y_object_rotation
 STA host_r1
 LDA z_object_rotation
 STA host_r2
 LDA #mathbox_code_rotation_angles08    ;result destination not required
 BNE mathbox_function_a_only            ;always

.mathbox_function_ax
 STX mathbox_save                       ;x = results destination in page zero
.mathbox_function_a_only
 STA host_function_code                 ;a = function required
 LDX #LO(mathbox_transfer_block)
 LDY #HI(mathbox_transfer_block)
 LDA #tube_reason_code_01               ;command for host ---> parasite - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDX #LO(-&0A)
.mathbox_send_data_command_01_loop      ;send r0-r3 registers, flag and function code
 LDY host_register_block + &0A,X        ;use zero page wrap-around for index and counter
 STY host_data_register_03
 INX
 BNE mathbox_send_data_command_01_loop
 NOP                                    ;wait required
 LDA mathbox_retrieve_data_length - &01,Y
 BEQ mathbox_exit                       ;nothing to retrieve, call only so exit
 STA mathbox_save_state                 ;save length
 BPL mathbox_no_wait_for_host_flag      ;no need to wait for mathbox result(s)
 ASL A                                  ;leave mathbox to re-enter later for result(s)
 BMI mathbox_exit                       ;only to be used on mathbox only versions

.mathbox_wait_for_host_flag             ;<--- re-enter here for results
 LDX #LO(mathbox_function_code_block)   ;wait until result(s) are available from the mathbox
 LDY #HI(mathbox_function_code_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDA host_data_register_03              ;read function code from mathbox, &ff if finished
 BPL mathbox_wait_for_host_flag         ;not ready yet, mathbox still working on it, go read again

.mathbox_no_wait_for_host_flag          ;<--- re-enter here only if absolutely certain results available
 LDX #LO(mathbox_transfer_block)
 LDY #HI(mathbox_transfer_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 JSR mathbox_command_tube_direct
 LDA mathbox_save_state                 ;retrieve length and clear any (wait/long function) top bits
 AND #&3F
 BEQ mathbox_exit                       ;check wait for results only ie &C0/&80
 TAY
 LDX mathbox_save                       ;results destination
.mathbox_retrieve_data_02
 LDA host_data_register_03
 STA page_zero,X
 INX
 DEY
 BNE mathbox_retrieve_data_02
.mathbox_function_exit_01
 RTS

 MACRO read_write_byte label            ;label to rts to
 LDA host_data_register_03              ;4 screen high address bit 7 = 1 then exit
 BMI label                              ;2
 STA mathbox_workspace + &01            ;3
 LDY host_data_register_03              ;4 screen low address
 LDA host_data_register_03              ;4 screen pixels
 ORA (mathbox_workspace),Y              ;5
 STA (mathbox_workspace),Y              ;5
 ENDMACRO

.mathbox_line_draw_16
 LDA #mathbox_code_line_draw16
 JSR mathbox_function_a_only
 LDX #LO(mathbox_line_block)            ;retrieve line data
 LDY #HI(mathbox_line_block)
 LDA #tube_reason_code_00               ;command for parasite ---> host - multiple byte transfer
 STA mathbox_workspace
 JSR mathbox_command_tube_direct
.mathbox_transfer_line
 read_write_byte mathbox_function_exit_01
 read_write_byte mathbox_function_exit_01
 read_write_byte mathbox_function_exit_01
 read_write_byte mathbox_function_exit_01
 read_write_byte mathbox_function_exit_00
 read_write_byte mathbox_function_exit_00
 read_write_byte mathbox_function_exit_00
 read_write_byte mathbox_function_exit_00
 JMP mathbox_transfer_line              ;write pixels per second - clock speed / cycles per byte
.mathbox_function_exit_00               ;2e6 / ((27 * 8 + 3) / 8)
 RTS                                    ;= 73.06kps/584.47kps min/max

MACRO mathbox_release_tube_direct       ;macro for inline release tube if required/fully tested and
 BIT mathbox_flag                       ;working as part of tube investigation, not used here
 BVC mathbox_release_exit_routine       ;as the game uses the tube exclusively
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
ENDMACRO

MACRO mathbox_bit_flip                  ;macro for inline execution switch
; routine_flip tied to n and v flag so as to reduce processor load,
; initial state of location is 0 make sure routines that always require results
; of others are called first at the start
;
; flag frequencies from initial location state of 0
; bit states tested using bit instruction
;
; n eor #&80 v
; 0    1     0
; 1    0     1
; 0    1     1
; 1    0     0

 LDA mathbox_routine_flip               ;bit flip n and v
 EOR #&80                               ;flip n flag bit 7
 BMI mathbox_no_flip_v_flag
 EOR #&40                               ;flip v flag bit 6
.mathbox_no_flip_v_flag
 STA routine_flip
 BIT routine_flip
ENDMACRO

MACRO mathbox_random_number             ;macro for inline random number
 ASL mathbox_random                     ;provides four random number bytes as atari pokey is accessed several
 ROL mathbox_random + &01               ;times for random numbers in one routine and changes value every
 ROL mathbox_random + &02               ;machine cycle, something that cannot be done here
 ROL mathbox_random + &03               ;placed here as part of general mathbox capabilities for games
 BCC mathbox_random_number_exit
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
.mathbox_random_number_exit
ENDMACRO
