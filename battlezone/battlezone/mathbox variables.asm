; mathbox variables
; constants
 mathbox_claim_id                       = &28                           ;arbitary number
 mathbox_toggle                         = &13C

; tube
; mathbox only uses reason codes &00/&01, rest for info only
 tube_reason_code_00                    = &00  ;parasite ---> host     - multiple byte transfer
 tube_reason_code_01                    = &01  ;host     ---> parasite - multiple byte transfer
 tube_reason_code_02                    = &02  ;multiple pairs of byte transfers
 tube_reason_code_03                    = &03  ;host     ---> parasite - multiple double byte transfer
 tube_reason_code_04                    = &04  ;execute in parasite
 tube_reason_code_05                    = &05  ;reserved
 tube_reason_code_06                    = &06  ;parasite ---> host     - 256 byte transfer
 tube_reason_code_07                    = &07  ;host     ---> parasite - 256 byte transfer

 tube_code_entry_point                  = &406

; mathbox functions codes
 mathbox_code_presence                  = &00
 mathbox_code_rotation_angles08         = &01
 mathbox_code_rotation_vertice16        = &02
 mathbox_code_rotate16                  = &03
 mathbox_code_line_draw16               = &04
 mathbox_code_graphic_origin16          = &05
 mathbox_code_screen_address            = &06
 mathbox_code_window16                  = &07
 mathbox_code_line_clip16               = &08
 mathbox_code_object_view_transform16   = &09
 mathbox_code_object_view_rotate16      = &0A

; addresses
 host_control_block_pointer             = &00                           ;<2>
 host_register_block                    = &02                           ;invoke mathbox function parameter block
 host_r0                                = host_register_block
 host_r1                                = host_register_block + &02
 host_r2                                = host_register_block + &04
 host_r3                                = host_register_block + &06
 host_flags                             = host_register_block + &08
 host_function_code                     = host_register_block + &09
 host_register_block_end                = host_function_code  + &01

 mathbox_workspace                      = host_register_block_end       ;<2> mathbox workspace
 mathbox_save_state                     = mathbox_workspace + &02
 mathbox_save                           = mathbox_save_state + &01
 mathbox_random                         = mathbox_save + &01            ;<4> mathbox random number

 host_control_block_tube_flag           = &14
 host_control_block_claim_id            = &15

; mathbox addresses
 mathbox_screen_address                 = &9000
 mathbox_register_block                 = &9800
 mathbox_execute_address                = &9814

; mathbox 16 bit registers optimised at 4 to take a full 16 bit line segment x0/y0 - x1/y1
 mathbox_r0                             = mathbox_register_block
 mathbox_r1                             = mathbox_register_block + &02
 mathbox_r2                             = mathbox_register_block + &04
 mathbox_r3                             = mathbox_register_block + &06

; mathbox host flags returned, principally used for line clipping bit 0
 mathbox_host_flags                     = mathbox_register_block + &08  ;pass back flags

; mathbox function to be invoked
 mathbox_function_code                  = mathbox_register_block + &09  ;function to invoke
