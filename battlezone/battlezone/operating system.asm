; operating system vector addresses
 event_vector_table                     = &200

 userv                                  = &200
 brkv                                   = &202
 irq1v                                  = &204
 irq2v                                  = &206
 cliv                                   = &208
 bytev                                  = &20A
 wordv                                  = &20C
 wrchv                                  = &20E
 rdchv                                  = &210
 filev                                  = &212
 argsv                                  = &214
 bgetv                                  = &216
 bputv                                  = &218
 gpbpv                                  = &21A
 findv                                  = &21C
 fscv                                   = &21E
 eventv                                 = &220
 uptv                                   = &222
 netv                                   = &224
 vduv                                   = &226
 keyv                                   = &228
 insv                                   = &22A
 remv                                   = &22C
 cnpv                                   = &22E
 ind1v                                  = &230
 ind2v                                  = &232
 ind3v                                  = &234

; system variables
 tube_presence                          = &27A
 language                               = &28C

; operating system mos addresses

 eventv_vector_table                    = &FFB7

 osfind                                 = &FFCE
 osgpbp                                 = &FFD1
 osbput                                 = &FFD4
 osbget                                 = &FFD7
 osargs                                 = &FFDA
 osfile                                 = &FFDD
 osrdch                                 = &FFE0
 osasci                                 = &FFE3
 osnewl                                 = &FFE7
 oswrch                                 = &FFEE
 osword                                 = &FFF1
 osbyte                                 = &FFF4
 oscli                                  = &FFF7

; bbc system/user via addresses
 sheila                                 = &FE00

 bbc_romsel                             = sheila + &30
 bbc_master_romsel                      = sheila + &32
 bbc_solidisk_romsel                    = sheila + &60

 system_via_orb_irb                     = sheila + &40
 system_via_ora_ira                     = sheila + &41
 system_via_ddr_b                       = sheila + &42
 system_via_ddr_a                       = sheila + &43
 system_via_timer_1_counter_lo          = sheila + &44
 system_via_timer_1_counter_hi          = sheila + &45
 system_via_timer_1_latch_lo            = sheila + &46
 system_via_timer_1_latch_hi            = sheila + &47
 system_via_timer_2_latch_lo            = sheila + &48
 system_via_timer_2_latch_hi            = sheila + &49
 system_via_timer_2_sreg                = sheila + &4A
 system_via_aux_reg                     = sheila + &4B
 system_via_per_ctl_reg                 = sheila + &4C
 system_via_ifr_reg                     = sheila + &4D
 system_via_ier_reg                     = sheila + &4E
 system_via_ora_ira_no_hand             = sheila + &4F

 system_via_aux_clear                   = &00
 system_via_aux_timer_1_one_shot        = &00
 system_via_aux_timer_2_one_shot        = &00
 system_via_aux_timer_1_continuous      = &C0
 system_via_aux_timer_2_continuous      = &82

 user_via_orb_irb                       = sheila + &60
 user_via_ora_ira                       = sheila + &61
 user_via_ddr_b                         = sheila + &62
 user_via_ddr_a                         = sheila + &63
 user_via_timer_1_counter_lo            = sheila + &64
 user_via_timer_1_counter_hi            = sheila + &65
 user_via_timer_1_latch_lo              = sheila + &66
 user_via_timer_1_latch_hi              = sheila + &67
 user_via_timer_2_latch_lo              = sheila + &68
 user_via_timer_2_latch_hi              = sheila + &69
 user_via_timer_2_sreg                  = sheila + &6A
 user_via_aux_reg                       = sheila + &6B
 user_via_per_ctl_reg                   = sheila + &6C
 user_via_ifr_reg                       = sheila + &6D
 user_via_ier_reg                       = sheila + &6E
 user_via_ora_ira_no_hand               = sheila + &6F

 user_via_aux_clear                     = &00
 user_via_aux_timer_1_one_shot          = &80
 user_via_aux_timer_1_continuous        = &C0
 user_via_aux_timer_2_continuous        = &82

 user_via_ier_timer_1                   = &C0
 user_via_ier_timer_2                   = &A0

; event types
 event_output_buffer_becomes_empty      = &00
 event_input_buffer_becomes_full        = &01
 event_character_entering_input_buffer  = &02
 event_adc_conversion_complete          = &03
 event_vertical_sync                    = &04
 event_interval_timer                   = &05
 event_escape_condition_detected        = &06
 event_rs423_error_detected             = &07
 event_econet_event                     = &08
 event_user_event                       = &09

; 6845 register values
 x6845_r0                               = &00
 x6845_r1                               = &01
 x6845_r2                               = &02
 x6845_r3                               = &03
 x6845_r4                               = &04
 x6845_r5                               = &05
 x6845_r6                               = &06
 x6845_r7                               = &07
 x6845_r8                               = &08
 x6845_r9                               = &09
 x6845_r10                              = &0A
 x6845_r11                              = &0B
 x6845_r12                              = &0C
 x6845_r13                              = &0D
 x6845_r14                              = &0E
 x6845_r15                              = &0F
 x6845_r16                              = &10
 x6845_r17                              = &11

; tube
 host_status_register_01                = &FEE0
 host_data_register_01                  = &FEE1
 host_status_register_02                = &FEE2
 host_data_register_02                  = &FEE3
 host_status_register_03                = &FEE4
 host_data_register_03                  = &FEE5
 host_status_register_04                = &FEE6
 host_data_register_04                  = &FEE7

; clear bit masks
 and_clear_bit_0                        = (&01)      EOR &FF
 and_clear_bit_1                        = (&01 << 1) EOR &FF
 and_clear_bit_2                        = (&01 << 2) EOR &FF
 and_clear_bit_3                        = (&01 << 3) EOR &FF
 and_clear_bit_4                        = (&01 << 4) EOR &FF
 and_clear_bit_5                        = (&01 << 5) EOR &FF
 and_clear_bit_6                        = (&01 << 6) EOR &FF
 and_clear_bit_7                        = (&01 << 7) EOR &FF

; set bit masks
 set_bit_0                              = &01
 set_bit_1                              = &01 << 1
 set_bit_2                              = &01 << 2
 set_bit_3                              = &01 << 3
 set_bit_4                              = &01 << 4
 set_bit_5                              = &01 << 5
 set_bit_6                              = &01 << 6
 set_bit_7                              = &01 << 7
