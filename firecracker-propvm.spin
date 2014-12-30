'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
'' Firecracker VM Source
''  _________________________
'' |        Pin Table        |
'' |____pin____|___purpose___|
'' |           |             |
'' |   0-15    |   outputs   |
'' |    16     |     SDA     |
'' |    17     |     SCL     |
'' |    18     |    SCLK     |
'' |    19     |    MOSI     |
'' |    20     |    MISO     |
'' |    21     |     CS      |
'' |    22     |  soft reset |
'' |    23     | amp control |
'' |   24-27   |   unused    |
'' |   28,29   |   EEPROM    |
'' |   30,31   | USB header  |
'' |___________|_____________|
''
''
'' There are numerous protocols to communicate with Firecracker, so the first thing
'' that is done, is figuring out which configuration we are using. The following is how
'' to initialize each of the possible supported communication protocols. Wait at least 200ms
'' upon powerup before initializing. If an error occurs during initialization, pins 16 and
'' 20 will be driven high, indicating a selection was not made. Firecracker must then be soft
'' reset before being connected to again.
''
'' SPI -
''      First, set the clock to the desired clock polarity. If you just leave it at zero,
''      it defaults to a polarity of 0, which means clock is normally low and data propigated
''      on the rising edge, and read on the falling edge. The CS line will follow the clock
''      polarity. After the clock line is set, drive the MOSI line high to indicate SPI.
''      The Firecracker will respond by driving the MISO line high.
''      As soon as the MOSI line is dropped low, FVM will be ready for processing. FVM looks
''      for a positive edge, so if the line is high when FVM gets control from the boot
''      loader, it will not initialize until the line goes low, and is then raised again.
''      FVM will not respond on the MISO line if this is the case. Response time should be
''      within 5us.
''
''
'' Firecracker structure -
''      I'm seeing this playing out that we use roughly 16K of code and variable space.
''      Of course 8K of that is just the macro work area. The remaining 16K in RAM will
''      be used for Bottle Rocket LED arrays. There will be ~24K available in EEPROM
''      for macro storage with 8K being loaded at once.
''
''    - One COG is dedicated to executing just macros. It will execute NOPs until a
''      macro is executed by a second COG that is interpreting the input stream.
''    - A third COG will be used to recieve this input stream from SPI/I2C.
''    - A fourth COG will be responsible for the PWM drivers
''    - A fifth COG will act as the macro/memory manager, loading and storing macros
''      to and from EEPROM as requested. Right now I am implementing this in
''      SPIN because the EEPROM interface is very convinient and written in SPIN.
''      Any macro called will be verified and loaded into RAM if it is not there.
''      It will be mightly slow in SPIN, so I added a 'preload macro' to FVM so that
''      a user/compiler can avoid the loading overhead when they call the macro for
''      time sensitive situations. I'll work on implementing assembly later.
''    - A sixth COG will run Bottle Rocket and be responsible for updating addressable
''      strips as the memory array is updated. Communication between Bottle Rocket and
''      FVM still needs to be worked out.
''
''    - I also added opcodes for 'wait signal' and 'post signal' so the user can communicate
''      with an already running macro. I think WAITS 0 will be wait for any non-zero signal.
''      Also, POSTS FF will be a termination signal. WAITS FF is just waiting for termination
''      which is essentially a delayed RETMC and POSTS 0 is just posting no signal, so it is
''      essentially a NOP. All other signals are free to use.
''
'' Macro memory structure -
''    - For each macro in RAM, there are five leading bytes that are of special use.
''    - The first two bytes is for allocation. The leading bit is whether or not that
''      memory location is in use. The remaining 15-bits are the length of the allocated
''      space if the leading bit is set to 1.
''    - The remaining 3 bytes are for macro use. When a macro is called from a macro
''      the macro number of the caller is placed in the first of the three bytes of the callee.
''      The next two bytes are for the callers program pointer. (the program pointer is of the
''      code section of the macro. Which means PC=0 is PC=macro_base+5)
''
'' What needs to be done -
''    - Separate input and macro interpreter versions of FVM
''    - Create a test with another board to test SPI
''    - Write SPI com
''    - Finish writing the MM
''    - Fill in missing opcodes (pretty much everything macro related)
''    - correct macro processing to account for memory structure
''      with the new limits of exactly 160 LEDs and
''    - update communication docs
''    - Testing
''      - FVM
''        - Everything
''      - BRKT
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

CON

  _CLKMODE = XTAL1 + PLL16X
  _CLKFREQ = 80_000_000

  FVM_DEFAULT_NUM_MACROS  = 256
  FVM_DEFAULT_STACK_SIZE  = 1024
  FVM_DEFAULT_BUFFER_SIZE = 256
  FVM_DEFAULT_NUM_OUTPUTS = 16

  FVM_OUTPUT_MASK = $FFFF0000

  FVM_NOP_OPCODE   = 0                                                          ' both
  FVM_PUSH_OPCODE  = 1                                                          ' both
  FVM_POP_OPCODE   = 2                                                          ' both
  FVM_WRITE_OPCODE = 3                                                          ' both
  FVM_DELAY_OPCODE = 4                                                          ' both
  FVM_INC_OPCODE   = 5                                                          ' both
  FVM_DEC_OPCODE   = 6                                                          ' both
  FVM_ADD_OPCODE   = 7                                                          ' both
  FVM_SUB_OPCODE   = 8                                                          ' both
  FVM_CMP_OPCODE   = 9                                                          ' macro only
  FVM_OR_OPCODE    = 10                                                         ' both
  FVM_AND_OPCODE   = 11                                                         ' both
  FVM_TEST_OPCODE  = 12                                                         ' macro only
  FVM_NOT_OPCODE   = 13                                                         ' both
  FVM_SWAP_OPCODE  = 14                                                         ' both
  FVM_DUP_OPCODE   = 15                                                         ' both
  FVM_IF_OPCODE    = 16                                                         ' macro only
  FVM_JMP_OPCODE   = 17         ' jump                                          ' macro only
  FVM_JMPR_OPCODE  = 18         ' jump relative                                 ' macro only
  FVM_DEFMC_OPCODE = 19         ' define macro                                  ' both
  FVM_CALMC_OPCODE = 20         ' call macro                                    ' both (different implementations)
  FVM_RETMC_OPCODE = 21         ' return from macro                             ' macro only
  FVM_SAVMC_OPCODE = 22         ' save macro                                    ' both
  FVM_DELMC_OPCODE = 23         ' delete macro                                  ' both
  FVM_LDMC_OPCODE  = 24         ' preload macro                                 ' both
  FVM_WAITS_OPCODE = 25         ' wait for signal                               ' both
  FVM_POSTS_OPCODE = 26         ' post signal                                   ' both
  FVM_KILLW_OPCODE = 27         ' kill wait                                     ' both

  MM_LOAD_MACRO    = 1
  MM_SAVE_MACRO    = 2
  MM_DEL_MACRO     = 4

  BRKT_OUTPUT_MASK = $0F000000

  BRKT_NUM_PINS = 4
  BRKT_BUFFER_LEN = 480
  BRKT_BASE_PIN = 0
  BRKT_TIMING_LEN = 5

  BRKT_USE_MASK_CON = %00000000000_000000000_000000000_00_1
  BRKT_PIN_MASK_CON = %00000000000_000000000_000000000_11_0
  BRKT_START_MASK_CON = %00000000000_000000000_111111111_00_0
  BRKT_END_MASK_CON = %00000000000_111111111_000000000_00_0
OBJ

  eeprom  :  "Propeller Eeprom"

VAR

  long FVM_PWM_table[FVM_DEFAULT_NUM_OUTPUTS] ' PWM outputs

  long FVM_macros[FVM_DEFAULT_NUM_MACROS]     ' macro table (first two bytes is EEPROM address and last two bytes are RAM address)

  byte FVM_buffer[FVM_DEFAULT_BUFFER_SIZE]    ' input buffer

  byte FVM_inpdat_stack[FVM_DEFAULT_STACK_SIZE] ' input data stack

  byte FVM_macdat_stack[FVM_DEFAULT_STACK_SIZE] ' macro processor stack

  byte FVM_buffer_index                       ' index of buffer filled

  byte FVM_manager_request                    ' indicates request for macro manager

  byte FVM_manager_request_addr               ' the macro number requested for operation

  byte FVM_signal                             ' signal line

  byte FVM_status                             ' Contains status of various Firecracker systems

  long BRKT_requests[BRKT_NUM_PINS]           ' Space for write requests

  long BRKT_bufs[BRKT_NUM_PINS*BRKT_BUFFER_LEN/4]  ' Space for data buffers

  long BRKT_timings[BRKT_NUM_PINS*BRKT_TIMING_LEN] ' Space for pin timings

  byte BRKTA_buf_lock                          ' Store the buffer lock

  byte BRKTA_tim_lock                          ' Store the timing lock

PUB Start | n

  dira := $0000_FFFF     ' configure outputs for our purposes
  buf_addr := @FVM_buffer
  buf_ind_addr := @FVM_buffer_index
  fvm_status_addr := @FVM_status
  cognew(@recv_entry, 0)

  cognew(@hires, @FVM_PWM_table)

  pwm_base   := @FVM_PWM_table
  buf_base   := @FVM_buffer
  stack_base := @FVM_inpdat_stack
  bufind_ptr := @FVM_buffer_index
  macro_base := @FVM_macros
  signal_ptr := @FVM_signal

  cognew(@fvm_entry, 0)

  cognew(@fvm_entry, 0)                                 ' start new macro version


  if ((BRKT_buf_lock := locknew) == -1)
    outa &= outa ' should actually flag error
  if ((BRKT_tim_lock := locknew) == -1)
    outa &= outa
  brkt_req_base := @BRKT_requests
  brkt_buf_base := @BRKT_bufs
  brkt_buf_lock := @BRKTA_buf_lock
  brkt_tim_base := @BRKT_timings
  brkt_tim_lock := @BRKTA_tim_lock
  cognew(@Bottlerocket, 0)

PUB MacroManager | address, s, len1, len2, end

  repeat while (true)

    repeat while (!FVM_manager_request)    ' wait for a macro request

    if (FVM_manager_request == MM_LOAD_MACRO)           ' request to load macro

      address := FVM_macros[FVM_manager_request_addr]   ' find entry

      if (address & $FFFF)                              ' if it has a valid address in RAM then don't worry
        FVM_manager_request := 0                        ' notify
        next

      if (!address)
        FVM_manager_request := -1                       ' notify that macro is undefined
        waitcnt(8000 + cnt)                             ' wait
        FVM_manager_request := 0                        ' clear error
        next

      address >>=  16                                   ' get EEPROM address

      eeprom.ToRam(@len1, @len1+1,address)              ' read descriptor (alloc bit and 15-bit length)

      if (!(len1 & $10000))                             ' ensure it's allocated in EEPROM
        FVM_manager_request := -1
        waitcnt(8000 + cnt)
        FVM_manager_request := 0
        next

      len1 &= $FFFF                                     ' extract length

      s    := @FVM_macros                               ' selected RAM address

      end  := @FVM_macros + 0-1                         ' end address ' What constant should be added here?

      len2 := word[s]                                   ' length of RAM block

      repeat while (len2 and (s < end))                 ' search for empty RAM block
        s += len2 + 1
        len2 := word[s]

      if (!len2 and ((end - s) => len1))
        eeprom.ToRam(s, s+len1, address)
        FVM_manager_request := 0
        next

    elseif (FVM_manager_request == MM_SAVE_MACRO)
    elseif (FVM_manager_request == MM_DEL_MACRO)

    else
      next


DAT FireCrackerVM

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''   FVM_entry -
''      Start the Firecracker VM
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                        org     0
fvm_entry

fvm_process
''
'' All macros take arguments from the stack except for stack
'' operations themselves. Stack operations get their arguments
'' (things like length and data) from the data input (either buffer
'' or macro).
''
''
                        mov     stack_ptr, stack_base         ' load base address of stack
                        add     stack_ptr, stack_ind          ' calculate address of top of stack
                        mov     G1, #1                        ' minimum length of 1 byte
                        call    #fvm_getdata                  ' get opcode address
                        rdbyte  opcode, G0                    ' read opcode

fvm_eval_opcode
                        mov     G1, #fvm_opcode_table         ' load G1 with opcode table address
                        add     G1, opcode                    ' add opcode offset

                        jmp     G1                            ' jump to correct index into jump table
fvm_opcode_table                                              ' opcodes unavailable on the live input interpreter will be NOPs
                                                              ' OPCODE - AVAILABLE IN THIS FVM (Y/N)
                        jmp     #fvm_nop                      ' NOP   - Y
                        jmp     #fvm_push                     ' PUSH  - Y
                        jmp     #fvm_pop                      ' POP   - Y
                        jmp     #fvm_write                    ' WRITE - Y
                        jmp     #fvm_delay                    ' DELAY - Y
                        jmp     #fvm_inc                      ' INC   - Y
                        jmp     #fvm_dec                      ' DEC   - Y
                        jmp     #fvm_add                      ' ADD   - Y
                        jmp     #fvm_sub                      ' SUB   - Y
                        jmp     #fvm_NOP                      ' CMP   - N
                        jmp     #fvm_or                       ' OR    - Y
                        jmp     #fvm_and                      ' AND   - Y
                        jmp     #fvm_NOP                      ' TEST  - N
                        jmp     #fvm_not                      ' NOT   - Y
                        jmp     #fvm_swap                     ' SWAP  - Y
                        jmp     #fvm_dup                      ' DUP   - Y
                        jmp     #fvm_NOP                      ' IF    - N
                        jmp     #fvm_NOP                      ' JMP   - N
                        jmp     #fvm_NOP                      ' JMPR  - N
                        jmp     #fvm_defmc                    ' DEFMC - Y
                        jmp     #fvm_calmc                    ' CALMC - Y
                        jmp     #fvm_NOP                      ' RETMC - N
                        jmp     #fvm_savmc                    ' SAVMC - Y
                        jmp     #fvm_delmc                    ' DELMC - Y
                        jmp     #fvm_ldmc                     ' LDMC  - Y
                        jmp     #fvm_waits                    ' WAITS - Y
                        jmp     #fvm_posts                    ' POSTS - Y
                        jmp     #fvm_killw                    ' KILLW - Y
                        jmp     #fvm_btime                    ' BTIME - Y
                        jmp     #fvm_bdelt                    ' BDELT - Y
                        jmp     #fvm_bwrit                    ' BWRIT - Y
fvm_nop
''
'' FVM_NOP macro does absolutely nothing but waste time and space.
''
''
                        add     count, #1
                        jmp     #fvm_end_processing

fvm_push
''
'' FVM_PUSH macro is followed by a 2 byte length specifying how many
'' bytes that follow are to be pushed to the stack. The macro takes
'' a minimum of two bytes
''
''
                        rdbyte  G0, bufind_ptr                ' load filled index
                        mov     G7, G0                        ' load G7 with filled index
                        sub     G7, buf_proc                  ' subtract index to get length available

                        cmp     G7, #3                  wz,wc ' ? length available >= length requested
              if_b      jmp     #fvm_end_processing           ' N - we reloop and wait
                        mov     G0, buf_base
                        add     G0, buf_proc

                        rdbyte  G1, G0                        ' read length into G1
                        add     buf_proc, #1                  ' increment buffer
                        and     buf_proc, buffer_limit        ' ? wrap around

                        mov     G0, buf_base
                        add     G0, buf_proc
                        ' 2 time wasters
                        rdbyte  G2, G0                        ' read lower length byte
                        shl     G1, #8                        ' move to upper 8 bits
                        or      G1, G2                        ' construct length in G1

                        add     stack_ind, G1                 ' calculate new stack index
                        sub     G1, #1                  wc    ' adjust to relative length
              if_nc     cmp     stack_limit, stack_ind  wc    ' ? - (stack limit < stack index) OR (length == 0)
              if_c      jmp     #fvm_push_01                  ' Y - no data gets pushed

                        add     buf_proc, #1

fvm_push_00

                        rdbyte  G3, G0                        ' read next byte
                        sub     G1, #1                  wz    ' decrement counter
                        add     G0, #1                        ' go to next byte in buffer
                        wrbyte  G3, stack_ptr                 ' store in stack
                        add     stack_ptr, #1                 ' increment stack ptr
              if_nz     jmp     #fvm_push_00                  ' reloop for data length

fvm_push_01
                        tjz     G1, #fvm_end_processing       ' length of zero - lets go home
                        jmp     #fvm_stack_error              ' if length is not zero, then we have a stack error

fvm_pop
''
'' FVM_POP macro is followed by a length byte and specifies
'' how many bytes are popped from the stack. The pop simply decrements
'' the stack pointer by the length field.
''
''
                        mov     G1, #2                        ' ensure we have a length byte present
                        call    #fvm_getdata                  ' ^
                        add     G0, #1                        ' go to length byte
                        add     count, #2                     ' increment by 2
                        nop
                        nop
                        rdbyte  G0, G0                        ' read length into G0
                        cmpsub  stack_ind, G0           wc    ' compare and subtract if we can
              if_c      jmp     #fvm_end_processing           ' if index >= length popped, exit successful
                        jmp     #fvm_stack_error              ' otherwise, raise stack error


fvm_write
''
'' FVM_WRITE macro takes a 1 byte pin address and a 1 byte value.
'' It writes the 8-bit PWM value to any of the lower 16 pins
'' (pins 0-15). The PWM signal generated is not fixed duty cycle.
''
''
                        mov     G0, #2
                        call    #fvm_checkstack

                        rdbyte  G1, stack_ptr                 ' read pin number
                        mov     G2, pwm_base                  ' load pwm base
                        sub     stack_ptr, #1                 ' go to value
                        rdbyte  G3, stack_ptr                 ' read value into G3
                        and     G1, #$0F                      ' cap at 4 bits (16 pins)
                        shl     G1, #2                        ' multiply by 4 (pin table offset)

                        add     G2, G1                        ' go to offset in PWM table
                        mov     G0, G3                        ' copy value to G0
                        shl     G0, #8                        ' shift up 8 bits
                        or      G3, G0                        ' merge all 16-bits

                        mov     G0, G3                        ' repeat but with 16-bits
                        shl     G0, #16
                        or      G3, G0
                        nop                                   ' above process scales 8-bit to 32-bit PWM (0x01010101 * value)

                        wrlong  G3, G2                        ' store value in table
                        add     count, #1                     ' add to count
                        jmp     #fvm_end_processing           ' exit


fvm_delay
''
'' FVM_DELAY macro takes an unsigned 32-bit integer in micro-seconds.
'' minimum wait time of 1.2us for 0 and 1us specified. All other values
'' delay precisely.
''
''
                        mov     G0, #4
                        call    #fvm_checkstack
                        ' read byte by byte to avoid alignment issues
                        rdbyte  G1, stack_ptr                 ' read byte
                        shl     G1, #24                       ' shift up
                        add     stack_ptr, #1                 ' go to next byte
                        rdbyte  G2, stack_ptr                 ' read byte
                        shl     G2, #16                       ' shift up
                        add     stack_ptr, #1                 ' go to next byte
                        rdbyte  G3, stack_ptr                 ' read byte
                        shl     G3, #8                        ' shift up
                        add     stack_ptr, #1                 ' go to next byte
                        rdbyte  G4, stack_ptr                 ' read byte
                        or      G4, G3
                        or      G4, G2

                        or      G4, G1                        ' construct number in G4
                        sub     G4, #2                  wz,wc ' adjust remaining time
              if_b      jmp     #fvm_end_processing           ' if time < 2 we leave
                        mov     G0,#13                        ' otherwise, delay
fvm_delay_00                                                  ' fill in to 2us
                        djnz    G0,#fvm_delay_00

fvm_delay_01
                        sub     G4, #1                  wz,wc '
              if_b      jmp     #fvm_end_processing
                        mov     G0,#16
fvm_delay_02
                        djnz    G0,#fvm_delay_02
                        jmp     #fvm_delay_01




fvm_inc
''
'' FVM_INC macro simply takes the byte on the top of the stack
'' and increments it by 1.
''
''
                        mov     G0, #1
                        call    #fvm_checkstack

                        rdbyte  G0, stack_ptr                 ' load byte
                        add     G0, #1                        ' increment
                        add     count,#1
                        wrbyte  G0, stack_ptr                 ' store

                         jmp    #fvm_end_processing           ' exit

fvm_dec
''
'' FVM_DEC macro simply takes the byte on the top of the stack
'' and decrements it by 1.
''
''
                        mov     G0, #1
                        call    #fvm_checkstack

                        rdbyte  G0, stack_ptr                 ' load byte
                        sub     G0, #1                        ' decrement
                        add     count,#1
                        wrbyte  G0, stack_ptr                 ' store

                        jmp     #fvm_end_processing           ' exit

fvm_add
''
'' FVM_ADD macro pops the top two bytes on the stack, adds them
'' and finally pushes the sum back to the stack
''
''
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata                   ' ensure data is present before writing
                        add     G0, G1
                        wrbyte  G0, stack_ptr
                        add     count,#1
                        sub     stack_ind, #1

                        jmp     #fvm_end_processing
fvm_sub
''
'' FVM_SUB macro pops the top two bytes on the stack, subtracts
'' the first item pushed to the stack, from the second or
'' the (top) - (top - 1), and pushes the result back
'' to the stack
''
''
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        sub     G0, G1
                        wrbyte  G0, stack_ptr
                        add     count, #1
                        sub     stack_ind, #1

                        jmp     #fvm_end_processing

fvm_cmp
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        add     count, #1

                        cmp     G0, G1                  wz,wc
                        muxc    flags, #FVM_STATE_C
                        muxz    flags, #FVM_STATE_Z
                        jmp     #fvm_end_processing

fvm_or
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        or      G0, G1
                        wrbyte  G0, stack_ptr
                        add     count, #1
                        sub     stack_ind, #1

                        jmp     #fvm_end_processing

fvm_and
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        and     G0, G1
                        wrbyte  G0, stack_ptr
                        add     count, #1
                        sub     stack_ind, #1

                        jmp     #fvm_end_processing

fvm_test
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        add     count, #1

                        test    G0, G1                  wz,wc
                        muxc    flags, #FVM_STATE_C
                        muxz    flags, #FVM_STATE_Z
                        jmp     #fvm_end_processing

fvm_not
                        mov     G0, #1
                        call    #fvm_checkstack

                        rdbyte  G0, stack_ptr
                        xor     G0, #$FF
                        add     count, #1
                        wrbyte  G0, stack_ptr

                        jmp     #fvm_end_processing

fvm_swap
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        cmp     stack_ind, #2           wz,wc
                        rdbyte  G1, stack_ptr
              if_b      jmp     #fvm_nodata
                        add     count, #1
                        wrbyte  G0, stack_ptr
                        add     stack_ptr, #1
                        nop
                        wrbyte  G1, stack_ptr

                        jmp     #fvm_end_processing

fvm_dup
                        mov     G1, #2
                        call    #fvm_getdata

                        add     G0, #1                        ' go to length count byte
                        mov     G2, stack_ind                 ' copy stack index into G2

                        rdbyte  G1, G0                        ' read length of data and test for zero
                        cmpsub  G2, G1                  wc    ' ? enough data present
              if_nc     jmp     #fvm_nodata                   ' N error

                        add     stack_ptr, G2                 ' go to new upper limit
                        cmp     stack_ptr, #255         wz,wc ' ? over stack limit
              if_a      jmp     #fvm_stack_error              ' Y
                        sub     stack_ptr, G2                 ' N

                        add     G2, stack_base                ' Go to bottom of values to duplicate
                        add     count, #2
                        tjz     G1, #fvm_end_processing       ' leave if zero

fvm_dup_00
                        rdbyte  G0, G2                        ' read byte
                        add     G2, #1                        ' go to next byte to copy
                        add     stack_ind, #1                 ' increment stack index
                        wrbyte  G0, stack_ptr                 ' write byte in upper area
                        add     stack_ptr, #1                 ' go to next write location
                        djnz    G1, #fvm_dup_00               ' reloop if not zero

                        jmp     #fvm_end_processing


fvm_if
''
'' FVM_IF opcode conditionally executes the next command after it. The next command
'' must be a one byte opcode (which means not push, pop or dup)
'' If condition is true then the next opcode is executed. If the condition is false
'' then the next opcode is skipped
''
                        mov     G1,#1
                        call    #fvm_checkstack               ' ensure we have data available

                        shr     flags,#1                wc,nr ' set carry flag
              if_c      xor     flags,#%0011            wz,nr ' set zero flag depending on carry

                        rdbyte  G1, stack_ptr                 ' read condition code
              if_nc     xor     flags,#%0010            wz,nr ' set zero if no carry
                        mov     fvm_if_wa,fvm_if_op           ' copy operation into work area
                        shl     G1,#18                        ' shift to condition code

                        or      fvm_if_wa,G1                  ' set condition code
fvm_if_wa     if_never  jmp     #fvm_if_end                   ' work area where we construct condition test
fvm_if_op     if_never  jmp     #fvm_if_end                   ' jump to address (no cond code)
                                                              ' if the condition is true, we continue as normal.
                                                              ' if it is not true, then we skip the next command
                        add     count, #1                     ' add count for following opcode
fvm_if_end
                        add     count, #1                     ' add count for IF opcode
                        jmp     #fvm_end_processing           ' exit

fvm_jmp
                        mov     G0, #2
                        call    #fvm_checkstack

                        rdbyte  G1, stack_ptr                 ' read first byte
                        shl     G1, #8                        ' shift into upper bits
                        add     stack_ptr, #1                 ' increment to next byte
                        rdbyte  G2, stack_ptr                 ' read
                        or      G1, G2                        ' construct address in G1
                        mov     mac_ind, G1                   ' set new code point

                        xor     count, count                  ' zero count since we are resetting our program pointer
                        cmp     mac_ind, mac_len              ' ? index pointing past macro
              if_b      jmp     #fvm_end_processing           ' N
                        jmp     #fvm_err_processing           ' Y

fvm_jmpr
                        mov     G1, #2                        '
                        call    #fvm_checkstack               '

                        rdbyte  G1, stack_ptr
                        shl     G1, #8
                        add     stack_ptr, #1
                        rdbyte  G2, stack_ptr
                        or      G1, G2
                        shl     G1, #16                       ' shift relative number up 16 bits
                        sar     G1, #16                       ' sign extend value
                        adds    mac_ind, G1                   ' perform signed addition
              if_c      jmp     #fvm_err_processing           ' ensure we are still in memory limits
                        cmp     mac_len, mac_ind        wz,wc '
              if_ae     jmp     #fvm_err_processing           ' ensure we are within macro limit
                        mov     G0, macro                     ' load macro base
                        add     G0, mac_ind                   ' go to processing point
                        cmp     macro, G0               wz,wc '
              if_b      jmp     #fvm_err_processing           ' ensure we are within macro limit
                        jmp     #fvm_end_processing           ' leave

fvm_defmc
                        mov     G1, #2
                        call    #fvm_checkstack               ' ensure we have length

                        add     G0, #1
                        rdbyte  G1, G0                        ' read first byte
                        add     G1, #2                        ' include opcode and length
                        call    #fvm_getdata                  ' ensure we have all bytes available
                        add     count, G1                     ' account for bytes we are about to read
                        sub     G1, #4                  wz,wc ' adjust to relative length
              if_b      jmp     fvm_end_processing            ' leave if length is zero


fvm_calmc

fvm_retmc

fvm_savmc
fvm_delmc
fvm_ldmc
fvm_waits
                        rdbyte  G0, signal_ptr                ' read signal
                        cmp     stack_ind, #1           wz,wc ' ensure we have a signal to wait for
              if_b      jmp     #fvm_nodata                   '
                        rdbyte  G1, stack_ptr                 ' read signal we are waiting on
                        tjnz    G1, #fvm_waits_00             ' branch if the signal we are waiting for is not zero (any signal)
                        tjnz    G0, #fvm_waits_end            ' if signal is not zero, we end the wait
                        jmp     #fvm_end_processing           ' if it's still zero, leave
fvm_waits_00
                        cmp     G1, #$FF                wz,wc ' ? waiting for termination
                        cmpsub  G1, G0                        ' see if they are equal
              if_ne     tjz     G1, #fvm_waits_end            ' if signals are equal and not FF, we end wait
                        tjnz    G1, #fvm_end_processing       ' if not equal, we continue wait

                        or      flags, FVM_STATE_HLT          ' set halt state
                        jmp     #fvm_end_processing           ' go to halt

fvm_waits_end
                        add     count, #1                     ' increment past waits opcode
                        jmp     #fvm_end_processing           ' leave
fvm_btime
                        cmp     stack_ind, #(BRKT_TIMING_LEN*4) wc,wz ' ? available data
              if_b      jmp     #fvm_nodata                   ' N - leave
                        add     count, #(BRKT_TIMING_LEN*4)   ' adjust count
                        mov     G0, stack_ptr
                        mov     G7, #5                        ' Number of longs to construct
                        movd    :cache_val, #:timing_cache
:read_loop              call    #fvm_rdlong
:cache_val              mov     0-0, G1
                        add     :cache_val, :d_inc
                        add     G0, #1
                        djnz    G7, #:read_loop               ' The worst IO alignment possible
                        rdbyte  G1, G0                        ' Get pin #
                        and     G1, #%11                      ' Truncate to lowest 2 bits
                        shl     G1, #2                        ' Start to convert into a byte offset, first multiply by 4
                        mov     G7, #5                        ' Next multiply by 5, since each timing is 20 bytes long
:calc_offset            add     G2, G1                        ' Accumulate 5 G1s in G2
                        djnz    G7, #:calc_offset             ' Might be worthwhile to unroll this loop
                        add     G2, b_time_ptr
                        movd    :write_loop, :timing_cache
                        mov     G7, #5
:get_lock               lockset b_time_lck              wc
              if_c      jmp     #:get_lock
:write_loop             wrlong  0-0, G2
                        add     :write_loop, :d_inc
                        add     G2, #4
                        djnz    G7, #:write_loop              ' Also really shitty alignment
:rel_lock               lockclr  b_time_lck
                        jmp     #fvm_end_processing           ' leave
:d_inc        long      1<<9                                  ' value needed to incremend destination by 1
:timing_cache long      0[BRKT_TIMING_LEN]                    ' Local cache for timing
fvm_bdelt
'' Delta Format:
'' Big Endian Long
'' [Pin#:2][Index:9][Count:9][PAD:12]
'' Little Endian Long
'' [PAD:12][Count:9][Index:9][Pin#:2]
'' Followed by bytes to insert
                        cmp     stack_ind, #(4)         wc,wz ' ? available data
              if_b      jmp     #fvm_nodata                   ' N - leave
                        add     count, #(4)                   ' adjust count
                        call    #fvm_rdlong                   ' G1 Contains packed delta
                        mov     G2, G1
                        and     G2, #:delt_pinmask
                        mov     G3, b_data_ptr
:find_buf               add     G3, :buf_len
                        djnz    G2, #:find_buf
                        mov     G2, G1
                        and     G2, :delt_indmask
                        shr     G2, #(2+9)
                        add     G3, G2                        ' G3 contains start address of writes
                        shr     G1, #(9+2)
                        mov     G0, stack_ind
:get_lock               lockset b_data_lck              wc
              if_c      jmp  #:get_lock
:write_loop '' TODO: Optimize to write longs
                        rdbyte  G2, G0
                        add     G0, #1
                        wrbyte  G2, G3
                        add     G3, #1
                        djnz    G1, #:write_loop
:rel_lock               lockclr b_data_lck
                        jmp     #fvm_end_processing           ' leave
:delt_pinmask long      %000000000000_000000000_000000000_11
:delt_indmask long      %000000000000_000000000_111111111_00
:delt_cntmask long      %000000000000_111111111_000000000_00
:buf_len      long      BRKT_BUFFER_LEN
fvm_bwrit
                        cmp     stack_ind, #(4)         wc,wz ' ? available data
              if_b      jmp     #fvm_nodata                   ' N - leave
                        add     count, #(4)                   ' adjust count
                        call    #fvm_rdlong                   ' G1 now contains our pre-packed req
                        mov     G2, G1
                        and     G2, :pin_mask
                        '' shr  G2, #1 we shift left in a moment, so this is unneeded
                        shl     G2, #(2-1)                    ' multiply by 4
                        add     b_req_ptr, G2
:get_lock               lockset b_req_lck               wc
              if_c      jmp     #:get_lock
                        wrlong  G1, b_req_ptr
                        sub     b_req_ptr, G2
:rel_lock               lockclr b_req_lck
                        jmp     #fvm_end_processing           ' leave
:pin_mask     long      BRKT_PIN_MASK_CON
fvm_posts
                        rdbyte  G0, stack_ptr                 ' read signal to post
                        cmp     stack_ind, #1           wz,wc ' ensure we have data
              if_b      jmp     #fvm_nodata                   '
                        wrbyte  G0, signal_ptr                ' post

fvm_killw               ' I dont know what I was planning with this kill wait. I'll leave it for if anyone gets an idea.

fvm_rdlong
'' Read the big-endian long that is on the stack into G1
'' G2-G4 are FUBAR after
                        rdbyte  G1, stack_ptr                 ' Most significant byte
                        shl     G1, #24
                        add     stack_ptr, #1
                        rdbyte  G2, stack_ptr                 ' Second MSB
                        shl     G2, #16
                        add     stack_ptr, #1
                        rdbyte  G3, stack_ptr                 ' Third MSB
                        shl     G3, #8
                        add     stack_ptr, #1
                        rdbyte  G4, stack_ptr                 ' LSB
                        add     G1, G2
                        add     G1, G3
                        add     G1, G4
                        sub     stack_ptr, #4
fvm_rdlong_ret          ret

fvm_checkstack
'' FVM_checkstack
''    G0 contains length of data requested
''    if this returns then you got the green light

                        cmp     stack_ind, G0           wz,wc ' ? - we have enough data
fvm_checkstack_ret
              if_be     ret                                   ' Y
                        jmp     #fvm_nodata

fvm_getdata             ' gets data from either buffer or macro area

'' FVM_getdata -
''    G1 should contain length of data requested upon entry
''    G0 will contain the address of first byte if data is available
''    All G2-G4 will be FUBAR
''
''    HUB access is available immidiately upon return from FVM_getdata
''
                                                              ' N - get data from buffer
                        rdbyte  G0, bufind_ptr                ' load filled index

                        mov     G7, G0                        ' load G7 with filled index
                        sub     G7, buf_proc                  ' subtract index to get length available

                        cmp     G7, G1                  wz,wc ' ? length available >= length requested (save result for later)
                        mov     G0, buf_base                  ' load G0 with buffer pointer
                        add     G0, buf_proc                  ' go to next process index
fvm_getdata_ret
              if_ae     ret                                   ' if length available >= length requested, return
                        xor     count, count                  ' zero count (no data follow through)
                        jmp     #fvm_end_processing           ' otherwise end processing


fvm_nodata              ' not enough data is on the stack
fvm_stack_error         ' stack overflow/underflow
fvm_err_processing
fvm_end_processing
                        add     buf_proc, count               ' add count to buffer processed index
                        and     buf_proc, buffer_limit        ' cap at limit (2^n - 1)
                        xor     count, count                  ' zero count
                        jmp     #fvm_process                  ' reloop


stack_limit   long      FVM_DEFAULT_STACK_SIZE-1
buffer_limit  long      FVM_DEFAULT_BUFFER_SIZE-1

' global resources
pwm_base      long      0       ' PWM table
macro_base    long      0       ' start of macro address table
signal_ptr    long      0       ' FVM signal channel

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' general registers
G0            long      0
G1            long      0
G2            long      0
G3            long      0
G4            long      0
G5            long      0
G6            long      0
G7            long      0
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' VM components
opcode        long      0       ' current opcode processed
count         long      0       ' number of bytes processed
flags         long      0       ' internal VM flags
' stack related variables
stack_base    long      0       ' start of data stack
stack_ind     long      0       ' current index in stack
stack_ptr     long      0       ' calculated at the start of each process
' interpreter data source
buf_base      long      0       ' buffer ptr
bufind_ptr    long      0       ' buffer index pointer
buf_proc      long      0       ' index of buffer processed
' macro processor data source
macro         long      0       ' start of macro (not including header)
mac_len       long      0       ' length of macro
mac_ind       long      0       ' macro index
' brkt addresses
b_time_lck    long      0
b_time_ptr    long      0
b_data_lck    long      0
b_data_ptr    long      0
b_req_lck     long      0
b_req_ptr     long      0
                        FIT

CON

  FVM_STATE_C   = %0000_0001    ' C flag
  FVM_STATE_Z   = %0000_0010    ' Z flag
  FVM_STATE_HLT = %0000_0100    ' halt state

CON
''
'' FireCracker SPI reciever -
''

  ctr_posedge = %01010
  ctr_negedge = %01110

  spi_clk  = 18                 ' clock pin
  spi_mosi = 19                 ' master out / slave in
  spi_miso = 20                 ' master in / slave out
  spi_cs   = 21                 ' chip select
  i2c_sda  = 16                 ' I2C data pin
  i2c_scl  = 17                 ' I2C clock pin
  i2c_addr = $11
  i2c_10bit = %000011110
  i2c_msbits = %000000111

DAT StartRecv
                        org     0
recv_entry '' I don't like how this is done
                        andn    dira, recv_mask ' Set pins to input
detect_protocol
i2c_entry
                        '' !!!!!!!!!!!!!!!!!!!!!!  SKETCHY STUFF BELOW
                        '' Bit banging i2c is not fun. I think we can
                        '' do it though. If we need to do any sort of
                        '' clock stretching, this is going to get nasty.
                        '' as it stands, this is gross
                        andn    outa, i2c_mask ' set outputs to pull down when enabled
                        movs    ctra, i2c_sda  ' set ctra to monitor sda
                        movs    ctrb, i2c_scl  ' set ctrb to monitor scl
                        mov     frqa, #1
                        mov     frqb, #1
i2c_start '' needs to be fixed to watch for edges
                        movi    ctra, ctr_negedge
                        movi    ctrb, ctr_negedge
                        andn    dira, i2c_mask               ' release scl and sda
:zero_ctr
                        waitpeq i2c_mask, i2c_mask           ' wait for both pins to rise
                        mov     phsa, #0
                        mov     phsb, #0
:sda_wait               tjz     phsa, #:sda_wait             ' Wait for sda to fall
                        test    ina, i2c_sclmask          wz ' scl should still be high
              if_z      jmp     #:zero_ctr                   ' If it's not, bail
:scl_wait               tjz     phsb, #:scl_wait             ' Wait for scl to fall
i2c_addr_frame
                        mov     buf_local, #0
                        call    #i2c_frame
                        ror     buf_local, #3
                        cmp     buf_local, #i2c_10bit     wz ' check addressing type
                        rol     buf_local, #3
              if_nz     jmp     #i2c_check_addr              ' use 7 bit address only if highest 5 bits not match 11110
                        and     buf_local, #i2c_msbits       ' save lowest 3 bits
                        call    #i2c_frame                   ' get another 8 bits
i2c_check_addr
                        cmp     buf_local, i2c_addrmask   wz ' check if address matches
              if_nz     andn    outa, i2c_sdamask            ' Let sda rise(NACK) if master is not trying to talk to us
              if_nz     jmp     #i2c_start                   ' If master not talking to us, wait for another start
                        call    #i2c_frame_end
i2c_check_rw
                        test    buf_local, #1             wz ' Z is set for write
              if_nz     jmp     #i2c_read
i2c_write
                        mov     buf_local, #0
                        call    #i2c_frame                   ' Grab next byte
                        wrbyte  buf_local, buf_addr          ' write it to main memory
                        add     buf_addr, #1
                        add     buf_ind, #1
                        wrbyte  buf_ind, buf_ind_addr        ' write the index we last wrote to
                        or      dira, i2c_sdamask            ' ACK the transfer
                        call    #i2c_frame_end               ' Deal with the frame end
                        jmp     #i2c_write                   ' If frame end did not jump, more data
i2c_read
                        mov     i2c_frame_ind, #8
                        movi    ctrb, ctr_negedge
                        rdbyte  buf_local, fvm_status_addr
                        shl     buf_local, #(3*8)
:bit_loop
                        rol     buf_local, #1             wc
              if_c      or      dira, i2c_sdamask
              if_nc     andn    dira, i2c_sdamask
                        mov     phsb, #0
:scl_fall               tjz     phsb, #:scl_fall     'wait for scl to fall
                        djnz    i2c_frame_ind, #:bit_loop
                        or      dira, i2c_sdamask            ' pull down for ack
                        call    #i2c_frame_end
                        jmp     #i2c_read                    ' Master wants us to tell more of the same
i2c_frame
                        mov     i2c_frame_ind, #8
                        movi    ctrb, ctr_posedge
                        andn    dira, i2c_mask               ' let scl and sda rise
:bit_loop               mov     phsb, #0
:scl_rise               tjz     phsb, #:scl_rise
                        test    ina, i2c_sdamask          wz ' Check for high or low
              if_nz     add     buf_local, #1
                        shl     buf_local, #1
                        djnz    i2c_frame_ind, #:bit_loop
                        or      dira, i2c_mask               ' Pull clock and data down
i2c_frame_ret           ret

i2c_frame_end
                        movi    ctrb, ctr_negedge
                        mov     phsb, #0
                        andn    outa, i2c_sclmask            ' allow clock to rise
:scl_fall               tjz     phsb, #:scl_fall             ' Wait for scl to fall
                        andn    outa, i2c_mask               ' Allow all pins to rise
                        movi    ctrb, ctr_posedge            ' Care if clock rises , indicates either end or repeat start
                        mov     phsb, #0
                        movi    ctra, ctr_negedge            ' Care if data falls, start of next frame
                        mov     phsa, #0
                        waitpeq i2c_sdamask, i2c_sdamask     ' sda will always rise after a frame
:scl_rise               tjz     phsb, #:sda_fall             ' Check if clock has already risen
                        jmp     #i2c_start                   ' If it did, start looking for another start
:sda_fall               tjnz    phsa, #i2c_frame_end_ret     ' If sda falls, master is starting next frame
                        jmp     #:scl_rise
i2c_frame_end_ret       ret

fvm_status_addr long    0

recv_mask     long      i2c_mask

i2c_addrmask  long      i2c_addr << 1
i2c_sdamask   long      1 << i2c_sda
i2c_sclmask   long      1 << i2c_scl
i2c_mask      long      i2c_sdamask | i2c_sclmask
i2c_startmask long      !i2c_sdamask | i2c_sclmask
i2c_frame_ind long      0  ' index into current i2c frame

buf_local     long      0  ' local scratch buffer
buf_addr      long      0  ' the next address in main ram we plan on writing to
buf_ind       long      0  ' how far we are into the buffer
buf_ind_addr  long      0  ' the address of the copy of buf_ind in main ram

              FIT

DAT PWMHandler

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''   hires - PWM driver
''   Code copied from PropPWM and modified for 16 outputs instead of all 32
''   allows for easier management of memory, as well as half the memory use
''
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

' 32-bit PWM with 384.62 kHz switching frequency (94% faster than 32 pins)
' Method uses the carry flag to proportion the on-time of the duty cycles
' Each cycle, the duty cycle of the pin is added to a counter for that
' pin. When this generates a carry, the pin is set to high, otherwise it is low.
' This means the a true duty cycle, that accurately represents the value of the
' signal takes whatever number of cycles it takes to accurately represent that
' the value in fractional form.


                        org     0
hires
                        mov     pinTableBase,par             ' Move in the HUBRAM address of the pin values table
                        mov     counter,#16                  ' Counter used to generate the table of pin HUBRAM addresses
                        mov     dutyReg,#pinAddress04

' Initializes a table containing the HUBRAM address of every pin
' in order to avoid having to increment a reference address each
' time we have to access the table, thus increasing speed.

setup
                        movd    tableEntry, dutyReg
                        add     dutyReg,#1
tableEntry
                        add     0000,pinTableBase
                        djnz    counter, #setup

dutyStart
                        ' Only update 12V pins - 5V pins (0-3) are reserved for BRKT

                        rdlong  dutyReg,pinAddress04          ' Read the value of the zero-th pin into the dutyReg
                        add     dutyTable04,dutyReg       wc  ' Add to the accumulator
              if_c      or      buffer,pinMask04              ' If a carry was generated, set the pin to high
                                                              ' repeat this process, each time going to the next pin, and next
                        rdlong  dutyReg,pinAddress05
                        add     dutyTable05,dutyReg       wc
              if_c      or      buffer,pinMask05

                        rdlong  dutyReg,pinAddress06
                        add     dutyTable06,dutyReg       wc
              if_c      or      buffer,pinMask06

                        rdlong  dutyReg,pinAddress07
                        add     dutyTable07,dutyReg       wc
              if_c      or      buffer,pinMask07

                        rdlong  dutyReg,pinAddress08
                        add     dutyTable08,dutyReg       wc
              if_c      or      buffer,pinMask08

                        rdlong  dutyReg,pinAddress09
                        add     dutyTable09,dutyReg       wc
              if_c      or      buffer,pinMask09

                        rdlong  dutyReg,pinAddress0A
                        add     dutyTable0A,dutyReg       wc
              if_c      or      buffer,pinMask0A

                        rdlong  dutyReg,pinAddress0B
                        add     dutyTable0B,dutyReg       wc
              if_c      or      buffer,pinMask0B

                        rdlong  dutyReg,pinAddress0C
                        add     dutyTable0C,dutyReg       wc
              if_c      or      buffer,pinMask0C

                        rdlong  dutyReg,pinAddress0D
                        add     dutyTable0D,dutyReg       wc
              if_c      or      buffer,pinMask0D

                        rdlong  dutyReg,pinAddress0E
                        add     dutyTable0E,dutyReg       wc
              if_c      or      buffer,pinMask0E

                        rdlong  dutyReg,pinAddress0F
                        add     dutyTable0F,dutyReg       wc
              if_c      or      buffer,pinMask0F

                        mov     dira,buffer                     ' Set those pins to output
                        mov     outa,buffer                     ' Write high to the pins set
                        xor     buffer,buffer                   ' Clear buffer for next cycle
                        jmp     #dutyStart                      ' Go to next cycle

' Pin mask table used to set pins
pinMask04     long      %0000_0000_0000_0000_0000_0000_0001_0000
pinMask05     long      %0000_0000_0000_0000_0000_0000_0010_0000
pinMask06     long      %0000_0000_0000_0000_0000_0000_0100_0000
pinMask07     long      %0000_0000_0000_0000_0000_0000_1000_0000
pinMask08     long      %0000_0000_0000_0000_0000_0001_0000_0000
pinMask09     long      %0000_0000_0000_0000_0000_0010_0000_0000
pinMask0A     long      %0000_0000_0000_0000_0000_0100_0000_0000
pinMask0B     long      %0000_0000_0000_0000_0000_1000_0000_0000
pinMask0C     long      %0000_0000_0000_0000_0001_0000_0000_0000
pinMask0D     long      %0000_0000_0000_0000_0010_0000_0000_0000
pinMask0E     long      %0000_0000_0000_0000_0100_0000_0000_0000
pinMask0F     long      %0000_0000_0000_0000_1000_0000_0000_0000

pinAddress04     long      16
pinAddress05     long      20
pinAddress06     long      24
pinAddress07     long      28
pinAddress08     long      32
pinAddress09     long      36
pinAddress0A     long      40
pinAddress0B     long      44
pinAddress0C     long      48
pinAddress0D     long      52
pinAddress0E     long      56
pinAddress0F     long      60

dutyTable04     long      0
dutyTable05     long      0
dutyTable06     long      0
dutyTable07     long      0
dutyTable08     long      0
dutyTable09     long      0
dutyTable0A     long      0
dutyTable0B     long      0
dutyTable0C     long      0
dutyTable0D     long      0
dutyTable0E     long      0
dutyTable0F     long      0

dutyReg       long      0    ' Register that duty cycle gets read into
counter       long      0    ' Counter for generating the address table
pinTableBase  long      0    ' HUBRAM address of pin addresses
buffer        long      0    ' Bitmask buffer
                        FIT


CON
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' Bottlerocket serial addressable LED driver
''
'' Bottlerocket maintains four 480-byte buffers of data used to feed
'' one pin each. It runs on its own cog and consumes write requests
'' consisting of a buffer number and a start and end index in that
'' buffer.
''
'' The buffers are locked while data is being copied to the local buffer
''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''


DAT Bottlerocket
                        org       0
brkt_00
wait_req
                        mov     brkt_reg_a, brkt_req_base
                        mov     brkt_reg_b, brkt_last_req_ind
:loop
:get_lock               lockset brkt_req_lock               wc
              if_c      jmp     #:get_lock
:read                   rdlong  brkt_req_cur, brkt_reg_a
                        test    brkt_req_cur, brkt_use_mask wz
              if_nz     wrlong  brkt_zero, brkt_reg_a
:rel_lock               lockclr brkt_req_lock
              if_nz     jmp     #copy_buf
                        add     brkt_reg_a, #4
                        djnz    brkt_reg_b, #:loop
                        mov     brkt_last_req_ind, #3
                        jmp     #wait_req
copy_buf
                        mov     brkt_reg_b, brkt_req_cur        ' store start index of copy
                        and     brkt_reg_b, brkt_start_mask
                        shr     brkt_reg_b, #(1+2)
                        mov     brkt_reg_a, brkt_req_cur        ' store end index of copy
                        and     brkt_reg_a, brkt_end_mask
                        shr     brkt_reg_a, #(1+2+9)
                        add     brkt_reg_a, #1
:find_length            cmpsub  brkt_reg_a, brkt_reg_b      wc
              if_nc     add     brkt_reg_a, brkt_buf_len
              if_nc     jmp     #:find_length
                        mov     brkt_reg_d, brkt_reg_a          ' Make a copy for determining # bits and end index
                        test    brkt_reg_a, #3              wz  ' check if we're going to need to round up
                        shr     brkt_reg_a, #2                  ' divide by 4
              if_nz     add     brkt_reg_a, #1                  ' round up so we don't miss the last few bytes
                        mov     brkt_reg_c, brkt_req_cur        ' Calculate start address of copy
                        and     brkt_reg_c, brkt_pin_mask
                        shr     brkt_reg_c, #1              wz
:loop
              if_nz     add     brkt_reg_b, brkt_buf_len        ' Z flag needed to prevent adding 512 on pin0
                        djnz    brkt_reg_c, #:loop          wz  ' Clears z flag if we jump back
                        add     brkt_reg_b, brkt_buf_base       ' brkt_reg_b now contains base address of this buffer in hubram
                        mov     brkt_reg_e, brkt_reg_b          ' brkt_reg_e now contains base address of this buffer in hubram
                        add     brkt_reg_b, brkt_reg_d          ' brkt_reg_b now contains last index we want to copy from
                        mov     brkt_reg_c, #brkt_buf_cur       ' brkt_reg_c now contains base address of buffer in cogram
                        add     brkt_reg_c, brkt_reg_a          ' brkt_reg_c now contains last address of buffer in cogram
get_buf
:get_lock               lockset brkt_buf_lock               wc  ' Get the lock for all of the buffers
              if_c      jmp     #:get_lock
:loop '' This number of unrolls gives us 1/2 efficiency on io access. See spreadsheet
:overhead
                        cmp     brkt_reg_b, brkt_reg_e      wc, wz' Check if we are trying to read from below our buffer
              if_b      add     brkt_reg_b, brkt_buf_len        ' If so, start reading from the end
                        movd    :read_0, brkt_reg_c             ' Prep the rd with the destination
                        sub     brkt_reg_c, #1                  ' decrement target address by 1
:read_0                 rdlong  0-0, brkt_reg_b                 ' grab long from hubram
                        sub     brkt_reg_b, #4                  ' decrement source address by 4
                        djnz    brkt_reg_a, #:loop              ' loop if we need more longs
:rel_lock               lockclr brkt_buf_lock                   ' Release it
get_tim
                        lockset brkt_tim_lock               wc  ' Get lock on timings
              if_c      jmp     #get_tim
                        mov     brkt_reg_a, brkt_tim_base       ' brkt_reg_a now contains address of timing[0]
                        mov     brkt_reg_b, brkt_req_cur
                        and     brkt_reg_b, brkt_pin_mask   wz  ' z flag will be set if not writing pin 0
                        shr     brkt_reg_b, #1
:loop
            if_nz       add     brkt_reg_a, #(BRKT_TIMING_LEN)  ' brkt_reg_a will contain address of correct timing at end of loop
            if_nz       djnz    brkt_reg_b, #:loop          wz
                        rdlong  brkt_t1h, brkt_reg_a            ' pull brkt_t1h, in clocks, from hubram
                        add     brkt_reg_a, #4                  ' brkt_reg_a now contains address of correct brkt_t1l
                        rdlong  brkt_t1l, brkt_reg_a            ' pull brkt_t1l, in clocks, from hubram
                        add     brkt_reg_a, #4                  ' brkt_reg_a now contains address of correct brkt_t0h
                        rdlong  brkt_t0h, brkt_reg_a            ' pull brkt_t0h, in clocks, from hubram
                        add     brkt_reg_a, #4                  ' brkt_reg_a now contains address of correct brkt_t0l
                        rdlong  brkt_t0l, brkt_reg_a            ' pull brkt_t0l, in clocks, from hubram
                        add     brkt_reg_a, #4                  ' brkt_reg_a now contains address of correct brkt_tReset
                        rdlong  brkt_tReset, brkt_reg_a         ' pull brkt_tReset, in clocks, from hubram
write_buf
                        shl     brkt_reg_d, #3                  ' multiply by 8 to get in # bits to send
                        mov     brkt_reg_e, brkt_reg_d          ' brkt_reg_e contains number of bits to send
                        sub     brkt_reg_e, #32                 ' brkt_reg_e will tell us to refresh data after sending 32 bits
                        mov     brkt_reg_b, brkt_req_cur        '
                        and     brkt_reg_b, brkt_start_mask     '
                        shr     brkt_reg_b, #(1+2)              '
                        and     brkt_reg_b, #3              wz  ' brkt_reg_b contains # bytes garbage
                        mov     brkt_reg_c, brkt_reg_b          ' brkt_reg_c contains # bytes garbage
                        shl     brkt_reg_c, #3                  ' brkt_reg_c contains # bits garbage
                        add     brkt_reg_e, brkt_reg_c          ' pretend to have sent our garbage
:discard_garbage        shl     brkt_buf_cur, #8                ' shift garbage bytes out from brkt_buf_cur
                        djnz    brkt_reg_b, #:discard_garbage
                        mov     brkt_reg_a, brkt_req_cur        ' Find which pin to output on
                        and     brkt_reg_a, brkt_pin_mask       '
                        shr     brkt_reg_a, #1                  '
                        add     brkt_reg_a, #BRKT_BASE_PIN      '
                        mov     brkt_reg_b, #1
                        shr     brkt_reg_b, brkt_reg_a          ' brkt_reg_b now contains pin mask
                        andn    outa, brkt_reg_b                ' Set output low
                        or      dira, brkt_reg_b                ' Toggle pin to output
                        mov     brkt_reg_c, #brkt_buf_cur       ' Prep registers
                        mov     brkt_reg_a, brkt_buf_cur        ' Prep registers
:bit_loop
                        cmp     brkt_reg_e, brkt_reg_d      wz  ' brkt_reg_e = index to get next long at
              if_z      movs    :read_next, brkt_reg_c          ' Tell our shift to use the next long
              if_z      add     brkt_reg_c, #1                  ' Increment our pointer into the buffer
              if_z      sub     brkt_reg_e, #32                 ' refresh data in 32 bits
:read_next              shl     brkt_reg_a, #1              wc  ' Check if we have a 1 or a zero bit
                        mov     brkt_wait_until, cnt            ' prepare wait register
              if_c      add     brkt_wait_until, brkt_t1h       ' prep to wait for brkt_t1h if shifted out 1
              if_nc     add     brkt_wait_until, brkt_t0h       ' prep wait for brkt_t0h if shifted out 0
                        or      outa, brkt_reg_b                ' output high
              if_c      waitcnt brkt_wait_until, brkt_t1l       ' wait for brkt_t1h, prep to wait for brkt_t1l
              if_nc     waitcnt brkt_wait_until, brkt_t0l       ' wait for brkt_t0h, prep to wait for brkt_t0l
                        andn    outa, brkt_reg_b                ' output low
                        waitcnt brkt_wait_until, brkt_tReset    ' wait for low time, prep for reset time if needed
                        djnz    brkt_reg_d, #:bit_loop          ' check if we've written all of our bits
                        waitcnt brkt_wait_until, #0             ' wait for reset time
write_done
                        jmp     wait_req

brkt_zero          long     0

brkt_use_mask      long     BRKT_USE_MASK_CON
brkt_pin_mask      long     BRKT_PIN_MASK_CON
brkt_start_mask    long     BRKT_START_MASK_CON
brkt_end_mask      long     BRKT_END_MASK_CON

brkt_buf_len       long     BRKT_BUFFER_LEN

brkt_req_lock      long     0
brkt_req_base      long     0
brkt_buf_lock      long     0
brkt_buf_base      long     0
brkt_tim_lock      long     0
brkt_tim_base      long     0

brkt_buf_cur       long     0[BRKT_BUFFER_LEN/4]
brkt_req_cur       long     0
brkt_last_req_ind  long     3

brkt_wait_until    long     0
brkt_t1h           long     0
brkt_t1l           long     0
brkt_t0h           long     0
brkt_t0l           long     0
brkt_tReset        long     0

brkt_reg_a         long     0
brkt_reg_b         long     0
brkt_reg_c         long     0
brkt_reg_d         long     0
brkt_reg_e         long     0
              FIT
