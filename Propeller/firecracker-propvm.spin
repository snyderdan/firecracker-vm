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
'' |   24-31   |   unused    |
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
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

CON

  FVM_DEFAULT_STACK_SIZE  = 256
  FVM_DEFAULT_BUFFER_SIZE = 256
  FVM_DEFAULT_NUM_MACROS  = 256
  FVM_DEFAULT_NUM_OUTPUTS = 16

  FVM_OUTPUT_MASK = $FFFF0000

  FVM_NOP_OPCODE   = 0
  FVM_PUSH_OPCODE  = 1
  FVM_POP_OPCODE   = 2
  FVM_WRITE_OPCODE = 3
  FVM_DELAY_OPCODE = 4
  FVM_INC_OPCODE   = 5
  FVM_DEC_OPCODE   = 6
  FVM_ADD_OPCODE   = 7
  FVM_SUB_OPCODE   = 8
  FVM_CMP_OPCODE   = 9
  FVM_OR_OPCODE    = 10
  FVM_AND_OPCODE   = 11
  FVM_TEST_OPCODE  = 12
  FVM_NOT_OPCODE   = 13
  FVM_SWAP_OPCODE  = 14
  FVM_DUP_OPCODE   = 15
  FVM_IF_OPCODE    = 16
  FVM_JUMP_OPCODE  = 17         ' jump
  FVM_JMPR_OPCODE  = 18         ' jump relative
  FVM_DEFMC_OPCODE = 19
  FVM_CALMC_OPCODE = 20
  FVM_RETMC_OPCODE = 21
  FVM_DLAYM_OPCODE = 22
  FVM_SAVMC_OPCODE = 23
  FVM_DELMC_OPCODE = 24

                                     
VAR

  long FVM_PWM_table[FVM_DEFAULT_NUM_OUTPUTS] ' PWM outputs          

  word FVM_macros[FVM_DEFAULT_NUM_MACROS]     ' macro addresses (words allocated first)

  byte FVM_buffer[FVM_DEFAULT_BUFFER_SIZE]    ' input buffer

  byte FVM_data_stack[FVM_DEFAULT_STACK_SIZE] ' Data stack that operations are performed on

  byte FVM_allocsz[FVM_DEFAULT_NUM_MACROS]    ' size of each macro

  byte FVM_buffer_index                       ' index of buffer filled

  byte FVM_heap                               ' just a reference for heap address
  
PUB Start

  dira := $0000_FFFF | spi_misomask | i2c_sdamask       ' configure outputs for our purposes   

  cognew(@hires, @FVM_PWM_table)
  cognew(@fvm_entry, @FVM_PWM_table)

DAT FireCrackerVM

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
''   FVM_entry -
''      Start the Firecracker VM 
''
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

fvm_entry
                        '
                        ' load pointer from par and calculate addresses for variables
                        '

                        mov     pwm_base,   par               ' PWM table
                        mov     macro_base, pwm_base               
                        add     macro_base, #64               ' macro table                                            
                        mov     buf_base,   macro_base
                        add     buf_base,   num512            ' buffer pointer
                        mov     stack_base, buf_base
                        add     stack_base, #256              ' stack pointer
                        mov     alloc_base, stack_base
                        add     alloc_base, #256              ' allocated size table
                        mov     bufin_ptr,  alloc_base      
                        add     bufin_ptr,  #256              ' index filled pointer
                        mov     heap_base,  bufin_ptr
                        add     heap_base,  #1                ' heap pointer

fvm_process
''
'' All macros take arguments from the stack except for stack
'' operations themselves. Stack operations get their arguments
'' (things like length and data) from the data input (either buffer
'' or macro).
''
''
                        mov     stack_ptr, stack_base
                        add     stack_ptr, stack_ind
                        mov     G1, #1                        ' get opcode address 
                        call    #fvm_getdata
                        rdbyte  opcode, G0                    ' read opcode
                        
fvm_eval_opcode
                        mov     G1, #fvm_opcode_table         ' load G1 with opcode table address
                        add     G1, opcode                    ' add opcode offset

                        jmp     G1                            ' jump to correct index into jump table
fvm_opcode_table                                              ' HUB access is aligned on first instruction upon entering each table entry
                        jmp     #fvm_nop                
                        jmp     #fvm_push
                        jmp     #fvm_pop
                        jmp     #fvm_write
                        jmp     #fvm_delay
                        jmp     #fvm_inc
                        jmp     #fvm_dec
                        jmp     #fvm_add
                        jmp     #fvm_sub
                        jmp     #fvm_cmp
                        jmp     #fvm_or
                        jmp     #fvm_and
                        jmp     #fvm_test
                        jmp     #fvm_not
                        jmp     #fvm_swap
                        jmp     #fvm_dup
                        jmp     #fvm_if
                        jmp     #fvm_jmp
                        jmp     #fvm_defmc
                        jmp     #fvm_calmc
                        jmp     #fvm_dlaym                

fvm_nop
''
'' FVM_NOP macro does absolutely nothing but waste time and space.
'' 
''   
                        add     count, #1
                        jmp     #fvm_end_processing

fvm_push
''
'' FVM_PUSH macro is followed by a length byte specifying how many
'' bytes that follow are to be pushed to the stack. The macro takes
'' a minimum of two bytes
''
''   
                        mov     G1, #2                        ' add push opcode and length byte
                        call    #fvm_getdata                  ' ensure data is available
                        add     G0, #1                        ' go to length byte
                        nop
                        nop
                        nop
                        rdbyte  G1, G0                        ' read length into G1 
                        
                        add     G1, #2                        ' add push opcode and length byte
                        call    #fvm_getdata                  ' ensure data is available
                        add     G0, #2                        ' go to data in buffer
                        add     count, G1                     ' update count of processed bytes
                        sub     G1, #2                  wz    ' adjust G1 to data length
                        mov     G2, stack_base                ' load stack pointer
                        
                        add     G2, stack_ind                 ' add stack index
                        add     stack_ind, G1                 ' update stack index
                        and     stack_ind, #$FF               ' cap at 8 bits
              if_z      jmp     #fvm_end_processing           ' return if length is zero

fvm_push_00
                        nop
                        rdbyte  G3, G0                        ' read next byte
                        sub     G1, #1                  wz    ' decrement counter
                        add     G0, #1                        ' increment data buffer pointer
                        wrbyte  G3, G2                        ' store in stack
                        add     G2, #1                        ' go to next stack value
              if_nz     jmp     #fvm_push_00                  ' reloop for data length 

                        jmp     #fvm_end_processing           ' exit
                        
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
                        sub     stack_ind, G0                 ' subtract our index to pop
                        and     stack_ind, #$FF               ' cap at 8 bits
                        jmp     #fvm_end_processing           ' exit
                        
                        
fvm_write
''
'' FVM_WRITE macro takes a 1 byte pin address and a 1 byte value.
'' It writes the 8-bit PWM value to any of the lower 16 pins
'' (pins 0-15). The PWM signal generated is not fixed duty cycle.
''
''
                        rdbyte  G1, stack_ptr                        ' read pin number
                        mov     G2, pwm_base                  ' load pwm base
                        add     G0, #1                        ' go to value
                        rdbyte  G3, G0                        ' read value into G3
                        and     G1, #$0F                      ' cap at 4 bits (16 pins)
                        shl     G1, #2                        ' multiply by 4

                        add     G2, G1                        ' go to offset in PWM table
                        shl     G3, #24                       ' convert to 32-bit value for processing
                        and     G1, write_mask                ' set all lower bits to 1 
                        sub     stack_ind, #2                 ' decrement stack
                        wrlong  G3, G2                        ' store value in table
                        add     count, #1                     ' add to count
                        and     stack_ind, #$FF
                        jmp     #fvm_end_processing           ' exit

write_mask    long      $00FF_FFFF                         
                        
fvm_delay
''
'' FVM_DELAY macro takes an unsigned 32-bit integer in nano-seconds.
'' of course, we are only running at 80 MHz so doing nano-seconds is tough.
'' the function waits a minimum of 1300 ns, and has a resolution of 100 ns
''
''
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

                        sub     G4, num1300             wz,wc ' adjust remaining time
              if_be     jmp     #fvm_delay_01                 ' if time is not positive, we leave
fvm_delay_00
                        sub     G4, #100                wz,wc ' 100ns per loop
              if_a      jmp     #fvm_delay_00                 ' reloop
fvm_delay_01
                        sub     stack_ind, #4                 ' subtract four bytes
                        and     stack_ind, #$FF               ' cap at four bits
                        add     count, #1                
                        jmp     #fvm_end_processing                                       
                                                         
fvm_inc
''
'' FVM_INC macro simply takes the byte on the top of the stack
'' and increments it by 1.
''
''
                         rdbyte G0, G1                        ' load byte
                         add    G0, #1                        ' increment
                         add    count,#1
                         wrbyte G0, G1                        ' store

                         jmp    #fvm_end_processing           ' exit

fvm_dec
''
'' FVM_DEC macro simply takes the byte on the top of the stack
'' and decrements it by 1.
''
''
                         rdbyte G0, G1                        ' load byte
                         sub    G0, #1                        ' increment
                         add    count,#1
                         wrbyte G0, G1                        ' store

                         jmp    #fvm_end_processing           ' exit

fvm_add             
''
'' FVM_ADD macro pops the top two bytes on the stack, adds them
'' and finally pushes the sum back to the stack
''
''
                        rdbyte  G0, stack_ptr
                        sub     stack_ptr, #1
                        sub     stack_ind, #1
                        rdbyte  G1, stack_ptr
                        add     G0, G1
                        and     stack_ind, #$FF
                        wrbyte  G0, stack_ptr

                        add     count,#1
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
                        sub     stack_ind, #1
                        rdbyte  G1, stack_ptr
                        sub     G0, G1
                        and     stack_ind, #$FF
                        wrbyte  G0, stack_ptr
                        
                        jmp     #fvm_end_processing
                                                
fvm_cmp
fvm_or
fvm_and
fvm_test
fvm_not
fvm_swap
fvm_dup
                        mov     G1, #2
                        call    #fvm_getdata
                        
                        add     G0, #1                        ' go to length count byte
                        mov     G2, stack_base                ' load stack base to G2
                        add     G2, stack_ind                 ' add stack index
                        mov     G3, G2                        ' copy to G3; one pointer goes up, the other goes down
                        
                        rdbyte  G1, G0                  wz    ' read length of data and test for zero

                        sub     G2, G1                        ' go to lowest data
              if_z      jmp     #fvm_end_processing           ' exit

fvm_dup_00
                        rdbyte  G0, G2                        ' read byte
                        add     G2, #1                        ' go to next byte to copy
                        sub     G1, #1                  wz    ' decrement counter
                        wrbyte  G0, G3                        ' write byte in upper area
                        add     G3, #1                        ' go to next write location
              if_nz     jmp     #fvm_dup_00                   ' reloop

                        jmp     #fvm_end_processing

                        
fvm_if
fvm_jmp
fvm_defmc

fvm_calmc
fvm_retmc
fvm_dlaym
''
'' FVM_DLAYM macro takes an unsigned 32-bit integer in micro-seconds.
'' minimum wait time of 1.2us for 0 and 1us specified. All other values
'' delay precisely.
''
''                       
                        mov     G0, stack_base                ' stack base
                        add     G0, stack_ind                 ' go to stack pointer
                        nop
                        nop                                   ' read byte by byte to avoid alignment issues
                        rdbyte  G1, G0                        ' read byte
                        shl     G1, #24                       ' shift up
                        add     G0, #1                        ' go to next byte
                        rdbyte  G2, G0                        ' read byte
                        shl     G2, #16                       ' shift up
                        add     G0, #1                        ' go to next byte
                        rdbyte  G3, G0                        ' read byte
                        shl     G3, #8                        ' shift up
                        add     G0, #1                        ' go to next byte
                        rdbyte  G4, G0                        ' read byte
                        or      G4, G3
                        or      G4, G2
                        or      G4, G1                        ' construct number in G4

                        sub     G4, #2                  wz,wc ' adjust remaining time
              if_be     jmp     #fvm_dlaym_03                 ' if time is not positive, we leave
fvm_dlaym_00            
                        mov     G3,8 
fvm_dlaym_01
                        sub     G3, #1                  wz
              if_nz     jmp     #fvm_dlaym_01

                        sub     G4, #1                  wz,wc ' subtract
              if_a      jmp     #fvm_dlaym_00                 ' reloop

                        mov     G3,14 
fvm_dlaym_02
                        sub     G3, #1                  wz,wc
              if_a      jmp     #fvm_dlaym_02
                        jmp     #fvm_end_processing

fvm_dlaym_03
                        mov     G3,14
              if_ae     jmp     #fvm_dlaym_02 
                        jmp     #fvm_end_processing           ' leave    


fvm_getdata             ' gets data from either buffer or macro area

'' FVM_getdata -
''    G1 should contain length of data requested upon entry
''    G0 will contain the address of first byte if data is available
''    All G2-G4 will be FUBAR
''
''    HUB access is available immidiately upon return from FVM_getdata
''   

                        or      macro, macro                  ' ? macro area
              if_nz     jmp     #fvm_getdata_01               ' Y - get data from macro
                                                              ' N - get data from buffer
                        rdbyte  G0, bufin_ptr                 ' load filled index

                        mov     G7, G0                        ' load G7 with filled index
                        sub     G7, buf_proc                  ' subtract index to get length available
                        cmp     G7, G1                  wz,wc ' ? length available >= length requested
              if_b      jmp     #fvm_end_processing           ' N - wait for more data
                        
                        mov     G0, buf_base                  ' load G0 with buffer pointer
                        add     G0, buf_proc                  ' go to next process index
                        jmp     #fvm_getdata_ret              ' exit

fvm_getdata_01
                        mov     G2, macno                     ' move in macro number
                        add     G2, macro_base                ' add base address of table
                        rdbyte  G3, G2                        ' read in first byte
                        shl     G3, #8                        ' free lower 8 bits
                        add     G2, #1                        ' point to next byte
                        rdbyte  G4, G2                        ' get next byte
                        mov     G2, macno                     ' load macro number
                        add     G2, alloc_base                ' point to length of macro
                        rdbyte  G0, G2                        ' read length into G0
                        or      G4, G3                        ' form full word of macro address in G4 
                        add     G1, macro                     ' get end of data in G1
                        
                        add     G4, G0                        ' get end of macro in G4
                        cmp     G4, G3                  wc,wz ' ? macro end >= data end
              if_ae     mov     G0, macro                     ' Y - return macro address
fvm_getdata_ret   
              if_ae     ret                                   '    
                        jmp     #fvm_err_processing           ' N - end processing with error


fvm_err_processing                                                                 
fvm_end_processing
                        or      macro, macro            wz
              if_z      jmp     #fvm_end_processing_00        ' if no macro address, it's through the buffer

                        add     macro, count                  ' otherwise add to macro address
                        xor     count, count                  ' zero count 
                        jmp     #fvm_process                  ' reloop   
                          
fvm_end_processing_00
                        add     buf_proc, count               ' if not in a macro, add count to buffer processed index
                        and     buf_proc, #$FF                ' cap at 8 bits
                        xor     count, count                  ' zero count

                        jmp     #fvm_process                  ' reloop
                        
                                                                                                                      

num1300       long      1300                                                       
num512        long      512
'
' experimental delay using counter and waitpeq
'
' main issue is that each clock is 12.5 ns, so we can't really subtract
' 12.5 ns and there is no way to accumulate the .5 without going to
' 25 ns resolution. And at that rate I may as well
' wait the 100 ns with no counter. 
' 
delay_ctr     long      %0_00100_000_00000000_000000_000_010000   ' use pin 16 for NCO, reads into pin 17

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' general registers
G0            res       1
G1            res       1
G2            res       1
G3            res       1
G4            res       1
G5            res       1
G6            res       1
G7            res       1          
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
opcode        res       1       ' current opcode processed
count         res       1       ' number of bytes processed
buf_proc      res       1       ' index of buffer processed
flags         res       1       ' internal VM flags
stack_ind     res       1       ' current index in stack
stack_ptr     res       1       ' calculated at the start of each process
macro         res       1       ' macro pointer
macno         res       1       ' macro number being executed

pwm_base      res       1       ' PWM table   
macro_base    res       1       ' start of macro address table
buf_base      res       1       ' buffer ptr
stack_base    res       1       ' start of data stack 
alloc_base    res       1       ' start of allocated size table
bufin_ptr     res       1       ' buffer index pointer
buflock       res       1       ' buffer lock 
heap_base     res       1       ' base pointer to heap

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

' 32-bit PWM with 294.12 kHz switching frequency (94% faster than 32 pins)
' Method uses the carry flag to proportion the on-time of the duty cycles
' Each cycle, the duty cycle of the pin is added to a counter for that
' pin. When this generates a carry, the pin is set to high, otherwise it is low.
' This means the a true duty cycle, that accurately represents the value of the
' signal takes whatever number of cycles it takes to accurately represent that
' the value in fractional form.


                        org     0
hires
                        mov     counter,#16                  ' Counter used to generate the table of pin HUBRAM addresses
                        mov     pinTableBase,par             ' Move in the HUBRAM address of the pin values table
                        
' Initializes a table containing the HUBRAM address of every pin
' in order to avoid having to increment a reference address each
' time we have to access the table, thus increasing speed.
setup
                        mov     dutyReg,#pinAddresses        ' Move the base pin COGRAM address to the dutyReg (sorry for meaningless name, recycled register)
                        add     dutyReg,#16                  ' Go to end of table
                        sub     dutyReg,counter              ' Index backwards based on counter
                        movd    tableSet,dutyReg             ' Move the register number into the destination for the next instruction
tableSet
                        mov     0000,pinTableBase            ' Store current HUBRAM address
                        add     pinTableBase,#4              ' Increment to next 32-bit int
                        djnz    counter,#setup               ' continue making table       
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
dutyStart               
                        rdlong  dutyReg,pinAddresses         ' Read the value of the zero-th pin into the dutyReg
                        add     dutyTable,dutyReg       wc   ' Add to the accumulator
              if_c      or      buffer,pinMask00             ' If a carry was generated, set the pin to high
                           
                        rdlong  dutyReg,pinAddresses+1       ' repeat this process, each time going to the next pin, and next 
                        add     dutyTable+1,dutyReg       wc
              if_c      or      buffer,pinMask01 

                        rdlong  dutyReg,pinAddresses+2       ' This goes on 16 times. Once per pin.
                        add     dutyTable+2,dutyReg       wc
              if_c      or      buffer,pinMask02 

                        rdlong  dutyReg,pinAddresses+3
                        add     dutyTable+3,dutyReg       wc
              if_c      or      buffer,pinMask03 

                        rdlong  dutyReg,pinAddresses+4
                        add     dutyTable+4,dutyReg       wc
              if_c      or      buffer,pinMask04 

                        rdlong  dutyReg,pinAddresses+5
                        add     dutyTable+5,dutyReg       wc
              if_c      or      buffer,pinMask05 

                        rdlong  dutyReg,pinAddresses+6
                        add     dutyTable+6,dutyReg       wc
              if_c      or      buffer,pinMask06 

                        rdlong  dutyReg,pinAddresses+7
                        add     dutyTable+7,dutyReg       wc
              if_c      or      buffer,pinMask07 

                        rdlong  dutyReg,pinAddresses+8
                        add     dutyTable+8,dutyReg       wc
              if_c      or      buffer,pinMask08 

                        rdlong  dutyReg,pinAddresses+9
                        add     dutyTable+9,dutyReg       wc
              if_c      or      buffer,pinMask09 

                        rdlong  dutyReg,pinAddresses+10
                        add     dutyTable+10,dutyReg       wc
              if_c      or      buffer,pinMask0A 

                        rdlong  dutyReg,pinAddresses+11
                        add     dutyTable+11,dutyReg       wc
              if_c      or      buffer,pinMask0B 

                        rdlong  dutyReg,pinAddresses+12
                        add     dutyTable+12,dutyReg       wc
              if_c      or      buffer,pinMask0C 

                        rdlong  dutyReg,pinAddresses+13
                        add     dutyTable+13,dutyReg       wc
              if_c      or      buffer,pinMask0D 

                        rdlong  dutyReg,pinAddresses+14
                        add     dutyTable+14,dutyReg       wc
              if_c      or      buffer,pinMask0E 

                        rdlong  dutyReg,pinAddresses+15
                        add     dutyTable+15,dutyReg       wc
              if_c      or      buffer,pinMask0F 

                        mov     dira,buffer                     ' Set those pins to output                       
                        mov     outa,buffer                     ' Write high to the pins set      
                        xor     buffer,buffer                   ' Clear buffer for next cycle
                        jmp     #dutyStart                      ' Go to next cycle

' Pin mask table used to set pins                        
pinMask00     long      %0000_0000_0000_0000_0000_0000_0000_0001
pinMask01     long      %0000_0000_0000_0000_0000_0000_0000_0010
pinMask02     long      %0000_0000_0000_0000_0000_0000_0000_0100
pinMask03     long      %0000_0000_0000_0000_0000_0000_0000_1000
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

dutyReg       res       1    ' Register that duty cycle gets read into
counter       res       1    ' Counter for generating the address table
pinTableBase  res       1    ' HUBRAM address of pin addresses
buffer        res       1    ' Bitmask buffer
pinAddresses  res       16   ' Table of HUBRAM addresses
dutyTable     res       16   ' Table of accumulators for each pins duty cycle     
                        FIT

CON
''
'' FireCracker SPI reciever -
''      


  spi_clk  = 18                 ' clock pin             
  spi_mosi = 19                 ' master out / slave in
  spi_miso = 20                 ' master in / slave out
  spi_cs   = 21                 ' chip select
  i2c_sda  = 16                 ' I2C data pin
  i2c_scl  = 17                 ' I2C clock pin

DAT StartRecv

recv_entry
                        mov     phsa,#0
                        mov     phsb,#0
                        mov     frqa,#1
                        mov     frqb,#1
                        mov     ctra,recv_spicntl
                        mov     ctrb,recv_i2ccntl
recv_entry00
                        or      phsa, phsb              wz    ' ? either pin gets a positive edge                     
              if_z      jmp     #recv_entry00                 ' N - reloop if neither pin set


                        cmp     ina, spi_mosimask       wz    ' ? SPI set
              if_z      jmp     #spi_entry                    ' Y - go to SPI

                        cmp     ina, i2c_sclmask              ' ? I2C set
              if_z      jmp     #i2c_entry                    ' Y - go to I2C

                        mov     dira, recv_errmask            ' move in error mask
                        mov     outa, recv_errmask            ' drive all lines high
                        cogid   zero
                        cogstop zero                          ' kill service

spi_entry
                        or      dira, spi_misomask            ' configure pin(s) for output

                        mov     frqa,#1
                        mov     frqb,#1                       '
                        mov     phsa,#0
                        mov     phsb,#0
                        mov     ctra,spi_clkcntl              ' wait for pins to both be low
                        movs    ctra,#spi_clk
                        movd    ctra,#spi_cs                  ' monitor cs and clk pins
                        mov     ctrb,spi_datcntl
                        movs    ctrb,#spi_mosi
                        mov     spi_count, #8

                        or      outa, spi_misomask
                        waitpeq zero, spi_mosimask

'' right now, capable speed is between 1.94Mb/s and 2.16Mb/s
'' which is between 248KB/s and 276KB/s. I would cap it
'' at 2Mb/s because going any faster requires the master
'' to be perfectly in sync with Firecracker.
''
'' Those speeds is the max data rate range. This is a synchronous
'' protocol, so going slower will work fine. Just don't go over
'' 2Mb/s.
''
spi_waitloop
                        tjz     phsa,#spi_waitloop            ' wait for both pins to be low
spi_waitloop00
                        shl     phsb,#1                       ' shift data in over
                        mov     phsa,#0                       ' zero event
                        waitpeq one, spi_clkmask              ' wait for raised clock
                        djnz    spi_count, #spi_waitloop      ' reloop if we do not have 8 bits

                        rdbyte  temp, buf_ind
                        add     temp, buf_addr
                        mov     spi_count, #8
                        wrbyte  phsb, temp
                        sub     temp, buf_addr
                        add     temp, #1
                        wrbyte  temp, buf_ind
                        jmp     #spi_waitloop
                        
                                    
                                  
i2c_entry
                                   

zero          long      0
one           long      1

recv_poscntl  long      %01010_000_00000000_000000_0_000000
recv_spicntl  long      recv_poscntl | spi_mosi
recv_i2ccntl  long      recv_poscntl | i2c_scl
recv_mask     long      spi_mosimask | i2c_sclmask
recv_errmask  long      spi_misomask | i2c_sdamask

spi_clkcntl   long      %10001_000_00000000_000000_0_000000
spi_datcntl   long      %01010_000_00000000_000000_0_000000
spi_clkmask   long      1 << spi_clk
spi_mosimask  long      1 << spi_mosi
spi_misomask  long      1 << spi_miso
spi_csmask    long      1 << spi_cs

i2c_sdamask   long      1 << i2c_sda
i2c_sclmask   long      1 << i2c_scl
spi_mode      long      0

temp          res       1
buf_addr      res       1
buf_ind       res       1
spi_count     res       1

              FIT