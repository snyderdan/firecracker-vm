'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' So here's the plan. We're gonna have a heap to define all of your functions/macros.
' It will be (32KB - codesize) * 2 as I am looking into accessing EEPROM for memory
' extensions. I may also implement a feature to save macros. I believe there is a
' 256 byte limit on macro sizes but that does not stop you from calling macros from
' other macros.
'
' Stack size will be there will also be a 256 byte recieving buffer, and somewhere
' between 256 and 512 bytes of stack space. The heap will be huge though. Well
' relatively speaking
'
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
  FVM_JUMP_OPCODE  = 17
  FVM_DEFMC_OPCODE = 18
  FVM_CALMC_OPCODE = 19
  FVM_RETMC_OPCODE = 20

OBJ 
  
  ' propPWM
  ' firecracker-recv
  
VAR

  long FVM_PWM_table[FVM_DEFAULT_NUM_OUTPUTS] ' PWM outputs          

  word FVM_macros[FVM_DEFAULT_NUM_MACROS]     ' macro addresses (words allocated first)

  byte FVM_buffer[FVM_DEFAULT_BUFFER_SIZE]    ' input buffer

  byte FVM_data_stack[FVM_DEFAULT_STACK_SIZE] ' Data stack that operations are performed on

  byte FVM_allocsz[FVM_DEFAULT_NUM_MACROS]    ' size of each macro

  byte FVM_buffer_index                       ' index of buffer filled

  byte FVM_buffer_lock                        ' lock for data buffer

  byte FVM_heap                               ' just a reference for heap address
  
PUB Start

  FVM_buffer_lock := locknew
  cognew(StartRecv, FVM_buffer)
  cognew(@hires, @FVM_PWM_table)
  cognew(@fvm_entry, @FVM_PWM_table)
  

PUB StartRecv

  ' call firecracker-recv.start
  
DAT

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
                        add     bufin_ptr,  #1                ' index filled pointer
                        mov     buflock,    bufin_ptr
                        add     buflock,    #1                ' point to lock number
                        mov     heap_base,  buflock
                        add     heap_base,  #1                ' heap pointer
                        rdbyte  buflock, buflock              ' read in lock number

fvm_process                       
fvm_wait_lock
                        lockset buflock                 wc    ' attempt lock set
                        nop                                   ' align HUB access
              if_c      jmp     #fvm_wait_lock                ' if previously set, then reloop

                        rdbyte  G0, bufin_ptr                 ' load filled index
                        
                        mov     G1, #1                        ' check for 1 byte available (opcode)
                        call    #fvm_bufcheck                 ' check for available data
                        mov     buf_addr, buf_base            ' load G2 with buffer pointer
                        add     buf_addr, buf_proc            ' go to next process index
                        nop                                   ' align timing

                        rdbyte  opcode, buf_addr              ' read next opcode
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

fvm_nop
                        add     buf_proc, #1
                        jmp     #fvm_end_processing

fvm_push
                        mov     G1, #2                        ' add push opcode and length byte
                        call    #fvm_bufcheck                 ' ensure data is available
                        add     buf_addr, #1                  ' go to length byte
                        
                        rdbyte  G1, buf_addr                  ' read length into G1
                        
                        add     G1, #2                        ' add push opcode and length byte
                        call    #fvm_bufcheck                 ' ensure data is available
                        add     buf_addr, #1                  ' go to data in buffer
                        add     buf_proc, G1                  ' update processed index
                        sub     G1, #2                        ' adjust G1 to data length
                        
                        mov     G2, stack_base                ' load stack pointer
                        add     G2, stack_ind                 ' add stack index
                        add     stack_ind, G1                 ' update stack index
                        and     stack_ind, #$FF               ' cap at 8 bits            

fvm_push_00
                        rdbyte  G3, buf_addr                  ' read next byte
                        sub     G1, #1                  wz    ' decrement counter
                        add     buf_addr, #1                  ' increment data buffer pointer
                        wrbyte  G3, G2                        ' store in stack
                        add     G2, #1                        ' go to next stack value
              if_nz     jmp     #fvm_push_00                  ' reloop for data length 

                        jmp     #fvm_end_processing           ' exit
                        
fvm_pop
                        mov     G1, #2                        ' ensure we have a length byte present
                        call    #fvm_bufcheck                 ' ^
                        add     buf_addr, #1                  ' go to length byte

                        rdbyte  G0, buf_addr                  ' read length into G0
                        sub     stack_ind, G0                 ' subtract our index to pop
                        jmp     #fvm_end_processing           ' exit
                        
                        
fvm_write
                        mov     G1, #3                        ' pin number and value
                        call    #fvm_bufcheck                 ' ensure data is available
                        add     buf_addr, #1                  ' go to pin number
                        rdbyte  G0, buf_addr                  ' read pin number
                        mov     G2, pwm_base                  ' load pwm base
                        add     buf_addr, #1                  ' go to value
                        rdbyte  G3, buf_addr                  ' read value into G3
                        and     G0, #$0F                      ' cap at 4 bits (16 pins)
                        shl     G0, #2                        ' multiply by 4
                        
                        add     G2, G0                        ' go to offset in PWM table
                        shl     G3, #24                       ' convert to 32-bit value for processing
                        nop
                        nop                                   ' align timing
                        wrlong  G3, G2                        ' store value in table

                        jmp     #fvm_end_processing           ' exit                          
                        
fvm_delay
fvm_inc
fvm_dec
fvm_add
fvm_sub
fvm_cmp
fvm_or
fvm_and
fvm_test
fvm_not
fvm_swap
fvm_dup
                        mov     G1, #2
                        call    #fvm_bufcheck
                        
fvm_if
                        mov     G1, #4
                        call    #fvm_bufcheck
fvm_jmp
                        mov     G1, #2
                        call    #fvm_bufcheck
fvm_defmc
                        mov     G1, #2
                        call    #fvm_bufcheck

fvm_calmc
fvm_retmc             

fvm_bufcheck            ' upon entry: G0 should contain filled index
                        '             buf_proc should contain processed index
                        '             G1 should contain length of data to check for
                        '
                        ' upon exit:  returns to call if enough data present
                        '             ends processing if there is not enough data
                        '             a call to bufcheck shifts HUB access by 8 cycles
                        '             meaning the call essentially counts as 8 cycles
                        '
                        mov     G3, G0                        ' load G7 with filled index
                        sub     G3, buf_proc                  ' subtract index to get length available
                        cmp     G3, G1                  wz,wc ' ? length available >= length requested
              if_ae     jmp     #fvm_bufcheck_ret             ' Y - return to process
                        jmp     #fvm_end_processing           ' N - wait for more data        
fvm_bufcheck_ret
                        ret
                                                                  
fvm_end_processing

                        lockret buflock                       ' return lock
                        mov     G0, cnt                       ' load clock counter
                        add     G0, #14                       ' add and waitcnt is 10 clocks + 14 is 24 clocks = max time for someone else to grab lock
                        waitcnt G0, #0                        ' wait 
                        jmp     #fvm_process                  ' reloop
                        
                                                                                                                      
                                                        
num512        long      512

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' general registers
G0            res       1
G1            res       1
G2            res       1
G3            res       1
'
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
opcode        res       1       ' current opcode processed
buf_addr      res       1       ' calculated pointer into buffer
buf_proc      res       1       ' index of buffer processed
flags         res       1       ' internal VM flags
stack_ind     res       1       ' current index in stack

pwm_base      res       1       ' PWM table   
macro_base    res       1       ' start of macro address table
buf_base      res       1       ' buffer ptr
stack_base    res       1       ' start of data stack 
alloc_base    res       1       ' start of allocated size table
bufin_ptr     res       1       ' buffer index pointer
buflock       res       1       ' buffer lock 
heap_base     res       1       ' base pointer to heap

                        FIT

DAT

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
''
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

' Ex.
' If you write 50% to a pin, the smallest fractional representation is 1/2
' meaning that every two cycles will be a complete duty cycle, with 1 on, and 
' one off, resulting in a PWM frequency of ~75 kHz. If, however, you write
' the value 52%, the smallest accurate representation is 26/50. This means
' That the pin will be on for 26 cycles, and off for 25 cycles. But unlike the
' time proportioning method, this method won't spend 26 steps straight high,
' and then 25 steps low. It will instead distribute these steps evenly to form
' baby-duty cycles, which closely represent 52%. For a perfect representation,
' the duty cycle would be after 50 actual switches and have a frequency of 3 kHz.
' It will appear to be 50%, as it is very close to that value, and after the extra
' 2% has added up enough, it will lengthen the time spent high for one step,
' changing the average time spent high to 52%.

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