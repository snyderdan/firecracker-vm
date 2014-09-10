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
  cognew(@fvm_entry, @FVM_macros)
  

PUB StartRecv

  ' call firecracker-recv.start
  
DAT

fvm_entry
                        '
                        ' load pointer from par and calculate addresses for variables
                        '
                        mov     macro_ptr, par                ' macro table                                            
                        mov     buff_ptr,  macro_ptr
                        add     buff_ptr,  num512             ' buffer pointer
                        mov     stack_ptr, buff_ptr
                        add     stack_ptr, #256               ' stack pointer
                        mov     alloc_ptr, stack_ptr
                        add     alloc_ptr, #256               ' allocated size table
                        mov     bufin_ptr, alloc_ptr
                        add     bufin_ptr, #1                 ' index filled pointer
                        mov     buflock,   bufin_ptr
                        add     buflock,   #1                 ' point to lock number
                        mov     heap_ptr,  buflock
                        add     heap_ptr,  #1                 ' heap pointer
                        rdbyte  buflock, buflock              ' read in lock number

fvm_process                       
fvm_wait_lock
                        lockset buflock                 wc    ' attempt lock set
                        nop                                   ' align HUB access
              if_c      jmp     #fvm_wait_lock                ' if previously set, then reloop

                        rdbyte  G0, bufin_ptr                 ' load filled index
                        
                        cmp     G0, buf_proc            wz    ' ? - our index is equal to filled index
              if_z      jmp     #fvm_end_processing           ' Y - end processing; no data to process
                        mov     G1, buff_ptr                  ' N - move buffer ptr into G1
                        add     G1, buf_proc                  ' go to next opcode to process

                        rdbyte  opcode, G1                    ' read next opcode
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
                        mov     G1, #1
                        call    #fvm_bufcheck
                        
fvm_pop
                        mov     G1, #1
                        call    #fvm_bufcheck
                        
fvm_write
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
                        mov     G1, #1
                        call    #fvm_bufcheck
                        
fvm_if
                        mov     G1, #3
                        call    #fvm_bufcheck
fvm_jmp
                        mov     G1, #1
                        call    #fvm_bufcheck
fvm_defmc
                        mov     G1, #1
                        call    #fvm_bufcheck

fvm_calmc
fvm_retmc             

fvm_bufcheck            ' upon entry: G0 should contain filled index
                        '             buf_proc should contain processed index
                        '             G1 should contain length of data to check for
                        '
                        ' upon exit:  returns to call if enough data present
                        '             ends processing if there is not enough data
                        '             HUB access is aligned with first instruction upon return
                        '
                        mov     G2, G0                                     
                        cmp     G2, buf_proc            wz,wc ' ? filled > processed
              if_be     jmp     #fvm_bufcheck_00              ' N - check buffer unnormalized
                                                              ' Y - check normalized
                        sub     G2, G1                        ' adjust for length of data 
                        cmp     G2, buf_proc            wz,wc ' ? (filled - len) > processed
              if_a      jmp     #fvm_bufcheck_ret             ' Y - return to process
                        jmp     #fvm_end_processing           ' N - need to wait for data
fvm_bufcheck_00

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
     
macro_ptr     res       1       ' start of macro address table
buff_ptr      res       1       ' buffer ptr
stack_ptr     res       1       ' start of data stack 
alloc_ptr     res       1       ' start of allocated size table
bufin_ptr     res       1       ' buffer index pointer
buflock       res       1       ' buffer lock 
heap_ptr      res       1       ' base pointer to heap

                        FIT 496