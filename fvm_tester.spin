{
Test module for Firecracker VM (Software designed for Bonfire LED driver)
that tests all features of FVM excluding any communication.

Some test features include:
 - Acting as data source allowing user to view execution inside emulators/debuggers
 - Directly writing to FVM interpreter input through serial terminal      
 - Validating outputs of the following types of FVM opcodes
   A) FVM signalling
   ** FOLLOWING ARE NOT IMPLEMENTED YET **
   B) PWM writes
   C) BRKT writes
   D) BRKT timing creations
   E) Macro definitions
   F) Math
}
OBJ

  pst   : "Parallax Serial Terminal" 
      

VAR
  word bufferAddr
  word bufferIndexPtr
  
PUB Start(buf_addr, bufptr_addr, feedback)
  bufferAddr := buf_addr
  bufferIndexPtr := bufptr_addr
  if feedback
    pst.Start(9600)

PUB execute(list_addr, length) | i

  i := 0

  repeat while i < length
    byte[bufferAddr+byte[bufferIndexPtr]] := byte[list_addr+i]
    byte[bufferIndexPtr] := (byte[bufferIndexPtr]+1) // 256
    i++

PUB interpret

  repeat while true
    byte[bufferAddr+byte[bufferIndexPtr]] := pst.DecIn
    byte[bufferIndexPtr] := (byte[bufferIndexPtr]+1) // 256

PUB validateSignal(sendSignal, sigaddr) | sig

  byte[bufferAddr+byte[bufferIndexPtr]] := 1            ' PUSH opcode
  byte[bufferAddr+byte[bufferIndexPtr]+1] := 0          ' length to push
  byte[bufferAddr+byte[bufferIndexPtr]+2] := 1          ' length to push  
  byte[bufferAddr+byte[bufferIndexPtr]+3] := sendSignal ' data to push
  byte[bufferAddr+byte[bufferIndexPtr]+4] := 26         ' POSTS opcode
  byte[bufferAddr+byte[bufferIndexPtr]+5] := 2          ' POP opcode
  byte[bufferAddr+byte[bufferIndexPtr]+6] := 1          ' number of bytes to pop 
  byte[bufferIndexPtr] := (byte[bufferIndexPtr]+7) // 256
  
  waitcnt(80000+cnt)
  
  sig := byte[sigaddr]
  
  byte[bufferAddr+byte[bufferIndexPtr]] := 1            ' PUSH opcode
  byte[bufferAddr+byte[bufferIndexPtr]+1] := 0          ' length to push
  byte[bufferAddr+byte[bufferIndexPtr]+2] := 1          ' length to push  
  byte[bufferAddr+byte[bufferIndexPtr]+3] := 0          ' data to push
  byte[bufferAddr+byte[bufferIndexPtr]+4] := 26         ' POSTS opcode
  byte[bufferAddr+byte[bufferIndexPtr]+5] := 2          ' POP opcode
  byte[bufferAddr+byte[bufferIndexPtr]+6] := 1          ' number of bytes to pop 
  byte[bufferIndexPtr] := (byte[bufferIndexPtr]+7) // 256

  if sig == sendSignal
    return true
  else
    return false

PUB signalTest(nSignals, sigaddr, feedback) | sig

  if feedback
    pst.Str(string("Beginning signal test with "))
    pst.Dec(nSignals)
    pst.Str(string(" signals",13))

  sig := nSignals

  result := true

  repeat while sig
    if validateSignal(sig, sigaddr) == false
      if feedback
        pst.Str(string(13, "Failed on signal: "))
        pst.Hex(sig,2)
      result := false
      return
    else
      if feedback
        pst.Str(string(13, "Signal validated: "))
        pst.Hex(sig,2)
    
