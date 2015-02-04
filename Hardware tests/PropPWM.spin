CON

  PIN_COUNT  = 32

VAR

  long PWMTable[PIN_COUNT]
  long deltaRef

PUB Start

  deltaAddr := @deltaRef
  cognew(@hires, @PWMTable)

PUB analogWrite(pin, value)

  PWMTable[pin] := value

PUB setPWM(pin, percent)

  PWMTable[pin] := $28F5C28*percent

PUB setFrequency(freq)
'' setFrequency sets the PWM switching frequency to the specified freq in Hz

  deltaRef := _CLKFREQ / freq  
  
                
DAT
' 32-bit PWM with 151.5 kHz switching frequency
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
                        mov     pinTableBase,par             ' Move in the HUBRAM address of the pin values table
                        mov     counter,#32                  ' Counter used to generate the table of pin HUBRAM addresses
                        mov     dutyReg,#pinAddress00
                        
' Initializes a table containing the HUBRAM address of every pin
' in order to avoid having to increment a reference address each
' time we have to access the table, thus increasing speed.

setup
                        movd    tableEntry, dutyReg
                        add     dutyReg,#1 
tableEntry                                   
                        add     0000,pinTableBase 
                        djnz    counter, #setup

                        mov     counter, cnt
                        add     counter, delta
                        
dutyStart               
                        rdlong  dutyReg,pinAddress00         ' Read the value of the zero-th pin into the dutyReg
                        add     dutyTable00,dutyReg       wc   ' Add to the accumulator
              if_c      or      buffer,pinMask00             ' If a carry was generated, set the pin to high
              
                        rdlong  dutyReg,pinAddress01         ' repeat this process, each time going to the next pin, and next 
                        add     dutyTable01,dutyReg       wc
              if_c      or      buffer,pinMask01 

                        rdlong  dutyReg,pinAddress02         ' This goes on 32 times. Once per pin.
                        add     dutyTable02,dutyReg       wc
              if_c      or      buffer,pinMask02 

                        rdlong  dutyReg,pinAddress03
                        add     dutyTable03,dutyReg       wc
              if_c      or      buffer,pinMask03 

                        rdlong  dutyReg,pinAddress04
                        add     dutyTable04,dutyReg       wc
              if_c      or      buffer,pinMask04 

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

                        rdlong  dutyReg,pinAddress10
                        add     dutyTable10,dutyReg       wc
              if_c      or      buffer,pinMask10 

                        rdlong  dutyReg,pinAddress11
                        add     dutyTable11,dutyReg       wc
              if_c      or      buffer,pinMask11 

                        rdlong  dutyReg,pinAddress12
                        add     dutyTable12,dutyReg       wc
              if_c      or      buffer,pinMask12 

                        rdlong  dutyReg,pinAddress13
                        add     dutyTable13,dutyReg       wc
              if_c      or      buffer,pinMask13 

                        rdlong  dutyReg,pinAddress14
                        add     dutyTable14,dutyReg       wc
              if_c      or      buffer,pinMask14 

                        rdlong  dutyReg,pinAddress15
                        add     dutyTable15,dutyReg       wc
              if_c      or      buffer,pinMask15 

                        rdlong  dutyReg,pinAddress16
                        add     dutyTable16,dutyReg       wc
              if_c      or      buffer,pinMask16 

                        rdlong  dutyReg,pinAddress17
                        add     dutyTable17,dutyReg       wc
              if_c      or      buffer,pinMask17 

                        rdlong  dutyReg,pinAddress18
                        add     dutyTable18,dutyReg       wc
              if_c      or      buffer,pinMask18 

                        rdlong  dutyReg,pinAddress19
                        add     dutyTable19,dutyReg       wc
              if_c      or      buffer,pinMask19 

                        rdlong  dutyReg,pinAddress1A
                        add     dutyTable1A,dutyReg       wc
              if_c      or      buffer,pinMask1A 

                        rdlong  dutyReg,pinAddress1B
                        add     dutyTable1B,dutyReg       wc
              if_c      or      buffer,pinMask1B 

                        rdlong  dutyReg,pinAddress1C
                        add     dutyTable1C,dutyReg       wc
              if_c      or      buffer,pinMask1C 

                        rdlong  dutyReg,pinAddress1D
                        add     dutyTable1D,dutyReg       wc
              if_c      or      buffer,pinMask1D 

                        rdlong  dutyReg,pinAddress1E
                        add     dutyTable1E,dutyReg       wc
              if_c      or      buffer,pinMask1E 

                        rdlong  dutyReg,pinAddress1F
                        add     dutyTable1F,dutyReg       wc
              if_c      or      buffer,pinMask1F

                        rdlong  delta, deltaAddr
                        waitcnt counter, delta                ' set up a syncronized window

                        mov     dira,buffer                     ' Set those pins to output                       
                        mov     outa,buffer                     ' Write high to the pins set      
                        xor     buffer,buffer                   ' Clear buffer for next cycle
                        jmp     #dutyStart                      ' Go to next cycle


' Pin mask table used to set pins


delta         long      25000
deltaAddr     long      0
                       
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
pinMask10     long      %0000_0000_0000_0001_0000_0000_0000_0000
pinMask11     long      %0000_0000_0000_0010_0000_0000_0000_0000
pinMask12     long      %0000_0000_0000_0100_0000_0000_0000_0000
pinMask13     long      %0000_0000_0000_1000_0000_0000_0000_0000
pinMask14     long      %0000_0000_0001_0000_0000_0000_0000_0000
pinMask15     long      %0000_0000_0010_0000_0000_0000_0000_0000
pinMask16     long      %0000_0000_0100_0000_0000_0000_0000_0000
pinMask17     long      %0000_0000_1000_0000_0000_0000_0000_0000
pinMask18     long      %0000_0001_0000_0000_0000_0000_0000_0000
pinMask19     long      %0000_0010_0000_0000_0000_0000_0000_0000
pinMask1A     long      %0000_0100_0000_0000_0000_0000_0000_0000
pinMask1B     long      %0000_1000_0000_0000_0000_0000_0000_0000
pinMask1C     long      %0001_0000_0000_0000_0000_0000_0000_0000
pinMask1D     long      %0010_0000_0000_0000_0000_0000_0000_0000
pinMask1E     long      %0100_0000_0000_0000_0000_0000_0000_0000
pinMask1F     long      %1000_0000_0000_0000_0000_0000_0000_0000

pinAddress00     long      0
pinAddress01     long      4
pinAddress02     long      8
pinAddress03     long      12
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
pinAddress10     long      64
pinAddress11     long      68
pinAddress12     long      72
pinAddress13     long      76
pinAddress14     long      80
pinAddress15     long      84
pinAddress16     long      88
pinAddress17     long      92
pinAddress18     long      96
pinAddress19     long      100
pinAddress1A     long      104
pinAddress1B     long      108
pinAddress1C     long      112
pinAddress1D     long      116
pinAddress1E     long      120
pinAddress1F     long      124

dutyTable00     long      0
dutyTable01     long      0
dutyTable02     long      0
dutyTable03     long      0
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
dutyTable10     long      0
dutyTable11     long      0
dutyTable12     long      0
dutyTable13     long      0
dutyTable14     long      0
dutyTable15     long      0
dutyTable16     long      0
dutyTable17     long      0
dutyTable18     long      0
dutyTable19     long      0
dutyTable1A     long      0
dutyTable1B     long      0
dutyTable1C     long      0
dutyTable1D     long      0
dutyTable1E     long      0
dutyTable1F     long      0

dutyReg       res       1    ' Register that duty cycle gets read into
counter       res       1    ' Counter for generating the address table
pinTableBase  res       1    ' HUBRAM address of pin addresses
buffer        res       1    ' Bitmask buffer  
                        FIT
                        