{{
This object simplifies writing to and reading from the Propeller chip's 24LC256 EEPROM
program memory.  This object also has methods that can be used to back up sections of
RAM so that they are automatically reloaded into RAM during reboot.

EXAMPLES:

  In addition to the examples below, see Test Propeller Eeprom...spin

  OBJ
    eeprom : "Propeller Eeprom"
   
  ...

  VAR 
    long value[31]
    word a, b, c, d
    byte e, f, g, mybytes[20]
   
  ...

  PUB
   
    'Copy everything from value[0]..mybytes[20] from RAM to the same addresses in EEPROM.
    'These values will automatically be loaded back into RAM in the event of a reboot.  
    eeprom.VarBackup(@value, @myBytes[20])         ' Copy from RAM to EEPROM
   
    ...
   
    'Restore a previous snapshot of variables in main RAM.
    eeprom.VarRestore(@value, @mybytes[20])
   
    ...
   
    'Copy a snapshot of just the values long array to the same addresses in EEPROM. Since
    'these are long variables, 3 has to be added to address passed to the FromRam method's
    'endAddr parameter to account for the three bytes that follow the long's address. 
    eeprom.VarBackup(@value, @value[31] + 3)
   
    ...
   
    'Copy all the variable contents from the start of the value array through the end of
    'the mybytes array to addresses 20_000..20_158 in EEPROM.
    eeprom.FromRam(@value, @mybytes[20], 20_000)
   
    ...
   
    'Copy only the longs and words back to RAM variables from EEPROM
    'addresses 20_000..20_135.
    eeprom.ToRam(@value, @d + 1, 20_000)
   
NOTES:

(1) The endAddr parameters are byte addresses.  If you use @myLastLongAddress, make sure
to add 3 to the address since it's pointing to the first of the four bytes that make up
the long.  Likewise, if you use @myLastWordAddress, add 1 to account for the two bytes
that make up the word.  Byte addresses do not need to be adjusted.

(2) Regardless of the order you declare the variables, the Spin compiler sets aside longs
first, then words, then bytes. If you are backing up or restoring sections of main RAM
that span more than one variable type with the VarBackup or VarRestore methods, make sure 
to declare all your variables in this order: long, word, byte.  Multiple method calls can
also be made to back up specific sections if variables have to be declared in groups
of different sizes that are not contiguous.  

(3) Programs are stored in EEPROM and RAM, starting at the smallest byte address and
building toward larger addresses.  So, datalogging programs that use EEPROM for storage
of accumulated measurements should start at the largest address and build toward smaller
addresses.

(4) BEWARE: Downloading a program overwrites ALL EEPROM!  Your datalogging application
needs to have a built-in way of uploading the data after the measurements have been
taken because a separate program will overwrite all the collected data.
}}

PUB Docs
