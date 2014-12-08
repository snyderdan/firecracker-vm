SPINC=openspin
SPINCFLAGS=-M 32768
all: eeprom

eeprom: firecracker-propvm.eeprom

binary: firecracker-propvm.binary

firecracker-propvm.eeprom: firecracker-propvm.spin
		$(SPINC) $(SPINCFLAGS) -e $^ -o $@

firecracker-propvm.binary: firecracker-propvm.spin
		$(SPINC) $(SPINCFLAGS) -b $^ -o $@
