CC=gcc
CFLAGS=-Wall -Wextra -Wformat=2 -Wswitch-default -Wcast-align -Wpointer-arith \
    -Wbad-function-cast -Wstrict-prototypes -Winline -Wundef -Wnested-externs \
    -Wcast-qual -Wshadow -Wwrite-strings -Wconversion -Wunreachable-code \
    -Wstrict-aliasing=2 -ffloat-store -fno-common -fstrict-aliasing \
    -lm -std=c89 -pedantic -O0 -ggdb3 -pg --coverage -std=c99

all: firecracker-vm

firecracker-vm: firecracker-vm.c firecracker-vm.h

run: firecracker-vm
	./firecracker-vm

clean:
	rm ./firecracker-vm