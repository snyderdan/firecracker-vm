CC=gcc
CFLAGS=-Wall -Wextra -Wformat=2 -Wswitch-default -Wcast-align -Wpointer-arith \
    -Wbad-function-cast -Wstrict-prototypes -Winline -Wundef -Wnested-externs \
    -Wcast-qual -Wshadow -Wwrite-strings -Wconversion -Wunreachable-code \
    -Wstrict-aliasing=2 -ffloat-store -fno-common -fstrict-aliasing \
    -lm -pedantic -O0 -ggdb3 -pg --coverage -std=gnu11
all: firecracker-tester

firecracker-tester: firecracker-tester.c firecracker-vm.o

firecracker-vm.o: firecracker-vm.c firecracker-vm.h

run: firecracker-tester
	./firecracker-tester

clean:
	rm -rf ./firecracker-vm.o
	rm -rf ./firecracker-tester
	rm -rf ./firecracker-tester.gcda
	rm -rf ./firecracker-tester.gcno
	rm -rf ./firecracker-vm.gcda
	rm -rf ./firecracker-vm.gcno
	rm -rf ./gmon.out