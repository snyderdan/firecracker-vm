/*
 * firecracker-vm.c - Desktop FVM Simulator
 *
 */
#include "firecracker-vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/*
	Firecracker VM parses a single input stream of mixed commands and data and splits it into two parts:
		The Stack - Composed of data
		The Commands - Macros to execute with data on the stack
	
	It defines a few basic Commands, which are baked in to the Firecracker Virtual Machine:
		Push => PU[n][data1][data2][...][datan]
			n <-- unsigned 8bit number of bytes to push to stack
			data <-- bytes to push to stack, so datan is on top
			Pushes [n] bytes of [data] to top of stack
		Pop => PO
			Pops one byte from the top of the stack
		Delay => DE[t]
			t <-- unsigned 32bit big-endian number of nanoseconds
			Pauses FVM for [t] nanoseconds
		Write => WR[n][d]
			n <-- unsigned 8bit output number
			d <-- unsigned 8bit PWM Duty Cycle
			Update output [n] to run at duty cycle [d]

	For simplicity, only the Push Command may write data to the stack for other Commands,
	which pop from the stack to fill their parametersso the syntax shown above must be rewritten. 
	For example:

	WR[0x00][0xff] --> PU[0x02][0xff][0x00]WR
[
	WR[0x00][0x80];DE[10^9] --> PU[0x06][10^9][0x80][0x00];WR;DE

	The PU commands could alse be broken up as follows:

	PU[0x06][10^9][0x80][0x00];WR;DE --> PU[0x02][0x80][0x00];WR;PU[0x04][10^9];DE

	While this is less efficient, it allows execution of the Write Command before the Delay Command is ready.


 */

int main(int argc, char const *argv[])
{
	fvm_t *my_fvm = malloc(sizeof(fvm_t));
	fvm_init(my_fvm, FVM_DEFAULT_NUM_OUTPUTS, FVM_DEFAULT_STACK_SIZE);
	fvm_free(my_fvm);
	return 0;
}

void flip_bytes(byte nbytes, byte data[])
{
	for(byte i = 0; i < nbytes/2; i++){
		// Naive Swap
		byte temp = data[i];
		data[i] = data[nbytes - i];
		data[nbytes - i] = temp;
	}
}

void fvm_init(fvm_t *fvm, unsigned int num_outputs, unsigned int stack_size){
	fvm_output_t *outputs = calloc(num_outputs, sizeof(fvm_output_t));
	fvm -> outputs = outputs;
	fvm -> stack_size = stack_size;
	fvm -> stack_bottom = malloc(stack_size);
	fvm -> stack_top = fvm -> stack_bottom;
}

void fvm_free(fvm_t *fvm){
	free(fvm->outputs);
	free(fvm->stack_bottom);
	free(fvm);
}

int fvm_stack_push(fvm_t *fvm, unsigned int nbytes, byte bytes[]){
	if(fvm->stack_top + nbytes > fvm->stack_bottom + fvm->stack_size - 1){
		return -1;
	}
	memcpy(fvm->stack_top, bytes, nbytes);
	fvm -> stack_top += nbytes;
	return fvm -> stack_top - fvm -> stack_bottom
}

int fvm_stack_pop(fvm_t *fvm, unsigned int nbytes){
	if(fvm -> stack_top - nbytes < fvm -> stack_bottom){
		return -1;
	}
	fvm -> stack_top -= nbytes;
	return fvm -> stack_top - fvm -> stack_bottom;
}