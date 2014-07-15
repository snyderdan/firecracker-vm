/*
 * firecracker-vm.h - Desktop FVM Simulator
 */

#ifndef _FVM_H

#define _FVM_H

#define FVM_DEFAULT_STACK_SIZE 256
#define FVM_DEFAULT_NUM_OUTPUTS 16

typedef unsigned char byte;

typedef struct fvm_output_s
{
	unsigned int channel;
	byte pwm_val;
} fvm_output_t;


typedef struct fvm_s
{
	unsigned int stack_size;
	byte * stack_top;
	byte * stack_bottom;
	fvm_output_t *outputs;
} fvm_t;

int main(int argc, char const *argv[]);
void flip_bytes(byte nbytes, byte data[]);

int fvm_init(fvm_t *fvm, unsigned int num_outputs, unsigned int stack_size);
int fvm_free(fvm_t *fvm);


#endif