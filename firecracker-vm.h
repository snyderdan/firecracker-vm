/*
 * firecracker-vm.h - Desktop FVM Simulator
 */

#ifndef _FVM_H

#define _FVM_H

#include <stdint.h>
#include <stdlib.h>

#define FVM_DEFAULT_STACK_SIZE 256
#define FVM_DEFAULT_NUM_OUTPUTS 16

#define FVM_MNEMONIC_LENGTH 2
#define FVM_PUSH_MNEMONIC "PU"
#define FVM_POP_MNEMONIC "PO"
#define FVM_WRITE_MNEMONIC "WR"
#define FVM_DELAY_MNEMONIC "DE"

typedef struct fvm_output_s
{
	uint8_t pwm_val;
	uint8_t locked;
} fvm_output_t;


typedef struct fvm_s
{
	uint16_t stack_size;
	uint8_t num_outputs;
	uint8_t * stack_top;
	uint8_t * stack_bottom;
	fvm_output_t *outputs;
} fvm_t;

typedef enum fvm_command_type_e 
{
	PUSH_COMMAND,
	POP_COMMAND,
	WRITE_COMMAND,
	DELAY_COMMAND
} fvm_command_type_t;

typedef struct fvm_command_s 
{
	fvm_command_type_t type;
	uint8_t * data;
} fvm_command_t;

void fvm_init(fvm_t *fvm, uint8_t num_outputs, uint16_t stack_size);
void fvm_free(fvm_t *fvm);

int8_t fvm_push(fvm_t *fvm, uint8_t bytes[], uint8_t nbytes);
int8_t fvm_pop(fvm_t *fvm, uint8_t nbytes);
int8_t fvm_write(fvm_t *fvm, uint8_t output_num, uint8_t duty_cycle);
int8_t fvm_write_normalized(fvm_t *fvm);
int8_t fvm_delay(uint32_t delay_time);
int8_t fvm_delay_normalized(fvm_t *fvm);

uint16_t fvm_count_normalized_commands(fvm_command_t commands[], uint16_t ncommands);
int8_t fvm_normalize_commands(fvm_command_t fat_commands[], uint16_t ncommands, fvm_command_t thin_commands[]);
uint16_t fvm_count_fat_commands(uint8_t text[], size_t len);
int8_t fvm_parse_all_fat_commands(uint8_t text[], size_t len, fvm_command_t commands[]);
int8_t fvm_execute_normalized_commands(fvm_t * fvm, fvm_command_t commands[], uint16_t ncommands);

#endif