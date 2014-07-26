/*
 * firecracker-vm.c - Desktop FVM Simulator
 *
 */
#include "firecracker-vm.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#endif

#ifndef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef STARTS_WITH
#define STARTS_WITH(buf,c) (strncmp((buf), (c), strlen(c)) == 0)
#endif
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
			Pops one uint8_t from the top of the stack
		Delay => DE[t]
			t <-- unsigned 32bit big-endian number of nanoseconds
			Pauses FVM for [t] nanoseconds
		Write => WR[n][d]
			n <-- unsigned 8bit output number
			d <-- unsigned 8bit PWM Duty Cycle
			Update output [n] to run at duty cycle [d]

	For simplicity, only the Push next_command may write data to the stack for other Commands,
	which pop from the stack to fill their parametersso the syntax shown above must be rewritten. 
	For example:

	WR[0x00][0xff] --> PU[0x02][0xff][0x00]WR
[
	WR[0x00][0x80];DE[10^9] --> PU[0x06][10^9][0x80][0x00];WR;DE

	The PU commands could alse be broken up as follows:

	PU[0x06][10^9][0x80][0x00];WR;DE --> PU[0x02][0x80][0x00];WR;PU[0x04][10^9];DE

	While this is less efficient, it allows execution of the Write Command before the Delay Command is ready.


 */



void flip_bytes(uint8_t data[], uint8_t nbytes)
{
	for(uint8_t i = 0; i < nbytes/2; i++){
		// Naive Swap
		uint8_t temp = data[i];
		data[i] = data[nbytes - i];
		data[nbytes - i] = temp;
	}
}

void fvm_init(fvm_t *fvm, uint8_t num_outputs, uint16_t stack_size){
	fvm_output_t *outputs = calloc(num_outputs, sizeof(fvm_output_t));
	fvm -> num_outputs = num_outputs;
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

int8_t fvm_push(fvm_t *fvm, uint8_t bytes[], uint8_t nbytes){
	if(fvm->stack_top + nbytes > fvm->stack_bottom + fvm->stack_size - 1){
		return -1;
	}
	memmove(fvm->stack_top, bytes, nbytes);
	return 0;
}

int8_t fvm_pop(fvm_t *fvm, uint8_t nbytes){
	if(fvm -> stack_top - nbytes < fvm -> stack_bottom){
		return -1;
	}
	fvm -> stack_top -= nbytes;
	return 0;
}

int8_t fvm_write(fvm_t *fvm, uint8_t output_num, uint8_t duty_cycle){
	if(output_num > fvm->num_outputs - 1){
		return -1;
	}
	fvm -> outputs[output_num].pwm_val = duty_cycle;
	return 0;
}

int8_t fvm_write_normalized(fvm_t *fvm){
	if(fvm_pop(fvm, 2) != 0){
		return -1;
	}
	return fvm_write(fvm, *(fvm -> stack_top +2), *(fvm -> stack_top +1));
}

int8_t fvm_delay(uint32_t delay_time){
	const struct timespec delay_timespec = {.tv_sec  = 0, .tv_nsec = delay_time};
	if(nanosleep(&delay_timespec, NULL)){
		return -1;
	}
	return 0;
}


uint16_t fvm_count_normalized_commands(fvm_command_t commands[], uint16_t ncommands){
	uint16_t count = 0;
	uint16_t datalen = 0;
	for (uint16_t i = 0; i < ncommands; i++){
		if(commands[i].type == PUSH_COMMAND){
			datalen += (1 + commands[i].data[0]);
		}else if(commands[i].type == POP_COMMAND){
			count++;
		}else if(commands[i].type == WRITE_COMMAND){
			count++;
			if(commands[i].data){
				datalen += 2;
			}
		}else if(commands[i].type == DELAY_COMMAND){
			count++;
			if(commands[i].data){
				datalen += 4;
			}
		}
	}
	if(datalen){
		count += ((datalen / 0xff) + (datalen % 0xff ? 1:0));
	}
	return count;
}

int8_t fvm_normalize_commands(fvm_command_t fat_commands[], uint16_t ncommands, fvm_command_t thin_commands[]){
	/* Returns PU's with all data, plus commands to execute	 */
	uint32_t datalen = 0;

	// Figure out how much data we need to store
	for (uint16_t i = 0; i < ncommands; i++){
		if(fat_commands[i].type == PUSH_COMMAND){
			datalen += 1 + fat_commands[i].data[0];
		}else if(fat_commands[i].type == POP_COMMAND){
			//pass
		}else if(fat_commands[i].type == WRITE_COMMAND){
			if(fat_commands[i].data){
				datalen += 2;
			}
		}else if(fat_commands[i].type == DELAY_COMMAND){
			if(fat_commands[i].data){
				datalen += 4;
			}
		}
	}
	// Copy data into temp buffer
	uint8_t * data = malloc(datalen);
	uint8_t * data_next_free = data;
	for (uint16_t i = 0; i < ncommands; i++){
		if(fat_commands[i].type == PUSH_COMMAND){
			//don't flip
			memmove(data_next_free, fat_commands[i].data+1, fat_commands[i].data[0]);
			data_next_free += fat_commands[i].data[0];
		}else if(fat_commands[i].type == POP_COMMAND){
			//pass
		}else if(fat_commands[i].type == WRITE_COMMAND){
			if(fat_commands[i].data){
				memmove(data_next_free, fat_commands[i].data, 2);
				flip_bytes(data_next_free, 2);
				data_next_free += 2;
			}
		}else if(fat_commands[i].type == DELAY_COMMAND){
			if(fat_commands[i].data){
				memmove(data_next_free, fat_commands[i].data, 4);
				flip_bytes(data_next_free, 4);
				data_next_free += 4;
			}
		}
	}
	//Create PUSH Command(s)
	fvm_command_t * thin_command_next_free = thin_commands;
	while(data <= data_next_free){
		uint16_t data_remaining = data_next_free - data;
		assert(data_remaining > 0);
		uint8_t push_data_len = MIN(datalen, 0xfe);
		thin_command_next_free -> type = PUSH_COMMAND;
		thin_command_next_free -> data = malloc(push_data_len + 1);
		thin_command_next_free -> data[0] = push_data_len;
		memmove(thin_command_next_free -> data + 1, data, push_data_len);
		data += push_data_len;
		thin_command_next_free++;
	}
	//Populate remainder of commands
	for (uint16_t i = 0; i < ncommands; i++){
		if(fat_commands[i].type != PUSH_COMMAND){
			thin_command_next_free -> type = fat_commands[i].type;
			thin_command_next_free -> data = NULL;
			thin_command_next_free++;
		}
	}
	free(data);
	return 0;
}

uint16_t fvm_count_fat_commands(uint8_t text[], size_t len){
	uint16_t count = 0;
	for(uint i = 0; i < len; i++){
		if(text[i] == ';' || i == len - 1){
			count++;
		}
	}
	return count;
}



int8_t fvm_parse_all_fat_commands(uint8_t text[], size_t len, fvm_command_t commands[]){
	uint8_t *text_end = text + len;
	fvm_command_t * next_command = commands;
	while(text < text_end){
		if(STARTS_WITH((char *)text, FVM_PUSH_MNEMONIC)){
			next_command -> type = PUSH_COMMAND;
			unsigned int datalen = text[2];
			next_command -> data = malloc(datalen);
			memmove(next_command -> data, text + 3, datalen);
			text += datalen + 1; //skip data + datalen byte
		}else if(STARTS_WITH((char *)text, FVM_POP_MNEMONIC)){
			next_command -> type = POP_COMMAND;
			next_command -> data = NULL;
			text += 0; //no data to skip
		}else if(STARTS_WITH((char *)text, FVM_WRITE_MNEMONIC)){
			next_command -> type = WRITE_COMMAND;
			next_command -> data = malloc(2);
			memmove(next_command -> data, text+2, 2);
			text += 2; //skip 2x 8bits
		}else if(STARTS_WITH((char *)text, FVM_DELAY_MNEMONIC)){
			next_command -> type = DELAY_COMMAND;
			next_command -> data = malloc(4);
			memmove(next_command -> data, text + 2, 4);
			text += 4; //skip 32bits
		}else{
			return -1;
		}
		text += FVM_MNEMONIC_LENGTH + 1;//skip semicolon and mnemonic
		next_command++;
	}
	return 0;
}

int8_t fvm_execute_normalized_commands(fvm_t * fvm, fvm_command_t commands[], uint16_t ncommands){
	for (uint16_t i = 0; i < ncommands; ++i)
	{
		switch(commands[i].type){
		case PUSH_COMMAND:
			fvm_push(fvm, commands[i].data+1, commands[i].data[0]);
			break;
		case POP_COMMAND:
			fvm_pop(fvm, 1);
			break;
		case WRITE_COMMAND:
			fvm_write_normalized(fvm);
			break;
		case DELAY_COMMAND:
			break;
		default:
			return -1;
		}
	}
	return 0;
}
 