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

#ifndef NS_PER_SEC
#define NS_PER_SEC 1000000000
#endif

void flip_bytes(uint8_t data[], uint8_t nbytes)
{
	for(uint8_t i = 0; i < nbytes/2; i++){
		// Naive Swap
		uint8_t temp = data[i];
		data[i] = data[nbytes - (i+1)];
		data[nbytes - (i+1)] = temp;
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
	if(fvm -> stack_top + nbytes > fvm -> stack_bottom + fvm -> stack_size - 1){
		return -1;
	}
	memmove(fvm->stack_top, bytes, nbytes);
	fvm -> stack_top += nbytes;
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
	if(output_num > fvm->num_outputs - 1 || fvm -> outputs[output_num].locked){
		return -1;
	}
	fvm -> outputs[output_num].pwm_val = duty_cycle;
	return 0;
}

int8_t fvm_write_normalized(fvm_t *fvm){
	if(fvm_pop(fvm, 2)){
		return -1;
	}
	return fvm_write(fvm, *(fvm -> stack_top +1), *(fvm -> stack_top));
}

int8_t fvm_delay(uint32_t delay_time){
	const struct timespec delay_timespec = {
			.tv_sec  = delay_time / NS_PER_SEC, 
			.tv_nsec = delay_time % NS_PER_SEC
	};
	if(nanosleep(&delay_timespec, NULL)){
		return -1;
	}
	return 0;
}

int8_t fvm_delay_normalized(fvm_t *fvm){
	if(fvm_pop(fvm, 4)){
		return -1;
	}
	/*
	Due to the wonders of reordering bytes to preserve the stack, 
	our delay_time is now little-endian, 
	despite having been passed in as big-endian

	We'll flip it if needed
	*/
	#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
	flip_bytes(fvm -> stack_top, 4);
	#endif
	uint32_t delay_time = *((uint32_t*) fvm -> stack_top);

	return fvm_delay(delay_time);
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
		fvm_command_t current_command = fat_commands[ncommands - (i+1)];
		//Must iterate through backwards to preserve order
		if(current_command.type == PUSH_COMMAND){
			//don't flip
			memmove(data_next_free, current_command.data+1, current_command.data[0]);
			data_next_free += current_command.data[0];
		}else if(current_command.type == POP_COMMAND){
			//pass
		}else if(current_command.type == WRITE_COMMAND){
			if(current_command.data){
				memmove(data_next_free, current_command.data, 2);
				flip_bytes(data_next_free, 2);
				data_next_free += 2;
			}
		}else if(current_command.type == DELAY_COMMAND){
			if(current_command.data){
				memmove(data_next_free, current_command.data, 4);
				flip_bytes(data_next_free, 4);
				data_next_free += 4;
			}
		}
	}
	//Create PUSH Command(s)
	fvm_command_t * thin_command_next_free = thin_commands;
	uint8_t * data_next_unused = data;
	uint8_t * data_end = data + datalen;
	while(data_next_unused < data_end){
		uint16_t data_remaining = data_end - data_next_unused;
		assert(data_remaining > 0);
		uint8_t push_data_len = MIN(datalen, 0xfe);
		thin_command_next_free -> type = PUSH_COMMAND;
		thin_command_next_free -> data = malloc(push_data_len + 1);
		thin_command_next_free -> data[0] = push_data_len;
		memmove(thin_command_next_free -> data + 1, data_next_unused, push_data_len);
		data_next_unused += push_data_len;
		datalen -= push_data_len;
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
	uint8_t *text_end = text + len;
	while(text < text_end){
		if(STARTS_WITH((char *)text, FVM_PUSH_MNEMONIC)){
			unsigned int datalen = text[2];
			text += datalen + 1; //skip data + datalen byte
		}else if(STARTS_WITH((char *)text, FVM_POP_MNEMONIC)){
			text += 0; //no data to skip
		}else if(STARTS_WITH((char *)text, FVM_WRITE_MNEMONIC)){
			text += 2; //skip 16bits
		}else if(STARTS_WITH((char *)text, FVM_DELAY_MNEMONIC)){
			text += 4; //skip 32bits
		}else{
			return -1;
		}
		text += FVM_MNEMONIC_LENGTH + 1;//skip semicolon and mnemonic
		count++;
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

void fvm_print_output_state(fvm_t * fvm){
	printf("|");
	for(uint i = 0; i < fvm -> num_outputs; i++){
		printf("0x%02x|", fvm -> outputs[i].pwm_val );
	}
	printf("\n" );
}


int8_t fvm_execute_normalized_commands(fvm_t * fvm, fvm_command_t commands[], uint16_t ncommands){
	int8_t retval = 0;
	printf("FVM Status: %d Outputs, %d Bytes Stack\n", fvm -> num_outputs, fvm -> stack_size);
	fvm_print_output_state(fvm);
	for (uint16_t i = 0; i < ncommands; ++i)
	{	
		switch(commands[i].type){
		case PUSH_COMMAND:
			printf("%s: %02x Bytes \n", FVM_PUSH_MNEMONIC, commands[i].data[0]);
			if((retval = fvm_push(fvm, commands[i].data+1, commands[i].data[0]))){
				return retval;
			}
			break;
		case POP_COMMAND:
			printf("%s\n", FVM_POP_MNEMONIC);
			if((retval = fvm_pop(fvm, 1))){
				return retval;
			}
			break;
		case WRITE_COMMAND:
			printf("%s\n", FVM_WRITE_MNEMONIC);
			if((retval = fvm_write_normalized(fvm))){
				return retval;
			}
			break;
		case DELAY_COMMAND:
			printf("%s\n", FVM_DELAY_MNEMONIC);
			if((retval = fvm_delay_normalized(fvm))){
				return retval;
			}
			break;
		default:
			return -1;
		}
		fvm_print_output_state(fvm);
	}
	return retval;
}
 