/*
 *	firecracker-tester.c
 */
#include "firecracker-vm.h"
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define TEST_COMMAND FVM_POP_MNEMONIC

int is_hex(char c){
	c= tolower(c);
	return ('a' <= c && 'f' >= c) || ('0' <= c && '9' >= c);
}

int count_unescaped_characters(char text[]){
	/*
		\\ --> \
		\ff --> [0xff]
	*/
	uint count = 0;
	for(uint i = 0; i < strlen(text); i++){
		if(text[i] == '\\'){
			if(text[i+1] == '\\'){
				count++;
				i++; // skip next char
			}else if(is_hex(text[i+1]) && is_hex(text[i+2])){
				count++;
				i+=2;
			}else{
				return -1;
			}
		}else{
			count++;
		}
	}
	return count;
}

int unescape_text(char escaped[], uint8_t unescaped[]){
	for (uint i = 0; i < strlen(escaped); i++){
		if(escaped[i] == '\\'){
			if(escaped[i+1] == '\\'){
				unescaped[0] = '\\';
				i++; // skip next char
			}else if(is_hex(escaped[i+1]) && is_hex(escaped[i+2])){
				char temp = escaped[i+3]; //Save value after hex string
				escaped[i+3] = '\0'; // create fake end of string
				unescaped[0] = (uint8_t)strtol(escaped + i+1, NULL, 16);
				escaped[i+3] = temp; //Restore value
				i+=2;//skip 2
			}else{
				return -1;
			}
		}else{
			unescaped[0] = escaped[i];
		}
		unescaped++;
	}
	return 0;
}

int main(int argc, char const *argv[])
{
	fvm_t *my_fvm = malloc(sizeof(fvm_t));
	fvm_init(my_fvm, FVM_DEFAULT_NUM_OUTPUTS, FVM_DEFAULT_STACK_SIZE);
	printf("Initialized FVM\n");
	if (argc > 1){
		char * command_text = argv[1];
		uint unescaped_len = count_unescaped_characters(command_text);
		uint8_t * unescaped_command_text = malloc(unescaped_len);
		printf("unescaped_len = %d\n", unescaped_len );
		unescape_text(command_text, unescaped_command_text);
		uint fat_commands_len = fvm_count_fat_commands(unescaped_command_text, unescaped_len);
		fvm_command_t  * fat_commands = calloc(fat_commands_len, sizeof(fvm_command_t));
		if(fvm_parse_all_fat_commands(unescaped_command_text, unescaped_len, fat_commands)){
			printf("Error parsing\n");
		}
		for (uint i = 0; i < fat_commands_len; ++i){
			printf("Parsing command %d\n", i);
			printf("Command: %d ; Data: %p\n", fat_commands[i].type, fat_commands[i].data);	
		}
		uint normalized_commands_len = fvm_count_normalized_commands(fat_commands, fat_commands_len);
		fvm_command_t * normalized_commands = calloc(normalized_commands_len, sizeof(fvm_command_t));
		if(fvm_normalize_commands(fat_commands, fat_commands_len, normalized_commands)){
			printf("Error normalizing\n");
		}
		for (uint i = 0; i < normalized_commands_len; ++i){
			printf("Normalizing command %d\n", i);
			printf("Command: %d ; Data: %p\n",normalized_commands[i].type, normalized_commands[i].data);	
		}
	}
	fvm_free(my_fvm);
	printf("Freed FVM\n");
	return 0;
}