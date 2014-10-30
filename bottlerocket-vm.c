/*
 * bottlerocket-vm.h
 */

#include "bottlerocket-vm.h"
#include <string.h>
#include <stdlib.h>
#include <time.h>

int8_t brkt_init(brkt_t *brkt, fvm_output_t *outputs, uint8_t num_timings){
	brkt -> outputs_owned_mask = 0;
	brkt -> outputs = outputs;
	brkt -> timings = calloc(num_timings, sizeof(brkt_timing_t));
	// Add default timings
	brkt_new_timing(brkt, 0, &WS2812_TIMING);
	brkt_new_timing(brkt, 1, &WS2811_TIMING);
	return 0;
}

int8_t brkt_free(brkt_t *brkt){
	free(brkt -> timings);
	free(brkt);
	return 0;
}
/*
 * It's the user's responsibility not to overwrite existing timings
 */
int8_t brkt_new_timing(brkt_t *brkt, uint8_t index, brkt_timing_t *timing){
	memmove(brkt -> timings+index, timing, sizeof(brkt_timing_t));
	return 0;
}

int8_t brkt_send(brkt_t *brkt, uint8_t output_num, uint8_t timing_num, uint8_t data_len, uint8_t *data){
	fvm_output_t output = brkt -> outputs[output_num];
	brkt_timing_t timing = brkt -> timings[timing_num];
	if(output.locked){
		//output already in use
		return -1;
	}
	output.locked = 1;
	//drop line to 0 for settle
	output.pwm_val = 0;
	fvm_delay(timing.time_reset);
	while(data_len > 0){
		for (int i = 0; i < 8; i++){
			if(*data >> i){
				output.pwm_val = BRKT_HIGH;
				fvm_delay(timing.time_high_1);
				output.pwm_val = BRKT_LOW;
				fvm_delay(timing.time_low_1);
			}else{
				output.pwm_val = BRKT_HIGH;
				fvm_delay(timing.time_high_0);
				output.pwm_val = BRKT_LOW;
				fvm_delay(timing.time_low_0);
			}
		}
		data_len--;
		data++;
	}
	return 0;

}