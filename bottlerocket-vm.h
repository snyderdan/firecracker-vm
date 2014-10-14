/*
 * bottlerocket-vm.h
 */

#ifndef _BRKT_VM
#define _BRKT_VM 

#include <stdint.h>
#include "firecracker-vm.h"


#ifndef BRKT_HIGH
#define BRKT_HIGH 0xff
#endif

#ifndef BRKT_LOW
#define BRKT_LOW 0
#endif

typedef struct brkt_vm_s
{
	uint8_t num_timings;
	brkt_timing_t * timings;
	uint8_t outputs_owned_mask;
	uint8_t num_outputs;
	fvm_output_t *outputs;
} brkt_t;

typedef struct brkt_timing_s
{
	uint32_t time_high_1;
	uint32_t time_low_1;
	uint32_t time_high_0;
	uint32_t time_low_0;
	uint32_t time_reset;
} brkt_timing_t;

const brkt_timing_t WS2811_TIMING = {1200, 1300, 500, 2000, 50000};
const brkt_timing_t WS2812_TIMING = {700, 600, 350, 800, 50000};

int8_t brkt_init(brkt_t *brkt, fvm_t *fvm, uint8_t num_timings);
int8_t brkt_free(brkt_t *brkt);
int8_t brkt_new_timing(brkt_t *brkt, uint8_t index, brkt_timing_t *timing);
int8_t brkt_send(brkt_t *brkt, uint8_t pin_num, uint8_t timing_num, uint8_t data_len, uint8_t *data);

#endif