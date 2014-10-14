# Firecracker
The Firecracker is an LED Driver board capable of powering and controlling a mix
of addressable and single- or tri-color LEDs with high combined draw. The default
hardware drives 16 channels of 12v/5v and can source a combined 15A of power.

## Firecracker VM

Firecracker VM parses a single input stream of mixed commands and data and splits it into two parts:

* The Stack - Composed of data
* The Commands - Macros to execute with data on the stack

It defines a few basic Commands, which are baked in to the Firecracker Virtual Machine:

* Push => PU[n][data1][data2][...][datan]

- n <-- unsigned 8bit number of bytes to push to stack
- data <-- bytes to push to stack, so datan is on top
- Pushes [n] bytes of [data] to top of stack

* Pop => PO

- Pops one byte from the top of the stack

* Delay => DE[t]

- t <-- unsigned 32bit big-endian number of nanoseconds
- Pauses FVM for [t] nanoseconds

* Write => WR[n][d]

- n <-- unsigned 8bit output number
- d <-- unsigned 8bit PWM Duty Cycle
- Update output [n] to run at duty cycle [d]

For simplicity, only the Push Command may write data to the stack for other Commands,
which pop from the stack to fill their parameters so the syntax shown above must be rewritten. 
For example:

```
WR[0x00][0xff] --> PU[0x02][0xff][0x00]WR

WR[0x00][0x80];DE[10^9] --> PU[0x06][10^9][0x80][0x00];WR;DE

PU[0x01][0x11];WR[0x0a][0xff] --> PU[0x03][0x11][0xff][0x0a];WR
```

The PU commands could alse be broken up as follows:

```
PU[0x06][10^9][0x80][0x00];WR;DE --> PU[0x02][0x80][0x00];WR;PU[0x04][10^9];DE
```

While this is less efficient, it allows execution of the Write Command before the Delay Command is ready.

### Custom Macros

Firecracker supports up to [n](64?) custom macros, each one up to n (512?) bytes long.
Macros are saved on the onboard EEPROM between each boot. Macros are manipulated with
the following commands:

* New Macro => NM[m][n][command1][command2][...][commandn]

- m <-- unsigned 8bit macro number and options
- n <-- unsigned 8bit number of commands that follow
- command1 ... commandn <-- parsed commands that are valid at time of


## Bottle Rocket

Bottle Rocket is a separate VM responsible for high-precision pulses of pins for
addressable LED strips. It uses a special commands to override the PWM driving 
logic and toggle pins at precise time intervals.

Bottle Rocket stores a set of timings for high and low bits

NT[n][T1H][T1L][T0H][T0L][RES]
new timing

#WS2812
NT[0][700][600][350][800][50000] 
#WS2811
NT[1][1200][1300][500][2000][50000]

BS[p][t][l][data1][data2][...][datan]
Bottlerocket send
p <-- 8bit  pin#
t <-- 8bit timing#
l <-- 8bit number of bytes of data
data <-- data to send




