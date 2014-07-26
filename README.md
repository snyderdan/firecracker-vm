# Firecracker
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
