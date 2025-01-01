
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>

// Define the 8080 CPU state structure
typedef struct CPU {
    uint8_t a, b, c, d, e, h, l;  // Registers

    uint16_t sp;         // Stack pointer
    uint16_t pc;         // Program counter
    unsigned long cycle; // Cycle counter

    bool int_pending;
    uint8_t int_vector;
    uint8_t int_delay;

    uint8_t psw;

    // Flags (boolean representation for easier handling)
    bool zf;          // Zero flag
    bool sf;          // Sign flag
    bool pf;          // Parity flag
    bool cf;          // Carry flag
    bool hf;          // Half carry flag
    bool ie;          // Interrupt Enable flag
    bool halted;      // 1 if CPU is halted, 0 otherwise

    // Adapted from https://github.com/superzazu/8080/blob/master/i8080.h
    // Memory and IO interface
    uint8_t (*read_byte)(void*, uint16_t);          // User function to read from memory
    void (*write_byte)(void*, uint16_t, uint8_t);   // User function to write to memory
    uint8_t (*port_in)(void*, uint8_t);             // User function to read from port
    void (*port_out)(void*, uint8_t, uint8_t);      // User function to write to port
    void *user_memory;                              // Pointer to custom data location

    // I/O Ports
    uint8_t in_ports[256];   // Input ports (0x00 to 0xFF)
    uint8_t out_ports[256];  // Output ports (0x00 to 0xFF)

} CPU;


// Emulator main function
void cpu_init(CPU *cpu);
void cpu_process(CPU *cpu);
void interrupt(CPU* cpu, uint8_t opcode);
