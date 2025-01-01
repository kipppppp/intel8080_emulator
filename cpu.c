
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cpu.h"

// Initialize cycle count lookup table
// This array copied from https://github.com/superzazu/8080/blob/master/i8080.c#L7
static const uint8_t OPCODES_CYCLES[256] = {
//  0  1   2   3   4   5   6   7   8  9   A   B   C   D   E  F
    4, 10, 7,  5,  5,  5,  7,  4,  4, 10, 7,  5,  5,  5,  7, 4,  // 0
    4, 10, 7,  5,  5,  5,  7,  4,  4, 10, 7,  5,  5,  5,  7, 4,  // 1
    4, 10, 16, 5,  5,  5,  7,  4,  4, 10, 16, 5,  5,  5,  7, 4,  // 2
    4, 10, 13, 5,  10, 10, 10, 4,  4, 10, 13, 5,  5,  5,  7, 4,  // 3
    5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 4
    5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 5
    5, 5,  5,  5,  5,  5,  7,  5,  5, 5,  5,  5,  5,  5,  7, 5,  // 6
    7, 7,  7,  7,  7,  7,  7,  7,  5, 5,  5,  5,  5,  5,  7, 5,  // 7
    4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // 8
    4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // 9
    4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // A
    4, 4,  4,  4,  4,  4,  7,  4,  4, 4,  4,  4,  4,  4,  7, 4,  // B
    5, 10, 10, 10, 11, 11, 7,  11, 5, 10, 10, 10, 11, 17, 7, 11, // C
    5, 10, 10, 10, 11, 11, 7,  11, 5, 10, 10, 10, 11, 17, 7, 11, // D
    5, 10, 10, 18, 11, 11, 7,  11, 5, 5,  10, 4,  11, 17, 7, 11, // E
    5, 10, 10, 4,  11, 11, 7,  11, 5, 5,  10, 4,  11, 17, 7, 11  // F
};

// Returns the 16-bit number stored in the BC register pair
static uint16_t get_bc(CPU * cpu) {
    return ((uint16_t)cpu->b << 8) | cpu->c;
}

static uint16_t get_de(CPU * cpu) {
    return ((uint16_t)cpu->d << 8) | cpu->e;
}

uint16_t get_hl(CPU * cpu) {
    return ((uint16_t)cpu->h << 8) | cpu->l;
}

// Sets the 16-bit number stored in the BC register pair
static void set_bc(CPU * cpu, uint16_t val) {
    cpu->b = val >> 8;
    cpu->c = val & 0xFF;
}

// Sets the 16-bit number stored in the DE register pair
static void set_de(CPU * cpu, uint16_t val) {
    cpu->d = val >> 8;
    cpu->e = val & 0xFF;
}

// Sets the 16-bit number stored in the HL register pair
static void set_hl(CPU * cpu, uint16_t val) {
    cpu->h = val >> 8;
    cpu->l = val & 0xFF;
}

// Returns a byte from memory at the given address
static uint8_t rd_byte(CPU * cpu, uint16_t addr) {
    return cpu->read_byte(cpu->user_memory, addr);
}

// Returns a word from memory
static uint16_t rd_word(CPU *cpu, uint16_t addr) {
    return cpu->read_byte(cpu->user_memory, addr + 1) << 8 | cpu->read_byte(cpu->user_memory, addr);
}

// Writes a byte to memory at the given address
static void wr_byte(CPU * cpu, uint16_t addr, uint8_t val) {
    // if (addr < 0x2000) {
    //     printf("Writing in ROM");
    //     exit(1);
    // }
    // if (addr >= 0x4000) {
    //     printf("Writing out of SI RAM");
    //     exit(1);
    // }
    cpu->write_byte(cpu->user_memory, addr, val);
}

// Writes a word to memory at the given address
static void wr_word(CPU * cpu, uint16_t addr, uint8_t val) {
    // if (addr < 0x2000) {
    //     printf("Writing in ROM");
    //     exit(1);
    // }
    // if (addr >= 0x4000) {
    //     printf("Writing out of SI RAM");
    //     exit(1);
    // }
    cpu->write_byte(cpu->user_memory, addr, val & 0xFF);
    cpu->write_byte(cpu->user_memory, addr + 1, val >> 8);
}

// Returns next byte in program memory, advances program counter
static int next_byte(CPU *cpu) {
    uint8_t val = rd_byte(cpu, cpu->pc);
    cpu->pc += 1;
    return val;
}

// Returns next word in program memory, advances program counter
static int next_word(CPU *cpu) {
    u_int16_t val = rd_word(cpu, cpu->pc);
    cpu->pc += 2;
    return val;
}

static void push_stack(CPU * cpu, uint16_t val) {
    cpu->sp -= 2;
    wr_word(cpu, cpu->sp, val);
}

static int pop_stack(CPU * cpu) {
    uint16_t val = rd_word(cpu, cpu->sp);
    cpu->sp += 2;
    return val;
}

void push_psw(CPU * cpu) {
    // note: bit 3 and 5 are always 0
    uint8_t psw = 0;
    psw |= cpu->sf << 7;
    psw |= cpu->zf << 6;
    psw |= cpu->pf << 2;
    psw |= 1 << 1; // bit 1 is always 1
    psw |= cpu->cf << 0;
    push_stack(cpu, cpu->a << 8 | psw);
}

void pop_psw(CPU * cpu) {
    uint16_t apsw = pop_stack(cpu);
    cpu->a = apsw >> 8;
    uint8_t psw = apsw & 0xFF;
    cpu->psw = psw;

    cpu->sf = (psw >> 7) & 1;
    cpu->zf = (psw >> 6) & 1;
    cpu->hf = (psw >> 4) & 1;
    cpu->pf = (psw >> 2) & 1;
    cpu->cf = (psw >> 0) & 1;
}

// If result is 0 set zero flag(zf) true, else false
void set_zf(CPU *cpu, uint8_t result) {
    if ((result & 0xff) == 0)
        cpu->zf = 1;
    else
        cpu->zf = 0;
}

// If result bit 7 is set, set sign flag(sf) true, else false
void set_sf(CPU *cpu, uint8_t result) {
    if (result & 0x80)
        cpu->sf = 1;
    else
        cpu->sf = 0;
}

// If result carries over, set carry flag(cf) true, else false
void set_cf(CPU *cpu, uint16_t result) {
    if (result > 0xff)
        cpu->cf = 1;
    else
        cpu->cf = 0;
}

// If result has even parity, set parity flag(pf) true, else false
void set_pf(CPU *cpu, uint8_t result) {
    int i;
    int p = 0;
    for (i = 0; i < 8; i++) {
        if (result & 0x1) {
            p += 1;
        }
        result = result >> 1;
    }
    if (p % 2 == 0) {
        cpu->pf = 1;
    } else if (result == 0)
    {
        cpu->pf = 1;
    }
        else cpu->pf = 0;
}

// Adds reg1 and reg2
// Sets cf, sf, zf, pf and updates accumulator (A)
void add(CPU *cpu, uint16_t reg1, uint16_t reg2) {
    uint16_t result = reg1 + reg2;
    set_cf(cpu, result);
    set_sf(cpu, result);
    set_zf(cpu, result);
    set_pf(cpu, result & 0xff);
    cpu->a = (result & 0xff);
}

// Adds reg1, reg2, and current value of cf
// Sets cf, sf, zf, pf and updates accumulator (A)
void adc(CPU *cpu, uint16_t reg1, uint16_t reg2) {
    uint16_t result = reg1 + reg2 + cpu->cf;
    set_cf(cpu, result);
    set_sf(cpu, result);
    set_zf(cpu, result);
    set_pf(cpu, result & 0xff);
    cpu->a = (result & 0xff);
}

// Adds register pair to HL
// Sets or clears cf based on overflow
void dad(CPU *cpu, uint16_t reg) {
    // If overflow set cf, else clear
    cpu->cf = (((uint32_t) get_hl(cpu) + (uint32_t) reg) >> 16) & 1;
    set_hl(cpu, get_hl(cpu) + reg);
}

// Subtracts reg2 from reg1
// Sets cf, sf, zf, pf and updates accumulator (A)
void sub(CPU *cpu, uint16_t reg1, uint16_t reg2) {
    uint16_t result = reg1 - reg2;
    set_cf(cpu, result);
    set_sf(cpu, result);
    set_zf(cpu, result);
    set_pf(cpu, result & 0xff);
    cpu->a = (result & 0xff);
}

// Subtracts reg2 and carry flag from reg1
// Sets cf, sf, zf, pf and updates accumulator (A)
void sbb(CPU *cpu, uint16_t reg1, uint16_t reg2) {
    uint16_t result = reg1 - reg2 - cpu->cf;
    set_cf(cpu, result);
    set_sf(cpu, result);
    set_zf(cpu, result);
    set_pf(cpu, result & 0xff);
    cpu->a = (result & 0xff);
}

// Logical AND (ANA reg)
void ana(CPU * cpu, uint8_t reg) {
    cpu->a &= reg;                 // Perform bitwise AND
    set_zf(cpu, cpu->a);         // Update Zero Flag
    set_sf(cpu, cpu->a);         // Update Sign Flag
    set_pf(cpu, cpu->a);         // Update Parity Flag
    cpu->cf = 0;                   // Carry flag is cleared
    // cpu->ac = 1;                   // Auxiliary Carry is set
}

// Logical Exclusive OR (XRA reg)
void xra(CPU * cpu, uint8_t reg) {
    cpu->a ^= reg;                 // Perform XOR
    set_zf(cpu, cpu->a);         // Update Zero Flag
    set_sf(cpu, cpu->a);         // Update Sign Flag
    set_pf(cpu, cpu->a);         // Update Parity Flag
    cpu->cf = 0;                   // Carry flag is cleared
    // cpu->ac = 0;                   // Auxiliary Carry is cleared
}

// Logical OR (ORA reg)
void ora(CPU * cpu, uint8_t reg) {
    cpu->a |= reg;                 // Perform bitwise OR
    set_zf(cpu, cpu->a);         // Update Zero Flag
    set_sf(cpu, cpu->a);         // Update Sign Flag
    set_pf(cpu, cpu->a);         // Update Parity Flag
    cpu->cf = 0;                   // Carry flag is cleared
    // cpu->ac = 0;                   // Auxiliary Carry is cleared
}

// Compare (CMP reg)
void cmp(CPU * cpu, uint8_t reg) {
    uint16_t result = (uint16_t)cpu->a - reg;  // Perform subtraction
    set_zf(cpu, result & 0xFF);                // Update Zero Flag
    set_sf(cpu, result & 0xFF);                // Update Sign Flag
    set_pf(cpu, result & 0xFF);                // Update Parity Flag
    cpu->cf = (cpu->a < reg) ? 1 : 0;        // Set Carry Flag
}

// Increments a register
// Sets zf, sf, pf
static uint8_t inr(CPU *cpu, uint8_t val) {
    uint8_t result = val + 1;
    set_zf(cpu, result);
    set_sf(cpu, result);
    set_pf(cpu, result);
    return result;
}

// Decrements a register
// Sets zf, sf, pf
static uint8_t dcr(CPU *cpu, uint8_t val) {
    uint8_t result = val - 1;
    set_zf(cpu, result);
    set_sf(cpu, result);
    set_pf(cpu, result);
    return result;
}

// Function copied from https://github.com/superzazu/8080/blob/master/i8080.c#L342
static void daa(CPU *cpu) {
    bool cy = cpu->cf;
    uint8_t correction = 0;

    uint8_t lsb = cpu->a & 0x0F;
    uint8_t msb = cpu->a >> 4;

    if (cpu->hf || lsb > 9) {
    correction += 0x06;
    }

    if (cpu->cf || msb > 9 || (msb >= 9 && lsb > 9)) {
    correction += 0x60;
    cy = 1;
    }

    add(cpu, cpu->a, correction);
    cpu->cf = cy;
}

// Rotate A left
// Sets carry flag to original A bit 7
static void rlc(CPU * cpu) {
    // Set carry flag to bit 7
    // Set 7 left bits of A, then OR with cf to set least significant bit
    uint8_t x = cpu->a;
    cpu->a = ((x & 0x80) >> 7) | (x << 1);
    cpu->cf = (0x80 == (x & 0x80));
}

// Rotate A right
// Sets carry flag to original A bit 0
static void rrc(CPU * cpu) {
    // Set carry flag to bit 0
    // Set 7 right bits of A, then OR with cf to set most significant bit
    uint8_t x = cpu->a;
    cpu->a = ((x & 1) << 7) | (x >> 1);
    cpu->cf = (1 == (x & 1));
}

// Rotate A left through carry flag
// Sets carry flag to original A bit 7
static void ral(CPU * cpu) {
    // Capture current cf
    bool cf = cpu->cf;
    // Set carry flag to bit 7
    // Set 7 left bits of A, then OR with original cf to set least significant bit
    cpu->cf = cpu->a >> 7;
    cpu->a = (cpu->a >> 1) | (cf << 7);
}

// Rotate A right through carry flag
// Sets carry flag to original A bit 0
static void rar(CPU * cpu) {
    // Capture current cf
    // Set carry flag to bit 0
    // Set 7 right bits of A, then OR with original cf to set most significant bit
    uint8_t x = cpu->a;
    cpu->a = (cpu->cf << 7) | (x >> 1);
    cpu->cf = (1 == (x & 1));
}

// Exchange DE register with HL
static void xchg(CPU *cpu) {
    uint16_t de = get_de(cpu);
    set_de(cpu, get_hl(cpu));
    set_hl(cpu, de);
}

// Pops word from stack to pc
static void ret(CPU *cpu) {
    cpu->pc = pop_stack(cpu);
}

// Pops word from stack to pc if condition is true
// Increments cycle 6 additional times
static void conditional_ret(CPU *cpu, bool flag) {
    if (flag) {
        ret(cpu);
        cpu->cycle += 6;
    }
}

// Sets pc to addr
static void jmp(CPU *cpu, uint16_t addr) {
    cpu->pc = addr;
}

// Sets pc to addr if flag condition is met
static void conditional_jmp(CPU *cpu, bool flag) {
    uint16_t addr = next_word(cpu);
    if (flag) {
        cpu->pc = addr;
    }
}

// Push current pc to stack, then jump to addr
static void call(CPU *cpu, uint16_t addr) {
    push_stack(cpu, cpu->pc);
    jmp(cpu, addr);
}

// Calls immediate word in memory if flag condition is met
// Costs 6 additional cycles
static void conditional_call(CPU *cpu, bool flag) {
    uint16_t addr = next_word(cpu);
    if (flag) {
        call(cpu, addr);
        cpu->cycle += 6;
    }
}

static void execute_instruction(CPU *cpu, uint8_t opcode) {
    cpu->cycle += OPCODES_CYCLES[opcode];

    switch (opcode) {
    // NOP
    case 0x00:
    case 0x08:
    case 0x10:
    case 0x18:
    case 0x20:
    case 0x28:
    case 0x30:
    case 0x38:
    case 0xcb:
    case 0xd9:
    case 0xdd:
    case 0xed:
    case 0xfd: cpu->pc += 1; break;

    case 0x76: cpu->halted = 1; break;  // HLT

    // Load direct memory into 16 bit register pair
    case 0x01: set_bc(cpu, next_word(cpu)); break;  // LXI B,word
    case 0x11: set_de(cpu, next_word(cpu)); break;  // LXI D,word
    case 0x21: set_hl(cpu, next_word(cpu)); break;  // LXI H,word
    case 0x31: cpu->sp = next_word(cpu); break;     // LXI SP,word

    // Store accumulator(A) contents to memory location stored in register pair
    case 0x02: wr_byte(cpu, get_bc(cpu), cpu->a); break;    // STAX B
    case 0x12: wr_byte(cpu, get_de(cpu), cpu->a); break;    // STAX D
    case 0x32: wr_byte(cpu, next_word(cpu), cpu->a); break; // STA adr 

    // Increment register
    case 0x04: cpu->b = inr(cpu, cpu->b); break;  // INR B
    case 0x0c: cpu->c = inr(cpu, cpu->c); break;  // INR C
    case 0x14: cpu->d = inr(cpu, cpu->d); break;  // INR D
    case 0x1c: cpu->e = inr(cpu, cpu->e); break;  // INR E
    case 0x24: cpu->h = inr(cpu, cpu->h); break;  // INR H
    case 0x2c: cpu->l = inr(cpu, cpu->l); break;  // INR L
    case 0x3c: cpu->a = inr(cpu, cpu->a); break;  // INR A
    case 0x34: wr_byte(cpu, get_hl(cpu), inr(cpu, rd_byte(cpu, get_hl(cpu)))); cpu->pc += 1; break; // INR M(HL)

    // Increment register pair, does not set flags
    case 0x03: set_bc(cpu, get_bc(cpu) + 1); break;  // INX B
    case 0x13: set_de(cpu, get_de(cpu) + 1); break;  // INX D
    case 0x23: set_hl(cpu, get_hl(cpu) + 1); break;  // INX H
    case 0x33: cpu->sp += 1; break;                  // INX SP

    // Decrement register
    case 0x05: cpu->b = dcr(cpu, cpu->b); break;  // DCR B
    case 0x0d: cpu->c = dcr(cpu, cpu->c); break;  // DCR C
    case 0x15: cpu->d = dcr(cpu, cpu->d); break;  // DCR D
    case 0x1d: cpu->e = dcr(cpu, cpu->e); break;  // DCR E
    case 0x25: cpu->h = dcr(cpu, cpu->h); break;  // DCR H
    case 0x2d: cpu->l = dcr(cpu, cpu->l); break;  // DCR L
    case 0x3d: cpu->a = dcr(cpu, cpu->a); break;  // DCR A
    case 0x35: wr_byte(cpu, get_hl(cpu), dcr(cpu, rd_byte(cpu, get_hl(cpu)))); break; // DCR M(HL)

    // Decrement register pair
    case 0x0b: set_bc(cpu, get_bc(cpu) - 1); break;  // DCX B
    case 0x1b: set_de(cpu, get_de(cpu) - 1); break;  // DCX D
    case 0x2b: set_hl(cpu, get_hl(cpu) - 1); break;  // DCX H
    case 0x3b: cpu->sp -= 1; break;                  // DCX SP

    // Moves next byte in memory into register
    case 0x06: cpu->b = next_byte(cpu); break;  // MVI B,byte
    case 0x0E: cpu->c = next_byte(cpu); break;  // MVI C,byte
    case 0x16: cpu->d = next_byte(cpu); break;  // MVI D,byte
    case 0x1E: cpu->e = next_byte(cpu); break;  // MVI E,byte
    case 0x26: cpu->h = next_byte(cpu); break;  // MVI H,byte
    case 0x2E: cpu->l = next_byte(cpu); break;  // MVI L,byte
    case 0x3E: cpu->a = next_byte(cpu); break;  // MVI A,byte
    case 0x36: wr_byte(cpu, get_hl(cpu), next_byte(cpu)); break;  // MVI M(HL)

    // Rotate accumulator
    case 0x07: rlc(cpu); break;  // RLC
    case 0x0F: rrc(cpu); break;  // RRC
    case 0x17: ral(cpu); break;  // RAL
    case 0x1F: rar(cpu); break;  // RAR

    // Add register pair to HL
    case 0x09: dad(cpu, get_bc(cpu)); break;    // DAD BC
    case 0x19: dad(cpu, get_de(cpu)); break;    // DAD DE
    case 0x29: dad(cpu, get_hl(cpu)); break;    // DAD HL
    case 0x39: dad(cpu, cpu->sp); break;        // DAD SP

    // Load byte from memory to accumulator
    case 0x0A: cpu->a = rd_byte(cpu, get_bc(cpu)); break;    // LDAX B
    case 0x1A: cpu->a = rd_byte(cpu, get_de(cpu)); break;    // LDAX D
    case 0x3A: cpu->a = rd_byte(cpu, next_word(cpu)); break; // LDA (addr)

    // Write contents of HL to memory at immediate address
    case 0x22: wr_word(cpu, next_word(cpu), get_hl(cpu)); break; // SHLD (addr)
    // Set HL to value in memory at immediate address
    case 0x2a: set_hl(cpu, rd_word(cpu, next_word(cpu))); break; // LHLD (addr)
    case 0xF9: cpu->sp = get_hl(cpu); break; // SPHL

    // Special operations
    case 0x27: daa(cpu); break; // DAA
    case 0x2F: cpu->a = ~cpu->a; break; // CMA
    case 0x37: cpu->cf = 1; break; // STC
    case 0x3F: cpu->cf = !cpu->cf; break; // CMC

    // Move contents from register or memory to B
    case 0x40: cpu->b = cpu->b; break;  // MOV B,B
    case 0x41: cpu->b = cpu->c; break;  // MOV B,C
    case 0x42: cpu->b = cpu->d; break;  // MOV B,D
    case 0x43: cpu->b = cpu->e; break;  // MOV B,E
    case 0x44: cpu->b = cpu->h; break;  // MOV B,H
    case 0x45: cpu->b = cpu->l; break;  // MOV B,L
    case 0x47: cpu->b = cpu->a; break;  // MOV B,A
    case 0x46: cpu->b = rd_byte(cpu, get_hl(cpu)); break;  // MOV B,M(HL)

    // Move contents from register or memory to C
    case 0x48: cpu->c = cpu->b; break;  // MOV C,B
    case 0x49: cpu->c = cpu->c; break;  // MOV C,C
    case 0x4a: cpu->c = cpu->d; break;  // MOV C,D
    case 0x4b: cpu->c = cpu->e; break;  // MOV C,E
    case 0x4c: cpu->c = cpu->h; break;  // MOV C,H
    case 0x4d: cpu->c = cpu->l; break;  // MOV C,L
    case 0x4f: cpu->c = cpu->a; break;  // MOV C,A
    case 0x4E: cpu->c = rd_byte(cpu, get_hl(cpu)); break;  // MOV C,M(HL)

    // Move contents from register or memory to D
    case 0x50: cpu->d = cpu->b; break;  // MOV D,B
    case 0x51: cpu->d = cpu->c; break;  // MOV D,C
    case 0x52: cpu->d = cpu->d; break;  // MOV D,D
    case 0x53: cpu->d = cpu->e; break;  // MOV D,E
    case 0x54: cpu->d = cpu->h; break;  // MOV D,H
    case 0x55: cpu->d = cpu->l; break;  // MOV D,L
    case 0x57: cpu->d = cpu->a; break;  // MOV D,A
    case 0x56: cpu->d = rd_byte(cpu, get_hl(cpu)); break;  // MOV D,M(HL)

    // Move contents from register or memory to E
    case 0x58: cpu->e = cpu->b; break;  // MOV E,B
    case 0x59: cpu->e = cpu->c; break;  // MOV E,C
    case 0x5a: cpu->e = cpu->d; break;  // MOV E,D
    case 0x5b: cpu->e = cpu->e; break;  // MOV E,E
    case 0x5c: cpu->e = cpu->h; break;  // MOV E,H
    case 0x5d: cpu->e = cpu->l; break;  // MOV E,L
    case 0x5f: cpu->e = cpu->a; break;  // MOV E,A
    case 0x5e: cpu->e = rd_byte(cpu, get_hl(cpu)); break;  // MOV E,M(HL)

    // Move contents from register to H
    case 0x60: cpu->h = cpu->b; break;  // MOV H,B
    case 0x61: cpu->h = cpu->c; break;  // MOV H,C
    case 0x62: cpu->h = cpu->d; break;  // MOV H,D
    case 0x63: cpu->h = cpu->e; break;  // MOV H,E
    case 0x64: cpu->h = cpu->h; break;  // MOV H,H
    case 0x65: cpu->h = cpu->l; break;  // MOV H,L
    case 0x67: cpu->h = cpu->a; break;  // MOV H,A
    case 0x66: cpu->h = rd_byte(cpu, get_hl(cpu)); break;  // MOV H,M(HL)

    // Move contents from register or memory to L
    case 0x68: cpu->l = cpu->b; break;  // MOV L,B
    case 0x69: cpu->l = cpu->c; break;  // MOV L,C
    case 0x6a: cpu->l = cpu->d; break;  // MOV L,D
    case 0x6b: cpu->l = cpu->e; break;  // MOV L,E
    case 0x6c: cpu->l = cpu->h; break;  // MOV L,H
    case 0x6d: cpu->l = cpu->l; break;  // MOV L,L
    case 0x6f: cpu->l = cpu->a; break;  // MOV L,A
    case 0x6e: cpu->l = rd_byte(cpu, get_hl(cpu)); break;  // MOV L,M(HL)

    // Move contents from register to memory
    case 0x70: wr_byte(cpu, get_hl(cpu), cpu->b); break;  // MOV M,B
    case 0x71: wr_byte(cpu, get_hl(cpu), cpu->c); break;  // MOV M,C
    case 0x72: wr_byte(cpu, get_hl(cpu), cpu->d); break;  // MOV M,D
    case 0x73: wr_byte(cpu, get_hl(cpu), cpu->e); break;  // MOV M,E
    case 0x74: wr_byte(cpu, get_hl(cpu), cpu->h); break;  // MOV M,H
    case 0x75: wr_byte(cpu, get_hl(cpu), cpu->l); break;  // MOV M,L
    case 0x77: wr_byte(cpu, get_hl(cpu), cpu->a); break;  // MOV M,A

    // Move contents from register or memory to A
    case 0x78: cpu->a = cpu->b; break;  // MOV A,B
    case 0x79: cpu->a = cpu->c; break;  // MOV A,C
    case 0x7A: cpu->a = cpu->d; break;  // MOV A,D
    case 0x7B: cpu->a = cpu->e; break;  // MOV A,E
    case 0x7C: cpu->a = cpu->h; break;  // MOV A,H
    case 0x7D: cpu->a = cpu->l; break;  // MOV A,L
    case 0x7F: cpu->a = cpu->a; break;  // MOV A,A
    case 0x7E: cpu->a = rd_byte(cpu, get_hl(cpu)); break;  // MOV A,M(HL)

    // Add register or memory to A and store in A
    case 0x80: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->b); break;  // ADD B
    case 0x81: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->c); break;  // ADD C
    case 0x82: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->d); break;  // ADD D
    case 0x83: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->e); break;  // ADD E
    case 0x84: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->h); break;  // ADD H
    case 0x85: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->l); break;  // ADD L
    case 0x87: add(cpu, (uint16_t)cpu->a, (uint16_t)cpu->a); break;  // ADD A
    case 0xC6: add(cpu, (uint16_t)cpu->a, next_byte(cpu)); break;    // ADI
    case 0x86: add(cpu, (uint16_t)cpu->a, (uint16_t)rd_byte(cpu, get_hl(cpu))); break;  // ADD M(HL)

    // Add register or memory and carry flag to A and store in A
    case 0x88: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->b); break;  // ADC B
    case 0x89: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->c); break;  // ADC C
    case 0x8a: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->d); break;  // ADC D
    case 0x8b: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->e); break;  // ADC E
    case 0x8c: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->h); break;  // ADC H
    case 0x8d: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->l); break;  // ADC L
    case 0x8f: adc(cpu, (uint16_t)cpu->a, (uint16_t)cpu->a); break;  // ADC A
    case 0xCE: adc(cpu, (uint16_t)cpu->a, next_byte(cpu)); break;    // ACI
    case 0x8e: adc(cpu, (uint16_t)cpu->a, (uint16_t)rd_byte(cpu, get_hl(cpu))); break;  // ADC M(HL)

    // Subtract register or memory from A and store in A
    case 0x90: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->b); break;  // SUB B
    case 0x91: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->c); break;  // SUB C
    case 0x92: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->d); break;  // SUB D
    case 0x93: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->e); break;  // SUB E
    case 0x94: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->h); break;  // SUB H
    case 0x95: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->l); break;  // SUB L
    case 0x97: sub(cpu, (uint16_t)cpu->a, (uint16_t)cpu->a); break;  // SUB A
    case 0xD6: sub(cpu, (uint16_t)cpu->a, next_byte(cpu)); break;    // SUI
    case 0x96: sub(cpu, (uint16_t)cpu->a, (uint16_t)rd_byte(cpu, get_hl(cpu))); break; // SUB M(HL)

    // Subtract register or memory + carry flag from A and store in A
    case 0x98: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->b); break;  // SBB B
    case 0x99: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->c); break;  // SBB C
    case 0x9a: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->d); break;  // SBB D
    case 0x9b: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->e); break;  // SBB E
    case 0x9c: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->h); break;  // SBB H
    case 0x9d: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->l); break;  // SBB L
    case 0x9f: sbb(cpu, (uint16_t)cpu->a, (uint16_t)cpu->a); break;  // SBB A
    case 0xDE: sbb(cpu, (uint16_t)cpu->a, next_byte(cpu)); break;    // SBI
    case 0x9e: sbb(cpu, (uint16_t)cpu->a, (uint16_t)rd_byte(cpu, get_hl(cpu))); break;  // SBB M(HL)

    case 0xA0: ana(cpu, cpu->b); break;         // ANA B
    case 0xA1: ana(cpu, cpu->c); break;         // ANA C
    case 0xA2: ana(cpu, cpu->d); break;         // ANA D
    case 0xA3: ana(cpu, cpu->e); break;         // ANA E
    case 0xA4: ana(cpu, cpu->h); break;         // ANA H
    case 0xA5: ana(cpu, cpu->l); break;         // ANA L
    case 0xA7: ana(cpu, cpu->a); break;         // ANA A
    case 0xE6: ana(cpu, next_byte(cpu)); break; // ANI
    case 0xA6: ana(cpu, rd_byte(cpu, get_hl(cpu))); break;  // ANA M

    case 0xA8: xra(cpu, cpu->b); break;         // XRA B
    case 0xA9: xra(cpu, cpu->c); break;         // XRA C
    case 0xAA: xra(cpu, cpu->d); break;         // XRA D
    case 0xAB: xra(cpu, cpu->e); break;         // XRA E
    case 0xAC: xra(cpu, cpu->h); break;         // XRA H
    case 0xAD: xra(cpu, cpu->l); break;         // XRA L
    case 0xAF: xra(cpu, cpu->a); break;         // XRA A
    case 0xEE: xra(cpu, next_byte(cpu)); break; // XRI
    case 0xAE: xra(cpu, rd_byte(cpu, get_hl(cpu))); break;  // XRA M

    case 0xB0: ora(cpu, cpu->b); break;         // ORA B
    case 0xB1: ora(cpu, cpu->c); break;         // ORA C
    case 0xB2: ora(cpu, cpu->d); break;         // ORA D
    case 0xB3: ora(cpu, cpu->e); break;         // ORA E
    case 0xB4: ora(cpu, cpu->h); break;         // ORA H
    case 0xB5: ora(cpu, cpu->l); break;         // ORA L
    case 0xB7: ora(cpu, cpu->a); break;         // ORA A
    case 0xF6: ora(cpu, next_byte(cpu)); break; // ORI
    case 0xB6: ora(cpu, rd_byte(cpu, get_hl(cpu))); break;  // ORA M

    case 0xB8: cmp(cpu, cpu->b); break;         // CMP B
    case 0xB9: cmp(cpu, cpu->c); break;         // CMP C
    case 0xBA: cmp(cpu, cpu->d); break;         // CMP D
    case 0xBB: cmp(cpu, cpu->e); break;         // CMP E
    case 0xBC: cmp(cpu, cpu->h); break;         // CMP H
    case 0xBD: cmp(cpu, cpu->l); break;         // CMP L
    case 0xBF: cmp(cpu, cpu->a); break;         // CMP A
    case 0xFE: cmp(cpu, next_byte(cpu)); break; // CPI
    case 0xBE: cmp(cpu, rd_byte(cpu, get_hl(cpu))); break;  // CMP M
    
    case 0xC5: push_stack(cpu, get_bc(cpu)); break;  // PUSH B
    case 0xD5: push_stack(cpu, get_de(cpu)); break;  // PUSH D
    case 0xE5: push_stack(cpu, get_hl(cpu)); break;  // PUSH H
    case 0xF5: push_psw(cpu); break;                 // PUSH PSW

    case 0xC1: set_bc(cpu, pop_stack(cpu)); break;  // POP B
    case 0xD1: set_de(cpu, pop_stack(cpu)); break;  // POP D
    case 0xE1: set_hl(cpu, pop_stack(cpu)); break;  // POP H
    case 0xF1: pop_psw(cpu); break;                 // POP PSW

    case 0xEB: xchg(cpu); break;              // XCHG
    case 0xE9: cpu->pc = get_hl(cpu); break;  // PCHL

    case 0xC9: ret(cpu); break;  // RET
    case 0xC0: conditional_ret(cpu, cpu->zf == 0); break;  // RNZ
    case 0xC8: conditional_ret(cpu, cpu->zf == 1); break;  // RZ
    case 0xD0: conditional_ret(cpu, cpu->cf == 0); break;  // RNC
    case 0xD8: conditional_ret(cpu, cpu->cf == 1); break;  // RC
    case 0xE0: conditional_ret(cpu, cpu->pf == 0); break;  // RPO
    case 0xE8: conditional_ret(cpu, cpu->pf == 1); break;  // RPE
    case 0xF0: conditional_ret(cpu, cpu->sf == 0); break;  // RP
    case 0xF8: conditional_ret(cpu, cpu->sf == 1); break;  // RM
    
    case 0xCD: call(cpu, next_word(cpu)); break;        // CALL addr
    case 0xCC: conditional_call(cpu, cpu->zf); break;   // CZ adr
    case 0xDC: conditional_call(cpu, cpu->cf); break;   // CC adr
    case 0xEC: conditional_call(cpu, cpu->pf); break;   // CPE adr
    case 0xFC: conditional_call(cpu, cpu->sf); break;   // CM adr
    case 0xC4: conditional_call(cpu, !cpu->zf); break;  // CNZ addr
    case 0xD4: conditional_call(cpu, !cpu->cf); break;  // CNC adr
    case 0xE4: conditional_call(cpu, !cpu->pf); break;  // CPO adr
    case 0xF4: conditional_call(cpu, !cpu->sf); break;  // CP adr

    case 0xC7: call(cpu, 0x00); break;  // RST 0
    case 0xCF: call(cpu, 0x08); break;  // RST 1
    case 0xD7: call(cpu, 0x10); break;  // RST 2
    case 0xDF: call(cpu, 0x18); break;  // RST 3
    case 0xE7: call(cpu, 0x20); break;  // RST 4
    case 0xEF: call(cpu, 0x28); break;  // RST 5
    case 0xF7: call(cpu, 0x30); break;  // RST 6
    case 0xFF: call(cpu, 0x38); break;  // RST 7

    case 0xC3: jmp(cpu, next_word(cpu)); break;        // JMP addr
    case 0xCA: conditional_jmp(cpu, cpu->zf); break;   // JZ addr
    case 0xDA: conditional_jmp(cpu, cpu->cf); break;   // JC addr
    case 0xD2: conditional_jmp(cpu, !cpu->cf); break;  // JNC addr
    case 0xC2: conditional_jmp(cpu, !cpu->zf); break;  // JNZ addr
    case 0xFA: conditional_jmp(cpu, !cpu->sf); break;  // JM addr

    case 0xDB: cpu->a = cpu->port_in(cpu->user_memory, next_byte(cpu)); break; // IN
    case 0xD3: cpu->port_out(cpu->user_memory, next_byte(cpu), cpu->a); break; // OUT

    case 0xF3: cpu->ie = 0; break;  // DI
    case 0xFB: cpu->ie = 1; break;  // EI

    default:
        printf("Unknown opcode: 0x%02X\n", opcode);
        cpu->halted = 1;
        exit(1);

    }
}

// Initialize 8080 CPU
void cpu_init(CPU *cpu) {
    cpu->a = 0;
    cpu->b = 0;
    cpu->c = 0;
    cpu->d = 0;
    cpu->e = 0;
    cpu->h = 0;
    cpu->l = 0;

    cpu->sp = 0;
    cpu->pc = 0;

    cpu->zf = 0;
    cpu->sf = 0;
    cpu->pf = 0;
    cpu->cf = 0;
    cpu->ie = 0;

    cpu->cycle = 0;
    cpu->halted = 0;

    cpu->read_byte = NULL;
    cpu->write_byte = NULL;
    cpu->port_in = NULL;
    cpu->port_out = NULL;
    cpu->user_memory = NULL;
}

// Checks for interrupts and executes one instruction
void cpu_process(CPU *cpu) {
    if (cpu->int_pending && cpu->ie && cpu->int_delay == 0) {
        cpu->int_pending = 0;
        cpu->ie = 0;
        cpu->halted = 0;
        execute_instruction(cpu, cpu->int_vector);
    } else if (!cpu->halted) {
        execute_instruction(cpu, next_byte(cpu));
    }
}

// Handle pending interrupt
void interrupt(CPU *cpu, uint8_t opcode) {
  cpu->int_pending = 1;
  cpu->int_vector = opcode;
}

// This method copied from https://github.com/kpmiller/emulator101/blob/master/CocoaPart3-Attract/8080emu.c#L356
// void ReadFileIntoMemoryAt(CPU * cpu, char* filename, uint32_t offset)
// {
//     FILE *f= fopen(filename, "rb");
//     if (f==NULL)
//     {
//       printf("error: Couldn't open %s\n", filename);
//       exit(1);
//     }
//     fseek(f, 0L, SEEK_END);
//     int fsize = ftell(f);
//     fseek(f, 0L, SEEK_SET);

//     uint8_t *buffer = &cpu->memory[offset];
//     fread(buffer, fsize, 1, f);
//     fclose(f);
// }

// int main(int argc, char**argv) {
//     // Initialize registers, flags and memory
//     struct cpu cpu;
//     emulator_init(&cpu);

//     ReadFileIntoMemoryAt(&cpu, "ROM/invaders.h", 0);
//     ReadFileIntoMemoryAt(&cpu, "ROM/invaders.g", 0x800);
//     ReadFileIntoMemoryAt(&cpu, "ROM/invaders.f", 0x1000);
//     ReadFileIntoMemoryAt(&cpu, "ROM/invaders.e", 0x1800);

//     uint16_t counter = 0;
//     // Instruction loop
//     while (true) {
//         emulate8080_instruction(&cpu, true);
//         /* print out processor CPU */
//         printf("\tC=%d,P=%d,S=%d,Z=%d\n", cpu.cf, cpu.pf,
//            cpu.sf, cpu.zf);
//         printf("\tA $%02x B $%02x C $%02x D $%02x E $%02x H $%02x L $%02x PC %04x SP %04x CYC %lu\n",
//            cpu.a, cpu.b, cpu.c, cpu.d,
//            cpu.e, cpu.h, cpu.l, cpu.pc, cpu.sp, cpu.cycle);
//         counter += 1;
//         if (counter > 40000) {
//             getchar();
//             counter = 0;
//         }
//         // getchar();
//     }
//     return 0;
// }