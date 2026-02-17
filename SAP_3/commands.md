--------------------------
# Appendix 6: SAP-3 Op Codes
--------------------------

Registers

A (0b111): Accumulator
B (0b000), C (0b001) |
D (0b010), E (0b011) | -> General registers
H (0b100), L (0b101) |
F : ALU Flags  Table 12-5 7 downto 0 --> sing flag & Zero flag & "0b000" & Parity & "0" & Carry 
M : Memory data on H-L double register pointer 
PC: Program Counter
PSW: Program status word --> PSW[MSB] <= A, PSW[LSB] <= F
SP: Stack Pointer

--------------------------
## ALU operations
--------------------------

| Instruction         | Op Code              | Flags            | Comments                    |
| ------------------- | -------------------- | ---------------- | --------------------------- |
| ADD (A/B/C/D/E/H/L/M) | 87/80/81/82/83/84/85/86 | All | Add Register to accumulator (A = A + reg) |
| ADC (A/B/C/D/E/H/L/M) | 8F/88/89/8A/8B/8C/8D/8E | All | Add Register to accumulator with carry (A = A + reg + carry) |
| SUB (A/B/C/D/E/H/L/M) | 97/90/91/92/93/94/95/96 | All | Subtract Register to accumulator (A = A - reg) |
| SBB (A/B/C/D/E/H/L/M) | 9F/98/99/9A/9B/9C/9D/9E | All | Subtract Register to accumulator with carry (A = A - reg - carry) |
| ANA reg (A/B/C/D/E/H/L/M) | A7/A0/A1/A2/A3/A4/A5/A6 | All (¿carry?) | A = A AND reg |
| ORA reg (A/B/C/D/E/H/L/M) | B7/B0/B1/B2/B3/B4/B5/B6 | All (¿carry?) | A = A OR reg |
| XRA reg (A/B/C/D/E/H/L/M) | AF/A8/A9/AA/AB/AC/AD/AE | All (¿carry?) | A = A XOR reg |
| CMP reg (A/B/C/D/E/H/L/M) | BF/B8/B9/BA/BB/BC/BD/BE | All           | if A = reg -> Zero = 1 |

0b10_(op 3 bits)_(reg 3 bits)

| reg | value | op  | value |
| A   |  111  | ADD |  000  |
| B   |  000  | ADC |  001  |
| C   |  001  | SUB |  010  |
| D   |  010  | SBB |  011  |
| E   |  011  | ANA |  100  |
| H   |  100  | XRA |  101  |
| L   |  101  | ORA |  110  |
| M   |  110  | CMP |  111  |

| ANI Byte | E6 | All | Bit-wise operation A AND byte register |
| ORI Byte | F6 | All | Bit-wise operation A OR byte register  |
| XRI Byte | EE | All | Bit-wise operation A XOR byte register |
| ADI Byte | C6 | All | A = A + byte |
| ACI Byte | CE | All | A = A + byte + carry |
| SUI Byte | D6 | All | A = A - byte |
| SBI Byte | DE | All | A = A - byte - carry |
| CPI Byte | FE | All | if A = byte -> Zero = 1 |

0b11_(op 3 bits)_(reg 3 bits)

| reg | value | op  | value |
| A   |  111  | ADD |  000  |
| B   |  000  | ADC |  001  |
| C   |  001  | SUB |  010  |
| D   |  010  | SBB |  011  |
| E   |  011  | ANA |  100  |
| H   |  100  | XRA |  101  |
| L   |  101  | ORA |  110  |
| M   |  110  | CMP |  111  |

| INR (A/B/C/D/E/H/L/M) | 3C/04/0C/14/1C/24/2C/34 (0b00_(reg 3 bits)_100) | All except carry        | Increment register (reg = reg + 1) |
| DCR (A/B/C/D/E/H/L/M) | 3D/05/0D/15/1D/25/2D/35 (0b00_(reg 3 bits)_101) | All except carry        | Decrement register (reg = reg - 1) |

| RLC | 07 | Carry | Rotate accumulator left with carry (A << 1, Carry = A[7]) |
| RRC | 0F | Carry | Rotate accumulator rigth with carry (A >> 1, Carry = A[0]) |
| RAL | 17 | Carry | Rotate accumulator left including carry ( Carry & A << 1) |
| RAR | 1F | Carry | Rotate accumulator rigth including carry ( Carry & A >> 1) |
| CMA | 2F | No flag | Bit-wise operation NOT A register |
| STC | 37 | carry   | Set carry flag (carry <- 0b1) |
| CMC | 3F | Carry   | carry <- not (carry) |

0b00_(op 3 bits)_111

| op  | value |
| RLC |  000  |
| RRC |  001  |
| RAL |  010  |
| RAR |  011  |
| CMA |  101  |
| STC |  110  |
| CMC |  111  |

| DAD reg-pair (B, D, H for pairs BC, DE and HL and SP) | 09/19/29/39 (0b00_(reg-pair 2 bits)_1001) | Carry | Double add, update HL adding it with reg-pair (HL = HL + reg-pair) |
| INX reg-pair (B, D, H for pairs BC, DE and HL and SP) | 03/13/23/33 (0b00_(reg-pair 2 bits)_0011) | No flag | Increment reg-pair (reg-pair = reg-pair + 1) |
| DCX reg-pair (B, D, H for pairs BC, DE and HL and SP) | 0B/1B/2B/3B (0b00_(reg-pair 2 bits)_1011) | No flag | Decrement reg-pair (reg-pair = reg-pair - 1) |

When any ALU operation executed corresponding flags are computed and registered:
* neg  = 1 if A < 0 else neg  = 0
* zero = 1 if A = 0 else zero = 0
* Carry for ADD/ADC/ADI/CY is output carry
* Carry for SUB/SBB/SUI/SBI is NOT (output carry)
* Parity = 1 if A has even number of 1's else Parity = 0

--------------------------
## Jump operations
--------------------------

| Instruction           | Op Code | Comments                                                           |
| --------------------- | ------- | ------------------------------------------------------------------ |
| JMP address (2 bytes) | C3      | Unconditional Jump, PC register updated to address                 |
| JM  address (2 bytes) | FA      | Jump if minus, PC register updated if neg flag is asserted         |  
| JP  address (2 bytes) | F2      | Jump if not minus, PC register updated if neg flag deasserted      |
| JZ  address (2 bytes) | CA      | Jump if zero, PC register update if zero flag is asserted          |
| JNZ address (2 bytes) | C2      | Jump if no zero, PC register update if zero flag is deasserted     |
| JC  address (2 bytes) | DA      | Jump if carry, PC register update if carry flag is asserted        |
| JNC address (2 bytes) | D2      | Jump if no carry, PC register update if carry flag is deasserted   |
| JPE address (2 bytes) | EA      | Jump if even parity, PC register update if Parity flag is asserted |
| JPO address (2 bytes) | E2      | Jump if odd parity, PC register update if Parity flag is deasserted|

for JM to JPO --> 0b11zy_x010

  * x : Value of flag to jump
  * zy: flag select
    - 0b00 --> Zero
    - 0b01 --> Carry
    - 0b10 --> Parity
    - 0b11 --> Negative

--------------------------
## Memory and Register operations
--------------------------

| Instruction           | Op Code | Comments                                                           |
| --------------------- | ------- | ------------------------------------------------------------------ |
| MOV reg1,reg2 (A/B/C/D/E/H/L) | 0b01_reg1_reg2 | Copy Data on register to another register (reg1 <- reg2) |
| MOV reg,M (A/B/C/D/E/H/L) | 0b01_reg_110 | Copy data on memory indirect address (HL pointer) to register |
| MOV M,reg (A/B/C/D/E/H/L) | 0b01_110_reg | Copy data on register to memory indirect address (HL pointer) |
| MVI reg,byte  (A/B/C/D/E/H/L) | 0b00_reg_110 | Copy byte to register (reg <- byte) |
| MVI M,byte   | 36 | Copy byte to memory indirect address (HL pointer)(M[HL] <- byte) |
| LXI reg, word (B, D, H for pairs BC, DE, HL and SP) (16 bit word) | (01/11/21/31) | Load extended register inmediatly with received word |
| LDA address (2 bytes) | 3A      | Load on register A, data on RAM address     |
| STA address (2 bytes) | 32      | Store on RAM address, content on register A |

--------------------------
## Stack Operations
--------------------------

| Instruction | Op Code | Comments |
| --------------------- | ------- | ------------- |
| POP reg-pair (B, D, H for pairs BC, DE and HL and PSW) | C1/D1/E1/F1 | Store reg-pair on memory according stack pointer (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| PUSH reg-pair (B, D, H for pairs BC, DE and HL and PSW) | C5/D5/E5/F5 | Get reg-pair from memory according stack pointer (MSB on M[SP+1], LSB on M[SP]), SP = SP + 2 |


| CALL address(2 bytes) | CD | Jump to Subroutine on address, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CNZ address(2 bytes) | C4 | Conditional Jump to Subroutine on address if Zero flag = 0, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CZ address(2 bytes) | CC | Conditional Jump to Subroutine on address if Zero flag = 1, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CNC address(2 bytes) | D4 | Conditional Jump to Subroutine on address if carry flag = 0, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CC address(2 bytes) | DC | Conditional Jump to Subroutine on address if carry flag = 1, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CPO address(2 bytes) | E4 | Conditional Jump to Subroutine on address if Parity flag = 0, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CPE address(2 bytes) | EC | Conditional Jump to Subroutine on address if parity flag = 1, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CP address(2 bytes) | F4 | Conditional Jump to Subroutine on address if negative flag = 0, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |
| CM address(2 bytes) | FC | Conditional Jump to Subroutine on address if negative flag = 1, PC pointer is poping to memory (MSB on M[SP-1], LSB on M[SP-2]), SP = SP - 2 |

for CNZ to CM --> 0b11zy_x100

  * x : Value of flag to jump
  * zy: flag select
    - 0b00 --> Zero
    - 0b01 --> Carry
    - 0b10 --> Parity
    - 0b11 --> Negative

| RET | C9 | Return for subroutine, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RNZ | C0 | Conditional return for subroutine if zero flag = 0, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RZ  | C8 | Conditional return for subroutine if zero flag = 1, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RNC | D0 | Conditional return for subroutine if carry flag = 0, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RC  | D8 | Conditional return for subroutine if carry flag = 1, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RPO | E0 | Conditional return for subroutine if parity flag = 0, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RPE | E8 | Conditional return for subroutine if parity flag = 1, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RP  | F0 | Conditional return for subroutine if negative flag = 0, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |
| RM  | F8 | Conditional return for subroutine if negative flag = 1, PC is recover pushing stack (MSB on M[SP], LSB on M[SP+1]), SP = SP + 2 |

for RNZ to RZ --> 0b11zy_x000

  * x : Value of flag to jump
  * zy: flag select
    - 0b00 --> Zero
    - 0b01 --> Carry
    - 0b10 --> Parity
    - 0b11 --> Negative

--------------------------
## Other operations
--------------------------

| Instruction           | Op Code | Comments                                                                 |
| --------------------- | ------- | ------------------------------------------------------------------------ |
| HLT                   | 76      | Halt data, end data processing                                           |
| IN byte               | DB      | Load byte data on specified input port (byte) to Accumulator register    |
| OUR byte              | D3      | Output byte data on Accumulator register to specified output port (byte) |
| NOP                   | 00      | No operation, used for delay program processing                          |