--------------------------
# Table 11-1: SAP-2 Op Codes
--------------------------

--------------------------
## ALU operations
--------------------------

| Instruction           | Op Code | Comments                                                           |
| --------------------- | ------- | ------------------------------------------------------------------ |
| ADD B                 | 80      | Add Register B to accumulator (reg A)                              |
| ADD C                 | 81      | Add Register C to accumulator (reg A)                              |
| SUB B                 | 90      | Subtract Register B to accumulator (reg A)                         |
| SUB C                 | 91      | Subtract Register C to accumulator (reg A)                         |
| INR A                 | 3C      | Increment 1 unit register A                                        |
| INR B                 | 04      | Increment 1 unit register B                                        |
| INR C                 | 0C      | Increment 1 unit register C                                        |
| DCR A                 | 3D      | Decrement 1 unit register A                                        |
| DCR B                 | 05      | Decrement 1 unit register B                                        |
| DCR C                 | 0D      | Decrement 1 unit register C                                        |
| ANA B                 | A0      | Bit-wise operation A AND B register                                |
| ANA C                 | A1      | Bit-wise operation A AND C register                                |
| ORA B                 | B0      | Bit-wise operation A OR B register                                 |
| ORA C                 | B1      | Bit-wise operation A OR C register                                 |
| XRA B                 | A8      | Bit-wise operation A XOR B register                                |
| XRA C                 | A9      | Bit-wise operation A XOR C register                                |
| ANI Byte              | E6      | Bit-wise operation A AND byte register                             |
| ORI Byte              | F6      | Bit-wise operation A OR byte register                              |
| XRI Byte              | EE      | Bit-wise operation A XOR byte register                             |
| CMA                   | 2F      | Bit-wise operation NOT A register                                  |

When any ALU operation executed (except CMA), zero and neg flags are recalculated so:
* neg  = 1 if A < 0 else neg  = 0
* zero = 1 if A = 0 else zero = 0

--------------------------
## Jump operations
--------------------------

| Instruction           | Op Code | Comments                                                           |
| --------------------- | ------- | ------------------------------------------------------------------ |
| JMP address (2 bytes) | C3      | Unconditional Jump, PC register updated to address                 |
| JM  address (2 bytes) | FA      | Jump if minus, PC register updated if neg flag asserted            |
| JZ  address (2 bytes) | CA      | Jump if zero, PC register update if zero flag asserted             |
| JNZ address (2 bytes) | C2      | Jump if no zero, PC register update if zero flag is deasserted     |
| CALL address(2 bytes) | CD      | Jump to Subroutine on address, (current PC pointer stored on address 0xFFFE and 0xFFFF) |
| RET                   | C9      | Set at the end of a subroutine, PC is recover to original program after CALL operation |

--------------------------
## Memory and Register operations
--------------------------

| Instruction           | Op Code | Comments                                                           |
| --------------------- | ------- | ------------------------------------------------------------------ |
| LDA address (2 bytes) | 3A      | Load on register A, data on RAM address                            |
| STA address (2 bytes) | 32      | Store on RAM address, content on register A                        |
| STA address (2 bytes) | 32      | Store on RAM address, content on register A                        |
| MOV A,B               | 78      | Copy Data on register B to register A                              |
| MOV A,C               | 79      | Copy Data on register C to register A                              |
| MOV B,A               | 47      | Copy Data on register A to register B                              |
| MOV B,C               | 41      | Copy Data on register C to register B                              |
| MOV C,A               | 4F      | Copy Data on register A to register C                              |
| MOV C,B               | 48      | Copy Data on register B to register C                              |
| MVI A,byte            | 3E      | Load inmediatly byte to register A                                 |
| MVI B,byte            | 06      | Load inmediatly byte to register B                                 |
| MVI C,byte            | 0E      | Load inmediatly byte to register C                                 |

--------------------------
## Other operations
--------------------------

| Instruction           | Op Code | Comments                                                                 |
| --------------------- | ------- | ------------------------------------------------------------------------ |
| HLT                   | 76      | Halt data, end data processing                                           |
| IN byte               | DB      | Load byte data on specified input port (byte) to Accumulator register    |
| OUR byte              | D3      | Output byte data on Accumulator register to specified output port (byte) |
| NOP                   | 00      | No operation, used for delay program processing                          |
| RAL                   | 17      | Shift Left Accumulator register 1-bit, MSB bit goes to LSB bit           |
| RAR                   | 1F      | Shift Rigth Accumulator register 1-bit, MSB bit goes to LSB bit          | 