# Python Tool to convert assembly code to memory file
#
# The purpose of this tool is make easier the debug of SAP machine
# using a tool that could translate assembly code more amigable to user
# to memory file data to be loaded on RAM.
#
# The tool get as input a .asm file with a set of instruction.
# Each instruction includes the following fields in order:
#
# [label:]   mnemonic   [operand1,operand2]   [;comment]
#
# * Label is an optional field that mark specific point of program.
#   When label is created, the address of corresponding instruction is registered.
#   Labels can be used as operands of other instructions to make address reference example:
#
#                 label: ADD B
#                        Jump label (jump to ADD B instruction)
#
#   Label goes before the opcode mnemonic and must be end with ":". specific rules for labels:
#              - Must start with alphabetic value (a to z)
#              - One label must specify only one instruction on code
#
# * mnemonic specify the corresponding command code according table of appendix 6 of
#   Digital Computer Electronics that is based on Intel 8080/8085 instructions.
# * Operand is a set of parameters needed depending on mnemonic, example for ADD, the operand
#   indicates the register to be added; for MOV, the operand indicates both registers that are moved.
#   and for RET, no operand is needed. Several Operands must be separated by commands and with no spaces.
# * All set of characters after a ";" is considered comments.
# 
# The final result after run python code is a .txt file that includes two types of data, address and data to be loaded on RAM,
# both separated by ":". Both address and data is represented in hexadecimal.
# 
# Example of translation:
# 
# Input File 
# --------------------------
#  MVI A,17 ; Move inmediate byte to A
#  MVI B,2D ; Move inmediate byte to B
#  ADD B    ; ADD both registers
#  STA 5600 ; Move result to RAM
#  INR A    ; Icrement A
#  MOV C,A  ; Move A to C
#  HLT      ; END program
# --------------------------
# 
# Output File
# --------------------------
# 0000:3E
# 0001:17
# 0002:06
# 0003:2D
# 0004:80
# 0005:32
# 0006:00
# 0007:56
# 0008:3C
# 0009:4F
# 000A:76
# --------------------------
# 
# python script is launched as:
# 
# python asm2mem.py --input <input_file> --output <output_file>
# 
# Input file option is mandatory.
# Output file option is optional, if not set, output file is called as input

import argparse

# Given instruction and operand, set code of corresponding Instruction
def translate_instruction(input_arg):
  # output code
  output_code = "FF"

  mnemonic = input_arg[0]
  operand  = input_arg[1] if len(input_arg) == 2 else ""  
  # Switch case
  match mnemonic:
    # ALU operations
    case "ADD":
        if   operand[0] == "A": output_code = "87"
        elif operand[0] == "B": output_code = "80"
        elif operand[0] == "C": output_code = "81"
        elif operand[0] == "D": output_code = "82"
        elif operand[0] == "E": output_code = "83"
        elif operand[0] == "H": output_code = "84"
        elif operand[0] == "L": output_code = "85"
        elif operand[0] == "M": output_code = "86"
        else: raise TypeError("Operands " + operand + " not supported for " + mnemonic)
    case "ADC":
        if   operand[0] == "A": output_code = "8F"
        elif operand[0] == "B": output_code = "88"
        elif operand[0] == "C": output_code = "89"
        elif operand[0] == "D": output_code = "8A"
        elif operand[0] == "E": output_code = "8B"
        elif operand[0] == "H": output_code = "8C"
        elif operand[0] == "L": output_code = "8D"
        elif operand[0] == "M": output_code = "8E"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "SUB":
        if   operand[0] == "A": output_code = "97"
        elif operand[0] == "B": output_code = "90"
        elif operand[0] == "C": output_code = "91"
        elif operand[0] == "D": output_code = "92"
        elif operand[0] == "E": output_code = "93"
        elif operand[0] == "H": output_code = "94"
        elif operand[0] == "L": output_code = "95"
        elif operand[0] == "M": output_code = "96"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "SBB":
        if   operand[0] == "A": output_code = "9F"
        elif operand[0] == "B": output_code = "98"
        elif operand[0] == "C": output_code = "99"
        elif operand[0] == "D": output_code = "9A"
        elif operand[0] == "E": output_code = "9B"
        elif operand[0] == "H": output_code = "9C"
        elif operand[0] == "L": output_code = "9D"
        elif operand[0] == "M": output_code = "9E"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "ANA":
        if   operand[0] == "A": output_code = "A7"
        elif operand[0] == "B": output_code = "A0"
        elif operand[0] == "C": output_code = "A1"
        elif operand[0] == "D": output_code = "A2"
        elif operand[0] == "E": output_code = "A3"
        elif operand[0] == "H": output_code = "A4"
        elif operand[0] == "L": output_code = "A5"
        elif operand[0] == "M": output_code = "A6"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "ORA":
        if   operand[0] == "A": output_code = "97"
        elif operand[0] == "B": output_code = "90"
        elif operand[0] == "C": output_code = "91"
        elif operand[0] == "D": output_code = "92"
        elif operand[0] == "E": output_code = "93"
        elif operand[0] == "H": output_code = "94"
        elif operand[0] == "L": output_code = "95"
        elif operand[0] == "M": output_code = "96"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "XRA":
        if   operand[0] == "A": output_code = "AF"
        elif operand[0] == "B": output_code = "A8"
        elif operand[0] == "C": output_code = "A9"
        elif operand[0] == "D": output_code = "AA"
        elif operand[0] == "E": output_code = "AB"
        elif operand[0] == "H": output_code = "AC"
        elif operand[0] == "L": output_code = "AD"
        elif operand[0] == "M": output_code = "AE"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "CMP":
        if   operand[0] == "A": output_code = "BF"
        elif operand[0] == "B": output_code = "B8"
        elif operand[0] == "C": output_code = "B9"
        elif operand[0] == "D": output_code = "BA"
        elif operand[0] == "E": output_code = "BB"
        elif operand[0] == "H": output_code = "BC"
        elif operand[0] == "L": output_code = "BD"
        elif operand[0] == "M": output_code = "BE"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "ADI": output_code = "C6"
    case "ACI": output_code = "CE"
    case "SUI": output_code = "D6"
    case "SBI": output_code = "DE"
    case "ANI": output_code = "E6"
    case "ORI": output_code = "F6"
    case "XRI": output_code = "EE"
    case "CPI": output_code = "FE"
    case "INR":
        if   operand[0] == "A": output_code = "3C"
        elif operand[0] == "B": output_code = "04"
        elif operand[0] == "C": output_code = "0C"
        elif operand[0] == "D": output_code = "14"
        elif operand[0] == "E": output_code = "1C"
        elif operand[0] == "H": output_code = "24"
        elif operand[0] == "L": output_code = "2C"
        elif operand[0] == "M": output_code = "34"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "DCR":
        if   operand[0] == "A": output_code = "3D"
        elif operand[0] == "B": output_code = "05"
        elif operand[0] == "C": output_code = "0D"
        elif operand[0] == "D": output_code = "15"
        elif operand[0] == "E": output_code = "1D"
        elif operand[0] == "H": output_code = "25"
        elif operand[0] == "L": output_code = "2D"
        elif operand[0] == "M": output_code = "35"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "RLC": output_code = "07"
    case "RRC": output_code = "0F"
    case "RAL": output_code = "17"
    case "RAR": output_code = "1F"
    case "CMA": output_code = "2F"
    case "STC": output_code = "37"
    case "CMC": output_code = "3F"
    case "DAD":
        if   operand[0] == "B" : output_code = "09"
        elif operand[0] == "D" : output_code = "19"
        elif operand[0] == "H" : output_code = "29"
        elif operand[0] == "SP": output_code = "39"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "INX":
        if   operand[0] == "B" : output_code = "03"
        elif operand[0] == "D" : output_code = "13"
        elif operand[0] == "H" : output_code = "23"
        elif operand[0] == "SP": output_code = "33"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "DCX":
        if   operand[0] == "B" : output_code = "0B"
        elif operand[0] == "D" : output_code = "1B"
        elif operand[0] == "H" : output_code = "2B"
        elif operand[0] == "SP": output_code = "3B"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    # Jump operations
    case "JMP": output_code = "C3"
    case "JM" : output_code = "FA"
    case "JP" : output_code = "F2"
    case "JZ" : output_code = "CA"
    case "JNZ": output_code = "C2"
    case "JC" : output_code = "DA"
    case "JNC": output_code = "D2"
    case "JPE": output_code = "EA"
    case "JPO": output_code = "E2"
    # Memory and register operations
    case "MOV":
        output_code = "01"
        for reg in operand:
          match reg:
            case "A": output_code += "111"
            case "B": output_code += "000"
            case "C": output_code += "001"
            case "D": output_code += "010"
            case "E": output_code += "011"
            case "H": output_code += "100"
            case "L": output_code += "101"
            case   _: output_code += "110"
        output_code = hex(int(output_code,2)).lstrip("0x")
                  
    case "MVI":
        if   operand[0] == "A" : output_code = "3E"
        elif operand[0] == "B" : output_code = "06"
        elif operand[0] == "C" : output_code = "0E"
        elif operand[0] == "D" : output_code = "16"
        elif operand[0] == "E" : output_code = "1E"
        elif operand[0] == "H" : output_code = "26"
        elif operand[0] == "L" : output_code = "2E"
        elif operand[0] == "M" : output_code = "36"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "LXI":
        if   operand[0] == "B" : output_code = "01"
        elif operand[0] == "D" : output_code = "11"
        elif operand[0] == "H" : output_code = "21"
        elif operand[0] == "SP" : output_code = "31"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "LDA": output_code = "3A"
    case "STA": output_code = "32"
    # Stack operations
    case "POP" :
        if   operand[0] == "B"  : output_code = "C1"
        elif operand[0] == "D"  : output_code = "D1"
        elif operand[0] == "H"  : output_code = "E1"
        elif operand[0] == "PSW": output_code = "F1"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "PUSH":
        if   operand[0] == "B"  : output_code = "C5"
        elif operand[0] == "D"  : output_code = "D5"
        elif operand[0] == "H"  : output_code = "E5"
        elif operand[0] == "PSW": output_code = "F5"
        else: raise TypeError("Operands " + operand[0] + " not supported for " + mnemonic)
    case "CALL": output_code = "CD"
    case "CM"  : output_code = "FC"
    case "CP"  : output_code = "F4"
    case "CZ"  : output_code = "CC"
    case "CNZ" : output_code = "C4"
    case "CC"  : output_code = "DC"
    case "CNC" : output_code = "D4"
    case "CPE" : output_code = "EC"
    case "CPO" : output_code = "E4"
    case "RET" : output_code = "C9"
    case "RM"  : output_code = "F8" 
    case "RP"  : output_code = "F0" 
    case "RZ"  : output_code = "C8" 
    case "RNZ" : output_code = "C0" 
    case "RC"  : output_code = "D8" 
    case "RNC" : output_code = "D0" 
    case "RPE" : output_code = "E8" 
    case "RPO" : output_code = "E0" 
    # Other operations
    case "HLT": output_code = "76"
    case "IN" : output_code = "DB"
    case "OUT": output_code = "D3"
    case "NOP": output_code = "00"
    # No identified Instruction, raise an error
    case _:
        raise TypeError("Intruction " + mnemonic + " not supported")
      
  return output_code

# Return the number of memory address needed to store full instruction
def num_mem_address_compute(mnemonic):
   
   match mnemonic:
      # Two bytes operations, mnemonic + byte
      case "ADI"|"ACI"|"SUI"|"SBI"|"ANI"|"ORI"|"XRI"|"CPI"|"MVI"|"IN"|"OUT":
        return 2
      # Three bytes operations, mnemonic + 16 bit address
      case "JMP"|"JM"|"JP"|"JZ"|"JNZ"|"JC"|"JNC"|"JPE"|"JPO"|"LXI"|"LDA"|"STA"|"CALL"|"CM"|"CP"|"CZ"|"CNZ"|"CC"|"CNC"|"CPE"|"CPO":
        return 3
      # The rest of instruction only use one byte
      case _:
        return 1

if __name__ == "__main__":

    labels_dict = {}

    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True)
    parser.add_argument("--output")
    args = parser.parse_args()
    
    input_file  = args.input
    output_file = args.output if args.output != None else args.input.replace('.asm','.txt') 

    with open(input_file, "r") as f:

        memory_count = 0

        # First gets all labels on asembly code and register it as  
        for line in f:
            # Separate operands by spaces
            line = line.split(' ')
            # Remove all white spaces elements
            line = [x for x in line if x]

            if ":" in line[0]:
              # If line contain label, store new label on dictionary 
              labels_dict[line[0].rstrip(":")] = memory_count
              # Store new memory address according instruction
              memory_count += num_mem_address_compute(line[1])
            else:
              # Update new memory address pointer according instruction
              memory_count += num_mem_address_compute(line[0])

    mem_output = []

    with open(input_file, "r") as f:
        
        # Reset memory count
        memory_count = 0

        for line in f:
            # Remove "\n" caracter
            line = line.rstrip("\n")
            # Remove comments
            line = line.split(";")[0]
            # Remove labels
            if ":" in line: line = line.split(":")[1]
            # Separate operands
            line = line.split(' ')
            line = [x for x in line if x]
            # If Instruction includes operands, split them by commas
            if len(line) == 2:
              line[1] = line[1].split(',')

            # One byte instruction
            if num_mem_address_compute(line[0]) == 1:
              mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + translate_instruction(line) + '\n')
              memory_count += 1
            # Two byte instruction
            elif num_mem_address_compute(line[0]) == 2:
              mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + translate_instruction(line) + '\n')
              memory_count += 1
            
              if line[0] == "MVI":
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + line[1][1] + '\n')
                memory_count += 1
              else:
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + line[1][0] + '\n')
                memory_count += 1
            # Three bytes instruction
            else:
              mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + translate_instruction(line) + '\n')
              memory_count += 1

              # For LXI, double byte is set on second operand as first one is the reg-pair
              if line[0] == "LXI":
                double_byte=line[1][1]
              # For the rest, only double byte is provided as operand
              else:
                double_byte=line[1][0]
                
              # Check if double byte provided is label
              if double_byte in labels_dict:
                label_address = labels_dict[double_byte]
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + f"{label_address:0{8 // 4}X}" + '\n')
                memory_count += 1
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + f"{(label_address >> 8):0{8 // 4}X}" + '\n')
                memory_count += 1
              else:
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + double_byte[2:4] + '\n')
                memory_count += 1
                mem_output.append(f"{memory_count:0{16 // 4}X}" + ":" + double_byte[0:2] + '\n')
                memory_count += 1
 

    with open(output_file, "w") as f:
      f.writelines(mem_output)      