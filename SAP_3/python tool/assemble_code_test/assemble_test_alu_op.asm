      MVI A,00 ; Precharge register values
      MVI B,76
      MVI C,03
MULT: ADD B    ; A = A + B (cycle 1: A = 76, cycle 2: EC, cycle 3: 62 with carry)
      DCR C    ; C = C - 1
      JNZ MULT ; Jump to MULT if C /= 0. Repeat 3 times
      ADC B    ; add with carry A = A + B + carry ( 76 + 62 + 1 = D9)
      MVI D,45
      SUB D    ; A = A - D = D9 - 45 = 94
      ANI 0F   ; apply mask to accumulator, get LSB byte A = 04
      CPI 04   ; Check if Accumulator is set to 04, result on Zero flag (equal = 0)
      CPI 00   ; check if accumulator is loaded to 0 by error on previous operation (zero flag = 1)
      STC      ; set carry
      RAL      ; Rotate left A = carry & A << 1 = 09
      RRC      ; Rotate rigth with carry A = A >> 1 = 84, carry = A[0] = 1
      CMC      ; carry = not carry = 0
      HLT