      MVI A,00 ; Precharge register values
      LXI B,1122 
      LXI D,3344
      LXI H,5566
      LXI SP,FFFF ; Send beginning of Stack pointer as address 0xFFFF
      ADD L
      ADD L
      ADD L ; Add 3 times L
      POP B   ; pop BC reg-pair to stack pointer
      POP D   ; pop DE reg-pair to stack pointer
      POP H   ; pop HL reg-pair to stack pointer
      POP PSW ; pop Accumulator and flags to stack pointer
      CNC SUBROUTINE ; Call subroutine if carry flag is clear, no jump as carry flag is set
      MVI A,01       ; Check new data loaded on A, means subroutine not call
      CC SUBROUTINE  ; Call subroutine if carry flag is set, jump
      PUSH PSW ; get value of Accumulator and flag stored on stackpointer
      PUSH H   ; get value of HL reg-pair from stack pointer
      PUSH D   ; get value of DE reg-pair from stack pointer
      PUSH B   ; get value of BC reg-pair from stack pointer
      HLT ; END PROGRAM
SUBROUTINE: MVI A,77   ; Change value of all registers
            LXI B,8899
            LXI D,AABB
            LXI H,CCDD
COMPARE:    CMP B ; Compare register B with A, zero flag is reset as values are not equal
            RZ ; Conditional return if Zero = 1, no return as Zero = 0
            CALL SUBROUTINE2 ; Go to subroutine inside subroutine
            JMP COMPARE ; Return to Compare line 
SUBROUTINE2: MOV A,B ; Move register value B to Accumulator
             RET ; Unconditional return to address on Stack pointer