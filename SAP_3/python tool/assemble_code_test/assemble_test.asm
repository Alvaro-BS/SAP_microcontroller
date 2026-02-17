       MVI A,17  ; Move inmediate byte to A
SUMAR: MVI B,2D  ; Move inmediate byte to B
       ADD B     ; ADD both registers
       STA SUMAR ; Move result to RAM
       INR A     ; Icrement A
       MOV C,A   ; Move A to C
       HLT       ; END program