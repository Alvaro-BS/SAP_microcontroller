# SAP_microcontroller
VHDL implementation of three steps Simple As Possible (SAP) microcontroller as indicated on "Digital Computer Electronics" from Albert Paul Malvino and Jerald A. Brown. SAP microcontroller is
based on Intel 8080/8085 8-bit data microcontroller. This repository is organized in three main folders corresponding to three implementation steps mentioned in previous book.

  * SAP_1 : Corresponds to chapter 10, defines a simple microcontroller only capable of 5
    operations: Load data from memory to accumulator (LDA), add or subtract data to accumulator
    (ADD/SUB), drive accumulator value to output port (OUT) and halt operation (HLT).
    It implements a RAM of 8-bit data and 16 address depth. Microprocessor follows a Von Neumann
    architecture where Instruction and data are allocated on same memory space. In addittion, on
    this microprocessor version each instruction is coded on LSB nibble where MSB nibble is
    generally used to make an address reference on RAM to data. Microprocessor cycle is commanded
    with a 6-bits RING count as original Intel 8080/8085 processors. This RING count keeps on
    following SAP versions. 

 * SAP_2 : Corresponds to chapter 11, SAP controller commands palette is incremented as shows
   'SAP_2/commands.md'. Also more HW is added:
      - RAM depth is incremented to 64K.
      - Added B and C internal registers.
      - Internal Word bus is incremented to 16-bits (contrary to book version).
      - Added two new Input ports.
      - Added one more Output port.
      - Added new register to store RAM output (MDR)
    See 'SAP_2/sap_2_microprocessor.vhd' description to get more information about assumptions
    on code from original logic. Main difference between SAP_1 and SAP_2 is the notable
    increment on supported commands and the difference on command codification, being now used
    one byte to indicate a command opCode and more bytes (depending of command) to indicate data
    assiciated to command (ex: on LDA operation, one byte is used to indicate LDA opCode and
    other two bytes to indicate RAM address where data is allocated). 

 * SAP_3 : Corresponds to chapter 12 and final version, it increments the supported commands list
   according to 'SAP_3/commands.md'. The most relevant changes on HW are:
     - Full internal register space is added as Intel 8080/8085.
     - Internal data bus is set again to 8-bits and addresses are separated in other different
       bus as Intel 8080.
   See 'SAP_3/sap_3_microprocessor.vhd' description to get more information about assumptions on
   code.
   
