-- Simple as possible Microprocessor - Part 3
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.sap_3_package.all;

entity controller_seq_sap_3 is
port(  
  sys_clk      : in std_logic;
  sys_rst      : in std_logic;
  IR_reg       : in std_logic_vector(7 downto 0);
  -- Input control bits
  alu_flags    : in alu_flags_t;
  -- Output Control bits
  control_word : out control_word_t
);
end controller_seq_sap_3;

architecture rtl of controller_seq_sap_3 is

  -- Ring count sequencer
  signal ring_count : std_logic_vector(5 downto 0) := "000001";
  -- Reset Ring count (asyncronous signal)
  signal reset_ring_count : std_logic;
  -- New program cycle count value
  signal new_program_cycle_count : unsigned(1 downto 0);
  -- Program cycle count
  signal program_cycle_count     : unsigned(1 downto 0);
  -- New value of control word
  signal new_control_word : control_word_t;

begin

  -- RING count process to control microprocessor sequence
  -- Fetch cycle
  -- T1 --> Set MAR with Program Count value
  -- T2 --> Update Program Count
  -- T3 --> Update Instruction register
  -- Execution cycle (T3, T4 AND t5)
  ring_counter_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      ring_count <= (0 => '1', others => '0');
    elsif falling_edge(sys_clk) then
      -- Continue working until IR is set to HLT
      if IR_reg /= x"76" then

        if reset_ring_count then
          ring_count <= (0 => '1', others => '0');
        else
          -- Update ring counter
          ring_count <= ring_count(ring_count'length-2 downto 0) & ring_count(ring_count'length-1);
        end if;
      end if;
    end if;
  end process;

  -- Control Update - sequential process and register update
  control_update_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      program_cycle_count   <= "00";
      -- Control word Reset status
      --  * Data bus drive temporal register
      --  * On register space B register is selected as 8bit input and output by default
      --  * Address bus set to PC counter by default
      --  * No operation on ALU to not update ALU flags
      --  * No register are loaded to new values
      --  * No increment on 16-bit registers
      control_word          <= (
        data_bus_sel      => EN_TMP,
        reg_8bit_sel_in   => B_select,
        reg_8bit_sel_out  => B_select,
        reg_16bit_sel_out => PC_select,
        reg_16bit_sel_up  => PC_select,
        alu_op_select     => NO_OP,
        others => '0');
    elsif falling_edge(sys_clk) then
      -- Update program cycle count
      program_cycle_count <= new_program_cycle_count;
      -- Update control signals
      control_word <= new_control_word;
    end if;
  end process;

  -- Control process - Combinational logic
  control_comb_process: process(all)
  begin
    -- Set all control word signals default value
    new_control_word <= (
        data_bus_sel      => EN_TMP,
        reg_8bit_sel_in   => B_select,
        reg_8bit_sel_out  => B_select,
        reg_16bit_sel_out => PC_select,
        reg_16bit_sel_up  => PC_select,
        alu_op_select     => NO_OP,
        others => '0');
    -- If no updated, program cycle count keeps it value
    new_program_cycle_count <= program_cycle_count;
    -- RING counter is not reset by default
    reset_ring_count <= '0';

    -- Fetch and T1 --> Set MAR with Program Count value
    if    ring_count(0) = '1' then
      -- Drive PC to address bus
      new_control_word.reg_16bit_sel_out <= PC_select;
      -- load address bus to MAR
      new_control_word.load_mar     <= '1';
    -- Fetch and T2 --> Update Program Count + Load data on RAM to MDR 
    elsif ring_count(1) = '1' then
      -- Program counter Count uP
      new_control_word.reg_16bit_sel_up  <= PC_select;
      new_control_word.reg_16bit_inx     <= '1';
      -- Load RAM data to MDR
      new_control_word.load_mdr <= '1';
      new_control_word.mdr_select_RAM_out <= '1';
      -- Precharge Implicit address
      -- Load HL pair implicit address to address bus
      new_control_word.reg_16bit_sel_out <= HL_select;
      -- Load address bus to MAR
      new_control_word.load_mar <= '1';
    -- Fetch and T3 --> Update Instruction register
    elsif program_cycle_count = "00" and ring_count(2) = '1' then
      -- Drive MDR to Data bus
      new_control_word.data_bus_sel <= EN_MDR;
      -- Load data bus on IR register
      new_control_word.load_ir <= '1';
      -- Precharge MDR with M[HL]
      -- Load data on MDR
      new_control_word.load_mdr <= '1';
      -- Select RAM output to be loaded on MDR
      new_control_word.mdr_select_RAM_out <= '1';
    --  If Instruction is Halt, sequence keeps blocked
    elsif IR_reg /= x"76" then
      -------------------------------------------------------------------
      -- ALU operation (check command.md file for opcodes)
      -------------------------------------------------------------------
      -- ALU operation with registers 0b10_(op 3 bits)_(reg 3 bits)
      if    IR_reg(7 downto 6) = "10" then

        -- T4: Load register to temporal one
        if ring_count(3) = '1' then
          -- Accumulator register
          if IR_reg(2 downto 0) = "111" then
            -- Drive accumulator register to data bus
            new_control_word.data_bus_sel <= EN_A;
          -- MDR register
          elsif IR_reg(2 downto 0) = "110" then
            -- Drive MDR to data bus
            new_control_word.data_bus_sel <= EN_MDR;
          else
            -- Drive register space output to data bus
            new_control_word.data_bus_sel <= EN_REG_SPACE;
            -- Select ouput register according Instruction
            new_control_word.reg_8bit_sel_out <= 
              B_select when IR_reg(2 downto 0) = "000" else
              C_select when IR_reg(2 downto 0) = "001" else
              D_select when IR_reg(2 downto 0) = "010" else
              E_select when IR_reg(2 downto 0) = "011" else
              H_select when IR_reg(2 downto 0) = "100" else
              L_select;
          end if; 
          -- Load data bus into temporal register
          new_control_word.load_temp    <= '1';
        -- T5: Perform ALU operation according Instruction
        else
          -- Set ALU operation
          new_control_word.alu_op_select <= 
            ADD_OP when IR_reg(5 downto 3) = "000" else -- ADD
            ADC_OP when IR_reg(5 downto 3) = "001" else -- ADC
            SUB_OP when IR_reg(5 downto 3) = "010" else -- SUB
            SBB_OP when IR_reg(5 downto 3) = "011" else -- SBB
            AND_OP when IR_reg(5 downto 3) = "100" else -- ANA 
            XOR_OP when IR_reg(5 downto 3) = "101" else -- XRA
            OR_OP  when IR_reg(5 downto 3) = "110" else -- ORA
            SUB_OP; -- CMP

          -- If Intruction is not CPI, ALU result is stored on Accumulator
          if IR_reg(5 downto 3) /= "111" then
            -- drive MDR to data bus
            new_control_word.data_bus_sel <= EN_ALU;
            -- Load data bus into accumulator
            new_control_word.load_accu <= '1';
          end if;
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Inmediate ALU operations (ANI/ORI/XRI/ADI/ACI/SUI/SBI/CPI)
      elsif IR_reg(7 downto 6) = "11" and IR_reg(2 downto 0) = "110" then

        -- T4, program cycle = 0: NO operation, read byte on following cycle
        if program_cycle_count = "00" then
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 1: Move byte to temporal register
        elsif ring_count(2) = '1' then
          -- drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Load data bus into temporal register
          new_control_word.load_temp    <= '1';
        -- T4, program cycle = 1: Perform ALU operation according Instruction
        else
          -- Perform ALU operation according Intruction register
          new_control_word.alu_op_select       <= 
            ADD_OP when IR_reg(5 downto 3) = "000" else -- ADI
            ADC_OP when IR_reg(5 downto 3) = "001" else -- ACI
            SUB_OP when IR_reg(5 downto 3) = "010" else -- SUI
            SBB_OP when IR_reg(5 downto 3) = "011" else -- SBI
            AND_OP when IR_reg(5 downto 3) = "100" else -- ANI
            XOR_OP when IR_reg(5 downto 3) = "101" else -- XRI
            OR_OP  when IR_reg(5 downto 3) = "110" else -- ORI
            SUB_OP; -- CPI

          -- If Intruction is not CPI, ALU result is stored on Accumulator
          if IR_reg(5 downto 3) /= "111" then
            -- drive MDR to data bus
            new_control_word.data_bus_sel <= EN_ALU;
            -- Load data bus into accumulator
            new_control_word.load_accu <= '1';
          end if;
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Increment and decrement register values (INR/DCR)
      elsif IR_reg(7 downto 6) = "00" and IR_reg(2 downto 1) = "10" then 

        -- T4: Load register to temporal one
        if ring_count(3) = '1' then
          -- Accumulator register
          if IR_reg(5 downto 3) = "111" then
            -- Drive accumulator register to data bus
            new_control_word.data_bus_sel <= EN_A;
          -- MDR register
          elsif IR_reg(5 downto 3) = "110" then
            -- Drive MDR to data bus
            new_control_word.data_bus_sel <= EN_MDR;
          else
            -- Drive register space output to data bus
            new_control_word.data_bus_sel <= EN_REG_SPACE;
            -- Select ouput register according Instruction
            new_control_word.reg_8bit_sel_out <= 
              B_select when IR_reg(5 downto 3) = "000" else
              C_select when IR_reg(5 downto 3) = "001" else
              D_select when IR_reg(5 downto 3) = "010" else
              E_select when IR_reg(5 downto 3) = "011" else
              H_select when IR_reg(5 downto 3) = "100" else
              L_select;
          end if; 
          -- Load data bus into temporal register
          new_control_word.load_temp    <= '1';
        -- T5: Perform ALU operation according Instruction
        elsif ring_count(4) = '1' then
          -- Perform ALU operation according Intruction register
          new_control_word.alu_op_select       <= 
            INR_OP when IR_reg(0) = '0' else -- Increment
            DCR_OP; -- Decrement
          -- Drive ALU output to data 
          new_control_word.data_bus_sel <= EN_ALU;
          -- Accumulator register
          if IR_reg(5 downto 3) = "111" then
            -- Load accumulator register
            new_control_word.load_accu <= '1';
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          -- MDR register
          elsif IR_reg(5 downto 3) = "110" then
            -- Drive MDR to data bus
            new_control_word.load_mdr <= '1';
            -- Select data from data bus to be loaded on MDR
            new_control_word.mdr_select_RAM_out <= '0';
          else
            -- Load register on reg Space
            new_control_word.reg_8bit_load <= '1';
            -- Select input register according Instruction
            new_control_word.reg_8bit_sel_in <= 
              B_select when IR_reg(5 downto 3) = "000" else
              C_select when IR_reg(5 downto 3) = "001" else
              D_select when IR_reg(5 downto 3) = "010" else
              E_select when IR_reg(5 downto 3) = "011" else
              H_select when IR_reg(5 downto 3) = "100" else
              L_select;
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if; 
        -- T6: Write data on MDR to RAM, only when operation over M[HL]
        else
          new_control_word.RAM_write_en <= '1'; 
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';  
        end if;
      
      -- Rotate and logic operations over accumulator (RLC/RRC/RAL/RAR/CMA/STC/CMC) 0b11_(op 3 bits)_111
      elsif IR_reg(7 downto 6) = "00" and IR_reg(2 downto 0) = "111" then
        -- ALU operation select
        new_control_word.alu_op_select <=
          RLC_OP when IR_reg(5 downto 3) = "000" else
          RRC_OP when IR_reg(5 downto 3) = "001" else
          RAL_OP when IR_reg(5 downto 3) = "010" else
          RAR_OP when IR_reg(5 downto 3) = "011" else
          NOT_OP when IR_reg(5 downto 3) = "101" else
          STC_OP when IR_reg(5 downto 3) = "110" else
          CMC_OP when IR_reg(5 downto 3) = "111" else
          NO_OP; 
        -- Drive ALU output to data bus
        new_control_word.data_bus_sel <= EN_ALU;
        -- Load Accumulator with data bus
        new_control_word.load_accu <= '1';
        -- END instruction
        new_program_cycle_count <= (others => '0');
        reset_ring_count    <= '1';

      -- Increment and decrement reg-pair operations (INX/DCX) 0b00_(reg-pair 2 bits)_0011/0b00_(reg-pair 2 bits)_1011
      elsif IR_reg(7 downto 6) = "00" and IR_reg(2 downto 0) = "011" then
        -- Depending on instruction, increment or decrement reg-pair
        if IR_reg(3) = '0' then
           new_control_word.reg_16bit_inx <= '1';
        else
           new_control_word.reg_16bit_dcx <= '1';
        end if;
        -- Select reg-pair depending instruction
         new_control_word.reg_16bit_sel_up <=
          BC_select when IR_reg(5 downto 4) = "00" else
          DE_select when IR_reg(5 downto 4) = "01" else
          HL_select when IR_reg(5 downto 4) = "10" else
          SP_select;
        -- END instruction
        new_program_cycle_count <= (others => '0');
        reset_ring_count    <= '1';

     -- Double ADD operation HL = HL + reg-pair (0b00_(reg-pair 2 bits)_1001
      elsif IR_reg(7 downto 6) = "00" and IR_reg(3 downto 0) = "1001" then
        -- T3, program cycle 1: decrement PC
        if ring_count(2) = '1' then
          -- Program counter Count down
          new_control_word.reg_16bit_sel_up  <= PC_select;
          new_control_word.reg_16bit_dcx     <= '1';
        -- T4, program cycle 0 and 1: load L then H to accumulator
        elsif ring_count(3) = '1' then
          -- Load accumulator with data bus
          new_control_word.load_accu <= '1';
          -- Drive register space output to data bus
          new_control_word.data_bus_sel <= EN_REG_SPACE;
          -- Select register space output (L first, H on following program cycle)
          new_control_word.reg_8bit_sel_out <=
            L_select when program_cycle_count = "00" else
            H_select;
        -- T5, program cycle 0 and 1: load reg pair to temporal register
        elsif ring_count(4) = '1' then
          -- Load accumulator with data bus
          new_control_word.load_accu <= '1';
          -- Drive register space output to data bus
          new_control_word.data_bus_sel <= EN_REG_SPACE;
          -- Select register space output (L first, H on following program cycle)
          new_control_word.reg_8bit_sel_out <=
            -- Program cycle 0, LSB of reg pair
            C_select      when program_cycle_count = "00" and IR_reg(5 downto 4) = "00" else
            E_select      when program_cycle_count = "00" and IR_reg(5 downto 4) = "01" else
            L_select      when program_cycle_count = "00" and IR_reg(5 downto 4) = "10" else
            SP_LSB_select when program_cycle_count = "00" and IR_reg(5 downto 4) = "11" else
            -- Program cycle 1, MSB of reg pair
            B_select      when program_cycle_count = "01" and IR_reg(5 downto 4) = "00" else
            D_select      when program_cycle_count = "01" and IR_reg(5 downto 4) = "01" else
            H_select      when program_cycle_count = "01" and IR_reg(5 downto 4) = "10" else
            SP_MSB_select;
        
        -- T6, program cycle 0 and 1: Add operation and load on L then on H registers
        else
          -- Load data bus on register space
          new_control_word.reg_8bit_load <= '1';
          -- Select input of register space (L first, H on following program cycle)
          new_control_word.reg_8bit_sel_in <=
            L_select when program_cycle_count = "00" else
            H_select;
          -- Select ALU output to be drived on data bus
          new_control_word.data_bus_sel <= EN_ALU;
          -- Select ALU operation (first ADD, ADC on following program cycle)
          new_control_word.alu_op_select <=
            ADD_OP when program_cycle_count = "00" else
            ADC_OP;
          -- End operation depend on program 
          if program_cycle_count = "00" then
            new_program_cycle_count <= program_cycle_count + 1;
          else
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;
        end if;

      -------------------------------------------------------------------
      -- Jump Instructions (check command.md file for opcodes)
      -------------------------------------------------------------------
      elsif IR_reg = x"C3" or (IR_reg(7 downto 6) = "11" and IR_reg(2 downto 0) = "010") then
        -- Jump depending on Instrucction and flag status
        if (IR_reg(5 downto 3) = "00" & alu_flags.zero)   OR -- Zero condition Jump
           (IR_reg(5 downto 3) = "01" & alu_flags.carry)  OR -- Carry condition Jump
           (IR_reg(5 downto 3) = "10" & alu_flags.parity) OR -- Parity condition Jump
           (IR_reg(5 downto 3) = "11" & alu_flags.neg)    OR -- Carry condition Jump 
           IR_reg = x"C3" then -- Unconditional Jump
           
          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if program_cycle_count = "00" then

            new_program_cycle_count  <= program_cycle_count + 1;
            reset_ring_count         <= '1';
          -- T3, program cycle 1: Load LSB addr to temporal register and
          --                      extract MSB addr from memory on following program cycle
          elsif program_cycle_count = "01" then

            new_program_cycle_count  <= program_cycle_count + 1;
            reset_ring_count         <= '1';
            -- Drive MDR to Data bus
            new_control_word.data_bus_sel <= EN_MDR;
            -- Load data bus on Temporal register
            new_control_word.load_temp <= '1';
          -- T3, program cycle 2: Load MSB addr to PC MSB
          elsif ring_count(2) = '1' then
            -- Drive MDR to Data bus
            new_control_word.data_bus_sel <= EN_MDR;
            -- Select Register space input as PC
            new_control_word.reg_8bit_sel_in <= PC_MSB_select;
            -- Load data bus on register space
            new_control_word.reg_8bit_load <= '1';
          -- T4, program cycle 2: Load Temporal register to PC LSB 
          else
            -- Drive Temporal register to Data bus
            new_control_word.data_bus_sel    <= EN_TMP;
            -- Select Register space input as PC
            new_control_word.reg_8bit_sel_in <= PC_LSB_select;
            -- Load data bus on register space
            new_control_word.reg_8bit_load <= '1';
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;
        -- No Jump
        else
          -- T4, T5: Update PC count to jump address
          if ring_count(3) = '1' or ring_count(4) = '1' then
            -- Program counter Count uP
            new_control_word.reg_16bit_sel_out <= PC_select;
            new_control_word.reg_16bit_inx     <= '1';
          -- T6: NOP - reset RING counter
          else
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;
        end if;
      
      -------------------------------------------------------------------
      -- Stack operations (check command.md file for opcodes)
      -------------------------------------------------------------------
      -- POP data on Stack
      elsif IR_reg(7 downto 6) = "11" and IR_reg(3 downto 0) = x"1" then
      
        ------------------------------------
        -- T op = 0             op = 1      
        ------------------------------------
        -- 1 fetch              fetch              
        -- 2 fetch              fetch              
        -- 3 fetch              PC-1
        -- 4 SP-1               SP-1
        -- 5 Load MDR and MAR   Load MDR and MAR
        -- 6 write RAM          write RAM
        ------------------------------------
        
        -- T3, program cycle = 1: Decrease PC counter
        if    ring_count(2) = '1' then
          -- Program counter Count down
          new_control_word.reg_16bit_sel_up  <= PC_select;
          new_control_word.reg_16bit_dcx     <= '1';
        -- T4, program cycle = 0 and 1: Decrease SP 
        elsif ring_count(3) = '1' then
          -- Stack pointer Count down
          new_control_word.reg_16bit_sel_up  <= SP_select;
          new_control_word.reg_16bit_dcx     <= '1';
        -- T5, program cycle = 0 and 1: Load data to MDR and Load SP to MAR
        elsif ring_count(4) = '1' then
          -- Load data bus into MDR
          new_control_word.load_mdr           <= '1';
          -- Select data bus to be loaded on MDR
          new_control_word.mdr_select_RAM_out <= '0';
          -- Select data to load depending on Instruction and program cycle
          new_control_word.data_bus_sel <= 
            EN_A         when IR_reg(5 downto 4) = "11" and program_cycle_count = "00" else -- PSW[MSB]
            EN_ALU_FLAGS when IR_reg(5 downto 4) = "11" and program_cycle_count = "01" else -- PSW[LSB]
            EN_REG_SPACE; -- Register space
          -- Select data on register space depending instruction and program cycle 
          new_control_word.reg_8bit_sel_out <=
            B_select when IR_reg(5 downto 4) = "00" and program_cycle_count = "00" else -- BC[MSB]
            D_select when IR_reg(5 downto 4) = "01" and program_cycle_count = "00" else -- DE[MSB]
            H_select when IR_reg(5 downto 4) = "10" and program_cycle_count = "00" else -- HL[MSB]
            C_select when IR_reg(5 downto 4) = "00" and program_cycle_count = "01" else -- BC[LSB]
            E_select when IR_reg(5 downto 4) = "01" and program_cycle_count = "01" else -- BC[LSB]
            L_select; -- HL[LSB]
          -- Select SP on register space to be drived to address bus
          new_control_word.reg_16bit_sel_out <= SP_select;
          -- Load MAR with address bus
          new_control_word.load_mar          <= '1';
        -- T6, program cycle = 0 and 1: Write RAM
        elsif ring_count(5) = '1' then
          -- Write MDR on RAM
          new_control_word.RAM_write_en      <= '1';
          -- Reset Ring count
          reset_ring_count    <= '1';
          -- Update count or end program
          if program_cycle_count = "00" then
            new_program_cycle_count <= program_cycle_count + 1;
          else
            new_program_cycle_count <= (others => '0');
          end if; 
        end if;

      -- Call subroutine (CALL/CNZ/CZ/CNC/CC/CPO/CPE/CP/CM)
      elsif IR_reg = x"CD" or (IR_reg(7 downto 6) = "11" and IR_reg(2 downto 0) = "100") then
      
        ----------------------------------------------------------------------
        -- T op = 0   op = 1      op = 2                          op = 3
        ----------------------------------------------------------------------
        -- 1 fetch    fetch       fetch                           fetch
        -- 2 fetch    fetch       fetch                           fetch
        -- 3 fetch    Z = MDR     W = MDR, SP-1                   PC[LSB] = Z
        -- 4 NOP      -           MDR = PC[MSB], L_MAR, SP-1      PC[MSB] = W
        -- 5 -        -           MDR = PC[LSB], L_MAR, write_RAM -
        -- 6 -        -           write_RAM                       -
        --------------------------------------------------------------------

          -- T4, program cycle = 0: NOP
          if    program_cycle_count = "00" then
            -- NOP, read LSB of subroutine address on next section
            new_program_cycle_count  <= program_cycle_count + 1;
            reset_ring_count         <= '1';
          -- T3, program cycle = 1: load MDR on Z register
          elsif program_cycle_count = "01" then
            -- Drive MDR to data bus
            new_control_word.data_bus_sel <= EN_MDR;
            -- Load data bus on register space
            new_control_word.reg_8bit_load <= '1';
            -- Select input register on register space as Z
            new_control_word.reg_8bit_sel_in <= Z_select;
            -- Update program count cycle
            new_program_cycle_count  <= program_cycle_count + 1;
            reset_ring_count         <= '1';
          -- T3, program cycle = 2: load MDR on W register, decrement SP if subroutine is loaded or end instruction 
          elsif program_cycle_count = "10" and ring_count(2) = '1' then
            -- Drive MDR to data bus
            new_control_word.data_bus_sel <= EN_MDR;
            -- Load data bus on register space
            new_control_word.reg_8bit_load <= '1';
            -- Select input register on register space as W
            new_control_word.reg_8bit_sel_in <= W_select;
            -- Call condition not achived, end instruction
            if IR_reg /= x"CD" and (
              IR_reg(5 downto 3) = "00" & not(alu_flags.zero)   or
              IR_reg(5 downto 3) = "01" & not(alu_flags.carry)  or
              IR_reg(5 downto 3) = "10" & not(alu_flags.parity) or
              IR_reg(5 downto 3) = "11" & not(alu_flags.neg)) then

              -- END instruction
              new_program_cycle_count <= (others => '0');
              reset_ring_count    <= '1';
            -- Call condition achived. Continue instruction
            else
              -- Decrement Stack pointer
              new_control_word.reg_16bit_sel_up  <= SP_select;
              new_control_word.reg_16bit_dcx     <= '1';
            end if;
          -- T4, program cycle = 2: Load MAR with SP, MDR with PC[MSB] and decrement SP 
          elsif program_cycle_count = "10" and ring_count(3) = '1' then
            -- Output PC[MSB] on reg space
            new_control_word.reg_8bit_sel_out   <= PC_MSB_select;
            -- Drive Register space output on data bus
            new_control_word.data_bus_sel       <= EN_REG_SPACE;
            -- Load value on MDR
            new_control_word.load_mdr           <= '1';
            -- Set data bus to be loaded on MDR
            new_control_word.mdr_select_RAM_out <= '0';
            -- Load MAR with address bus
            new_control_word.load_mar           <= '1';
            -- Select SP as output of register space
            new_control_word.reg_16bit_sel_out  <= SP_select;
            -- Decrement Stack pointer
            new_control_word.reg_16bit_sel_up   <= SP_select;
            new_control_word.reg_16bit_dcx      <= '1';
          -- T5, program cycle = 2: Load MAR with SP, MDR with PC[LSB] and write MDR to RAM 
          elsif program_cycle_count = "10" and ring_count(4) = '1' then
            -- Output PC[LSB] on reg space
            new_control_word.reg_8bit_sel_out   <= PC_LSB_select;
            -- Drive Register space output on data bus
            new_control_word.data_bus_sel       <= EN_REG_SPACE;
            -- Load value on MDR
            new_control_word.load_mdr           <= '1';
            -- Set data bus to be loaded on MDR
            new_control_word.mdr_select_RAM_out <= '0';
            -- Load MAR with address bus
            new_control_word.load_mar           <= '1';
            -- Select SP as output of register space
            new_control_word.reg_16bit_sel_out  <= SP_select;
            -- Write MDR on RAM
            new_control_word.RAM_write_en       <= '1';
          -- T6, program cycle = 2: write MDR to RAM
          elsif program_cycle_count = "10" and ring_count(5) = '1' then
            -- Write MDR on RAM
            new_control_word.RAM_write_en       <= '1';
            -- Update program count cycle
            new_program_cycle_count  <= program_cycle_count + 1;
            reset_ring_count         <= '1';
          -- T3, program cycle = 3: Load PC[LSB] = Z
          elsif program_cycle_count = "11" and ring_count(2) = '1' then
            -- Select PC[LSB] as input register on Register space
            new_control_word.reg_8bit_sel_in   <= PC_LSB_select;
            -- Load data bus on input register
            new_control_word.reg_8bit_load     <= '1';
            -- Output Z register on reg space
            new_control_word.reg_8bit_sel_out  <= Z_select;
            -- Drive Register space output on data bus
            new_control_word.data_bus_sel      <= EN_REG_SPACE;
          -- T4, program cycle = 3: Load PC[MSB] = W
          else
            -- Select PC[MSB] as input register on Register space
            new_control_word.reg_8bit_sel_in   <= PC_MSB_select;
            -- Load data bus on input register
            new_control_word.reg_8bit_load     <= '1';
            -- Output W register on reg space
            new_control_word.reg_8bit_sel_out  <= W_select;
            -- Drive Register space output on data bus
            new_control_word.data_bus_sel      <= EN_REG_SPACE;
            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;

      -- PUSH data on Stack
      elsif IR_reg(7 downto 6) = "11" and IR_reg(3 downto 0) = x"5" then

        ------------------------------------
        -- T op = 0             op = 1      
        ------------------------------------
        -- 1 fetch              fetch              
        -- 2 fetch              fetch              
        -- 3 fetch              PC-1
        -- 4 Load MAR           Load MAR
        -- 5 Load MDR, SP+1     Load MDR, SP+1
        -- 6 Load reg           Load reg
        ------------------------------------

        -- T3, program cycle = 1: Decrease PC counter
        if    ring_count(2) = '1' then
          -- Program counter Count down
          new_control_word.reg_16bit_sel_up  <= PC_select;
          new_control_word.reg_16bit_dcx     <= '1';
        -- T4, program cycle = 0 and 1: Load SP on MAR 
        elsif ring_count(3) = '1' then
          -- Load MAR with address bus
          new_control_word.load_mar <= '1';
          -- Output SP to address bus
          new_control_word.reg_16bit_sel_out <= SP_select; 
        -- T5, program cycle = 0 and 1: Load data to MDR and increment SP
        elsif ring_count(4) = '1' then
          -- Stack pointer Count up
          new_control_word.reg_16bit_sel_up  <= SP_select;
          new_control_word.reg_16bit_inx     <= '1';
          -- Load data bus into MDR
          new_control_word.load_mdr           <= '1';
          -- Select RAM output to be loaded on MDR
          new_control_word.mdr_select_RAM_out <= '1';
        -- T6, program cycle = 0 and 1: Write RAM
        elsif ring_count(5) = '1' then
          -- Drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Load data bus into register depending instrucction and program cycle
          if    IR_reg(5 downto 4) = "11" and program_cycle_count = "00" then
            new_control_word.load_alu_flags <= '1';
          elsif IR_reg(5 downto 4) = "11" and program_cycle_count = "01" then
            new_control_word.load_accu <= '1';
          else
            -- Load data on register space
            new_control_word.reg_8bit_load <= '1';
            -- Select register to be loaded
            new_control_word.reg_8bit_sel_in <=
              C_select when IR_reg(5 downto 4) = "00" and program_cycle_count = "00" else -- BC[LSB]
              E_select when IR_reg(5 downto 4) = "01" and program_cycle_count = "00" else -- DE[LSB]
              L_select when IR_reg(5 downto 4) = "10" and program_cycle_count = "00" else -- HL[LSB]
              B_select when IR_reg(5 downto 4) = "00" and program_cycle_count = "01" else -- BC[MSB]
              D_select when IR_reg(5 downto 4) = "01" and program_cycle_count = "01" else -- DE[MSB]
              H_select; -- HL[MSB]   
          end if;
          -- Reset Ring count
          reset_ring_count    <= '1';
          -- Update count or end program
          if program_cycle_count = "00" then
            new_program_cycle_count <= program_cycle_count + 1;
          else
            new_program_cycle_count <= (others => '0');
          end if; 
        end if;

      -- Return from subroutine (RET/RNZ/RZ/RNC/RC/RPO/RPE/RP/RM)
      elsif IR_reg = x"C9" or (IR_reg(7 downto 6) = "11" and IR_reg(2 downto 0) = "000") then

        ------------------------------------
        -- T op = 0             op = 1      
        ------------------------------------
        -- 1 fetch              fetch              
        -- 2 fetch              fetch              
        -- 3 fetch              PC[LSB] = Z
        -- 4 Load MAR           Load MAR
        -- 5 Load MDR, SP+1     Load MDR, SP+1
        -- 6 Load Z             PC[MSB] = MDR
        ------------------------------------

        -- T3, program cycle = 1: Load PC[LSB] with Z
        if    ring_count(2) = '1' then
          -- Select PC[LSB] as input register on Register space
          new_control_word.reg_8bit_sel_in   <= PC_LSB_select;
          -- Load data bus on input register
          new_control_word.reg_8bit_load     <= '1';
          -- Output Z register on reg space
          new_control_word.reg_8bit_sel_out  <= Z_select;
          -- Drive Register space output on data bus
          new_control_word.data_bus_sel      <= EN_REG_SPACE;
        -- T4, program cycle = 0 and 1: Load SP on MAR, IF RETURN CONDITION IS NOT ACHIVED, INSTRUCTION END
        elsif ring_count(3) = '1' then
          -- Load MAR with address bus
          new_control_word.load_mar <= '1';
          -- Output SP to address bus
          new_control_word.reg_16bit_sel_out <= SP_select;
          -- End Instruction depending on flags and instruction
          if IR_reg /= x"C9" and (
            IR_reg(5 downto 3) = "00" & not(alu_flags.zero)   or
            IR_reg(5 downto 3) = "01" & not(alu_flags.carry)  or
            IR_reg(5 downto 3) = "10" & not(alu_flags.parity) or
            IR_reg(5 downto 3) = "11" & not(alu_flags.neg)) then

            -- END instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;
        -- T5, program cycle = 0 and 1: Load data to MDR and increment SP
        elsif ring_count(4) = '1' then
          -- Stack pointer Count up
          new_control_word.reg_16bit_sel_up  <= SP_select;
          new_control_word.reg_16bit_inx     <= '1';
          -- Load data bus into MDR
          new_control_word.load_mdr           <= '1';
          -- Select RAM output to be loaded on MDR
          new_control_word.mdr_select_RAM_out <= '1';
        -- T6, program cycle = 0 and 1: Load Z register or PC[MSB] with MDR 
        elsif ring_count(5) = '1' then
          -- Drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Select PCMLSB] as input register or Z on Register space depending program cycle
          new_control_word.reg_8bit_sel_in   <= 
            Z_select when program_cycle_count = "00" else
            PC_MSB_select;
          -- Load data bus on input register
          new_control_word.reg_8bit_load     <= '1';
          -- Reset Ring count
          reset_ring_count    <= '1';
          -- Update count or end program
          if program_cycle_count = "00" then
            new_program_cycle_count <= program_cycle_count + 1;
          else
            new_program_cycle_count <= (others => '0');
          end if; 
        end if;

      -------------------------------------------------------------------
      -- Memory and register operation (check command.md file for opcodes)
      -------------------------------------------------------------------
      -- Move data between registers
      elsif IR_reg(7 downto 6) = "01" then

        -- T4: Move data between registers 
        if ring_count(3) = '1' then
          -- reg1 --
          -- reg1 = Accumulator
          if    IR_reg(5 downto 3) = "111" then
            new_control_word.load_accu <= '1';
          -- reg1 = M[HL], load on MDR 
          elsif IR_reg(5 downto 3) = "110" then
            new_control_word.load_mdr  <= '1';
            new_control_word.mdr_select_RAM_out <= '0';         
          -- reg
          else
            -- Load register
            new_control_word.reg_8bit_load <= '1';
            -- Select register space input depending on instruction
            new_control_word.reg_8bit_sel_in  <= 
              B_select when IR_reg(5 downto 3) = "000" else
              C_select when IR_reg(5 downto 3) = "001" else
              D_select when IR_reg(5 downto 3) = "010" else
              E_select when IR_reg(5 downto 3) = "011" else
              H_select when IR_reg(5 downto 3) = "100" else
              L_select;
          end if;
          -- reg2 --
          -- Drive Accumulator register to data bus if selected, otherwise drive register space output
          new_control_word.data_bus_sel     <=
            EN_A   when IR_reg(2 downto 0) = "111" else
            EN_MDR when IR_reg(2 downto 0) = "110" else
            EN_REG_SPACE;
          -- Select register space output depending on instruction
          new_control_word.reg_8bit_sel_out <= 
            B_select when IR_reg(2 downto 0) = "000" else
            C_select when IR_reg(2 downto 0) = "001" else
            D_select when IR_reg(2 downto 0) = "010" else
            E_select when IR_reg(2 downto 0) = "011" else
            H_select when IR_reg(2 downto 0) = "100" else
            L_select;

        -- T5: End instruction. Write data on RAM 
        else
          -- Write data on RAM, If MDR does not change, no change is produced on RAM
          new_control_word.RAM_write_en <= '1';
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Move data inmediatly to register
      elsif IR_reg(7 downto 6) = "00" and IR_reg(2 downto 0) = "110" then

        -- T4, program cycle = 0: NO operation, read data to move to registers on following program cycle
        if program_cycle_count = "00" then
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 1: Load data on MDR to register, or write on RAM if M[HL] is selected 
        else
          -- Accumulator register selected
          if    IR_reg(5 downto 3) = "111" then
            new_control_word.load_accu     <= '1';
          -- M[HL] selected
          elsif IR_reg(5 downto 3) = "110" then
            -- Write data on RAM. Address precharged on T2, program cycle = 01
            new_control_word.RAM_write_en <= '1';
          else
            -- Load register
            new_control_word.reg_8bit_load <= '1';
            -- Select register space input depending on instruction
            new_control_word.reg_8bit_sel_in  <= 
              B_select when IR_reg(5 downto 3) = "000" else
              C_select when IR_reg(5 downto 3) = "001" else
              D_select when IR_reg(5 downto 3) = "010" else
              E_select when IR_reg(5 downto 3) = "011" else
              H_select when IR_reg(5 downto 3) = "100" else
              L_select;
          end if;
          -- Load MDR data to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Load register-pair inmediatly
      elsif IR_reg(7 downto 6) = "00" and IR_reg(3 downto 0) = x"1" then

        -- T4, program cycle = 0: NO operation, read LSB data word on following cycle
        if program_cycle_count = "00" then
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 1: Load LSB data to reg-pair, read MSB data word on following cycle
        elsif program_cycle_count = "01" then
          -- Load data on Register space
          new_control_word.reg_8bit_load <= '1';
          -- Select LSB register of reg-pair depending on instruction
          new_control_word.reg_8bit_sel_in <=
            C_select when IR_reg(5 downto 4) = "00" else
            E_select when IR_reg(5 downto 4) = "01" else
            L_select when IR_reg(5 downto 4) = "10" else
            SP_LSB_select;
          -- drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 2: Load MSB data to reg-pair
        else
          -- Load data on Register space
          new_control_word.reg_8bit_load <= '1';
          -- Select LSB register of reg-pair depending on instruction
          new_control_word.reg_8bit_sel_in <=
            B_select when IR_reg(5 downto 4) = "00" else
            D_select when IR_reg(5 downto 4) = "01" else
            H_select when IR_reg(5 downto 4) = "10" else
            SP_MSB_select;
          -- drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Load data on RAM address to accumulator (LDA) or load accumulator to RAM address (STA)
      elsif IR_reg = x"3A" or IR_reg = x"32" then

       -----------------------
       -- T op    op     op
       -----------------------
       -- 1 fetch fetch  fetch
       -- 2 fetch fetch  fetch
       -- 3 fetch load_Z load_W
       -- 4 nop   -      load WZ address
       -- 5 -     -      load MDR (RAM to MDR, A to MDR)
       -- 6 -     -      write RAM/load A
       ----------------------

        -- T4, program cycle = 0: NO operation, read LSB address on following cycle
        if program_cycle_count = "00" then
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 1: Load LSB address to Z register, read MSB address on following cycle
        elsif program_cycle_count = "01" then
          -- Load data on Register space
          new_control_word.reg_8bit_load <= '1';
          -- Select Z register
          new_control_word.reg_8bit_sel_in <= Z_select;
          -- drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Program Cycle count update
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle = 2: Load MSB data to W register
        elsif ring_count(2) = '1' then
          -- Load data on Register space
          new_control_word.reg_8bit_load <= '1';
          -- Select W register
          new_control_word.reg_8bit_sel_in <= W_select;
          -- drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
        -- T4, program cycle = 2: Load WZ reg-pair to address register
        elsif ring_count(3) = '1' then
          -- Load MAR register with address bus
          new_control_word.load_mar <= '1';
          -- Drive WZ reg-pair to address bus
          new_control_word.reg_16bit_sel_out <= WZ_select;
        -- T5, program cycle = 2: Load en MDR data on WZ address
        elsif ring_count(4) = '1' then
          -- Load MDR register
          new_control_word.load_mdr <= '1';
          -- Load MDR with RAM output
          new_control_word.mdr_select_RAM_out <= 
            '0' when IR_reg = x"32" else -- STA, load data bus into MDR
            '1';                         -- LDA, load RAM output into MDR
          -- Drive Accumulator to data bus (not used on LDA)
          new_control_word.data_bus_sel <= EN_A;
        -- T6, program cyle = 2: Load Accumulator with MDR (LDA) or Load RAM with MDR (STA) 
        else
          -- Drive MDR value to data bus (not used on STA)
          new_control_word.data_bus_sel <= EN_MDR;
          -- STA
          if IR_reg = x"32" then
            -- Write MDR on RAM
            new_control_word.RAM_write_en <= '1';
          -- LDA
          else
            -- Load Accumulator with data bus
            new_control_word.load_accu <= '1';
          end if;
          -- END instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;
      
      -------------------------------------------------------------------
      -- Other operations (check command.md file for opcodes)
      -------------------------------------------------------------------
      -- Load byte data on specified input port (byte) to Accumulator register
      elsif IR_reg = x"DB" then

        -- T4, program cycle 0: NOP - extract data from memory on following program cycle
        if program_cycle_count = "00" then
          
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count    <= '1';
        -- T3, program cycle 1: Load input switch multiplexer  
        elsif program_cycle_count = "001" and ring_count(2) = '1' then
          -- Drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Load data bus to Input switch register
          new_control_word.load_in_switch <= '1';
        -- T4, program cycle 1: Move Input to accumulator 
        else
          -- Drive Input port switched to data bus
          new_control_word.data_bus_sel <= IN_PORT;
          -- Load Accumulator with data bus
          new_control_word.load_accu <= '1';
          -- End instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- Output byte data on Accumulator register to specified output port (byte)
      elsif IR_reg = x"D3" then
      
        -- T4, program cycle 0: NOP - extract data from memory on following program cycle
        if program_cycle_count = "00" then
          
          new_program_cycle_count <= program_cycle_count + 1;
          reset_ring_count        <= '1';
        -- T3, program cycle 1: Load output switch multiplexer  
        elsif program_cycle_count = "001" and ring_count(2) = '1' then
          -- Drive MDR to data bus
          new_control_word.data_bus_sel <= EN_MDR;
          -- Load Output switch register
          new_control_word.load_out_switch <= '1';
        -- T4, program cycle 1: Move Input to accumulator 
        else
          -- Drive Accumulator to data bus
          new_control_word.data_bus_sel <= EN_A;
          -- load_outport
          new_control_word.load_outport <= '1';
          -- End instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';
        end if;

      -- No operation
      else
        -- T4, program cycle 0: NOP - reset Ring count
        reset_ring_count    <= '1';
      end if;
    end if;
  end process;

end rtl;