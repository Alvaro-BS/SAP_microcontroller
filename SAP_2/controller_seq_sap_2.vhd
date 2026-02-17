-- Simple as possible Microprocessor - Part 2
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.sap_2_package.all;

entity controller_seq_sap_2 is
port(  
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  IR        : in std_logic_vector(7 downto 0);
  -- Input control bits
  neg_flag              : in std_logic;
  zero_flag             : in std_logic;
  -- Output Control bits
  load_call_reg         : out std_logic;
  load_in_switch        : out std_logic;
  en_word_bus           : out std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0);
  ALU_select            : out std_logic_vector(ALU_SELECT_WIDTH-1 downto 0);
  load_pc               : out std_logic;
  count_update          : out std_logic;
  load_mar              : out std_logic;
  load_LSB_MDR_RAM_data : out std_logic;
  load_MSB_MDR_RAM_data : out std_logic;
  load_MDR_word_bus     : out std_logic;
  RAM_write_en_cntrl    : out std_logic;
  load_ir               : out std_logic;
  load_accu             : out std_logic;
  load_flag             : out std_logic;
  load_reg_temp         : out std_logic;
  load_reg_b            : out std_logic;
  load_reg_c            : out std_logic;
  load_outport          : out std_logic;
  load_out_switch       : out std_logic
);
end controller_seq_sap_2;

architecture rtl of controller_seq_sap_2 is

  -- Ring count sequencer
  signal ring_count : std_logic_vector(5 downto 0) := "000001";
  -- Control word new value on following cycle 
  -- (load_call_reg|load_in_switch|en_word_bus|ALU_select|load_mar|load_LSB_MDR_RAM_data|load_MSB_MDR_RAM_data
  -- |load_MDR_word_bus|RAM_write_en_cntrl|load_ir|load_accu|load_flag|load_reg_temp
  -- |load_reg_b|load_reg_c|load_outport|load_out_switch)        
  signal new_control_word : std_logic_vector(24 downto 0);
  -- New program cycle count value
  signal new_program_cycle_count : unsigned(1 downto 0);
  -- Program cycle count
  signal program_cycle_count     : unsigned(1 downto 0);
  -- Reset Ring count (asyncronous signal)
  signal reset_ring_count : std_logic;

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
      if IR /= HLT then

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
      load_call_reg         <= '0';
      load_in_switch        <= '0';
      en_word_bus           <= (others => '0');
      ALU_select            <= (others => '0');
      load_pc               <= '0';
      count_update          <= '0';
      load_mar              <= '0';
      load_LSB_MDR_RAM_data <= '0';
      load_MSB_MDR_RAM_data <= '0';
      load_MDR_word_bus     <= '0';
      RAM_write_en_cntrl    <= '0';
      load_ir               <= '0';
      load_accu             <= '0';
      load_flag             <= '0';
      load_reg_temp         <= '0';
      load_reg_b            <= '0';
      load_reg_c            <= '0';
      load_outport          <= '0';
      load_out_switch       <= '0';
    elsif falling_edge(sys_clk) then
      -- Update program cycle count
      program_cycle_count <= new_program_cycle_count;
      -- Update control signals
      load_call_reg         <= new_control_word(24);
      load_in_switch        <= new_control_word(23);
      en_word_bus           <= new_control_word(22 downto 19);
      ALU_select            <= new_control_word(18 downto 15);
      load_pc               <= new_control_word(14);
      count_update          <= new_control_word(13);
      load_mar              <= new_control_word(12);
      load_LSB_MDR_RAM_data <= new_control_word(11);
      load_MSB_MDR_RAM_data <= new_control_word(10);
      load_MDR_word_bus     <= new_control_word(9);
      RAM_write_en_cntrl    <= new_control_word(8);
      load_ir               <= new_control_word(7);
      load_accu             <= new_control_word(6);
      load_flag             <= new_control_word(5);
      load_reg_temp         <= new_control_word(4);
      load_reg_b            <= new_control_word(3);
      load_reg_c            <= new_control_word(2);
      load_outport          <= new_control_word(1);
      load_out_switch       <= new_control_word(0);
    end if;
  end process;

  -- Control process - Combinational logic
  control_comb_process: process(all)
  begin

    -- Set No operation on microcontroller by default
    new_control_word <= (others => '0');
    -- If no updated, program cycle count keeps it value
    new_program_cycle_count <= program_cycle_count;
    -- RING counter is not reset by default
    reset_ring_count <= '0';

    -- Fetch and T1 --> Set MAR with Program Count value
    if    ring_count(0) = '1' then
      -- load_mar
      new_control_word(12) <= '1';
      -- en_word_bus
      new_control_word(22 downto 19) <= EN_PC;
    -- Fetch and T2 --> Update Program Count + Load data on RAM to MDR 
    elsif ring_count(1) = '1' then
      -- Program counter Count uP
      new_control_word(13) <= '1';
      -- program count 0 : opcode
      -- program count 1 : LSB byte
      -- program count 2 : MSB byte
      if program_cycle_count = "10" then
        -- load_MSB_MDR_RAM_data
        new_control_word(10) <= '1';
      else
        -- load_LSB_MDR_RAM_data
        new_control_word(11) <= '1';
      end if;

    -- Fetch and T3 --> Update Instruction register
    elsif program_cycle_count = "00" and ring_count(2) = '1' then
      -- load_ir
      new_control_word(7) <= '1';
      -- en_word_bus
      new_control_word(22 downto 19) <= EN_MDR;
    else

      case(IR) is
        -- ADD ALU operations A = A + (B/C) 
        when ADD_B | ADD_C =>
          
          -- T4 load other B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load B reg (according Intel datasheet, three LSB means the register)
            if IR(2 downto 0) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= ADD_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;

        -- ADD ALU operations A = A - (B/C)
        when SUB_B | SUB_C =>

          -- T4 load other B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load B reg (according Intel datasheet, three LSB means the register)
            if IR(2 downto 0) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= SUB_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;
        
        -- ADD ALU operations A = A AND (B/C)
        when ANA_B | ANA_C =>

          -- T4 load other B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load B reg (according Intel datasheet, three LSB means the register)
            if IR(2 downto 0) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= AND_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;
        
        -- ADD ALU operations A = A OR (B/C)
        when ORA_B | ORA_C =>

          -- T4 load other B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load B reg (according Intel datasheet, three LSB means the register)
            if IR(2 downto 0) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= OR_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;
        
        -- ADD ALU operations A = A XOR (B/C)
        when XRA_B | XRA_C =>

          -- T4 load other B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load B reg (according Intel datasheet, three LSB means the register)
            if IR(2 downto 0) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= XOR_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;

        -- Left Shift rotation accumulator
        when RAL =>
          -- en_word_bus
          new_control_word(22 downto 19) <= EN_ALU;
          -- ALU_select
          new_control_word(18 downto 15) <= RAL_OP;
          -- load_accu
          new_control_word(6) <= '1';
          -- End instruction
          reset_ring_count    <= '1';

        -- Rigth Shift rotation accumulator
        when RAR =>
          -- en_word_bus
          new_control_word(22 downto 19) <= EN_ALU;
          -- ALU_select
          new_control_word(18 downto 15) <= RAR_OP;
          -- load_accu
          new_control_word(6) <= '1';
          -- End instruction
          reset_ring_count    <= '1';

        -- Not operation on accumulator
        when CMA =>
          -- en_word_bus
          new_control_word(22 downto 19) <= EN_ALU;
          -- ALU_select
          new_control_word(18 downto 15) <= NOT_OP;
          -- load_accu
          new_control_word(6) <= '1';
          -- End instruction
          reset_ring_count    <= '1';

        -- Increment corresponding register
        when INR_A | INR_B | INR_C =>

          -- T4 load other A/B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load A reg (according Intel datasheet, three LSB means the register)
            if    IR(5 downto 3) = "111" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_A;
            -- Load B reg
            elsif IR(5 downto 3) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= INR_OP;
            -- Load A reg (according Intel datasheet, three LSB means the register)
            if    IR(5 downto 3) = "111" then
              -- load_accu 
              new_control_word(6) <= '1';
            -- Load B reg
            elsif IR(5 downto 3) = "000" then
              -- load_reg_b
              new_control_word(3) <= '1';
            -- Load C reg
            else
              -- load_reg_c
              new_control_word(2) <= '1';
            end if;
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;

        -- Decrement corresponding register
        when DCR_A | DCR_B | DCR_C =>

          -- T4 load other A/B/C register to Temporal register
          if ring_count(3) = '1' then
            -- load_reg_temp
            new_control_word(4) <= '1';
            -- Load A reg (according Intel datasheet, three LSB means the register)
            if    IR(5 downto 3) = "111" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_A;
            -- Load B reg
            elsif IR(5 downto 3) = "000" then
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_B;
            -- Load C reg
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_C;
            end if;
          -- T5 ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= DCR_OP;
            -- Load A reg (according Intel datasheet, three LSB means the register)
            if    IR(5 downto 3) = "111" then
              -- load_accu 
              new_control_word(6) <= '1';
            -- Load B reg
            elsif IR(5 downto 3) = "000" then
              -- load_reg_b
              new_control_word(3) <= '1';
            -- Load C reg
            else
              -- load_reg_c
              new_control_word(2) <= '1';
            end if;
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
          end if;

        -- Accumulator AND Byte
        when ANI =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if ring_count(3) = '1' and program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: MDR to Temporal register
          elsif ring_count(2) = '1' and program_cycle_count = "001" then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_reg_temp
            new_control_word(4) <= '1';
          -- T4, program cycle 1: ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= AND_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Accumulator OR Byte
        when ORI =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if ring_count(3) = '1' and program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: MDR to Temporal register
          elsif ring_count(2) = '1' and program_cycle_count = "001" then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_reg_temp
            new_control_word(4) <= '1';
          -- T4, program cycle 1: ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= OR_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Accumulator XOR Byte
        when XRI =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if ring_count(3) = '1' and program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: MDR to Temporal register
          elsif ring_count(2) = '1' and program_cycle_count = "001" then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_reg_temp
            new_control_word(4) <= '1';
          -- T4, program cycle 1: ALU operation
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_ALU;
            -- ALU_select
            new_control_word(18 downto 15) <= XOR_OP;
            -- load_accu
            new_control_word(6) <= '1';
            -- load_flag
            new_control_word(5) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Mov register data to other register
        when MOV_AB | MOV_AC | MOV_BA | MOV_BC | MOV_CA | MOV_CB =>

          -- Register to be updated
          -- A Reg
          if    IR(5 downto 3) = "111" then
            -- load_accu
            new_control_word(6) <= '1';
          -- B reg
          elsif IR(5 downto 3) = "000" then
            -- load_reg_b
            new_control_word(3) <= '1';
          -- C reg
          else
            -- load_reg_c
            new_control_word(2) <= '1';
          end if;

          -- Register to be copied
          -- A Reg
          if    IR(2 downto 0) = "111" then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_A;
          -- B reg
          elsif IR(2 downto 0) = "000" then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_B;
          -- C reg
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_C;
          end if;

          -- End instruction
          reset_ring_count    <= '1';

        -- Move byte to register
        when MVI_A | MVI_B | MVI_C =>
        
          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if ring_count(3) = '1' and program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: MDR to corresponding register
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- Register to be updated
            -- A Reg
            if    IR(5 downto 3) = "111" then
              -- load_accu
              new_control_word(6) <= '1';
            -- B reg
            elsif IR(5 downto 3) = "000" then
              -- load_reg_b
              new_control_word(3) <= '1';
            -- C reg
            else
              -- load_reg_c
              new_control_word(2) <= '1';
            end if;
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Load data on memory address to accumulator
        when LDA =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          -- T3, program cycle 1: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" or program_cycle_count = "001" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 2: MDR to MAR
          elsif ring_count(2) = '1' then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_mar
            new_control_word(12) <= '1';
          -- T4, program cycle 2: Memory to MDR
          elsif ring_count(3) = '1' then
            -- load_LSB_MDR_RAM_data
            new_control_word(11) <= '1';
          -- T5, program cycle 2: Load data on MDR to Accumulator
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_accu
            new_control_word(6) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Load data on accumulator to memory address
        when SDA =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          -- T3, program cycle 1: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" or program_cycle_count = "001" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 2: MDR to MAR
          elsif ring_count(2) = '1' then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_mar
            new_control_word(12) <= '1';
          -- T4, program cycle 2: Accumulator to MDR
          elsif ring_count(3) = '1' then
            -- load_MDR_word_bus
            new_control_word(9) <= '1';
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_A;
          -- T5, progra cycle 2: write data on MDR to RAM
          else
            -- RAM_write_en_cntrl
            new_control_word(8) <= '1';
            -- End instruction
            reset_ring_count    <= '1';
            new_program_cycle_count      <= (others => '0');
          end if;

        -- Store data on input to accumulator
        when IN_BYTE =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: Load input switch multiplexer  
          elsif program_cycle_count = "001" and ring_count(2) = '1' then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_in_switch
            new_control_word(23) <= '1';
          -- T4, program cycle 1: Move Input to accumulator 
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= IN_PORT;
            -- load_accu
            new_control_word(6) <= '1';
            -- End instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;

        -- Send data on accumulator to output port
        when OUT_BYTE =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" then
            
            new_program_cycle_count <= program_cycle_count + 1;
            reset_ring_count    <= '1';
          -- T3, program cycle 1: Load output switch multiplexer  
          elsif program_cycle_count = "001" and ring_count(2) = '1' then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_out_switch
            new_control_word(0) <= '1';
          -- T4, program cycle 1: Move Input to accumulator 
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_A;
            -- load_outport
            new_control_word(1) <= '1';
            -- End instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;

        -- HLT 
        when HLT =>
          -- T4, program cycle 0: NOP
          null;

        -- JMP
        when JMP =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          -- T3, program cycle 1: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" or program_cycle_count = "001" then
            
            new_program_cycle_count     <= program_cycle_count + 1;
            reset_ring_count   <= '1';
          -- T3, program cycle 2: load MDR to PR
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_pc
            new_control_word(14) <= '1';
            -- End instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;

        -- JZ
        when JZ =>

          -- Jump if Zero
          if zero_flag then
            -- T4, program cycle 0: NOP - extract data from memory on following program cycle
            -- T3, program cycle 1: NOP - extract data from memory on following program cycle
            if program_cycle_count = "000" or program_cycle_count = "001" then
            
              new_program_cycle_count     <= program_cycle_count + 1;
              reset_ring_count   <= '1';
            -- T3, program cycle 2: load MDR to PR
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_MDR;
              -- load_pc
              new_control_word(14) <= '1';
              -- End instruction
              new_program_cycle_count <= (others => '0');
              reset_ring_count    <= '1';
            end if;
          -- No Jump
          else
            -- T4, T5: Update PC count to jump address
            if ring_count(3) = '1' or ring_count(4) = '1' then
              -- count_update
              new_control_word(13) <= '1';
            -- T6: NOP - reset RING counter
            else
              -- END instruction
              reset_ring_count    <= '1';
            end if;
          end if;

        -- JNZ
        when JNZ =>

          -- Jump if not Zero
          if not zero_flag then
            -- T4, program cycle 0: NOP - extract data from memory on following program cycle
            -- T3, program cycle 1: NOP - extract data from memory on following program cycle
            if program_cycle_count = "000" or program_cycle_count = "001" then
            
              new_program_cycle_count     <= program_cycle_count + 1;
              reset_ring_count   <= '1';
            -- T3, program cycle 2: load MDR to PR
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_MDR;
              -- load_pc
              new_control_word(14) <= '1';
              -- End instruction
              new_program_cycle_count <= (others => '0');
              reset_ring_count    <= '1';
            end if;
          -- No Jump
          else
            -- T4, T5: Update PC count to jump address
            if ring_count(3) = '1' or ring_count(4) = '1' then
              -- count_update
              new_control_word(13) <= '1';
            -- T6: NOP - reset RING counter
            else
              -- END instruction
              reset_ring_count    <= '1';
            end if;
          end if;

        -- JM
        when JM =>

          -- Jump if not Zero
          if neg_flag then
            -- T4, program cycle 0: NOP - extract data from memory on following program cycle
            -- T3, program cycle 1: NOP - extract data from memory on following program cycle
            if program_cycle_count = "000" or program_cycle_count = "001" then
            
              new_program_cycle_count     <= program_cycle_count + 1;
              reset_ring_count   <= '1';
            -- T3, program cycle 2: load MDR to PR
            else
              -- en_word_bus
              new_control_word(22 downto 19) <= EN_MDR;
              -- load_pc
              new_control_word(14) <= '1';
              -- End instruction
              new_program_cycle_count <= (others => '0');
              reset_ring_count    <= '1';
            end if;
          -- No Jump
          else
            -- T4, T5: Update PC count to jump address
            if ring_count(3) = '1' or ring_count(4) = '1' then
              -- count_update
              new_control_word(13) <= '1';
            -- T6: NOP - reset RING counter
            else
              -- END instruction
              reset_ring_count    <= '1';
            end if;
          end if;

        -- Call subprogram
        when CALL =>

          -- T4, program cycle 0: NOP - extract data from memory on following program cycle
          -- T3, program cycle 1: NOP - extract data from memory on following program cycle
          if program_cycle_count = "000" or program_cycle_count = "001" then
            
            new_program_cycle_count     <= program_cycle_count + 1;
            reset_ring_count   <= '1';
          -- T3, program cycle 2: load PC into CALL register
          elsif ring_count(2) = '1' then
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_PC;
            -- load_call_reg
            new_control_word(24) <= '1';
          -- T4, program cycle 2: Load PC with data on MDR
          else
            -- en_word_bus
            new_control_word(22 downto 19) <= EN_MDR;
            -- load_pc
            new_control_word(14) <= '1';
            -- End instruction
            new_program_cycle_count <= (others => '0');
            reset_ring_count    <= '1';
          end if;

        -- Return to normal program
        when RET =>
          -- T4, program cycle 2: load PC into CALL register
          -- en_word_bus
          new_control_word(22 downto 19) <= EN_CALL;
          -- load_pc
          new_control_word(14) <= '1';
          -- End instruction
          new_program_cycle_count <= (others => '0');
          reset_ring_count    <= '1';

        -- No operation
        when others =>
          -- T4, program cycle 0: NOP - reset Ring count
          reset_ring_count    <= '1';

      end case;
    end if;
  end process;

end rtl;