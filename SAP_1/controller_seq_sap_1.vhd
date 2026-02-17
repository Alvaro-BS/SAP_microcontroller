-- Simple as possible Microprocessor - Part 1
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity controller_seq_sap_1 is
port(  
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  
  IR        : in std_logic_vector(3 downto 0);
  
  Enable_counter_out : out std_logic := '0';
  Enable_RAM_out_n : out std_logic := '0';
  Enable_Instruction_out_n : out std_logic := '0';
  Enable_Accumulator_out : out std_logic := '0';
  Enable_ALU_out : out std_logic := '0';
  count_update   : out std_logic := '0';
  neg_op         : out std_logic := '0';
  load_ir_n      : out std_logic := '0';
  load_accu_n    : out std_logic := '0';
  load_b_reg_n   : out std_logic := '0';
  load_out_reg_n : out std_logic := '0';
  load_mar_n     : out std_logic := '0'
);
end controller_seq_sap_1;

architecture rtl of controller_seq_sap_1 is

  -- Ring count sequencer
  signal ring_count : std_logic_vector(5 downto 0) := "000001";

  type address_rom_type_t is array (2**4-1 downto 0) of std_logic_vector(3 downto 0);
  -- Address ROM, depending on Intruction, an address of control ROM is selected Table 10-7
  signal address_ROM : address_rom_type_t := (16#0# => "0011", -- LDA
                                              16#1# => "0110", -- ADD
                                              16#2# => "1001", -- SUB
                                              16#E# => "1100", -- OUT
                                              others => "0000"); -- Not used

  type control_rom_type_t is array (2**4-1 downto 0) of std_logic_vector(11 downto 0);
  -- Control ROM, depending on Intruction and sequence, a control data is selected Table 10-6
  signal control_ROM : control_rom_type_t := (16#0# => x"5E3", -- Fetch T1
                                              16#1# => x"BE3", -- Fetch T2
                                              16#2# => x"263", -- Fetch T3
                                              16#3# => x"1A3", -- LDA T4
                                              16#4# => x"2C3", -- LDA T5
                                              16#5# => x"3E3", -- LDA T6
                                              16#6# => x"1A3", -- ADD T4
                                              16#7# => x"2E1", -- ADD T5
                                              16#8# => x"3C7", -- ADD T6
                                              16#9# => x"1A3", -- SUB T4
                                              16#A# => x"2E1", -- SUB T5
                                              16#B# => x"3CF", -- SUB T6
                                              16#C# => x"3F2", -- OUT T4
                                              16#D# => x"3E3", -- OUT T5
                                              16#E# => x"3E3", -- OUT T6
                                              others => x"3E3"); -- Not used

  signal pre_count : unsigned(3 downto 0) := (others => '0');

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
      if (ring_count(ring_count'LENGTH-1) = '1')
        OR (control_ROM(to_integer(unsigned(pre_count))) = x"3E3" AND ring_count(4 downto 3) /= "000") then
        ring_count <= (0 => '1', others => '0');
      -- If HLT operation, keeps blocked, otherwise count continue 
      elsif ring_count(2) /= '1' or IR /= "1111" then 
        ring_count <= ring_count sll 1;
      end if;      
    end if;
  end process;

  -- Pressetable Counter register
  presettable_counter_proc: process(sys_clk, sys_rst)
  begin
    -- Reset when System reset or in T1 sequence
    if sys_rst then
      pre_count <= (others => '0');
    elsif falling_edge(sys_clk) then
      -- If Instruction register is set to Hlt, set control bits to noOP  
      if IR = "1111" then
        pre_count <=  (others => '1');
      -- Reset count when program cycle ends
      elsif ring_count(ring_count'LENGTH-1) = '1'
        OR (control_ROM(to_integer(unsigned(pre_count))) = x"3E3" AND ring_count(4 downto 3) /= "000") then
        pre_count <=  (others => '0');
      -- Load a new data when T3 is set
      elsif ring_count(2) = '1' then
        pre_count <= unsigned(address_ROM(to_integer(unsigned(IR))));
      else
        pre_count <= pre_count + 1; 
      end if;      
    end if;
  end process;

  -- Output control bits
  (count_update, Enable_counter_out, load_mar_n, Enable_RAM_out_n, load_ir_n, Enable_Instruction_out_n,
  load_accu_n, Enable_Accumulator_out, neg_op, Enable_ALU_out, load_b_reg_n, load_out_reg_n) <= control_ROM(to_integer(unsigned(pre_count)));

end rtl;