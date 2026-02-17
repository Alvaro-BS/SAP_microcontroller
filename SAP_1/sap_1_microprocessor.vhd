-- Simple as possible Microprocessor - Part 1
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;

entity sap_1_microprocessor is
port(  
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  
  program_flag : in std_logic;
  RAM_in_addr  : in std_logic_vector(7 downto 0);
  RAM_write_en : in std_logic;
  RAM_write_data : in std_logic_vector(7 downto 0);
  OUT_reg        : out std_logic_vector(7 downto 0)
);
end sap_1_microprocessor;

architecture rtl of sap_1_microprocessor is

  -- Word Inter-bus
  signal word_bus      : std_logic_vector(7 downto 0);

  -- Internal registers
  signal program_count : unsigned(7 downto 0) := (others => '0');
  signal MAR_reg  : std_logic_vector(7 downto 0) := (others => '0');
  signal MAR_out  : std_logic_vector(7 downto 0) := (others => '0');
  signal Accu_reg : unsigned(7 downto 0) := (others => '0');
  signal B_reg    : unsigned(7 downto 0) := (others => '0');
  signal alu_out  : unsigned(7 downto 0) := (others => '0');
  signal IR_reg   : std_logic_vector(7 downto 0) := (others => '0');
  signal RAM_read_data : std_logic_vector(7 downto 0);

  -- Control signals
  signal Enable_counter_out : std_logic;
  signal Enable_RAM_out_n : std_logic;
  signal Enable_Instruction_out_n : std_logic;
  signal Enable_Accumulator_out : std_logic;
  signal Enable_ALU_out : std_logic;
  signal count_update   : std_logic;
  signal neg_op         : std_logic;
  signal load_ir_n      : std_logic;
  signal load_accu_n    : std_logic;
  signal load_b_reg_n   : std_logic;
  signal load_out_reg_n : std_logic;
  signal load_mar_n     : std_logic;

  -- RAM component
  component ram is
  generic(
    DATA_WIDTH : integer := 8;
    RAM_DEPTH  : integer := 2**4  
  );
  port(  
    sys_clk   : in std_logic;
    write_en   : in std_logic;
    addr       : in std_logic_vector(integer(ceil(log2(real(RAM_DEPTH))))-1 downto 0);
    write_data : in std_logic_vector(DATA_WIDTH-1 downto 0);
    read_data  : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
  end component ram;

  -- Controller Sequence SAP-1 component
  component controller_seq_sap_1 is
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
  end component controller_seq_sap_1;

begin

  -- Word Inter-Bus output selector 
  word_bus_control : process(all)
  begin
    -- Output of program count is drived to word bus
    if Enable_counter_out then
      word_bus <= std_logic_vector(program_count);
    -- Output of RAM is drived to word bus
    elsif not Enable_RAM_out_n then
      word_bus <= RAM_read_data;
    -- Output of Instruction register is drived to word bus
    elsif not Enable_Instruction_out_n then
      word_bus <= IR_reg;
    -- Output of Accumulator register is drived to word bus
    elsif Enable_Accumulator_out then
      word_bus <= std_logic_vector(Accu_reg);
    -- Output of ALU is drived to word bus
    elsif Enable_ALU_out then
      word_bus <= std_logic_vector(alu_out);
    -- Not used
    else
      word_bus <= (others => '0');
    end if;
  end process;
  
  -- Program Counter
  program_counter: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      program_count <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if count_update then
        program_count <= program_count + 1;
      end if;
    end if;
  end process;

  -- Memory Address Register
  MAR_proccess: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      MAR_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if not load_mar_n then
        MAR_reg <= word_bus;
      end if;
    end if;
  end process;

  -- MAR output, if Program flag is set address selected on RAM comes from input 
  MAR_out <= RAM_in_addr when program_flag else MAR_reg;

  -- RAM 16x8
  RAM_16x8: ram
  generic map(
    DATA_WIDTH => 8,
    RAM_DEPTH  => 2**4  
  )
  port map(  
    sys_clk    => sys_clk,
    write_en   => RAM_write_en,
    addr       => MAR_out(3 downto 0),
    write_data => RAM_write_data,
    read_data  => RAM_read_data
  );

  -- Instruction Register
  IR_proccess: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      IR_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if not load_ir_n then
        IR_reg <= word_bus;
      end if;
    end if;
  end process;

  -- Controller Sequence for SAP-1 microprocessor
  controller_seq: controller_seq_sap_1
  port map(  
    sys_clk   => sys_clk,
    sys_rst   => sys_rst,
    IR        => IR_reg(7 downto 4),

    Enable_counter_out => Enable_counter_out,
    Enable_RAM_out_n => Enable_RAM_out_n,
    Enable_Instruction_out_n => Enable_Instruction_out_n,
    Enable_Accumulator_out => Enable_Accumulator_out,
    Enable_ALU_out => Enable_ALU_out,
    count_update   => count_update,
    neg_op         => neg_op,
    load_ir_n      => load_ir_n,
    load_accu_n    => load_accu_n,
    load_b_reg_n   => load_b_reg_n,
    load_out_reg_n => load_out_reg_n,
    load_mar_n     => load_mar_n
  );

  -- Accumulator
  accumulator_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      Accu_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if not load_accu_n then
        Accu_reg <= unsigned(word_bus);
      end if;
    end if;
  end process;

  -- ALU Process
  ALU_process: process(all)
  begin
    if not neg_op then
      alu_out <= Accu_reg + B_reg; 
    else
      alu_out <= Accu_reg - B_reg;
    end if;
  end process;

  -- Register B
  reg_b_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      B_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if not load_b_reg_n then
        B_reg <= unsigned(word_bus);
      end if;
    end if;
  end process;

  -- Output Register
  reg_out_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      OUT_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if not load_out_reg_n then
        OUT_reg <= word_bus;
      end if;
    end if;
  end process;

end rtl;