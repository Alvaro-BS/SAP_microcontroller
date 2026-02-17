-- Simple as possible Microprocessor - Part 3
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;
use work.sap_3_package.all;

----------------------------------------------------------------
-- Doubs and assumptions from original SAP-3 Microcontroller:
--
-- * On this microcontroller version, it has been added the temporal internal register pair
--   W and Z as appears on Intel 8080/8085 datasheets. This register pair remove the need ofç
--   increment MDR register of previous SAP-2 version to 16 bits.
--
-- * Also as Intel 8080/8085 datasheeets shows, 16 bits address and 8-bit data are separated on
--   both different buses to ensure partitioning of diferent parts of microcontroller.
--
-- * New data bus interconnects the following elements:
--     - Accumulator register (in/out bus link)
--     - Temporal register connected to ALU input (in/out bus link)
--     - Input ports (in bus link)
--     - Output ports (out bus link)
--     - Instruction register (out bus link)
--     - Flag register (in bus link)
--     - ALU output port result (in bus link)
--     - Memory data register (MDR) (in/out bus link)
--     - Register Space (8-bit registers: B/C/D/E/H/L/W/Z/SP[MSB]/SP[LSB]/PC[MSB]/PC[LSB]) (in/out bus link)
--
-- * New address bus interconnects the following elements:
--     - Memory address register (MAR) (out bus link)
--     - Register Space (16-bit registers and pairs: W-Z/B-C/D-E/H-L/SP/PC) (in bus link)
--
-- * Register Space includes input and output select logic to manage data transference on 8-bit registers.
--
-- * Register Space includes output select logic to manage transference of 16-bit registers to Address bus.
--
-- * Register Space includes an increment/decrement logic for all 16-bit/reg-pairs.
--
--  * SAP-3 microprocessor also includes a program_flag input. When program_flag is set,
--    RAM is accesible to be written from signals Program_RAM_in_addr (address to be written),
--    Program_RAM_write_en (Write enable signal) and Program_RAM_write_data (Data to be stored on RAM
--    on Program_RAM_in_addr when Program_RAM_write_en is set). After set this flag and ends programation,
--    Microprocessor must be reset to ensure correct operation of program.
--
--  * Digital Computer Electronics mention about SAP-2 Memory includes a reserved
--    2K ROM space from address 0x0000 to 0x07FF for power-up initialization and the
--    rest corrresponds to 62K RAM from 0x0800 to 0xFFFF addresses. Due to this initialization
--    is really dependent of type of micro and their use, it has been decided as simplification,
--    use all 64K memory as RAM.
--
-- * There are some incongruencies with the ALU computation, flag calculation and register connections.
--   on Figure 11-2 it is indicated that ALU only receive as input the accumulator and the temporal register
--   then on figure 11-8 it is indicated that flags are set from accumulator. However, on table 11-2
--   it is indicated that flags are also computed for DCR operations (including DCR B and DCR C).
--   The main problem is that if flags are only computed from Accumulator and not output of ALU, that means
--   that programs like the one of example 11-8 of multiply 12 to 8 is not possible (due to accumulator is used as
--   intermediate register storing result, can't be updated with result of DCR C for example). For that reason
--   flags are computed on this code directly from ALU. This seems the correct behaviour checking Intel 8080/8085
--   datasheets.
--
-- * Another problem relates with the differences on cycles for microintructions on Table 11-3 to presented HW.
--   Assuming we spent 3 T states for fetch cycle (1º PC to MAR, 2º PC count up and load data on MDR, 3º MDR to IR).
--   It is expected that operations like DCR A spends 4 cycles as Accumulator is directly connected to ALU. However,
--   operations like ADD B or DCR C needs cannot reach that limit as an extra bit is needed to load data of B or C register
--   to temporal one.   
----------------------------------------------------------------

entity sap_3_microprocessor is
port(
  -- General ports
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  -- I/O Ports
  Input_port1 : in std_logic_vector(7 downto 0);
  Input_port2 : in std_logic_vector(7 downto 0);
  Output_port3 : out std_logic_vector(7 downto 0) := (others => '0');
  Output_port4 : out std_logic_vector(7 downto 0) := (others => '0');
  -- Program Interface
  program_flag : in std_logic;
  program_RAM_in_addr  : in std_logic_vector(15 downto 0);
  program_RAM_write_en : in std_logic;
  program_RAM_write_data : in std_logic_vector(7 downto 0)
);
end sap_3_microprocessor;

architecture rtl of sap_3_microprocessor is

  -- Control word output from sequencer to all requested submodules
  signal control_word : control_word_t;
  -- General 8-bit data bus
  signal data_bus     : std_logic_vector(7 downto 0);
  -- Register Space 8-bit data output
  signal reg_space_out : std_logic_vector(7 downto 0);
  -- General 16-bit address bus
  signal address_bus  : std_logic_vector(15 downto 0);
  -- ALU Output
  signal alu_out      : std_logic_vector(7 downto 0);
  -- ALU Flags
  signal alu_flags    : alu_flags_t;
  -- Instruction Register
  signal IR_reg   : std_logic_vector(7 downto 0);
  -- Accumulator Register
  signal Accu_reg : std_logic_vector(7 downto 0);
  -- Temporal input ALU Register
  signal TEMP_reg : std_logic_vector(7 downto 0);
  -- Memory Address Register
  signal MAR_reg  : std_logic_vector(15 downto 0) := (others => '0'); 
  -- Memory Data Register
  signal MDR_reg  : std_logic_vector(7 downto 0) := (others => '0');
  -- RAM read data
  signal RAM_read_data  : std_logic_vector(7 downto 0);
  -- RAM write enable
  signal RAM_write_en   : std_logic;
  -- RAM address
  signal RAM_addr       : std_logic_vector(15 downto 0);
  -- RAM write data
  signal RAM_write_data : std_logic_vector(7 downto 0);
  -- Input port switched
  signal Input_port    : std_logic_vector(7 downto 0);
  -- Input switch register
  signal in_switch_reg : std_logic_vector(7 downto 0);
  -- Input switch register
  signal out_switch_reg : std_logic_vector(7 downto 0);

begin

  -- Data Inter-Bus output selector 
  data_bus_control : process(all)
  begin
    case (control_word.data_bus_sel) is
      -- Drive Input ports to data bus
      when IN_PORT      => data_bus <= Input_port;
      -- Drive register space output to data bus (B/C/D/E/H/L/SP[MSB]/SP[LSB]/PC[MSB]/PC[LSB]) 
      when EN_REG_SPACE => data_bus <= reg_space_out;
      -- Drive ALU output to data bus
      when EN_ALU       => data_bus <= alu_out;
      -- ALU Flags
      when EN_ALU_FLAGS => data_bus <= (
        0 => alu_flags.carry,
        2 => alu_flags.parity,
        7 => alu_flags.neg,
        6 => alu_flags.zero,
        others => '0');
      -- Drive Memory Data Read to data bus
      when EN_MDR       => data_bus <= MDR_reg;
      -- Drive Register A to data bus
      when EN_A         => data_bus <= Accu_reg;
      -- Drive Register Temporally to data bus
      when EN_TMP       => data_bus <= TEMP_reg;
      -- Drive ALU Output to word bus
      when others       => data_bus <= x"00";
    end case;
  end process;

  -- Register Space Control Submodule
  -- B (0b000), C (0b001) |
  -- D (0b010), E (0b011) | -> General registers (and register pairs)
  -- H (0b100), L (0b101) |
  -- PC: Program Counter
  -- SP: Stack Pointer
  register_space_submodule: entity work.sap_3_reg_space(rtl)
  port map(
    sys_clk => sys_clk,
    sys_rst => sys_rst or program_flag,
    -- Control signals
    reg_8bit_sel_in   => control_word.reg_8bit_sel_in,
    reg_8bit_sel_out  => control_word.reg_8bit_sel_out,
    reg_8bit_load     => control_word.reg_8bit_load,
    reg_16bit_sel_out => control_word.reg_16bit_sel_out,
    reg_16bit_dcx     => control_word.reg_16bit_dcx,
    reg_16bit_inx     => control_word.reg_16bit_inx,
    reg_16bit_sel_up  => control_word.reg_16bit_sel_up,
    -- Data flow
    reg_8bit_in       => data_bus,
    reg_8bit_out      => reg_space_out,
    reg_16bit_out     => address_bus 
  );

  -- Arithmetich Logic Unit Submodule
  ALU_submodule: entity work.sap_3_alu(rtl)
  port map(
    sys_clk  => sys_clk,
    sys_rst  => sys_rst or program_flag,
    data_bus => data_bus,
    -- ALU inputs
    port_a  => Accu_reg,
    port_b  => TEMP_reg,
    -- Control signals
    alu_op_select  => control_word.alu_op_select,
    load_alu_flags => control_word.load_alu_flags,
    -- ALU outputs
    alu_out   => alu_out, 
    alu_flags => alu_flags 
  );

  -- Memory registers
  MEM_regs: process(sys_clk, sys_rst, program_flag)
  begin
    if sys_rst or program_flag then
      MDR_reg <= (others => '0'); 
      MAR_reg <= (others => '0');
    elsif rising_edge(sys_clk) then
      -- Memory Address register
      if control_word.load_mar then
        MAR_reg <= address_bus;
      end if;
      -- Memory data register
      if control_word.load_mdr then
        if control_word.mdr_select_RAM_out then
          MDR_reg <= RAM_read_data;
        else
          MDR_reg <= data_bus;
        end if;
      end if;
    end if;
  end process;

  -- Drive RAM signals depending if program flag is set
  RAM_write_en   <= program_RAM_write_en when program_flag else control_word.RAM_write_en; 
  RAM_addr       <= program_RAM_in_addr  when program_flag else MAR_reg;
  RAM_write_data <= program_RAM_write_data when program_flag else MDR_reg;

  -- RAM 8x2**16 = 64 Kb 
  RAM_64K: entity work.ram(rtl)
  generic map(
    DATA_WIDTH => 8,
    RAM_DEPTH  => 2**16  
  )
  port map(  
    sys_clk    => sys_clk,
    write_en   => RAM_write_en,
    addr       => RAM_addr,
    write_data => RAM_write_data,
    read_data  => RAM_read_data
  );

  -- SAP-3 special registers
  sap_3_registers: process(sys_clk, sys_rst, program_flag)
  begin
    if sys_rst or program_flag then
      IR_reg <= (others => '0');
      Accu_reg <= (others => '0');
      TEMP_reg <= (others => '0');
    elsif rising_edge(sys_clk) then
      -- Instruction register
      if control_word.load_ir then
        IR_reg <= data_bus;
      end if;
      -- Accumulator register
      if control_word.load_accu then
        Accu_reg <= data_bus;
      end if;
      -- Temporal register
      if control_word.load_temp then
        TEMP_reg <= data_bus;
      end if;
    end if;
  end process;

  -- I/O switch process
  IO_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      Output_port3   <= (others => '0');
      Output_port4   <= (others => '0');
      out_switch_reg <= (others => '0');
      in_switch_reg  <= (others => '0');
    elsif rising_edge(sys_clk) then

      -- Load Input switch
      if control_word.load_in_switch then
        in_switch_reg <= data_bus;
      end if;

      -- Update Output switch register
      if control_word.load_out_switch then
        out_switch_reg <= data_bus;
      end if;

      -- Load output port depending on switch register
      if control_word.load_outport then
        if out_switch_reg = x"03" then
          Output_port3 <= data_bus;
        else
          Output_port4 <= data_bus;
        end if;
      end if;
    end if;
  end process;

  -- Switch Input ports
  Input_port <= Input_port1 when in_switch_reg = x"01" else Input_port2;

  -- Controller Sequencer for SAP-3
  controller_seq: entity work.controller_seq_sap_3(rtl)
  port map(  
    sys_clk      => sys_clk,
    sys_rst      => sys_rst,
    -- Controller interface
    IR_reg       => IR_reg,
    alu_flags    => alu_flags,
    control_word => control_word
  );

end rtl;