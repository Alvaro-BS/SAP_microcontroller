-- Simple as possible Microprocessor - Part 2
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;
use work.sap_2_package.all;

----------------------------------------------------------------
-- Doubs and assumptions from original SAP-2 Microcontroller:
--
-- * Data link between MDR and word bus is expressed as 8 bits,
--   however input to PC and MAR is about 16-bits. On Jumps
--   and Memory access microintructions, SAP-2 needs some way
--   to restructure data comming from memory referencing addreses
--   ej:
--
--           address | Data        |
--           ------- | ----------- |
--           0x2000  | JMP op code |
--           0x2001  | 0x76        |
--           0x2002  | 0x40        |
--
--    Jump address is 0x4076, but outputs of MDR would be 0x76 and
--    then 0x40. Microprocessor needs some way to joint this two bytes
--    in a single 16-bits words to be drived to PC (or MAR if needed)
--    Suggested way is increase MDR output and set two load control bytes
--    One to load data on MSB bytes of MDR register and other for the LSB byte.
--    Data comming from word bus to be load on MDR is always loaded on LSB byte
--    of MDR. On same way, data from MDR to be stored on RAM is the LSB byte
--    of MDR register. On Intel 8080/8085 this is resolved with registers HL, that
--    works as two 8-bit registers or single 16-bit register. 
--
--  * SAP-2 microprocessor includes a program_flag input, when program_flag is set
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
--   flags are computed on this code directly from ALU.
--
-- * Another problem relates with the differences on cycles for microintructions on Table 11-3 to presented HW.
--   Assuming we spent 3 T states for fetch cycle (1º PC to MAR, 2º PC count up and load data on MDR, 3º MDR to IR).
--   It is expected that operations like DCR A spends 4 cycles as Accumulator is directly connected to ALU. However,
--   operations like ADD B or DCR C needs cannot reach that limit as an extra bit is needed to load data of B or C register
--   to temporal one.
--
-- * Auxiliar CALL register is added to be able to store program count on CALL intruction. Really 
--   this register does not exist on Intel 8080/8085, stack register pointer is used to store data on RAM.
--   However, this is implemented an explained in SAP-3.   
----------------------------------------------------------------

entity sap_2_microprocessor is
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
end sap_2_microprocessor;

architecture rtl of sap_2_microprocessor is

  component controller_seq_sap_2 is
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
  end component controller_seq_sap_2;

  component ram is
  generic(
    DATA_WIDTH : integer := 8;
    RAM_DEPTH  : integer := 2**4  
  );
  port(  
    sys_clk    : in std_logic;
    write_en   : in std_logic;
    addr       : in std_logic_vector(integer(ceil(log2(real(RAM_DEPTH))))-1 downto 0);
    write_data : in std_logic_vector(DATA_WIDTH-1 downto 0);
    read_data  : out std_logic_vector(DATA_WIDTH-1 downto 0)
  );
  end component ram;

  -- Word Bus enable set
  signal en_word_bus : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0);
  -- ALU operation selector
  signal ALU_select : std_logic_vector(ALU_SELECT_WIDTH-1 downto 0);
  -- Load Program counter with data on word bus
  signal load_pc : std_logic := '0';
  -- Program Counter update flag
  signal count_update : std_logic := '0';
  -- Load word bus data on MAR register
  signal load_mar : std_logic := '0';
  -- Load LSB LDR register with data on RAM
  signal load_LSB_MDR_RAM_data : std_logic := '0';
  -- Load LSB MDR register with data on RAM
  signal load_MSB_MDR_RAM_data : std_logic := '0';
  -- Load MDR register with WORD bus
  signal load_MDR_word_bus : std_logic := '0';
  -- RAM write enable control
  signal RAM_write_en_cntrl : std_logic := '0';
  -- Load Instruction register
  signal load_ir : std_logic := '0';
  -- Load Accumulator register
  signal load_accu : std_logic := '0';
  -- Load Temporal register
  signal load_reg_temp : std_logic := '0';
  -- Load B register
  signal load_reg_b : std_logic := '0';
  -- Load C register
  signal load_reg_c : std_logic := '0';
  -- Load Output register
  signal load_outport    : std_logic := '0';
  -- Load Output switch reg
  signal load_out_switch : std_logic := '0';
  -- Load input switch reg
  signal load_in_switch : std_logic := '0';
  -- Load ALU flags
  signal load_flag      : std_logic;
  -- Load CALL auxiliary register
  signal load_call_reg  : std_logic;
  -- negative flag, set to 1 if ALU result is 0x00
  signal neg_flag       : std_logic;
  -- Zero flag, set to 1 if ALU result is negative (MSB bit is '1')
  signal zero_flag      : std_logic;

  -- Input port switched
  signal Input_port : std_logic_vector(7 downto 0);
  -- Input switch register
  signal in_switch_reg : std_logic_vector(7 downto 0);
  -- Input switch register
  signal out_switch_reg : std_logic_vector(7 downto 0);
  -- Word Inter-Bus
  signal word_bus : std_logic_vector(15 downto 0);
  -- Program Counter
  signal program_count : unsigned(15 downto 0);
  -- Memory Address Register
  signal MAR_reg : std_logic_vector(15 downto 0);
  -- Memory Address Register output (after multiplex address)
  signal MAR_out : std_logic_vector(15 downto 0);
  -- RAM read data
  signal RAM_read_data : std_logic_vector(7 downto 0);
  -- Memory Data Register
  signal MDR_reg : std_logic_vector(15 downto 0);
  -- Memory Data Read register output
  signal MDR_out : std_logic_vector(7 downto 0);
  -- RAM write enable signal multiplexed with input
  signal RAM_write_en : std_logic;
  -- Instruction register
  signal IR_reg : std_logic_vector(7 downto 0) := (others => '0');
  -- Accumulator Register
  signal Accu_reg : unsigned(7 downto 0) := (others => '0');
  -- ALU output result
  signal alu_out  : unsigned(7 downto 0) := (others => '0');
  -- B register
  signal B_reg : unsigned(7 downto 0) := (others => '0');
  -- C register
  signal C_reg : unsigned(7 downto 0) := (others => '0');
  -- Temporal register
  signal TEMP_reg : unsigned(7 downto 0) := (others => '0');
  -- CALL auxiliar register
  signal call_reg : std_logic_vector(15 downto 0) := (others => '0');

begin

  -- Word Inter-Bus output selector 
  word_bus_control : process(all)
  begin
    case (en_word_bus) is
      when IN_PORT  => word_bus <= x"00" & Input_port;
      -- Drive Program Count to word bus
      when EN_PC    => word_bus <= std_logic_vector(program_count);
      -- Drive Memory Data Read to word bus
      when EN_MDR   => word_bus <= MDR_reg;
      -- Drive Register A to word bus
      when EN_A     => word_bus <= x"00" & std_logic_vector(Accu_reg);
      -- Drive Register B to word bus
      when EN_B     => word_bus <= x"00" & std_logic_vector(B_reg);
      -- Drive Register C to word bus
      when EN_C     => word_bus <= x"00" & std_logic_vector(C_reg);
      -- Drive Register Temporally to word bus
      when EN_TMP   => word_bus <= x"00" & std_logic_vector(TEMP_reg);
      -- Drive ALU output to word bus
      when EN_ALU   => word_bus <= x"00" & std_logic_vector(alu_out);
      -- Drive Auxiliar CALL register to word bus
      when EN_CALL  => word_bus <= call_reg;
      -- Drive ALU Output to word bus
      when others   => word_bus <= x"0000"; 
    end case;
  end process;

  -- Input switch
  in_switch_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      in_switch_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_in_switch then
        in_switch_reg <= word_bus(7 downto 0);
      end if;
    end if;
  end process;

  -- Switch Input ports
  Input_port <= Input_port1 when in_switch_reg = x"01" else Input_port2;

  -- Program Counter
  program_counter: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      program_count <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if count_update then
        program_count <= program_count + 1;
      elsif load_pc then
        program_count <= unsigned(word_bus);
      end if;
    end if;
  end process;

  -- Memory Address Register
  MAR_proccess: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      MAR_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_mar then
        MAR_reg <= word_bus;
      end if;
    end if;
  end process;

  -- MAR output, if Program flag is set address selected on RAM comes from input 
  MAR_out <= Program_RAM_in_addr when program_flag else MAR_reg;
  -- RAM write enable, if Program flag is set write enable flag coomes from input 
  RAM_write_en <= Program_RAM_write_en when program_flag else RAM_write_en_cntrl;

  -- RAM 64K
  RAM_64K: ram
  generic map(
    DATA_WIDTH => 8,
    RAM_DEPTH  => 2**16  
  )
  port map(  
    sys_clk    => sys_clk,

    write_en   => RAM_write_en,
    addr       => MAR_out,
    write_data => MDR_out,
    read_data  => RAM_read_data
  );

  -- MDR Register
  MDR_proccess: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      MDR_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      -- Load LSB byte of MDR register with data comming from RAM
      if load_LSB_MDR_RAM_data then
        MDR_reg(7 downto 0)  <= RAM_read_data;
      -- Load MSB byte of MDR register with data comming from RAM
      elsif load_MSB_MDR_RAM_data then
        MDR_reg(15 downto 8) <= RAM_read_data;
      -- Load data on Word bus to MDR
      elsif load_MDR_word_bus then
        MDR_reg  <= word_bus;
      end if;
    end if;
  end process;

  -- MDR output, if Program flag is set data selected on RAM comes from input 
  MDR_out <= Program_RAM_write_data when program_flag else MDR_reg(7 downto 0);

  -- Instruction Register
  IR_proccess: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      IR_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_ir then
        IR_reg <= word_bus(7 downto 0);
      end if;
    end if;
  end process;

  -- Controller sequencer FSM
  controller_seq: controller_seq_sap_2
  port map(
    sys_clk   => sys_clk,
    sys_rst   => sys_rst,
    IR        => IR_reg,
    -- Input control bits
    neg_flag              => neg_flag,
    zero_flag             => zero_flag,
    -- Output Control bits
    load_call_reg         => load_call_reg,
    load_in_switch        => load_in_switch, 
    en_word_bus           => en_word_bus,
    ALU_select            => ALU_select,
    load_pc               => load_pc,      
    count_update          => count_update,         
    load_mar              => load_mar,
    load_LSB_MDR_RAM_data => load_LSB_MDR_RAM_data,
    load_MSB_MDR_RAM_data => load_MSB_MDR_RAM_data,
    load_MDR_word_bus     => load_MDR_word_bus,
    RAM_write_en_cntrl    => RAM_write_en_cntrl,   
    load_ir               => load_ir,
    load_accu             => load_accu,            
    load_flag             => load_flag,       
    load_reg_temp         => load_reg_temp,        
    load_reg_b            => load_reg_b,       
    load_reg_c            => load_reg_c,        
    load_outport          => load_outport,       
    load_out_switch       => load_out_switch
  );

  -- CALL Auxiliar register
  call_reg_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      call_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_call_reg then
        call_reg <= word_bus;
      end if;
    end if;
  end process;

  -- Accumulator
  accumulator_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      Accu_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_accu then
        Accu_reg <= unsigned(word_bus(7 downto 0));
      end if;
    end if;
  end process;

  -- ALU Process
  ALU_process: process(all)
  begin
    case(ALU_select) is

      when ADD_OP => alu_out <= Accu_reg + TEMP_reg;
      when SUB_OP => alu_out <= Accu_reg - TEMP_reg;
      when INR_OP => alu_out <= TEMP_reg + 1;
      when DCR_OP => alu_out <= TEMP_reg - 1;
      when AND_OP => alu_out <= Accu_reg AND TEMP_reg;
      when OR_OP  => alu_out <= Accu_reg OR TEMP_reg;
      when XOR_OP => alu_out <= Accu_reg XOR TEMP_reg;
      when RAL_OP => alu_out <= Accu_reg(Accu_reg'length-2 downto 0) & Accu_reg(Accu_reg'length-1);
      when RAR_OP => alu_out <= Accu_reg(0) & Accu_reg(Accu_reg'length-1 downto 1); 
      -- Not operation NOT_OP
      when others => alu_out <= NOT Accu_reg;
    end case;
  end process;

  -- ALU_flags
  ALU_flags_reg_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      neg_flag  <= '0';
      zero_flag <= '0'; 
    elsif rising_edge(sys_clk) then
      if load_flag then
        -- Negative flag is asserted if ALU output MSB is '1'
        neg_flag  <= alu_out(alu_out'length-1);
        -- Zero flag is set if ALU output is 0x00
        zero_flag <= NOR (alu_out);
      end if;
    end if;
  end process;

  -- Register Temporal
  reg_temp_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      TEMP_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_reg_temp then
        TEMP_reg <= unsigned(word_bus(7 downto 0));
      end if;
    end if;
  end process;

  -- Register B
  reg_b_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      B_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_reg_b then
        B_reg <= unsigned(word_bus(7 downto 0));
      end if;
    end if;
  end process;

  -- Register C
  reg_c_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      C_reg <= (others => '0'); 
    elsif rising_edge(sys_clk) then
      if load_reg_c then
        C_reg <= unsigned(word_bus(7 downto 0));
      end if;
    end if;
  end process;

  -- Output Port
  output_port_process: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      Output_port3 <= (others => '0');
      Output_port4 <= (others => '0');
      out_switch_reg <= (others => '0');
    elsif rising_edge(sys_clk) then
      -- Update Output switch register
      if load_out_switch then
        out_switch_reg <= word_bus(7 downto 0);
      end if;
      -- Load output port depending on switch register
      if load_outport then
        if out_switch_reg = x"03" then
          Output_port3 <= word_bus(7 downto 0);
        else
          Output_port4 <= word_bus(7 downto 0);
        end if;
      end if;
    end if;
  end process;

end rtl;