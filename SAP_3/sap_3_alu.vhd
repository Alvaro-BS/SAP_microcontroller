library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;
use work.sap_3_package.all;

entity sap_3_alu is
port(
  -------------------------------------
  -- General ports
  -------------------------------------
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  data_bus  : in std_logic_vector(7 downto 0);
  -------------------------------------
  -- ALU ports
  -------------------------------------
  -- Input port A (Accumulator)
  port_a  : in std_logic_vector(7 downto 0);
  -- Input port B (Temporal register)
  port_b  : in std_logic_vector(7 downto 0);
  -- ALU operation Select signal
  alu_op_select : in alu_op_select_t;
  -- Load ALU flags
  load_alu_flags : in std_logic;
  -- ALU Output
  alu_out : out std_logic_vector(7 downto 0); 
  -- ALU Flags
  alu_flags : out alu_flags_t := (others => '0')
);
end sap_3_alu;

architecture rtl of sap_3_alu is

  -- Next ALU flags signal
  signal next_alu_flags : alu_flags_t;

begin

  -- Update flags register
  flag_reg_update: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      alu_flags <= (others => '0');
    elsif rising_edge(sys_clk) then
      if load_alu_flags then
        alu_flags.carry  <= data_bus(0);
        alu_flags.parity <= data_bus(2);
        alu_flags.neg    <= data_bus(7);
        alu_flags.zero   <= data_bus(6);
      else
        alu_flags <= next_alu_flags;
      end if;
    end if;
  end process;

  -- ALU operations 
  alu_operation: process(all)
    variable port_a_ext : unsigned(8 downto 0);
    variable port_b_ext : unsigned(8 downto 0);
    variable port_b_neg : unsigned(7 downto 0);
    variable port_b_neg_ext : unsigned(8 downto 0);
    variable port_b_ext_inr : unsigned(8 downto 0);
    variable port_b_ext_dcr : unsigned(8 downto 0);
    variable add_op_var : unsigned(8 downto 0);
    variable sub_op_var : unsigned(8 downto 0);
    variable adc_op_var : unsigned(8 downto 0);
    variable sbb_op_var : unsigned(8 downto 0);
    variable a_and_b    : std_logic_vector(7 downto 0);
    variable a_or_b     : std_logic_vector(7 downto 0);
    variable a_xor_b    : std_logic_vector(7 downto 0);
  begin
    -- Keeps ALU flags by default
    next_alu_flags <= alu_flags;
    alu_out        <= port_a;
    -- ALU operations  
    port_a_ext     := '0' & unsigned(port_a);
    port_b_neg     := not(unsigned(port_b)) + 1;
    port_b_ext     := '0' & unsigned(port_b);
    port_b_neg_ext := '0' & port_b_neg;
    port_b_ext_inr := port_b_ext + 1;
    port_b_ext_dcr := port_b_ext - 1;
    add_op_var     := port_a_ext + port_b_ext;
    sub_op_var     := port_a_ext + port_b_neg_ext;
    a_and_b        := port_a and port_b;
    a_or_b         := port_a or port_b;
    a_xor_b        := port_a xor port_b;

    if alu_flags.carry then
      adc_op_var   := add_op_var + 1;
    else
      adc_op_var   := add_op_var;
    end if;
    if alu_flags.carry then
      sbb_op_var   := sub_op_var + 1;
    else
      sbb_op_var   := sub_op_var;
    end if; 

    case (alu_op_select) is 
      ----------------------------------------------------
      -- Arithmetic operations
      ----------------------------------------------------
      -- ADD operation
      when ADD_OP =>
        alu_out <= std_logic_vector(add_op_var(7 downto 0));
        next_alu_flags.carry <= add_op_var(8);
        next_alu_flags.neg   <= add_op_var(7);
        next_alu_flags.zero  <= NOR add_op_var(7 downto 0);
        next_alu_flags.parity <= XNOR add_op_var(7 downto 0);
      -- ADC operation
      when ADC_OP =>
        alu_out <= std_logic_vector(adc_op_var(7 downto 0));
        next_alu_flags.carry <= adc_op_var(8);
        next_alu_flags.neg   <= adc_op_var(7);
        next_alu_flags.zero  <= NOR adc_op_var(7 downto 0);
        next_alu_flags.parity <= XNOR adc_op_var(7 downto 0);
      -- SUB operation
      when SUB_OP =>
        alu_out <= std_logic_vector(sub_op_var(7 downto 0));
        next_alu_flags.carry <= not(sub_op_var(8));
        next_alu_flags.neg   <= sub_op_var(7);
        next_alu_flags.zero  <= NOR sub_op_var(7 downto 0);
        next_alu_flags.parity <= XNOR sub_op_var(7 downto 0);
      -- SBB operation
      when SBB_OP =>
        alu_out <= std_logic_vector(sbb_op_var(7 downto 0));
        next_alu_flags.carry <= not(sbb_op_var(8));
        next_alu_flags.neg   <= sbb_op_var(7);
        next_alu_flags.zero  <= NOR sbb_op_var(7 downto 0);
        next_alu_flags.parity <= XNOR sbb_op_var(7 downto 0);
      -- Decrement port B
      when DCR_OP =>
        alu_out <= std_logic_vector(port_b_ext_dcr(7 downto 0));
        next_alu_flags.neg <= port_b_ext_dcr(7);
        next_alu_flags.zero  <= NOR port_b_ext_dcr(7 downto 0);
        next_alu_flags.parity <= XNOR port_b_ext_dcr(7 downto 0);
      -- Increment port B
      when INR_OP =>
        alu_out <= std_logic_vector(port_b_ext_inr(7 downto 0));
        next_alu_flags.neg <= port_b_ext_inr(7);
        next_alu_flags.zero  <= NOR port_b_ext_inr(7 downto 0);
        next_alu_flags.parity <= XNOR port_b_ext_inr(7 downto 0);
      ----------------------------------------------------
      -- Logic operations
      ----------------------------------------------------
      -- AND operation
      when AND_OP =>
        alu_out <= a_and_b;
        next_alu_flags.neg   <= a_and_b(7);
        next_alu_flags.zero  <= NOR a_and_b;
        next_alu_flags.parity <= XNOR a_and_b;
      -- OR operation
      when OR_OP  =>
          alu_out <= a_or_b;
          next_alu_flags.neg   <= a_or_b(7);
          next_alu_flags.zero  <= NOR a_or_b;
          next_alu_flags.parity <= XNOR a_or_b;
      -- XOR operation
      when XOR_OP =>
          alu_out <= a_xor_b;
          next_alu_flags.neg   <= a_xor_b(7);
          next_alu_flags.zero  <= NOR a_xor_b;
          next_alu_flags.parity <= XNOR a_xor_b;
      -- Rotate all left
      when RAL_OP =>
        next_alu_flags.carry <= port_a(port_a'LENGTH-1);
        alu_out <= port_a(port_a'LENGTH-2 downto 0) & alu_flags.carry;
      -- Rotate left with carry 
      when RLC_OP =>
        next_alu_flags.carry <= port_a(port_a'LENGTH-1);
        alu_out <= port_a(port_a'LENGTH-2 downto 0) & port_a(port_a'LENGTH-1);
      -- Rotate all rigth
      when RAR_OP =>
        next_alu_flags.carry <= port_a(0);
        alu_out <= alu_flags.carry & port_a(port_a'LENGTH-1 downto 1);
      -- Rotate rigth with carry
      when RRC_OP =>
        next_alu_flags.carry <= port_a(0);
        alu_out <= port_a(0) & port_a(port_a'LENGTH-1 downto 1);
      -- Invert port_a value
      when NOT_OP =>
        alu_out <= not(port_a);
      -- Set carry flag
      when STC_OP =>
        next_alu_flags.carry <= '1';
      -- Invert ALU flags
      when CMC_OP =>
        next_alu_flags.carry <= not(alu_flags.carry);
      
      -- Not operation suported (NO_OP)
      when others => 
        null;
    end case;
  end process;
end rtl;