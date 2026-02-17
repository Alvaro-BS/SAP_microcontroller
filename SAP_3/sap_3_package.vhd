library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package sap_3_package is

  -- Register Space 8-bit select type
  type reg_8bit_select_t is (B_select, C_select, D_select, E_select, H_select, L_select,
    W_select, Z_select, SP_MSB_select, SP_LSB_select, PC_MSB_select, PC_LSB_select);
  -- Register Space 16-bit select type
  type reg_16bit_select_t is (BC_select, DE_select, HL_select, WZ_select, SP_select, PC_select);
  -- ALU Operation select type
  type alu_op_select_t is (NO_OP, ADD_OP, ADC_OP, SUB_OP, SBB_OP, DCR_OP, INR_OP, AND_OP, OR_OP, XOR_OP,
    RAL_OP, RLC_OP, RAR_OP, RRC_OP, NOT_OP, STC_OP, CMC_OP);
  -- Data Bus select type
  type data_bus_sel_t is (IN_PORT, EN_REG_SPACE, EN_ALU, EN_ALU_FLAGS, EN_MDR, EN_A, EN_TMP);

  -- ALU Flags
  type alu_flags_t is record
    -- Carry Flag 
    carry  : std_logic;
    -- Negative flag
    neg    : std_logic;
    -- Zero flag
    zero   : std_logic;
    -- Even parity
    parity : std_logic; 
  end record alu_flags_t;

  -- Control word record
  type control_word_t is record
    -- Data bus select control signal
    data_bus_sel : data_bus_sel_t;
    -- Load Intruction register
    load_ir   : std_logic;
    -- Load Accumulator register
    load_accu : std_logic;
    -- Load Temporal register
    load_temp : std_logic;
    -- Load new address on MAR comming from address bus
    load_mar : std_logic;
    -- Load new data on MDR
    load_mdr : std_logic;
    -- Select data to be loaded on MDR (1: RAM, 0: data bus)
    mdr_select_RAM_out : std_logic;
    -- RAM write enable signal
    RAM_write_en      : std_logic;
    -- Input select for 8bit registers
    reg_8bit_sel_in   : reg_8bit_select_t;
    -- Output select for 8bit registers
    reg_8bit_sel_out  : reg_8bit_select_t;
    -- Load reg_8bit_in into reg_8bit_sel_in when asserted
    reg_8bit_load     : std_logic;
    -- Output select for 16-bit registers
    reg_16bit_sel_out : reg_16bit_select_t;
    -- Decrement reg_16bit_sel_up when set
    reg_16bit_dcx     : std_logic;
    -- Increment reg_16bit_sel_up when set
    reg_16bit_inx     : std_logic;
    -- Register pair 16 bits select for dcx/inx operations
    reg_16bit_sel_up  : reg_16bit_select_t;
    -- Load ALU flags
    load_alu_flags    : std_logic;
    -- ALU Operation select
    alu_op_select     : alu_op_select_t;
    -- Load Output register
    load_outport      : std_logic;
    -- Load Output switch reg
    load_out_switch   : std_logic;
    -- Load input switch reg
    load_in_switch    : std_logic;
  end record control_word_t;

end package sap_3_package;