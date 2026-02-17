library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package sap_2_package is

  -------------------------------------------------------------
  -- ALU operations code
  -------------------------------------------------------------

  -- en_word_bus width
  constant EN_WORD_BUS_WIDTH : integer := 4;
  -- Input Port 1 word bus code
  constant IN_PORT : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)  := std_logic_vector(to_unsigned(0,EN_WORD_BUS_WIDTH));
  -- Enable Program counter output word bus code
  constant EN_PC : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)    := std_logic_vector(to_unsigned(1,EN_WORD_BUS_WIDTH));
  -- Enable Memory Data Read word bus code
  constant EN_MDR : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(2,EN_WORD_BUS_WIDTH));
  -- Enable A register output word bus code
  constant EN_A   : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(3,EN_WORD_BUS_WIDTH));
  -- Enable B register output word bus code
  constant EN_B   : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(4,EN_WORD_BUS_WIDTH));
  -- Enable C register output word bus code
  constant EN_C   : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(5,EN_WORD_BUS_WIDTH));
  -- Enable Temporal register output word bus code
  constant EN_TMP : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(6,EN_WORD_BUS_WIDTH));
  -- Enable ALU output word bus code
  constant EN_ALU : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)   := std_logic_vector(to_unsigned(7,EN_WORD_BUS_WIDTH));
 -- Enable CALL register output word bus code
  constant EN_CALL : std_logic_vector(EN_WORD_BUS_WIDTH-1 downto 0)  := std_logic_vector(to_unsigned(8,EN_WORD_BUS_WIDTH));

  -------------------------------------------------------------
  -- ALU operations code
  -------------------------------------------------------------

    -- ALU_select width
  constant ALU_SELECT_WIDTH : integer := 4;
  -- ADD operation code in ALU
  constant ADD_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(0,ALU_SELECT_WIDTH));
  -- SUB operation code in ALU
  constant SUB_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(1,ALU_SELECT_WIDTH));
  -- INR operation code in ALU
  constant INR_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(2,ALU_SELECT_WIDTH));
  -- DCR operation code in ALU
  constant DCR_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(3,ALU_SELECT_WIDTH));
  -- AND operation code in ALU
  constant AND_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(4,ALU_SELECT_WIDTH));
  -- OR operation code in ALU
  constant OR_OP : std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(5,ALU_SELECT_WIDTH));
  -- XOR operation code in ALU
  constant XOR_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(6,ALU_SELECT_WIDTH));
  -- NOT operation code in ALU
  constant NOT_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(7,ALU_SELECT_WIDTH));
  -- Shift left rotation code in ALU
  constant RAL_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(8,ALU_SELECT_WIDTH));
  -- Shift rigth rotation code in ALU
  constant RAR_OP: std_logic_vector(ALU_SELECT_WIDTH-1 downto 0) := std_logic_vector(to_unsigned(9,ALU_SELECT_WIDTH));

  -----------------------------------------------------
  -- Microprocessor Instruction Codes
  -----------------------------------------------------

  -- ALU Operation
  constant ADD_B    : std_logic_vector(7 downto 0) := x"80";
  constant ADD_C    : std_logic_vector(7 downto 0) := x"81";
  constant SUB_B    : std_logic_vector(7 downto 0) := x"90";
  constant SUB_C    : std_logic_vector(7 downto 0) := x"91";
  constant ANA_B    : std_logic_vector(7 downto 0) := x"A0";
  constant ANA_C    : std_logic_vector(7 downto 0) := x"A1";
  constant ORA_B    : std_logic_vector(7 downto 0) := x"B0";
  constant ORA_C    : std_logic_vector(7 downto 0) := x"B1";
  constant XRA_B    : std_logic_vector(7 downto 0) := x"A8";
  constant XRA_C    : std_logic_vector(7 downto 0) := x"A9";
  constant RAL      : std_logic_vector(7 downto 0) := x"17";
  constant RAR      : std_logic_vector(7 downto 0) := x"1F";
  constant CMA      : std_logic_vector(7 downto 0) := x"2F";
  constant INR_A    : std_logic_vector(7 downto 0) := x"3C";
  constant INR_B    : std_logic_vector(7 downto 0) := x"04";
  constant INR_C    : std_logic_vector(7 downto 0) := x"0C";
  constant DCR_A    : std_logic_vector(7 downto 0) := x"3D";
  constant DCR_B    : std_logic_vector(7 downto 0) := x"05";
  constant DCR_C    : std_logic_vector(7 downto 0) := x"0D";
  constant ANI      : std_logic_vector(7 downto 0) := x"E6";
  constant ORI      : std_logic_vector(7 downto 0) := x"F6";
  constant XRI      : std_logic_vector(7 downto 0) := x"EE";
  -- Memory and Register operations
  constant MOV_AB   : std_logic_vector(7 downto 0) := x"78";
  constant MOV_AC   : std_logic_vector(7 downto 0) := x"79";
  constant MOV_BA   : std_logic_vector(7 downto 0) := x"47";
  constant MOV_BC   : std_logic_vector(7 downto 0) := x"41";
  constant MOV_CA   : std_logic_vector(7 downto 0) := x"4F";
  constant MOV_CB   : std_logic_vector(7 downto 0) := x"48";
  constant MVI_A    : std_logic_vector(7 downto 0) := x"3E";
  constant MVI_B    : std_logic_vector(7 downto 0) := x"06";
  constant MVI_C    : std_logic_vector(7 downto 0) := x"0E";
  constant LDA      : std_logic_vector(7 downto 0) := x"3A";
  constant SDA      : std_logic_vector(7 downto 0) := x"32";
  -- JUMP operations
  constant JMP      : std_logic_vector(7 downto 0) := x"C3";
  constant JZ       : std_logic_vector(7 downto 0) := x"CA";
  constant JNZ      : std_logic_vector(7 downto 0) := x"C2";
  constant JM       : std_logic_vector(7 downto 0) := x"FA";
  constant CALL     : std_logic_vector(7 downto 0) := x"CD";
  constant RET      : std_logic_vector(7 downto 0) := x"C9";
  -- Miscelaneous
  constant HLT      : std_logic_vector(7 downto 0) := x"76";
  constant IN_BYTE  : std_logic_vector(7 downto 0) := x"DB";
  constant OUT_BYTE : std_logic_vector(7 downto 0) := x"D3";

end package sap_2_package;