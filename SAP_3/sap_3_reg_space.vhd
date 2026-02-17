library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use IEEE.math_real.all;
use work.sap_3_package.all;

entity sap_3_reg_space is
port(
  -------------------------------------
  -- General ports
  -------------------------------------
  sys_clk   : in std_logic;
  sys_rst   : in std_logic;
  -------------------------------------
  -- Register Space control logic
  -------------------------------------
  -- Input select for 8bit registers
  reg_8bit_sel_in   : in reg_8bit_select_t;
  -- Output select for 8bit registers
  reg_8bit_sel_out  : in reg_8bit_select_t;
  -- Load reg_8bit_in into reg_8bit_sel_in when asserted
  reg_8bit_load     : in std_logic;
  -- Output select for 16-bit registers
  reg_16bit_sel_out : in reg_16bit_select_t;
  -- Decrement reg_16bit_sel_up when set
  reg_16bit_dcx     : in std_logic;
  -- Increment reg_16bit_sel_up when set
  reg_16bit_inx     : in std_logic;
  -- Register 16 bits selection for update with DCX/INX
  reg_16bit_sel_up  : in reg_16bit_select_t;
  -------------------------------------
  -- Register space Data/address flow
  -------------------------------------
  -- 8-bit register input 
  reg_8bit_in  : in std_logic_vector(7 downto 0);
  -- 8-bit register output
  reg_8bit_out : out std_logic_vector(7 downto 0);
  -- 16-bit register output 
  reg_16bit_out : out std_logic_vector(15 downto 0)
);
end sap_3_reg_space;

architecture rtl of sap_3_reg_space is

  -- Register pairs
  signal BC_reg : unsigned(15 downto 0) := (others => '0');
  signal DE_reg : unsigned(15 downto 0) := (others => '0');
  signal HL_reg : unsigned(15 downto 0) := (others => '0');
  signal WZ_reg : unsigned(15 downto 0) := (others => '0');
  -- Double registers
  signal PC_reg : unsigned(15 downto 0) := (others => '0');
  signal SP_reg : unsigned(15 downto 0) := (others => '0');

begin

  -- Register Space Update process
  reg_space_update: process(sys_clk, sys_rst)
  begin
    if sys_rst then
      BC_reg <= (others => '0');
      DE_reg <= (others => '0');
      HL_reg <= (others => '0');
      WZ_reg <= (others => '0');
      PC_reg <= (others => '0');
      SP_reg <= (others => '0');
    elsif rising_edge(sys_clk) then
      if reg_8bit_load then
        -- Update Corrresponding 8-bit register depending on select signal
        case (reg_8bit_sel_in) is
          when B_select      => BC_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when C_select      => BC_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when D_select      => DE_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when E_select      => DE_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when H_select      => HL_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when L_select      => HL_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when W_select      => WZ_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when Z_select      => WZ_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when SP_MSB_select => SP_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when SP_LSB_select => SP_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when PC_MSB_select => PC_reg(15 downto 8) <= unsigned(reg_8bit_in);
          when PC_LSB_select => PC_reg( 7 downto 0) <= unsigned(reg_8bit_in);
          when others => null;
        end case;
      end if;

      if reg_16bit_dcx then
        -- Decrement register pair depending on select signal
        case (reg_16bit_sel_up) is
          when BC_select => BC_reg <= BC_reg - 1;
          when DE_select => DE_reg <= DE_reg - 1;
          when HL_select => HL_reg <= HL_reg - 1;
          when WZ_select => WZ_reg <= WZ_reg - 1;
          when SP_select => SP_reg <= SP_reg - 1;
          when PC_select => PC_reg <= PC_reg - 1;
          when others => null;
        end case;
      elsif reg_16bit_inx then
        -- Increment register pair depending on select signal
        case (reg_16bit_sel_up) is
          when BC_select => BC_reg <= BC_reg + 1;
          when DE_select => DE_reg <= DE_reg + 1;
          when HL_select => HL_reg <= HL_reg + 1;
          when WZ_select => WZ_reg <= WZ_reg + 1;
          when SP_select => SP_reg <= SP_reg + 1;
          when PC_select => PC_reg <= PC_reg + 1;
          when others => null;
        end case;
      end if;
    end if;
  end process;

  -- Output 8-bit register select logic
  reg_8bit_out <= 
    std_logic_vector(BC_reg(15 downto 8)) when reg_8bit_sel_out = B_select else
    std_logic_vector(BC_reg( 7 downto 0)) when reg_8bit_sel_out = C_select else
    std_logic_vector(DE_reg(15 downto 8)) when reg_8bit_sel_out = D_select else
    std_logic_vector(DE_reg( 7 downto 0)) when reg_8bit_sel_out = E_select else
    std_logic_vector(HL_reg(15 downto 8)) when reg_8bit_sel_out = H_select else
    std_logic_vector(HL_reg( 7 downto 0)) when reg_8bit_sel_out = L_select else
    std_logic_vector(WZ_reg(15 downto 8)) when reg_8bit_sel_out = W_select else
    std_logic_vector(WZ_reg( 7 downto 0)) when reg_8bit_sel_out = Z_select else
    std_logic_vector(SP_reg(15 downto 8)) when reg_8bit_sel_out = SP_MSB_select else
    std_logic_vector(SP_reg( 7 downto 0)) when reg_8bit_sel_out = SP_LSB_select else
    std_logic_vector(PC_reg(15 downto 8)) when reg_8bit_sel_out = PC_MSB_select else
    std_logic_vector(PC_reg( 7 downto 0)) when reg_8bit_sel_out = PC_LSB_select else
    x"00"; -- Avoid latches

  -- Output 16-bit register select logic
  reg_16bit_out <= 
    std_logic_vector(BC_reg) when reg_16bit_sel_out = BC_select else
    std_logic_vector(DE_reg) when reg_16bit_sel_out = DE_select else
    std_logic_vector(HL_reg) when reg_16bit_sel_out = HL_select else
    std_logic_vector(WZ_reg) when reg_16bit_sel_out = WZ_select else
    std_logic_vector(SP_reg) when reg_16bit_sel_out = SP_select else
    std_logic_vector(PC_reg) when reg_16bit_sel_out = PC_select else
    x"0000"; -- Avoid latches
   

end rtl;