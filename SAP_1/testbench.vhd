-- Testbench for SAP-1
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity testbench is
-- empty
end testbench; 

architecture tb of testbench is

  component sap_1_microprocessor is
  port(  
    sys_clk   : in std_logic;
    sys_rst   : in std_logic;
    program_flag : in std_logic;
    RAM_in_addr  : in std_logic_vector(7 downto 0);
    RAM_write_en : in std_logic;
    RAM_write_data : in std_logic_vector(7 downto 0);
    OUT_reg        : out std_logic_vector(7 downto 0)
  );
  end component sap_1_microprocessor;

  signal sys_clk : std_logic := '1';
  signal sys_rst : std_logic;
  signal program_flag : std_logic;
  signal RAM_write_en : std_logic;
  signal RAM_write_data : std_logic_vector(7 downto 0);
  signal RAM_in_addr : std_logic_vector(7 downto 0); 
  signal OUT_reg : std_logic_vector(7 downto 0);
  signal finished: std_logic := '0';


  type RAM_program_t is array (0 to 2**4-1) of std_logic_vector(7 downto 0);
  ---------------------------------
  -- Operation: 16 + 20 + 24 - 32
  -- Expected result: 28
  ---------------------------------
  signal RAM_program_a : RAM_program_t := ( 16#0# => x"09", -- LDA 0x9
                                            16#1# => x"1A", -- ADD 0xA
                                            16#2# => x"1B", -- ADD 0xB
                                            16#3# => x"2C", -- SUB 0xC
                                            16#4# => x"E0", -- OUT
                                            16#5# => x"F0", -- HLT
                                            16#9# => x"10",
                                            16#A# => x"14",
                                            16#B# => x"18",
                                            16#C# => x"20",
                                            others => x"FF"
                                          );

  ---------------------------------
  -- Operation: 132 + 45 - 100 - 50 + 3
  -- Expected result: 30
  ---------------------------------
  signal RAM_program_b : RAM_program_t := ( 16#0# => x"09", -- LDA 0x9
                                            16#1# => x"1A", -- ADD 0xA
                                            16#2# => x"2B", -- SUB 0xB
                                            16#3# => x"2C", -- SUB 0xC
                                            16#4# => x"1D", -- ADD 0xD
                                            16#5# => x"E0", -- OUT
                                            16#6# => x"F0", -- HLT
                                            ---- Data Memory -----------
                                            16#9# => x"84", -- 132
                                            16#A# => x"2d", -- 45
                                            16#B# => x"64", -- 100
                                            16#C# => x"32", -- 50
                                            16#D# => x"03", -- 3
                                            ----------------------------
                                            others => x"FF"
                                          );

  ---------------------------------
  -- Operation: 132 + 45 - 100 - 50 + 3
  -- Expected result 1: 177
  -- Operation 2: 177 - 100 - 50 + 3
  -- Expected result 2: 30
  ---------------------------------
  signal RAM_program_c : RAM_program_t := ( 16#0# => x"09", -- LDA 0x9
                                            16#1# => x"1A", -- ADD 0xA
                                            16#2# => x"E0", -- OUT
                                            16#3# => x"2B", -- SUB 0xB
                                            16#4# => x"2C", -- SUB 0xC
                                            16#5# => x"1D", -- ADD 0xD
                                            16#6# => x"E0", -- OUT
                                            16#7# => x"F0", -- HLT
                                            ---- Data Memory -----------
                                            16#9# => x"84", -- 132
                                            16#A# => x"2d", -- 45
                                            16#B# => x"64", -- 100
                                            16#C# => x"32", -- 50
                                            16#D# => x"03", -- 3
                                            ----------------------------
                                            others => x"FF"
                                          );

begin

  sys_clk <= not sys_clk after 5 ns when finished /= '1' else '0';
  
  -- Connect DUT
  DUT: sap_1_microprocessor
  port map(
    sys_clk   => sys_clk,
    sys_rst   => sys_rst,
    program_flag => program_flag,
    RAM_in_addr  => RAM_in_addr,
    RAM_write_en => RAM_write_en,
    RAM_write_data => RAM_write_data,
    OUT_reg        => OUT_reg
  );
  
  process
  begin
    sys_rst        <= '1';
    program_flag   <= '1';
    RAM_in_addr    <= (others => '0');
    RAM_write_en   <= '0';
    RAM_write_data <= (others => '0');
    wait for 100 ns;
    sys_rst <= '0';
    wait for 20 ns;

    assert false report "Start RAM programation" severity note;
    RAM_write_en <= '1';

    for idx in RAM_program_a'range loop
      RAM_in_addr    <= std_logic_vector(to_unsigned(idx, RAM_in_addr'length));
      RAM_write_data <= RAM_program_c(idx);
      wait for 10 ns;
    end loop;

    assert false report "RAM programed with Machine code" severity note;
    RAM_write_en   <= '0'; 
    program_flag   <= '0';
    sys_rst        <= '1';
    wait for 100 ns;
    sys_rst        <= '0';
    assert false report "Start Program Execution" severity note;
    wait for 500 ns;
    finished <='1';
    assert false report "Test done." severity note;
    wait;
  end process;

end tb;