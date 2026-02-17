library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use ieee.math_real.all;

entity ram is
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
end ram;

architecture rtl of ram is

  type ram_type is array (RAM_DEPTH-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
  signal RAM_data : ram_type := (others => x"FF");

begin

  ram_process: process(sys_clk)
  begin
    if rising_edge(sys_clk) then
      if write_en then
        RAM_data(to_integer(unsigned(addr))) <= write_data;
      end if;
    end if;
  end process;

  read_data <= RAM_data(to_integer(unsigned(addr)));

end rtl;