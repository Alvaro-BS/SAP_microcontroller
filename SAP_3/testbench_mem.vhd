-- Testbench for SAP-1
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.sap_3_package.all;
use std.textio.all;

entity testbench is
-- empty
end testbench; 

architecture tb of testbench is

  component sap_3_microprocessor is
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
  end component sap_3_microprocessor;

  signal sys_clk : std_logic := '1';
  signal sys_rst : std_logic;

  signal Input_port1 : std_logic_vector(7 downto 0);
  signal Input_port2 : std_logic_vector(7 downto 0);
  signal Output_port3 : std_logic_vector(7 downto 0);
  signal Output_port4 : std_logic_vector(7 downto 0);

  signal program_flag : std_logic;
  signal program_RAM_in_addr  : std_logic_vector(15 downto 0);
  signal program_RAM_write_en : std_logic;
  signal program_RAM_write_data : std_logic_vector(7 downto 0);

  constant ram_mem_file : string := "assemble_test.txt";

begin

  sys_clk <= not sys_clk after 5 ns;
  
  -- Connect DUT
  DUT: sap_3_microprocessor
  port map(
    sys_clk => sys_clk,
    sys_rst => sys_rst,
    Input_port1 => Input_port1,
    Input_port2 => Input_port2,
    Output_port3 => Output_port3,
    Output_port4 => Output_port4,
    program_flag => program_flag,
    program_RAM_in_addr => program_RAM_in_addr,
    program_RAM_write_en => program_RAM_write_en,
    program_RAM_write_data => program_RAM_write_data
  );
  
  process
    variable line_v : line;
    file read_file  : text;
    variable colon: character;
    variable address_v : std_logic_vector(15 downto 0);
    variable data_v    : std_logic_vector(7 downto 0);
  begin
    sys_rst        <= '1';
    program_flag   <= '1';
    program_RAM_in_addr    <= (others => '0');
    program_RAM_write_en   <= '0';
    program_RAM_write_data <= (others => '0');
    Input_port1 <= x"59";
    Input_port2 <= x"F2"; 
    wait for 100 ns;
    assert false report "Start RAM programation" severity note;
    program_RAM_write_en <= '1';

    file_open(read_file, ram_mem_file, read_mode);
    while not endfile(read_file) loop
      readline(read_file, line_v);
      hread(line_v, address_v);
      read(line_v, colon);
      hread(line_v, data_v);
      program_RAM_in_addr <= address_v;
      program_RAM_write_data <= data_v;
      wait for 10 ns;
    end loop;
    file_close(read_file);

    assert false report "RAM programed with Machine code" severity note;
    program_RAM_write_en   <= '0'; 
    program_flag   <= '0';
    sys_rst        <= '1';
    wait for 100 ns;
    sys_rst        <= '0';
    wait for 10000 ns;
    Input_port2    <= x"2F";
    assert false report "Start Program Execution" severity note;
    wait;
  end process;

end tb;