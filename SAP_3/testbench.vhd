-- Testbench for SAP-1
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use work.sap_3_package.all;

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

  type RAM_program_t is array (0 to 2**16-1) of std_logic_vector(7 downto 0);

  ---------------------------------
  -- Test Memory and Register operations
  ---------------------------------
  signal RAM_program_1 : RAM_program_t := ( 16#0# => x"3E", -- MVI A, 0x49
                                            16#1# => x"49",
                                            16#2# => x"06", -- MVI B, 0x4A
                                            16#3# => x"4A",
                                            16#4# => x"0E", -- MVI C, 0x4B
                                            16#5# => x"4B",
                                            16#6# => x"16", -- MVI D, 0x55
                                            16#7# => x"55",
                                            16#8# => x"1E", -- MVI E, 0x97
                                            16#9# => x"97",
                                            16#A# => x"26", -- MVI H, 0x40
                                            16#B# => x"40",
                                            16#C# => x"2E", -- MVI L, 0x05
                                            16#D# => x"05",
                                            16#E# => x"36", -- MVI M, 0x67
                                            16#F# => x"67",
                                            16#10# => x"7E", -- MOV A, M
                                            16#11# => x"71", -- MOV M, C
                                            16#12# => x"32", -- STA 0x6285
                                            16#13# => x"85",
                                            16#14# => x"62",
                                            16#15# => x"3A", -- LDA 0x4005
                                            16#16# => x"05",
                                            16#17# => x"40",
                                            16#18# => x"01", -- LXI B, 0x8745
                                            16#19# => x"45",
                                            16#1A# => x"87",
                                            16#1B# => x"76", -- HLT
                                            others => x"00" -- NOP
                                          );

  ---------------------------------
  -- Example 11-3
  ---------------------------------
  signal RAM_program_2 : RAM_program_t := ( 
                                            16#0# => x"3E", -- MVI A, 0x17
                                            16#1# => x"17",
                                            16#2# => x"06", -- MVI B, 0x2D
                                            16#3# => x"2D",
                                            16#4# => x"80", -- ADD B
                                            16#5# => x"32", -- STA 0x5600
                                            16#6# => x"00",
                                            16#7# => x"56",
                                            16#8# => x"3C", -- INR A
                                            16#9# => x"4F", -- MOV C,A
                                            16#A# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-5
  ---------------------------------
  signal RAM_program_3 : RAM_program_t := ( 16#0# => x"0E", -- MVI C, 03H
                                            16#1# => x"03",
                                            16#2# => x"0D", -- DCR C
                                            16#3# => x"CA", -- JZ 0x0009
                                            16#4# => x"09",
                                            16#5# => x"00",
                                            16#6# => x"C3", -- JMP 0x00002
                                            16#7# => x"02",
                                            16#8# => x"00",
                                            16#9# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-8
  ---------------------------------
  signal RAM_program_4 : RAM_program_t := ( 
                                            16#0# => x"3E", -- MVI A, 0x00
                                            16#1# => x"00",
                                            16#2# => x"06", -- MVI B, 0x0C
                                            16#3# => x"0C",
                                            16#4# => x"0E", -- MVI C, 0x08
                                            16#5# => x"08",
                                            16#6# => x"80", -- ADD B
                                            16#7# => x"0D", -- DCR C
                                            16#8# => x"CA", -- JZ 0x000E
                                            16#9# => x"0E",
                                            16#A# => x"00",
                                            16#B# => x"C3", -- JMP 0x0006
                                            16#C# => x"06",
                                            16#D# => x"00",
                                            16#E# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-9
  ---------------------------------
  signal RAM_program_5 : RAM_program_t := ( 
                                            16#0# => x"3E", -- MVI A, 0x00
                                            16#1# => x"00",
                                            16#2# => x"06", -- MVI B, 0x0C
                                            16#3# => x"0C",
                                            16#4# => x"0E", -- MVI C, 0x08
                                            16#5# => x"08",
                                            16#6# => x"80", -- ADD B
                                            16#7# => x"0D", -- DCR C
                                            16#8# => x"C2", -- JNZ 0x0006
                                            16#9# => x"06",
                                            16#A# => x"00", 
                                            16#B# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-12 and 11-11
  ---------------------------------
  signal RAM_program_6 : RAM_program_t := ( 
                                            16#0# => x"3E", -- MVI A, 0x00
                                            16#1# => x"00",
                                            16#2# => x"06", -- MVI B, 0x10
                                            16#3# => x"10",
                                            16#4# => x"0E", -- MVI C, 0x0E
                                            16#5# => x"0E",
                                            16#6# => x"CD", -- CALL 0xF006
                                            16#7# => x"06",
                                            16#8# => x"F0",
                                            16#9# => x"76", -- HLT
                                            -----------------------------
                                            16#F006# => x"80", -- ADD B
                                            16#F007# => x"0D", -- DCR C
                                            16#F008# => x"C2", -- JNZ 0xF006 
                                            16#F009# => x"06",
                                            16#F00A# => x"F0",
                                            16#F00B# => x"C9", -- RET
                                            -----------------------------
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-13
  ---------------------------------
  signal RAM_program_7 : RAM_program_t := ( 
                                            16#0# => x"DB", -- IN 0x02
                                            16#1# => x"02",
                                            16#2# => x"E6", -- ANI 0x01
                                            16#3# => x"01",
                                            16#4# => x"C2", -- JNZ 0x000C
                                            16#5# => x"0C",
                                            16#6# => x"00",
                                            16#7# => x"3E", -- MVI A, 0x4E
                                            16#8# => x"4E",
                                            16#9# => x"C3", -- JMP 0x000E
                                            16#A# => x"0E",
                                            16#B# => x"00",
                                            16#C# => x"3E", -- MVI A, 0x59
                                            16#D# => x"59",
                                            16#E# => x"D3", -- OUT 0x03
                                            16#F# => x"03",
                                            16#10# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-14
  ---------------------------------
  signal RAM_program_8 : RAM_program_t := (
                                            16#0# => x"DB", -- IN 0x02
                                            16#1# => x"02",
                                            16#2# => x"E6", -- ANI 0x01
                                            16#3# => x"01",
                                            16#4# => x"C2", -- JNZ 0x000C
                                            16#5# => x"0C",
                                            16#6# => x"00",
                                            16#7# => x"3E", -- MVI A, 0x4E
                                            16#8# => x"4E",
                                            16#9# => x"C3", -- JMP 0x000E
                                            16#A# => x"0E",
                                            16#B# => x"00",
                                            16#C# => x"3E", -- MVI A, 0x59
                                            16#D# => x"59",
                                            16#E# => x"0E", -- MVI C, 0x08
                                            16#F# => x"08",
                                            16#10# => x"D3", -- OUT 0x04
                                            16#11# => x"04",
                                            16#12# => x"1F", -- RAR
                                            16#13# => x"0D", -- DCR C
                                            16#14# => x"C2", -- JNZ 0x0010
                                            16#15# => x"10",
                                            16#16# => x"00",
                                            16#17# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-15
  ---------------------------------
  signal RAM_program_9 : RAM_program_t := ( 
                                            16#0# => x"DB", -- IN 0x02
                                            16#1# => x"02",
                                            16#2# => x"E6", -- ANI 0x01
                                            16#3# => x"01",
                                            16#4# => x"CA", -- JZ 0x0000
                                            16#5# => x"00",
                                            16#6# => x"00",
                                            16#7# => x"DB", -- IN 0x01
                                            16#8# => x"01",
                                            16#9# => x"47", -- MOV B,A
                                            16#A# => x"3E", -- MVI A 0x80
                                            16#B# => x"80",
                                            16#C# => x"D3", -- OUT 0x04
                                            16#D# => x"04",
                                            16#E# => x"3E", -- MVI A, 0x00
                                            16#F# => x"00",
                                            16#10# => x"D3", -- OUT 0x04
                                            16#11# => x"04",
                                            16#12# => x"76", -- HLT
                                            others => x"00"
                                          );

  ---------------------------------
  -- Example 11-19
  ---------------------------------
  signal RAM_program_a : RAM_program_t := ( 
                                            16#0# => x"3E", -- MVI A, 0x32
                                            16#1# => x"32",
                                            ------------------ Green Light ---------
                                            16#2# => x"32", -- STA 0x6445
                                            16#3# => x"45",
                                            16#4# => x"64",
                                            16#5# => x"3E", -- MVI A, 0x02
                                            16#6# => x"02",
                                            16#7# => x"D3", -- OUT 0x04
                                            16#8# => x"04",
                                            16#9# => x"CD", -- CALL 0x2000
                                            16#A# => x"00",
                                            16#B# => x"20",
                                            16#C# => x"3A", -- LDA 0x6445
                                            16#D# => x"45",
                                            16#E# => x"64",
                                            16#F# => x"3D", -- DCR A
                                            16#10# => x"32", -- STA 0x6445
                                            16#11# => x"45",
                                            16#12# => x"64",
                                            16#13# => x"C2", -- JNZ 0x0009
                                            16#14# => x"09",
                                            16#15# => x"00",
                                            16#16# => x"3E", -- MVI A, 0x06
                                            16#17# => x"06",
                                            ------------------ Yellow Light ---------
                                            16#18# => x"32", -- STA 0x6445
                                            16#19# => x"45",
                                            16#1A# => x"64",
                                            16#1B# => x"3E", -- MVI A, 0x04
                                            16#1C# => x"04",
                                            16#1D# => x"D3", -- OUT 0x04
                                            16#1E# => x"04",
                                            16#1F# => x"CD", -- CALL 0x2000
                                            16#20# => x"00",
                                            16#21# => x"20",
                                            16#22# => x"3A", -- LDA 0x6445
                                            16#23# => x"45",
                                            16#24# => x"64",
                                            16#25# => x"3D", -- DCR A
                                            16#26# => x"32", -- STA 0x6445
                                            16#27# => x"45",
                                            16#28# => x"64",
                                            16#29# => x"C2", -- JNZ 0x001F
                                            16#2A# => x"00",
                                            16#2B# => x"1F",
                                            16#2C# => x"3E", -- MVI A, 0x1E
                                            16#2D# => x"06",
                                            ------------------ Red Light ---------
                                            16#2E# => x"32", -- STA 6445
                                            16#2F# => x"45",
                                            16#30# => x"64",
                                            16#31# => x"3E", -- MVI A, 0x08
                                            16#32# => x"08",
                                            16#33# => x"D3", -- OUT 0x04
                                            16#34# => x"04",
                                            16#35# => x"CD", -- CALL 0x2000
                                            16#36# => x"00",
                                            16#37# => x"20",
                                            16#38# => x"3A", -- LDA 6445
                                            16#39# => x"45",
                                            16#3A# => x"64",
                                            16#3B# => x"3D", -- DCR A
                                            16#3C# => x"32", -- STA 6445
                                            16#3D# => x"45",
                                            16#3E# => x"64",
                                            16#3F# => x"C2", -- JNZ 0x0035
                                            16#40# => x"35",
                                            16#41# => x"00",
                                            16#42# => x"C3", -- JMP 0x0000
                                            16#43# => x"00",
                                            16#44# => x"00",
                                            16#45# => x"76", -- HLT
                                            -------------------------------------
                                            16#2000# => x"3E", -- MVI A, 0x0A
                                            16#2001# => x"0A",
                                            16#2002# => x"06", -- MVI B, 0x64
                                            16#2003# => x"64",
                                            16#2004# => x"0E", -- MVI C, 0x47
                                            16#2005# => x"47",
                                            16#2006# => x"0D", -- DCR C
                                            16#2007# => x"C2", -- JNZ 0x2006
                                            16#2008# => x"06",
                                            16#2009# => x"20",
                                            16#200A# => x"05", -- DCR B
                                            16#200B# => x"C2", -- JNZ 0x2004
                                            16#200C# => x"04",
                                            16#200D# => x"20",
                                            16#200E# => x"3D", -- DCR A
                                            16#200F# => x"C2", -- JNZ 0x2002
                                            16#2010# => x"02",
                                            16#2011# => x"20",
                                            16#2012# => x"C9", -- RET
                                            -------------------------------------
                                            others => x"00"
                                          );
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

    -- Load Program
    for idx in 0 to 50 loop
      program_RAM_in_addr    <= std_logic_vector(to_unsigned(idx, program_RAM_in_addr'length));
      program_RAM_write_data <= RAM_program_1(idx);
      wait for 10 ns;
    end loop;

    -- Load possible subroutines
    for idx in 16#2000# to 16#2050# loop
      program_RAM_in_addr    <= std_logic_vector(to_unsigned(idx, program_RAM_in_addr'length));
      program_RAM_write_data <= RAM_program_1(idx);
      wait for 10 ns;
    end loop;

        -- Load possible subroutines
    for idx in 16#F006# to 16#F010# loop
      program_RAM_in_addr    <= std_logic_vector(to_unsigned(idx, program_RAM_in_addr'length));
      program_RAM_write_data <= RAM_program_1(idx);
      wait for 10 ns;
    end loop;
    
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