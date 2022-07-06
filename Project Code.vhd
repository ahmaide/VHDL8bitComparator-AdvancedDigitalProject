-- Ahmaide Al-Awawdah
-- 1190823


--                Inverters
-- ==================================================================================================== 
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY inverter9 IS
	PORT (x : IN std_logic_vector(8 downto 0);
	y: OUT std_logic_vector(8 downto 0));
	END;
											
ARCHITECTURE invert9 OF inverter9 IS 		   
BEGIN									   
	y <= NOT x AFTER 2 ns;					   
END;										  
--------------------------------------------------
                
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY inverter IS
	PORT (x : IN std_logic;
	y: OUT std_logic);
	END;
	
ARCHITECTURE invert OF inverter IS 
BEGIN
	y <= NOT x AFTER 2 ns;
END;
--------------------------------------------------				  


--                NOR Gates
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY NOR9 IS
	PORT (x, y, w, v, u, t, s, r, q: IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE NOR_9 OF NOR9 IS 
BEGIN
	z <= NOT (x OR y OR w OR v OR u OR t OR s OR r OR q) AFTER 5 ns;
END;				  
---------------------------------------------------------------------


--                AND GATES
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY AND2 IS
	PORT (x, y : IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE AND_2 OF AND2 IS 
BEGIN
	z <= x AND y AFTER 7 ns;
END;	
------------------------------------------

Library ieee;
USE ieee.std_logic_1164.all;

ENTITY AND3 IS
	PORT (x, y, w : IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE AND_3 OF AND3 IS 
BEGIN
	z <= x AND y AND w AFTER 7 ns;
END;	
------------------------------------------


--                OR GATES
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY OR2 IS
	PORT (x ,y : IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE OR_2 OF OR2 IS 
BEGIN
	z <= x OR y AFTER 7 ns;
END;	  
------------------------------------------------------------

Library ieee;
USE ieee.std_logic_1164.all;

ENTITY OR8 IS
	PORT (x, y, w, v, u, t, s, r : IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE OR_8 OF OR8 IS 
BEGIN
	z <= (x OR y OR w OR v OR u OR t OR s OR r) AFTER 7 ns;
END;	  
------------------------------------------------------------ 


--                XOR GATES
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY XOR2 IS
	PORT (x ,y : IN std_logic;
	z: OUT std_logic);
	END;
	
ARCHITECTURE XOR_2 OF XOR2 IS 
BEGIN
	z <= x XOR y AFTER 12 ns;
END;		   
---------------------------------------------------

Library ieee;
USE ieee.std_logic_1164.all;

ENTITY XOR8 IS
	PORT (x ,y : IN std_logic_vector( 7 downto 0);
	z: OUT std_logic_vector( 7 downto 0));
	END;
	
ARCHITECTURE XOR_8 OF XOR8 IS 
BEGIN
	z <= x XOR y AFTER 12 ns;
END;
-----------------------------------------------------


--                D FLIP-FLOP
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;

ENTITY DFF IS
	PORT (a,b,c, clock, reset: IN std_logic;
	qa, qb, qc: OUT std_logic);
END;

ARCHITECTURE DFF3 OF DFF IS
BEGIN
	PROCESS (clock)
	BEGIN
		IF rising_edge(clock) THEN
			IF (reset='1') THEN
				qa <= '0';
				qb <= '0';
				qc <= '0';
			ELSE
				qa <= a;
				qb <= b;
				qc <= c;
			END IF;
		END IF;
	END PROCESS;
END;
-----------------------------------------------


--                FULL ADDER
-- ====================================================================================================
LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY ADDER IS
	PORT (a, b, cin: IN std_logic;
	sum, cout: OUT std_logic);
END;

ARCHITECTURE simple OF ADDER IS
SIGNAL s1, s2, s3: std_logic;
BEGIN
	g1: ENTITY work.XOR2(XOR_2) PORT MAP (a,b,s1);
	g2: ENTITY work.XOR2(XOR_2) PORT MAP (s1,cin,sum);
	g3: ENTITY work.AND2(AND_2) PORT MAP (s1,cin,s2);
	g4: ENTITY work.AND2(AND_2) PORT MAP (a, b,s3);	
	g5: ENTITY work.OR2(OR_2) PORT MAP (s2, s3, cout);
END;
-----------------------------------------------------------


--                9-BIT ADDER
-- ====================================================================================================
LIBRARY ieee;
USE ieee.std_logic_1164.all; 
USE ieee.std_logic_signed.all;

ENTITY Bit9Adder IS
	PORT (a, b: IN std_logic_vector(8 downto 0);
	cin: IN std_logic;
	s: OUT std_logic_vector(8 downto 0);
	cout: OUT std_logic);
END;

ARCHITECTURE structAdd OF Bit9Adder IS	  
SIGNAL carrySignal: STD_logic_vector( 9 downto 0);
CONSTANT n: INTEGER := 9;
SIGNAL sig: STD_logic_vector( 8 downto 0);	  
BEGIN		   
	carrySignal(0) <= cin;   
	gen0:
	FOR i IN 0 TO n-1 GENERATE -- Loop to go on all the Bits of both inputs
		g1: ENTITY work.ADDER(simple) PORT MAP (a(i), b(i), carrySignal(i), sig(i), carrySignal(i+1));
	END GENERATE gen0;
	s <= sig;
	cout <= carrySignal(9);	
END;
-----------------------------------------------------------------------------------------------------


--                A subtractor that takes 8-bit inputs and gives a 9-bit subtraction output
-- ====================================================================================================

LIBRARY ieee;
USE ieee.std_logic_1164.all; 
USE ieee.std_logic_signed.all;

ENTITY Bit8Subtractor IS
	PORT(a, b: IN std_logic_vector(7 downto 0);
	sub: OUT std_logic_vector(8 downto 0));
END;

ARCHITECTURE structSub OF Bit8Subtractor IS
SIGNAL onesComp: std_logic_vector( 8 downto 0);-- B Inverted	   
SIGNAL nun: std_logic;
SIGNAL extraA, extraB: std_logic; -- A and B in 9-bit representation
SIGNAL a9, b9: std_logic_vector( 8 downto 0); -- 9th Bit of A and B
BEGIN	
	extraA <= a(7);
	extraB <= b(7);
	a9 <= extraA&a;
	b9 <= extraB&b;
	g0: ENTITY work.inverter9(invert9) PORT MAP (b9, onesComp);
	g1: ENTITY work.Bit9Adder(structAdd) PORT MAP (a9, onesComp, '1', sub, nun);
END;
------------------------------------------------------------------------------------


--                One bit comparator that depends on previous higher bits
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;   
USE ieee.std_logic_unsigned.all;

ENTITY BitCompare IS
	PORT (a, b, pstateA, pstateB: IN std_logic;
	stateA, stateB: OUT std_logic);
END;

ARCHITECTURE chip OF BitCompare IS 
SIGNAL bitDif, Abit, Bbit, As, Bs, nstateA, nstateB: std_logic;
BEGIN
	g1: ENTITY work.inverter(invert) PORT MAP (pstateA, nstateA);
	g2: ENTITY work.inverter(invert) PORT MAP (pstateB, nstateB);
	g3: ENTITY work.XOR2(XOR_2) PORT MAP (a, b, bitDif);
	g4: ENTITY work.AND3(AND_3) PORT MAP (a, bitDif, nstateB, Abit);
	g5: ENTITY work.AND3(AND_3) PORT MAP (b, bitDif, nstateA, Bbit);
	g6: ENTITY work.OR2(OR_2) PORT MAP (pstateA, Abit, As);
	g7: ENTITY work.OR2(OR_2) PORT MAP (pstateB, Bbit, Bs);
	stateA <= As;
	stateB <= Bs;
END;
----------------------------------------------------------------------------


--                          THE COMPARATORS
-- ====================================================================================================
-- ====================================================================================================
Library ieee;
USE ieee.std_logic_1164.all;   
USE ieee.std_logic_unsigned.all;

ENTITY comparator IS
	PORT (a,b: IN std_logic_vector(7 downto 0);
	clk,reset: IN std_logic;
	F1, F2, F3: OUT std_logic);
END;

--                STAGE 2: USING MAGNITUDE COMPARATOR AND SIGN BIT
-- ====================================================================================================
ARCHITECTURE magnitudeComparator OF comparator IS					  
CONSTANT n: INTEGER := 8;	 
SIGNAL sgnDif, nA7, nB7, sgnAl, sgnBl, sgnEQ, nstateA, nstateB, bitAl, bitBl, E1, E2, E3: std_logic;	
SIGNAL stateA, stateB: std_logic_vector(7 downto 0) ;	 
BEGIN 
	stateA(7) <= '0';
	stateB(7) <= '0';
	g0: ENTITY work.inverter(invert) PORT MAP (a(n-1), nA7);
	g1: ENTITY work.inverter(invert) PORT MAP (b(n-1), nB7);
	g2: ENTITY work.XOR2(XOR_2) PORT MAP (a(n-1), b(n-1), sgnDif);
	g3: ENTITY work.AND2(AND_2) PORT MAP (nA7, sgnDif, sgnAl);
	g4: ENTITY work.AND2(AND_2) PORT MAP (nB7, sgnDif, sgnBl);	   
	g5: ENTITY work.inverter(invert) PORT MAP (sgnDif, sgnEQ);	
	gen: FOR i IN 2 TO n GENERATE
		g6: ENTITY work.BitCompare(chip)
			PORT MAP (a(n-i), b(n-i), stateA(n+1-i), stateB(n+1-i),
			stateA(n-i), stateB(n-i));
	END GENERATE gen;
	g7: ENTITY work.inverter(invert) PORT MAP (stateA(0), nstateA); 
	g8: ENTITY work.inverter(invert) PORT MAP (stateB(0), nstateB);
	g9: ENTITY work.AND2(AND_2) PORT MAP (sgnEQ, stateA(0), bitAl);
	g10: ENTITY work.AND2(AND_2) PORT MAP (sgnEQ, stateB(0), bitBl);
	g11: ENTITY work.AND3(AND_3) PORT MAP (nstateA, nstateB, sgnEQ, E1);
	g12: ENTITY work.OR2(OR_2) PORT MAP (bitAl, sgnAl, E2);	 
	g13: ENTITY work.OR2(OR_2) PORT MAP (bitBl, sgnBl, E3);
	g14: ENTITY work.DFF(DFF3) PORT MAP (E1, E2, E3, clk, reset, F1, F2, F3);
END;
------------------------------------------------------------------------------------------

--                STAGE 1: STAGE 1 USING RIPPLE ADDER
-- ====================================================================================================
ARCHITECTURE adderComparator OF comparator IS 
SIGNAL sgnPos, notZero, E1, E2, E3: std_logic;
SIGNAL sbt: std_logic_vector( 8 downto 0 );
BEGIN
	g0: ENTITY work.Bit8Subtractor(structSub) PORT MAP (a, b, sbt);
	g1: ENTITY  work.OR8(OR_8) PORT MAP (sbt(0), sbt(1), sbt(2), sbt(3), sbt(4), sbt(5), sbt(6), sbt(7), notZero); 
	g2: ENTITY work.inverter(invert) PORT MAP (sbt(8), sgnPos);
	g3: ENTITY work.NOR9(NOR_9) PORT MAP (sbt(0), sbt(1), sbt(2), sbt(3), sbt(4), sbt(5), sbt(6), sbt(7), sbt(8), E1);
	g4: ENTITY work.AND2(AND_2) PORT MAP (notZero, sgnPos, E2);
	E3 <= sbt(8);
	g5: ENTITY work.DFF(DFF3) PORT MAP (E1, E2, E3, clk, reset, F1, F2, F3);
END;
--------------------------------------------------------------------------------------------

--                TEST GENERATOR 
-- ====================================================================================================
Library IEEE;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY generator IS 
	PORT (clock : IN std_logic;
	testA, testB: OUT std_logic_vector(7 downto 0);
	expF1, expF2, expF3: OUT std_logic);
END;

ARCHITECTURE tester OF generator IS
BEGIN
	PROCESS
	BEGIN
		FOR i IN -128 TO 127 LOOP
			FOR j IN -128 TO 127 LOOP
				testA <= CONV_STD_LOGIC_VECTOR(i, 8);  
				testB <= CONV_STD_LOGIC_VECTOR(j, 8);
				IF ( i > j ) THEN
					expF1 <= '0';
					expF2 <= '1';
					expF3 <= '0';
				ELSIF ( j > i ) THEN
					expF1 <= '0';
					expF2 <= '0';
					expF3 <= '1';
				ELSE
					expF1 <= '1';
					expF2 <= '0';
					expF3 <= '0';
				END IF;
				WAIT UNTIL rising_edge(clock);
			END LOOP;
		END LOOP; 
		WAIT;
	END PROCESS;
END;
--------------------------------------------------------------


--                RESULT ANALYSER
-- ====================================================================================================
Library IEEE;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY analyser IS
	PORT ( expF1, expF2, expF3, actF1, actF2, actF3 : IN std_logic;
	clock: IN std_logic);
END;

ARCHITECTURE resultAnalyser OF analyser IS
BEGIN 
	PROCESS(clock)
	BEGIN
		IF rising_edge(clock) THEN
			ASSERT (expF1 = actF1 AND expF2 = actF2 AND expF3 = actF3)
			REPORT "The output is incorrect"
			SEVERITY ERROR;
		END IF;
	END PROCESS;
END;
----------------------------------------------------------------------------


--                         TESTING FOR BOTH STAGES
-- ====================================================================================================
-- ====================================================================================================
Library IEEE;
USE ieee.std_logic_1164.ALL;
USE ieee.std_logic_arith.ALL;
USE ieee.std_logic_unsigned.ALL;

ENTITY testBinch IS
END;   

--                TESTING FOR STAGE 1
-- ====================================================================================================
ARCHITECTURE adderCompTest OF testBinch IS
SIGNAL clock, clock2, expF1, expF2, expF3, actF1, actF2, actF3: std_logic:='0';
SIGNAL A, B: std_logic_vector( 7 downto 0 ):="00000000";
BEGIN
	clock <= not clock after 165 ns;
	clock2 <= not clock2 after 330 ns;
	g1: ENTITY work.generator(tester) PORT MAP (clock2, A, B, expF1, expF2, expF3);
	g2: ENTITY work.comparator(adderComparator) PORT MAP (A, B, clock, '0', actF1, actF2, actF3);
	g3: ENTITY work.analyser(resultAnalyser) PORT MAP (expF1, expF2, expF3, actF1, actF2, actF3, clock2);
END;
-----------------------------------------------------------------------------------------

--                TESTING FOR STAGE 2
-- ====================================================================================================
ARCHITECTURE magCompTest OF testBinch IS
SIGNAL clock, clock2, expF1, expF2, expF3, actF1, actF2, actF3: std_logic:='0';
SIGNAL A, B: std_logic_vector( 7 downto 0 ):="00000000";
BEGIN
	clock <= not clock after 92 ns;
	clock2 <= not clock2 after 184 ns;
	g1: ENTITY work.generator(tester) PORT MAP (clock2, A, B, expF1, expF2, expF3);
	g2: ENTITY work.comparator(magnitudeComparator) PORT MAP (A, B, clock, '0', actF1, actF2, actF3);
	g3: ENTITY work.analyser(resultAnalyser) PORT MAP (expF1, expF2, expF3, actF1, actF2, actF3, clock2);
END;
-----------------------------------------------------------------------------------------------------------------
		
				