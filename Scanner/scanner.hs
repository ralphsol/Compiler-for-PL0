import Data.Char (ord)
import Data.Char (isAlphaNum)
import Data.Char (isNumber)
import Data.Char (toUpper)
import System.Environment
import System.IO
import Control.Monad

tokenise ([h]) (tokh:tokt) n =
	if n=="INIT" then 
		if h=='+' then ("BINADD":tokh:tokt)
		else if h=='~' then ("UNMINUS":tokh:tokt)
		else if h=='-' then ("BINSUB":tokh:tokt)
		else if h=='/' then ("BINDIV":tokh:tokt)
		else if h=='*' then ("BINMUL":tokh:tokt)
		else if h=='%' then ("BINMOD":tokh:tokt)
		else if h=='!' then ("NEG":tokh:tokt)
		else if h==':' then ("COLON":tokh:tokt)
		else if h=='=' then ("EQ":tokh:tokt)
		else if h=='<' then ("LT":tokh:tokt)
		else if h=='>' then ("GT":tokh:tokt)
		else if h=='(' then ("LB":tokh:tokt)
		else if h==')' then ("RB":tokh:tokt)
		else if h=='{' then ("LP":tokh:tokt)
		else if h=='}' then ("RP":tokh:tokt)
		else if h==';' then ("EOS":tokh:tokt)
		else if h==',' then ("COMMA":tokh:tokt)
		else if (isNumber h) then ("INTLIT":tokh:tokt)  -- numeral
		else if (isAlphaNum h) then ("IDENT":tokh:tokt)	-- alphanum
		else if (h==' ') then (tokh:tokt);
		else ("ERROR":tokt);
	else if n=="HALFAND" then
		if h=='&' then ("AND":tokt)
		else ("ERROR":tokt);
	else if n=="HALFOR" then
		if h=='|' then ("OR":tokt)
		else ("ERROR":tokt);
	else if n=="COLON" then
		if h=='=' then ("ASSIGN":tokt)
		else ("ERROR":tokt);
	else if n=="LT" then
		if h=='>' then ("NE":tokt)
		else if h=='=' then ("LTE":tokt)
		else ("ERROR":tokt);
	else if n=="GT" then
		if h=='=' then ("GTE":tokt)
		else ("ERROR":tokt);
	else if n=="INTLIT" then
		if (ord(h)>=48 && ord(h)<=57) then ("INTLIT":tokt)				-- numeral
		else ("ERROR":tokt);
	else if n=="I" then
		if h=='N' then ("IDENT":tokt)
		else if h=='F' then ("IF":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="IN" then
		if h=='T' then ("INT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="INT" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="IF" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="B" then
		if h=='O' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="BO" then
		if h=='O' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="BOO" then
		if h=='L' then ("BOOL":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="BOOL" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="T" then
		if h=='H' then ("IDENT":tokt)
		else if h=='T' then ("TT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="TH" then
		if h=='E' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="THE" then
		if h=='N' then ("THEN":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="THEN" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="TT" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="E" then
		if h=='L' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="EL" then
		if h=='S' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="ELS" then
		if h=='E' then ("ELSE":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="ELSE" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="W" then
		if h=='H' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="WH" then
		if h=='I' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="WHI" then
		if h=='L' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="WHIL" then
		if h=='E' then ("WHILE":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="WHILE" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="P" then
		if h=='R' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PR" then
		if h=='O' then ("IDENT":tokt)
		else if h=='I' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PRO" then
		if h=='C' then ("PROC":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PROC" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PRI" then
		if h=='N' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PRIN" then
		if h=='T' then ("PRINT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="PRINT" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="R" then
		if h=='E' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="RE" then
		if h=='A' then ("IDENT":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="REA" then
		if h=='D' then ("READ":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="READ" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="F" then
		if h=='F' then ("FF":tokt)
		else if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="FF" then
		if (isAlphaNum h) then ("IDENT":tokt)	-- alphanum
		else ("ERROR":tokt);
	else if n=="IDENT" then
		if (isAlphaNum h) then (n:tokt)	-- alphanum
		else ("ERROR":tokt);
	else (["ERROR"]);


tokenise (h:t) (tokh:tokt) n =
	if n=="INIT" then 
		if h=='+' then (tokenise (t) ("+":tokh:tokt) "BINADD")
		else if h=='~' then (tokenise (t) ("~":tokh:tokt) "UNMINUS")
		else if h=='-' then (tokenise (t) ("-":tokh:tokt) "BINSUB")
		else if h=='/' then (tokenise (t) ("/":tokh:tokt) "BINDIV")
		else if h=='*' then (tokenise (t) ("*":tokh:tokt) "BINMUL")
		else if h=='%' then (tokenise (t) ("%":tokh:tokt) "BINMOD")
		else if h=='!' then (tokenise (t) ("!":tokh:tokt) "NEG")
		else if h=='&' then (tokenise (t) ("&":tokh:tokt) "HALFAND")
		else if h=='|' then (tokenise (t) ("|":tokh:tokt) "HALFOR")
		else if h==':' then (tokenise (t) (":":tokh:tokt) "COLON")
		else if h=='=' then (tokenise (t) ("=":tokh:tokt) "EQ")
		else if h=='<' then (tokenise (t) ("<":tokh:tokt) "LT")
		else if h=='>' then (tokenise (t) (">":tokh:tokt) "GT")
		else if h=='(' then (tokenise (t) ("(":tokh:tokt) "LP")
		else if h==')' then (tokenise (t) (")":tokh:tokt) "RP")
		else if h=='{' then (tokenise (t) ("{":tokh:tokt) "LB")
		else if h=='}' then (tokenise (t) ("}":tokh:tokt) "RB")
		else if h==';' then (tokenise (t) (";":tokh:tokt) "EOS")
		else if h==',' then (tokenise (t) (",":tokh:tokt) "COMMA")
		else if (isNumber h) then (tokenise (t) ([h]:tokh:tokt) "INTLIT")
		else if h=='I' then (tokenise (t) ([h]:tokh:tokt) "I")
		else if h=='T' then (tokenise (t) ([h]:tokh:tokt) "T")
		else if h=='F' then (tokenise (t) ([h]:tokh:tokt) "F")
		else if h=='W' then (tokenise (t) ([h]:tokh:tokt) "W")
		else if h=='E' then (tokenise (t) ([h]:tokh:tokt) "E")
		else if h=='B' then (tokenise (t) ([h]:tokh:tokt) "B")
		else if h=='P' then (tokenise (t) ([h]:tokh:tokt) "P")
		else if h=='R' then (tokenise (t) ([h]:tokh:tokt) "R")
		else if (isAlphaNum h) then (tokenise (t) ([h]:tokh:tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (tokh:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="UNMINUS" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BINADD" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="UNMINUS" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BINSUB" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BINDIV" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BINMUL" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BINMOD" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="NEG" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="HALFAND" then
		if h=='&' then (tokenise (t) ("":"AND":tokt) "INIT")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="HALFOR" then
		if h=='|' then (tokenise (t) ("":"OR":tokt) "INIT")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="COLON" then
		if h=='=' then (tokenise (t) ("":"ASSIGN":tokt) "INIT")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="EQ" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="LT" then
		if h=='=' then (tokenise (t) ("":"LTE":tokt) "INIT")
		else if h=='>' then (tokenise (t) ("":"NE":tokt) "INIT")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="GT" then
		if h=='=' then (tokenise (t) ("":"GTE":tokt) "INIT")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT")
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="LP" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="RP" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="LB" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="RB" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="EOS" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="COMMA" then
		if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t)  (n:tokt) "INIT")
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="HALFAND" then
		if h=='&' then (tokenise (t) ((tokh ++ [h]):tokt) "AND")
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="HALFOR" then
		if h=='|' then (tokenise (t) ((tokh ++ [h]):tokt) "OR")
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="COLON" then
		if h=='=' then (tokenise (t) ((tokh ++ [h]):tokt) "ASSIGN")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="LT" then
		if h=='>' then (tokenise (t) ((tokh ++ [h]):tokt) "NE")
		else if h=='=' then (tokenise (t) ((tokh ++ [h]):tokt) "LTE")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="GT" then
		if h=='=' then (tokenise (t) ((tokh ++ [h]):tokt) "GTE")
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="INTLIT" then
		if (isNumber h) then (tokenise (t) ((tokh ++ [h]):tokt) "INTLIT")				-- numeral
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="I" then
		if h=='N' then (tokenise (t) ((tokh ++ [h]):tokt) "IN")
		else if h=='F' then (tokenise (t) ((tokh ++ [h]):tokt) "IF")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="IN" then
		if h=='T' then (tokenise (t) ((tokh ++ [h]):tokt) "INT")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="INT" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="IF" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="B" then
		if h=='O' then (tokenise (t) ((tokh ++ [h]):tokt) "BO")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BO" then
		if h=='O' then (tokenise (t) ((tokh ++ [h]):tokt) "BOO")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BOO" then
		if h=='L' then (tokenise (t) ((tokh ++ [h]):tokt) "BOOL")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="BOOL" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="T" then
		if h=='H' then (tokenise (t) ((tokh ++ [h]):tokt) "TH")
		else if h=='T' then (tokenise (t) ((tokh ++ [h]):tokt) "TT")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="TH" then
		if h=='E' then (tokenise (t) ((tokh ++ [h]):tokt) "THE")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="THE" then
		if h=='N' then (tokenise (t) ((tokh ++ [h]):tokt) "THEN")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="THEN" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="TT" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("BOOLVAL":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="E" then
		if h=='L' then (tokenise (t) ((tokh ++ [h]):tokt) "EL")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="EL" then
		if h=='S' then (tokenise (t) ((tokh ++ [h]):tokt) "ELS")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="ELS" then
		if h=='E' then (tokenise (t) ((tokh ++ [h]):tokt) "ELSE")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="ELSE" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="W" then
		if h=='H' then (tokenise (t) ((tokh ++ [h]):tokt) "WH")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="WH" then
		if h=='I' then (tokenise (t) ((tokh ++ [h]):tokt) "WHI")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="WHI" then
		if h=='L' then (tokenise (t) ((tokh ++ [h]):tokt) "WHIL")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="WHIL" then
		if h=='E' then (tokenise (t) ((tokh ++ [h]):tokt) "WHILE")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="WHILE" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="P" then
		if h=='R' then (tokenise (t) ((tokh ++ [h]):tokt) "PR")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PR" then
		if h=='O' then (tokenise (t) ((tokh ++ [h]):tokt) "PRO")
		else if h=='I' then (tokenise (t) ((tokh ++ [h]):tokt) "PRI")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PRO" then
		if h=='C' then (tokenise (t) ((tokh ++ [h]):tokt) "PROC")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PROC" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PRI" then
		if h=='N' then (tokenise (t) ((tokh ++ [h]):tokt) "PRIN")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PRIN" then
		if h=='T' then (tokenise (t) ((tokh ++ [h]):tokt) "PRINT")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="PRINT" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="R" then
		if h=='E' then (tokenise (t) ((tokh ++ [h]):tokt) "RE")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="RE" then
		if h=='A' then (tokenise (t) ((tokh ++ [h]):tokt) "REA")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT")
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="REA" then
		if h=='D' then (tokenise (t) ((tokh ++ [h]):tokt) "READ")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="READ" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "READ")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="F" then
		if h=='F' then (tokenise (t) ((tokh ++ [h]):tokt) "FF")
		else if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("IDENT":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="FF" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) ("BOOLVAL":tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else if n=="IDENT" then
		if (isAlphaNum h) then (tokenise (t) ((tokh ++ [h]):tokt) "IDENT")	-- alphanum
		else if h==' ' then
			if tokh=="ERROR" then (tokenise (t) ("":tokh:tokt) "INIT")
			else (tokenise (t) (n:tokt) "INIT");
		else (tokenise (t) ("ERROR":tokt) n);
	else ("ERROR":tokh:tokt);
	
spacify (h:t) (resh:rest) = if ((h==',')||(h==';')||(h=='(')||(h=='{')||(h=='}')||(h==')')||(h=='+')||(h=='-')||(h=='>')||(h=='=')||(h==':')||(h=='<')||(h=='|')||(h=='&')||(h=='!')||(h=='/')||(h=='%')||(h=='*')||(h=='~')) then (spacify (t) (' ':h:' ':resh:rest));
							else (spacify (t) (h:resh:rest));
spacify ([]) (resh:rest) = (resh:rest);
