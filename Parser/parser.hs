import System.Environment
import System.IO
import Control.Monad

command ([h]) = [h];
command (h:t) = 	if assignCmd(h:t)/=["ERROR"] then
						if assignCmd(h:t)==[] then [];
						else if head(assignCmd(h:t))=="EOS" then
							if command(tail(assignCmd(h:t)))==["ERROR"] then ["ERROR"];
							else command(tail(assignCmd(h:t)));
						else ["ERROR"];
					else if callCmd(h:t)/=["ERROR"] then
						if callCmd(h:t)==[] then [];
						else if head(callCmd(h:t))=="EOS" then
							if command(tail(callCmd(h:t)))==["ERROR"] then ["ERROR"];
							else command(tail(callCmd(h:t)));
						else ["ERROR"];
					else if readCmd(h:t)/=["ERROR"] then
						if readCmd(h:t)==[] then [];
						else if head(readCmd(h:t))=="EOS" then
							if command(tail(readCmd(h:t)))==["ERROR"] then ["ERROR"];
							else command(tail(readCmd(h:t)));
						else ["ERROR"];
					else if printCmd(h:t)/=["ERROR"] then
						if printCmd(h:t)==[] then [];
						else if head(printCmd(h:t))=="EOS" then
							if command(tail(printCmd(h:t)))==["ERROR"] then ["ERROR"];
							else command(tail(printCmd(h:t)));
						else ["ERROR"];
					else (h:t);

condCmd (h:t) = 	if h=="IF" then
						if boolExpress(t)==["ERROR"] then ["ERROR"];
						else 
							if head(boolExpress(t))=="THEN" then
								if commandSeq(tail(boolExpress(t)))==["ERROR"] then ["ERROR"];
								else
									if head(commandSeq(tail(boolExpress(t))))=="ELSE" then
										if commandSeq(tail(commandSeq(tail(boolExpress(t)))))==["ERROR"] then ["ERROR"];
										else commandSeq(tail(commandSeq(tail(boolExpress(t)))));
									else ["ERROR"];
							else ["ERROR"];
					else ["ERROR"];

whileCmd (h:t) = 	if h=="WHILE" then
						if boolExpress(t)==["ERROR"] then ["ERROR"];
						else 
							if commandSeq(boolExpress(t))==["ERROR"] then ["ERROR"];
							else commandSeq(boolExpress(t));
					else ["ERROR"];

assignCmd (h:t) = 	if h=="IDENT" then
						if head(t)=="ASSIGN" then
							if express(tail(t))==["ERROR"] then ["ERROR"];
							else express(tail(t));
						else ["ERROR"];
					else ["ERROR"];

express (h:t) = boolExpress(h:t);

boolJ ([h]) = if h=="BOOLVAL" then [""];
			else if intExpress([h])/=["ERROR"] then intExpress([h]);
			else ["ERROR"];
boolJ (h:t) = if h=="BOOLVAL" then (t);
			else if intExpress(h:t)/=["ERROR"] then intExpress(h:t);
			else ["ERROR"];

boolI ([h]) = if h=="NEG" then ["ERROR"];
			else boolJ([h]);
boolI (h:t) = if h=="NEG" then boolJ(t);
			else boolJ(h:t);

boolH1 (h:t) = 	if h=="LT" then boolH(t);
				else if h=="LTE" then boolH(t);
				else if h=="GT" then boolH(t);
				else if h=="GTE" then boolH(t);
				else (h:t);

boolH (h:t)=if boolI(h:t)==["ERROR"] then ["ERROR"];
			else 
				if boolH1(boolI(h:t))==["ERROR"] then ["ERROR"];
				else boolH1(boolI(h:t));

boolG1 (h:t) = 	if h=="EQ" then boolG(t);
				else if h=="NE" then boolG(t);
				else (h:t);

boolG (h:t)=if boolH(h:t)==["ERROR"] then ["ERROR"];
			else 
				if boolG1(boolH(h:t))==["ERROR"] then ["ERROR"];
				else boolG1(boolH(h:t));

boolF1 (h:t) = if h=="AND" then boolF(t);
			else (h:t);

boolF (h:t)=if boolG(h:t)==["ERROR"] then ["ERROR"];
			else 
				if boolF1(boolG(h:t))==["ERROR"] then ["ERROR"];
				else boolF1(boolG(h:t));
	
boolE (h:t) = if h=="OR" then boolExpress(t);
			else (h:t);
	
boolExpress (h:t) = if boolF(h:t)==["ERROR"] then ["ERROR"];
					else 
						if boolE(boolF(h:t))==["ERROR"] then ["ERROR"];
						else boolE(boolF(h:t));

intF1 ([h]) = if h=="IDENT" then [""];
			else if h=="INTLIT" then [""];
			else ["ERROR"];
intF1 (h:t) = if h=="IDENT" then (t);
			else if h=="INTLIT" then (t);
			else if h=="LP" then
				if boolExpress(t)==["ERROR"] then ["ERROR"];
				else 
					if head(boolExpress(t))=="RP" then tail(boolExpress(t));
					else ["ERROR"];
			else ["ERROR"];

intF ([h]) = if h=="UNARY" then ["ERROR"];
			else intF1([h]);
intF (h:t) = if h=="UNARY" then intF1(t);
			else intF1(h:t);

intT1 (h:t) = if h=="BINDIV" then intT(t);
			else if h=="BINMUL" then intT(t);
			else if h=="BINMOD" then intT(t);
			else (h:t);

intE (h:t) = if h=="BINADD" then intExpress(t);
			else if h=="BINSUB" then intExpress(t);
			else (h:t);

intT ([h]) =if intF([h])==["ERROR"] then ["ERROR"];
			else intF([h]);
intT (h:t) =if intF(h:t)==["ERROR"] then ["ERROR"];
			else 
				if intT1(intF(h:t))==["ERROR"] then ["ERROR"];
				else intT1(intF(h:t));

intExpress (h:t) = 	if intT(h:t)==["ERROR"] then ["ERROR"];
					else 
						if intE(intT(h:t))==["ERROR"] then ["ERROR"];
						else intE(intT(h:t));

callCmd (h:t) = if h=="CALL" then
					if head(t)=="IDENT" then
						if t==["IDENT"] then [];
						else tail(t);
					else ["ERROR"];
				else ["ERROR"];

readCmd (h:t) = if h=="READ" then
					if head(t)=="LP" then
						if head(tail(t))=="IDENT" then
							if head(tail(tail(t)))=="RP" then
								if tail(tail(t))==["IDENT"] then [];
								else tail(tail(tail(t)));
							else ["ERROR"];
						else ["ERROR"];
					else ["ERROR"];
				else ["ERROR"];

printCmd (h:t) = if h=="PRINT" then
					if head(t)=="LP" then
						if head(tail(t))=="IDENT" then
							if head(tail(tail(t)))=="RP" then [""];
							else ["ERROR"];
						else ["ERROR"];
					else ["ERROR"];
				else ["ERROR"];

commandSeq ([]) = [];
commandSeq ([h]) = [h];
commandSeq (h:t) = 	if (h=="LB") then
						if command(t)==["ERROR"] then ["ERROR"];
						else 
							if head(command(t))=="RB" then 
								if command(t)==["RB"] then [];
								else tail(command(t));
							else ["ERROR"];
					else ["ERROR"];

procDecls ([]) = [];
procDecls ([h]) = [h];
procDecls (h:t) = 	if (h=="PROC") then
						if (head(t)=="IDENT") then
							if (block (tail(t)))==["ERROR"] then ["ERROR"];
							else 
								if head(block (tail(t)))=="EOS" then
									if procDecls(tail(block (tail(t))))==["ERROR"] then ["ERROR"];
									else procDecls(tail(block (tail(t))));
								else ["ERROR"];
						else ["ERROR"];
					else (h:t);

bool_varDecls ([]) = [];
bool_varDecls (h:t)=if (h=="BOOL") then
						if (varDef(t)==["ERROR"]) then ["ERROR"];
						else varDef(t);
					else (h:t);

varDef1 ([]) = [];
varDef1 ([h]) = if (h=="EOS") then [];
				else ["ERROR"];
varDef1 (h:t) = if (h=="COMMA") then 
					if (varDef(t)==["ERROR"]) then ["ERROR"];
					else varDef(t);
				else if (h=="EOS") then (t);
				else ["ERROR"];

varDef (h:t) = 	if (h=="IDENT") then 
					if (varDef1(t)==["ERROR"]) then ["ERROR"];
					else varDef1(t);
				else ["ERROR"];

int_varDecls ([]) = [];
int_varDecls (h:t) =if (h=="INT") then
						if (varDef(t)==["ERROR"]) then ["ERROR"];
						else varDef(t);
					else (h:t);

varDecls (h:t) = 	if (int_varDecls (h:t))==["ERROR"] then ["ERROR"];
					else 
						if (bool_varDecls (int_varDecls(h:t)))==["ERROR"] then ["ERROR"];
						else (bool_varDecls (int_varDecls(h:t)));

declarSeq (h:t) = 	if (varDecls (h:t))==["ERROR"] then ["ERROR"];
					else 
						if (procDecls (varDecls(h:t)))==["ERROR"] then ["ERROR"];
						else (procDecls (varDecls(h:t)));

block (h:t) = 	if (declarSeq (h:t))==["ERROR"] then ["ERROR"];
				else 
					if (commandSeq (declarSeq(h:t)))==["ERROR"] then ["ERROR"];
					else (commandSeq (declarSeq(h:t)));
					
program (h:t) = do block (h:t); 

wrapper (h:t) = do print(program (h:t));

process (h:t) res = if h=='(' then res;
				  else process (t) (res ++ [h]);
procWrap (h:t) = process (h:t) "";
procArr [] = []
procArr (h:t) = (procWrap (h)):(procArr (t));

wrapfile str (file) = do appendFile (file) $ str++"\n";
wraptable str (file) =  appendFile (file) $ str++"\n";

main = do 	
			args <- getArgs
			handle <- openFile (head(args)) ReadMode;
			contents <- hGetContents handle;
			print contents;
			let 
				singlewords = lines contents;
			print(procArr(singlewords));
			print(program(procArr(singlewords)));
			print(intExpress(["INTLIT", "BINDIV", "UNARY", "LP", "NEG", "BOOLVAL", "LT", "BOOLVAL", "EQ", "BOOLVAL", "AND", "BOOLVAL" ,"RP", "BINDIV", "INTLIT", "BINSUB", "INTLIT", "BINDIV", "UNARY", "INTLIT", "BINDIV", "INTLIT"]));