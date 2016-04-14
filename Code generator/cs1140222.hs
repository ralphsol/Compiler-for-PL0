import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)
import Data.Char (isAlphaNum)
import Data.Char (isNumber)
import Data.Char (toUpper)
import System.Environment
import System.IO
import Control.Monad

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getVal (h:t) (str) = 	if(fst(h)==str) then snd(h);
						else (getVal (t) (str));
getVal ([]) (str) = "0";

putEnt (h:t) (res) = putEnt (t) (res++h++"\n");
putEnt ([]) (res) = res;

genarr (h:t) (n) (resh:rest) = genarr (t) (n+1) ((n, h):resh:rest);
genarr ([]) (n) (resh:rest) = resh:rest;

caller (h:t) (n) (varh:vart) (resh:rest) = 	
								if (Map.member n (Map.fromList(h:t))) then
									if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="DECLARE_INT") then (caller (h:t) (n+1) (Map.toList( Map.insert (head(tail ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) ) ("0") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="DECLARE_BOOL") then (caller (h:t) (n+1) (Map.toList( Map.insert (head(tail ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="DECLARE_PROC") then (caller (h:t) (n+1) (Map.toList( Map.insert (head(tail ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) ) (head(tail(tail ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) )) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="ASSIGN") then
										if (Map.member (head(tail((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))))) (Map.fromList(varh:vart))) then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (getVal (varh:vart) (head(tail ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ))) (Map.fromList(varh:vart)) )) (resh:rest));
										else (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) (head(tail((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="READ") then (caller (h:t) (n+1) (Map.toList(Map.insert ("Init") (putEnt (tail(wordsWhen (=='\n') (getVal (varh:vart) "Init"))) ("")) ((Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n)))) (head ((wordsWhen (=='\n') (getVal (varh:vart) ("Init"))))) (Map.fromList(varh:vart)))))) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="GOTO") then (caller (h:t) (read(head(tail(tail(wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))::Int) (varh:vart) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="CALL") then (caller (h:t) (read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int) (Map.toList( Map.insert ("proc_call") (show(n+1)) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="EQ") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart)))== (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="NEQ") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))) /= (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="LT") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))) < (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="LTE") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))) <= (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="GT") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))) > (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="GTE") then 
										if((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))) >= (Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="AND") then 
										if(((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart)))==Just("tt"))&&((Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))==Just("tt"))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="OR") then 
										if(((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart)))==Just("tt"))||((Map.lookup (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))) (Map.fromList(varh:vart)))==Just("tt"))) then
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											 (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="PLUS") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show((read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int)+(read(getVal (varh:vart) (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))))::Int))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="MINUS") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show((read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int)-(read(getVal (varh:vart) (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))))::Int))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="MULT") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show((read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int)*(read(getVal (varh:vart) (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))))::Int))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="DIV") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show(div (read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int) (read(getVal (varh:vart) (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))))::Int))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="MOD") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show(mod (read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int) (read(getVal (varh:vart) (head(tail(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))))::Int))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="IF") then 
										if ((Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart)))==Just("tt")) then
											(caller (h:t) (n+1) (varh:vart) (resh:rest));
										else
											(caller (h:t) (read(head(tail(tail(wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )))::Int) (varh:vart) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="UPLUS") then (caller (h:t) (n+1) (varh:vart) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="UMINUS") then (caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) (show((read(getVal (varh:vart) (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))))::Int)*(-1))) (Map.fromList(varh:vart)) )) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="NOT") then 
										if(Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))==Just("tt")) then
											(caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("ff") (Map.fromList(varh:vart)) )) (resh:rest));
										else if(Map.lookup (head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) (Map.fromList(varh:vart))==Just("ff")) then
											(caller (h:t) (n+1) (Map.toList( Map.insert (last ((wordsWhen (==' ') (Map.fromList(h:t)Map.!n))) ) ("tt") (Map.fromList(varh:vart)) )) (resh:rest));
										else
											(caller (h:t) (n+1) (varh:vart) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="RETURN") then (caller (h:t) (read(getVal (varh:vart) ("proc_call"))::Int) (varh:vart) (resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="PRINT") then 
--										WRITE TO THE FILE
										(caller (h:t) (n+1) (varh:vart) ((getVal (varh:vart)(head(tail( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) ))) ):resh:rest));
									else if (head( (wordsWhen (==' ') (Map.fromList(h:t)Map.!n)) )=="END_OF_CODE") then (resh:rest);
									else (resh:rest);
								else (resh:rest);

wrapfile (str) (outfile) = do appendFile (outfile) $ str++"\n";
wrapper (h:t) res = wrapper(t) (res++h++"\n");
wrapper([]) res = res;
write (h:t) (outfile) = do writeFile (outfile) (""); (wrapfile (wrapper (init(h:t)) ("")) (outfile));

main = do 	infile <- getLine;
			outfile <- getLine;
			handle <- openFile infile ReadMode;
			contents <- hGetContents handle;
--			print(genarr (wordsWhen (=='\n') contents) (0) ([(1000, "")]));
			val <- getContents;
			(write (caller (genarr (wordsWhen (=='\n') contents) (0) ([(1000, "")]))  0 [("Init", val)] [""]) (outfile));