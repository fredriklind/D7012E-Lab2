module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
     Assignment String Expr.T 
   | Skip
   | If Expr.T Statement Statement
   | Begin [Statement]
   | While Expr.T Statement
   | Read String
   | Write Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIfStmt
buildIfStmt ((e, s1), s2) = If e s1 s2

begin = accept "begin" -# iter parse #- require "end" >-> buildBeginStmt
buildBeginStmt (s) = Begin s

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhileStmt
buildWhileStmt (e, s) = While e s

readStmt = accept "read" -# word #- require ";" >-> buildReadStmt
buildReadStmt e = Read e

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWriteStmt
buildWriteStmt e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec [] _ _ = []
exec (Begin stmt:stmts) dict input = exec (stmt++stmts) dict input

exec (While cond stmt: stmts) dict input = 
      if (Expr.value cond dict)>0
      then exec (stmt:While cond stmt:stmts) dict input
      else exec stmts dict input

exec (Read str:stmts) dict (i:input) = exec stmts (Dictionary.insert (str, i) dict) input 
exec (Write expr:stmts) dict input = (Expr.value expr dict):exec stmts dict input 

exec (Assignment str expr :stmts) dict input = exec stmts (Dictionary.insert (str, Expr.value expr dict) dict) input

exec (Skip:stmts) dict input = exec stmts dict input 

instance Parse Statement where
  parse = skip ! assignment ! ifStmt ! begin ! while ! readStmt ! writeStmt
  toString (Assignment str expr) = str ++ " := " ++ (Expr.toString expr) ++ "\n"
  toString (Skip) = "skip;\n"
  toString (If expr s1 s2) = "if " ++ (Expr.toString expr) ++ " then\n\t" ++ toString s1 ++ "else\n" ++ toString s2
  toString (Begin stmts) = "begin\n" ++ concat (map toString stmts) ++ "end;\n" 
  toString (While cond stmt) = "while " ++ (Expr.toString cond) ++ " do\n" ++ toString stmt 
  toString (Read str) = "read " ++ str ++ "\n"
  toString (Write expr) = "write " ++ (Expr.toString expr) ++ "\n" 