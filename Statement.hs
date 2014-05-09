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
   | Read Expr.T
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

readStmt = accept "read" -# Expr.parse #- require ";" >-> buildReadStmt
buildReadStmt e = Read e

writeStmt = accept "write" -# Expr.parse #- require ";" >-> buildWriteStmt
buildWriteStmt e = Write e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Begin stmt:[]) dict input = exec stmt dict input
exec (Begin stmt:stmts) dict input = exec stmts dict input

exec (While cond (Begin stmts)) dict input =
	if (Expr.value cond dict)>0
	then exec stmts dict input

exec (Read e) dict input = 

instance Parse Statement where
  parse = skip ! assignment ! ifStmt ! begin ! while ! readStmt ! writeStmt
  toString = error "Statement.toString not implemented"