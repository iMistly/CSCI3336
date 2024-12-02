module Syntax where
-- Context-free Grammar
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | if <expr> then <stmts> | if <expr> then <stmts> else <stmts> | 
        while <expr> do <stmts> | for i<stmt> c<expr> u<stmt> do <stmts> | print <expr>
<var> -> string
<op> -> <arithmaticOp> | <comparisonOp> | <logicOp>
<arithmaticOp> -> + | - | * | /
<comparisonOp> -> eq | gt | lt | geq | leq
<logicOp> -> And | Or
<expr> -> <val> | <expr> <op> <expr> | func <var> <expr> | app <expr> <expr>
<val> -> integers | booleans | strings
-}
data Prog = Program Stmts -- Prog is a data type while Program is a constructor

data Stmts = End Stmt 
           | Sequence Stmt Stmts

data Stmt = Assign Var Expr 
          | IfThen Expr Stmts
          | IfThenElse Expr Stmts Stmts
          | While Expr Stmts 
          | For Stmt Expr Stmt Stmts -- init, check, update, body
          | Print Expr

type Var = String

data ArithOp = Add | Sub | Mul | Div -- Arithmatic Operators
    deriving Show

data CompOp = Eq | Gt | Lt | Geq | Leq -- Comparison Operators
    deriving Show

data LogicOp = And | Or -- Logical Operators
    deriving Show

data Op = Arith ArithOp | Comp CompOp | Logic LogicOp

data Expr = Value Val -- just a value
          | UnaryExp Expr -- not logical operator
          | BinExpr Expr Op Expr -- binary expression
          | Func Var Type Expr -- function definition
          | App Expr Expr -- function application
          | Ref Var -- variable reference

-- Int, Bool and String are predefined types in Haskell
data Val = IntVal Int
         | BoolVal Bool
         | StringVal String
         | ErrorVal String

-- The type is different from the value itself. The type is used for type checking
data Type = IntType | BoolType | StringType

type Env = [(Type, Var, Val)]

data Result = Valid Env | Error String
    deriving Show

precedence :: Op -> Int
precedence (Arith Add) = 0
precedence (Arith Sub) = 0
precedence (Arith Mul) = 1
precedence (Arith Div) = 1
precedence (Comp Eq) = -1
precedence (Comp Gt) = -1
precedence (Comp Lt) = -1
precedence (Comp Geq) = -1
precedence (Comp Leq) = -1
precedence (Logic And) = -2
precedence (Logic Or) = -2

-- Paranthesis for clarity
requiresParenth :: Op -> Expr -> Bool
requiresParenth op (BinExpr _ o2 _) = precedence op < precedence o2
requiresParenth _ _ = False

instance Show Prog where
    show (Program ss) = "main() {\n" ++ show ss ++ "\n}"

instance Show Stmts where
    show (End s) = "    " ++ show s ++ ";"
    show (Sequence s ss) = "    " ++ show s ++ ";\n" ++ show ss

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal b) = show b
    show (StringVal s) = s
    show (ErrorVal e) = "!ERROR!\n" ++ e ++ "\n!ERROR!"

instance Show Stmt where
    show (Assign v e) = v ++ " = " ++ show e
    show (IfThen e ss) = "if " ++ show e ++ " then\n" ++ show ss
    show (IfThenElse e ss1 ss2) = "if " ++ show e ++ " then\n" ++ show ss1 ++ " else\n" ++ show ss2
    show (While e ss) = "while(" ++ show e ++ "){\n" ++ show ss ++ "\n}"
    show (For init check update ss) = "for(" ++ show init ++ "; " ++ show check ++ "; " ++ show update ++ "){\n" ++ show ss ++ "\n}"
    show (Print e) = "print " ++ show e

instance Show Expr where
    show (Value v) = show v
    show (UnaryExp e) = "not " ++ show e
    show (BinExpr e1 op e2) = if requiresParenth op e2 then
                                show e1 ++ " " ++ show op ++ " (" ++ show e2 ++ ")"
                                else show e1 ++ " " ++ show op ++ " " ++ show e2
    show (Func x t e) = "func " ++ x ++ " : " ++ show t ++ " -> " ++ show e
    show (App e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
    show (Ref x) = x

instance Show Op where
    show (Arith Add) = "+"
    show (Arith Sub) = "-"
    show (Arith Mul) = "*"
    show (Arith Div) = "/"
    show (Comp Eq) = "=="
    show (Comp Gt) = ">"
    show (Comp Lt) = "<"
    show (Comp Geq) = ">="
    show (Comp Leq) = "<="
    show (Logic And) = "&&"
    show (Logic Or) = "||"

instance Show Type where
    show IntType = "int"
    show BoolType = "bool"
    show StringType = "string"