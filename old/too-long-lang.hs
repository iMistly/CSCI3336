-- Context-free Grammar
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> then <expr>
<var> -> string
<op> -> + | - | * | / | | eq | gt | lt | geq | leq
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> | if <expr> then <expr> else <expr> | func <var> <expr> | <expr> <expr>
<val> -> integers | booleans | strings
-}

newtype Program = Begin Statements

data Statements = End Statement | Sequence Statements Statement

data Statement = Assignment Variable Expression | While Expression Expression

type Variable = String

data Expression = Value Val | BinaryOp Expression Operator Expression | IfThen Expression Expression | IfElse Expression Expression Expression | Function Variable Expression | Application Expression Expression

data Operator = Arith ArithOp | Compare BoolOp | Not

data ArithOp = Add | Sub | Arith2

data Arith2 = Mul | Div

data BoolOp = And | Or | Eq | Gt | Lt | Geq | Leq

data Val = ValI Integer | ValB Bool | ValE String

type Env = [(Variable, Val)]

data Result = Valid Env | Error String

evaluateP :: Program -> Result
evaluateP (Begin ss) = evaluateSS ss[]

evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) = evaluate s env

evaluate :: Expression -> Env -> Val
evaluate (Value v) _ = v

evaluateArith :: Integer -> ArithOp -> Integer -> Integer
evaluateArith x Add y = x + y
evaluateArith x Sub y = x - y
-- evaluate x Arith2 y = case 