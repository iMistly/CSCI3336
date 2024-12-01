-- Context-free Grammar
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr> | while <expr> do <stmts> | for i<stmt> c<expr> u<stmt> do <stmts> | print <expr>
<var> -> string
<op> -> <arithmaticOp> | <comparisonOp> | <logicOp>
<arithmaticOp> -> + | - | * | /
<comparisonOp> -> eq | gt | lt | geq | leq
<logicOp> -> And | Or
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> | if <expr> then <expr> else <expr> | func <var> <expr> | <expr> <expr>
<val> -> integers | booleans | strings
-}
data Prog = Program Stmts -- Prog is a data type while Program is a constructor
    deriving Show

data Stmts = End Stmt 
           | Sequence Stmt Stmts
    deriving Show

data Stmt = Assign Var Expr 
          | While Expr Stmts 
          | For Stmt Expr Stmt Stmts
          | Print Expr
    deriving Show

type Var = String

data ArithOp = Add | Sub | Mul | Div -- Arithmatic Operators
    deriving Show

data CompOp = Eq | Gt | Lt | Geq | Leq -- Comparison Operators
    deriving Show

data LogicOp = And | Or -- Logical Operators
    deriving Show

data Op = Arith ArithOp | Comp CompOp | Logic LogicOp
    deriving Show

data Expr = Value Val -- just a value
          | UnaryExp Expr -- not logical operator
          | BinExpr Expr Op Expr -- binary expression
          | IfThen Expr Expr
          | IfThenElse Expr Expr Expr
          | Func Var Type Expr -- function definition
          | App Expr Expr -- function application
          | Ref Var -- variable reference
    deriving Show

data Val = IntVal Int | BoolVal Bool | StringVal String | ErrorVal String-- Int, Bool and String are predefined types in Haskell
    deriving Show

data Type = IntType | BoolType | StringType
    deriving Show

type Env = [(Type, Var, Val)]

data Result = Valid Env | Error String
    deriving Show

concatSequence :: Stmts -> Stmt -> Stmts
concatSequence (End s) su = Sequence s (End su)
sequenceSS (Sequence s1 ss) su = Sequence s1 (concatSequence ss su)

evaluateP :: Prog -> Result
evaluateP (Program ss) = evaluateSS ss []

evaluateSS :: Stmts -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Sequence s ss) env = case evaluateS s env of
                                    Valid env' -> evaluateSS ss env'
                                    Error s -> Error s

evaluateS :: Stmt -> Env -> Result
evaluateS (Assign x e) env = case evaluate e env of
                                IntVal vi -> Valid $ (IntType, x, IntVal vi) : env
                                BoolVal vb -> Valid $ (BoolType, x, BoolVal vb) : env
                                StringVal vs -> Valid $ (StringType, x, StringVal vs) : env
                                ErrorVal em -> Error em
evaluateS (While check ss) env = case evaluate check env of
                                BoolVal False -> Valid env -- Just return the environment
                                BoolVal True -> case evaluateSS ss env of
                                                    Valid env' -> evaluateS (While check ss) env'
                                                    Error em -> Error em
                                _ -> Error "Condition must result in a boolean"
evaluateS (For init check update ss) env = case evaluateS init env of
                                            Error em -> Error em
                                            Valid env' -> evaluateS (While check (concatSequence ss update)) env'


evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v
evaluate (UnaryExp e) env = case evaluate e env of
                                BoolVal b -> BoolVal (not b)
                                _ -> ErrorVal ""
evaluate (BinExpr e1 (Arith op) e2) env = case (evaluate e1 env, evaluate e2 env) of
                                            (IntVal v1, IntVal v2) -> IntVal (evalBinOp v1 op v2)
                                            _ -> ErrorVal "Arithmetic operation must be applied to integers"
evaluate (BinExpr e1 (Comp op) e2) env = case (evaluate e1 env, evaluate e2 env) of
                                            (IntVal v1, IntVal v2) -> BoolVal (evalCompOp v1 op v2)
                                            _ -> ErrorVal "Comparison operation must be applied to integers"
evaluate (BinExpr e1 (Logic op) e2) env = case (evaluate e1 env, evaluate e2 env) of
                                            (BoolVal v1, BoolVal v2) -> BoolVal (evalLogicOp v1 op v2)
                                            _ -> ErrorVal "Logical operation must be applied to booleans"
evaluate (IfThen e1 e2) env = case evaluate e1 env of
                                (BoolVal True) -> evaluate e2 env
                                (BoolVal False) -> BoolVal False
                                _ -> ErrorVal "Condition must be a boolean"
evaluate (IfThenElse e1 e2 e3) env = case evaluate e1 env of
                                        (BoolVal True) -> evaluate e2 env
                                        (BoolVal False) -> evaluate e3 env
                                        _ -> ErrorVal "Condition must be a boolean"



evalBinOp :: Int -> ArithOp -> Int -> Int
evalBinOp x Add y = x + y
evalBinOp x Sub y = x - y
evalBinOp x Mul y = x * y
evalBinOp x Div y = x `div` y

evalCompOp :: Int -> CompOp -> Int -> Bool
evalCompOp x Eq  y = x == y
evalCompOp x Gt  y = x > y
evalCompOp x Lt  y = x < y
evalCompOp x Geq y = x >= y
evalCompOp x Leq y = x <= y

evalLogicOp :: Bool -> LogicOp -> Bool -> Bool
evalLogicOp x And y = x && y
evalLogicOp x Or y  = x || y

-- Redundant
--evalUnaryOp :: Bool -> Bool
--evalUnaryOp x = not x

-- p :: Prog -- x=30 While x > 2 then print x
-- p = Program (Sequence (Assign "x" (Value (IntVal 30)))
--                   (While (BinExpr (Ref "x") (Comp Gt) (Value (IntVal 2)))
--                          (Sequence (Print (Ref "x"))
--                                    (Assign "x" (BinExpr (Ref "x") (Arith Sub) (Value (IntVal 1)))))))