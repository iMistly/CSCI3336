module Evaluator where
import Syntax
import Examples
import System.IO.Unsafe

concatSequence :: Stmts -> Stmt -> Stmts
concatSequence (End s) su = Sequence s (End su)
sequenceSS (Sequence s1 ss) su = Sequence s1 (concatSequence ss su)

-- I needed a lot of help with this function
lookupVar :: Var -> Env -> Maybe Val
lookupVar _ [] = Nothing
lookupVar x ((_, v, val):env)
  | x == v    = Just val
  | otherwise = lookupVar x env

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
evaluateS (IfThen e ss) env = case evaluate e env of
                                (BoolVal True) -> evaluateSS ss env
                                (BoolVal False) -> Valid env
                                _ -> Error "Condition must be a boolean"
evaluateS (IfThenElse e ss1 ss2) env = case evaluate e env of
                                        (BoolVal True) -> evaluateSS ss1 env
                                        (BoolVal False) -> evaluateSS ss2 env
                                        _ -> Error "Condition must be a boolean"
evaluateS (While check ss) env = case evaluate check env of
                                BoolVal False -> Valid env -- Just return the environment
                                BoolVal True -> case evaluateSS ss env of
                                                    Valid env' -> evaluateS (While check ss) env'
                                                    Error em -> Error em
                                _ -> Error "Condition must result in a boolean"
evaluateS (For init check update ss) env = case evaluateS init env of
                                            Error em -> Error em
                                            Valid env' -> evaluateS (While check (concatSequence ss update)) env'
evaluateS (Print e) env = case evaluate e env of
                            ErrorVal em -> unsafePerformIO(print em >> return (Error em))
                            v -> unsafePerformIO(print v >> return (Valid env))


evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v
evaluate (UnaryExp e) env = case evaluate e env of
                                BoolVal b -> BoolVal (not b)
                                _ -> BoolVal False -- Assume that non-false values are true
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
                                    (IntVal v1, IntVal v2) -> case op of
                                                                Arith a -> IntVal (evalBinOp v1 a v2)
                                                                Comp c -> BoolVal (evalCompOp v1 c v2)
                                    (BoolVal v1, BoolVal v2) -> case op of
                                                                Logic l -> BoolVal (evalLogicOp v1 l v2)
                                    _ -> ErrorVal "Invalid operation or operand types"
evaluate (App (Func x t e1) e2) env = case (evaluate e2 env, t) of
                                        (IntVal vi, IntType) -> evaluate e1 ((IntType, x, IntVal vi) : env)
                                        (BoolVal vb, BoolType) -> evaluate e1 ((BoolType, x, BoolVal vb) : env)
                                        (StringVal vs, StringType) -> evaluate e1 ((StringType, x, StringVal vs) : env)
                                        _ -> ErrorVal "Type mismatch in function application"
evaluate (Ref x) env = case lookupVar x env of
                        Nothing -> ErrorVal "Variable not in scope"
                        Just v -> v

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