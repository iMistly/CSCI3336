module Evaluator where
import Syntax
import Examples
import System.IO.Unsafe

sequenceSS :: Statements -> Stmt -> Statements
sequenceSS (End s) su = Seq s (End su)
sequenceSS (Seq s1 ss) su = Seq s1 (sequenceSS ss su)

evaluateP :: Program -> Result
evaluateP (BeginEnd ss) = evaluateSS ss []

evaluateSS :: Statements -> Env -> Result
evaluateSS (End s) env = evaluateS s env
evaluateSS (Seq s ss) env = case evaluateS s env of
                                Valid env' -> evaluateSS ss env'
                                Error s -> Error s
        -- evaluateSS ss (evaluateS s env)

evaluateS :: Stmt -> Env -> Result
evaluateS (Assign x t e) env = case (evaluate e env, t) of
                                (ValI vi, TypeI) -> Valid ((x, ValI vi) : env)
                                (ValB vb, TypeB) -> Valid $ (x, ValB vb) : env -- $ is shorcut for () til end of line
                                (ValI _, TypeB) -> Error $ "Type mismatch for " ++ x -- _ is placeholder since we don't use it in the returning statement ->
                                (ValB _, TypeI) -> Error $ "Type mismatch for " ++ x
                                (ValE s, _) -> Error s
evaluateS (While e ss) env = case evaluate e env of -- evaluate expression e in the given env
                                ValE em -> Error em -- Best practice to start with the mroe specific cases at the top and more general cases at the bottom
                                ValB False -> Valid env -- If it's false just return the updated environment
                                ValI 0 -> Valid env
                                _ -> case evaluateSS ss env of -- We only check if it's false, if it is literally anything else that is valid then continue
                                    Error em -> Error em
                                    Valid env' -> evaluateS (While e ss) env'
evaluateS (For si e su ss) env = case evaluateS si env of -- Statement init | Expression Check | Statement Update | Set of Statements
                                    Error em -> Error em
                                    Valid env' -> evaluateS (While e (sequenceSS ss su)) env' 
                                    -- A for loop is basically a while loop. We already initialized, 
                                    -- so all we have to do is concatnate the update statement to the end of 
                                    -- the statements that are set to run. The check for expression e occurs in the While loop! :)
evaluateS (Print e) env = case evaluate e env of
                            ValE em -> unsafePerformIO(print em >> return (Error em))
                            v -> unsafePerformIO(print v >> return (Valid env)) -- We aren't updating the environment but we have to return one 

evaluateOp:: Int -> Op -> Int -> Val
evaluateOp i1 Add i2 = ValI (i1 + i2)
evaluateOp i1 Sub i2 = ValI (i1 - i2)
evaluateOp i1 Mul i2 = ValI (i1 * i2)
evaluateOp i1 Div i2 = ValI (i1 `div` i2)
evaluateOp i1 GEq i2 = ValB (i1 >= i2)

evaluate :: Expr -> Env -> Val
evaluate (Value v) _ = v
evaluate (IfElse c e1 e2) env = case evaluate c env of
                                ValB True -> evaluate e1 env
                                ValB False -> evaluate e2 env
                                ValI 0 -> evaluate e2 env
                                ValI _ -> evaluate e1 env
                                ValE s -> ValE s
                                --ValI i -> error "condition should be a boolean expression"
evaluate (BinExpr e1 op e2) env = case (evaluate e1 env, evaluate e2 env) of
                                (ValI i1, ValI i2) -> evaluateOp i1 op i2
                                (ValE em1, _) -> ValE em1
                                (_, ValE em2) -> ValE em2
                                _ -> ValE "operands should be integer"
evaluate (Ref x) env = case lookup x env of
                        Nothing -> ValE "Variable not in scope"
                        Just v -> v
evaluate (App (Func x t e) e2) env = case (evaluate e2 env, t) of
                                        (ValI vi, TypeI) -> evaluate e ((x, ValI vi) : env)
                                        (ValB vb, TypeB) -> evaluate e ((x, ValB vb) : env)
                                        (ValI _, TypeB) -> ValE $ "Type mismatch for " ++ x
                                        (ValB _, TypeI) -> ValE $ "Type mismatch for " ++ x
                                        (ValE s, _) -> ValE s
evaluate _ env = ValE "undefined" -- Func or App e1 e2 where e1 is not a function
