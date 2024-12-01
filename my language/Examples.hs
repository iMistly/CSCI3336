module Examples where
import Syntax

-- 5 / 6 + 7 * 8
e1 :: Expr
e1 = BinExpr (BinExpr (Value (IntVal 5)) (Arith Div) (Value (IntVal 6))) (Arith Add) (BinExpr (Value (IntVal 7)) (Arith Mul) (Value (IntVal 8)))

-- a = 5; b = 6; a + b
p1 :: Prog
p1 = Program ss1

ss1 :: Stmts
ss1 = Sequence s1 (Sequence s2 (End s3))

s1 :: Stmt
s1 = Assign "a" (Value (IntVal 5))

s2 :: Stmt
s2 = Assign "b" (Value (IntVal 6))

s3 :: Stmt
s3 = Print (BinExpr (Ref "a") (Arith Add) (Ref "b"))

-- square(x) = x * x; square(5)
p2 :: Prog
p2 = Program ss2

ss2 :: Stmts
ss2 = Sequence s4 (End s5)

s4 :: Stmt
s4 = Assign "square" (Func "x" IntType (BinExpr (Ref "x") (Arith Mul) (Ref "x")))

s5 :: Stmt
s5 = Print (App (Ref "square") (Value (IntVal 5)))

-- c = 0; while (c <= 5) do { print c; c = c + 1;  }
p3 :: Prog
p3 = Program ss3

ss3 :: Stmts
ss3 = Sequence s6 (End s7)

s6 :: Stmt
s6 = Assign "c" (Value (IntVal 0))

s7 :: Stmt
s7 = While(BinExpr (Ref "c") (Comp Leq) (Value (IntVal 5)))(
        Sequence (Print (Ref "c")) 
        (End(Assign "c" (BinExpr (Ref "c") (Arith Add) (Value (IntVal 1)))))
    )

-- for i = 0; i < 5; i = i + 1 do { print i; }
p4 :: Prog
p4 = Program (End s8)

s8 :: Stmt
s8 = For(
        Assign "i" (Value (IntVal 0)))
        (BinExpr (Ref "i") (Comp Lt) (Value (IntVal 5))) 
        (Assign "i" (BinExpr (Ref "i") (Arith Add) (Value (IntVal 1)))
        )(
            End(Print (Ref "i"))
        )