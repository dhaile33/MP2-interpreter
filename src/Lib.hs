module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp t) env  = IntVal t
eval (BoolExp t) env = BoolVal t

--- ### Variables

eval (VarExp str) env = 
    case H.lookup str env of
        Just v  -> v
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env =
    let v1     = eval e1 env
        v2     = eval e2 env
        Just f = H.lookup op intOps
    in case (op, v2) of
        ("/", IntVal 0) -> ExnVal "Division by 0"
        _               ->liftIntOp f v1 v2

--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env = 
    let v1     = eval e1 env
        v2     = eval e2 env
        Just f = H.lookup op boolOps
    in 
        liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env = 
    let v1     = eval e1 env
        v2     = eval e2 env
        Just f = H.lookup op compOps
    in 
        liftCompOp f v1 v2

--- ### If Expressions

eval (IfExp e1 e2 e3) env =
    case (eval e1 env) of
        BoolVal True  -> eval e2 env
        BoolVal False -> eval e3 env
        _             -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp v e1) env =
    CloVal v e1 env

eval (AppExp e1 e2) env = 
    let
        closure = eval e1 env
        arg     = map (\a -> eval a env) e2
    in case closure of
        CloVal vv e3 cenv -> eval e3 $ H.union(H.fromList(zip vv arg)) cenv
        _                 -> ExnVal "Apply to non-closure"
        
--- ### Let Expressions

eval (LetExp xl ef) env =
    let
        vals = H.fromList $ map(\(k,v) -> (k, eval v env)) xl
    in 
        eval ef (H.union vals env)

--- Statements
--- ----------

exec :: Stmt -> PEnv -> Env -> Result

--- ### Print Statement

exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env = ("", penv, (H.insert var val env))
    where val = eval e env

--- ### Sequencing

exec (SeqStmt (x:xs)) penv env = (p1 ++ p2, penv2, env2)
    where (p1, penv1, env1)    = exec x penv env
          (p2, penv2, env2)    = exec (SeqStmt xs) penv1 env1

exec (SeqStmt []) penv env     = ("", penv, env)
--- ### If Statements

exec (IfStmt e s1 s2) penv env =
    case eval e env of
        BoolVal True  -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _             -> (show $ ExnVal "Condition is not a Bool" , penv, env)
    

--- ### Procedure and Call Statements

exec p@(ProcedureStmt f ps body) penv env = ("", H.insert f p penv, env)

exec (CallStmt f en) penv env =
    let
        el  = map(\e -> eval e env) en
        ff = H.lookup f penv
    in case ff of
        Just (ProcedureStmt _ fps bd) -> exec bd penv (H.union (H.fromList (zip fps el)) env)
        Nothing                       -> ("Procedure "++ f ++" undefined", penv, env)





