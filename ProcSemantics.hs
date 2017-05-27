module ProcSemantics where

import ProcParser
import Prelude hiding (Num)


type Z        = Integer
type T        = Bool
type State    = Var -> Z
type EnvP     = Pname -> Stm






--------------------------------------------------------------------------------
-- Numerals
--------------------------------------------------------------------------------
-- Recall:
--      Type Num = Integer
--      Type Z   = Integer
nVal :: Num -> Z
nVal n = n


--------------------------------------------------------------------------------
-- Atithmetic Expressions
--
-- data Aexp = N Num
--           | V Var
--           | Mult Aexp Aexp
--           | Add Aexp Aexp
--           | Sub Aexp Aexp
--           deriving (Eq, Show)
--------------------------------------------------------------------------------

-- The semantics of a Arthmetic expression in a particular state
-- Recall:
--      type State = Var -> Z
--      type Z     = Integer
aVal :: Aexp -> State -> Z
aVal  (N numeral )  state = nVal   numeral
aVal  (V variable)  state = state  variable                     -- state is a function from vars to integers
aVal  (Mult a1 a2)  state = (aVal a1 state) * (aVal a2 state)   --
aVal  (Add  a1 a2)  state = (aVal a1 state) + (aVal a2 state)
aVal  (Sub  a1 a2)  state = (aVal a1 state) - (aVal a2 state)


--------------------------------------------------------------------------------
-- Boolean Expressions
--
-- data Bexp = TRUE
--           | FALSE
--           | Neg Bexp
--           | And Bexp Bexp
--           | Le Aexp Aexp
--           | Eq Aexp Aexp
--           deriving (Eq, Show)
--------------------------------------------------------------------------------

-- Evaluates a Bexp, and returns a single boolean value in  a particular state
-- Recall:
--      type State = Var -> Z
--      type T     = Boolean
--      type Z     = Integer
bVal :: Bexp -> State -> T
bVal (TRUE       )  state = True
bVal (FALSE      )  state = False
bVal (Neg b1     )  state = not (bVal b1 state)
bVal (And b1 b2  )  state = (bVal b1 state) && (bVal b2 state)
bVal (Eq  b1 b2  )  state = (aVal b1 state) == (aVal b2 state)
bVal (Le  b1 b2  )  state = (aVal b1 state) <= (aVal b2 state)


-- TODO  Document Page 51
-- Recall:
--   type DecV  = [(Var, Aexp)]
--   type Var   = String
dv :: DecV -> [Var]
dv []         = []
dv ((x,_):xs) = x:(dv xs)

--------------------------------------------------------------------------------
-- Helpfull General functions
--
-- Generalised usefull functions that have appered out of repitioon of simular
-- repetitive functions
--------------------------------------------------------------------------------

-- TODO Document
-- Recall:
--      type State = Var -> Z
-- In the context of restoring variables:
--      restore :: State    -> [Var] -> State    -> State
--      restore :: (Var->Z) -> [Var] -> (Var->Z) -> (Var->Z)
-- The function takes the new state, a list of variable names,  the current
-- state, and the first variable section, `x` of the returned state. The
-- function then checks whether `x` is an element of the vars list, and decides
-- whch state to return accordingly
-- More info on Page 52
restore :: (Eq a) => (a -> b) -> [a] -> (a -> b) -> (a -> b)
restore newState vars currState  x = if (x `elem` vars)
                                        then currState  x
                                        else newState   x

-- TODO Document
-- Recall:
--      EnvP  :: Pname -> Stm
--      State :: Var   -> Z
-- Simular to the `restore` function.
-- Examples for updating preocedures and variables
--      update_Proc ::    Stm -> Pname    -> EnvP         -> EnvP
--      update_Proc ::    Stm -> Pname    -> (Pname->Stm) -> (Pname->Stm)
--      update_Var  ::    Z   -> Var      -> State        -> State
--      update_Var  ::    Z   -> (Var->Z) -> (Var->Z)     -> (Var->Z)
update ::  (Eq b) => a -> b -> (b -> a) -> (b -> a)
update  i v s x = if (x==v)
                  then i
                  else s x

--  Folds over a list calling the update function
-- Recall:
--      type DecP  = [(Pname, Stm)]
--      type EnvP  =  Pname -> Stm
-- keepUpdating_proc :: Decp           -> EnvP -          > EnvP
-- keepUpdating_proc :: [(Pname, Stm)] -> (Pname -> Stm) -> (Pname -> Stm)
keepUpdating :: (Eq a) => [(a, b)]     -> (a -> b)       -> (a -> b)
keepUpdating dps env =  foldr( \(n,s)  -> update s n) env  dps

--------------------------------------------------------------------------------
-- Dynamic Natural Semantics
--------------------------------------------------------------------------------

-- The function that initally gets called by `s_dynamic` with an empty enviroment,
-- and then recursivly calls itself to get the final output state
sDynamic :: EnvP -> Stm  -> State -> State
-- Dynamic evaluation for `skip`
sDynamic env (Skip              ) state = state
-- Dynamic evaluation for `assignment`
sDynamic env (Ass var aexp      ) state = update (aVal aexp state) var state
-- Dynamic evaluation for `compsition`
sDynamic env (Comp s1 s2        ) state = finalState where
        interimState  = sDynamic env s1 state
        finalState    = sDynamic env s2 interimState
-- Dynamic evaluation for an `if` conditional
sDynamic env (If bexp s1 s2     ) state = if (bVal bexp state) then sDynamic env s1 state else sDynamic env s2 state
-- Dynamic evaluation for a `while` loop
sDynamic env (While bexp s1     ) state = if (bVal bexp state) then sWhile3               else state  where
            sWhile2 = sDynamic env s1 state
            sWhile3 = sDynamic env (While bexp s1) sWhile2
-- Dynamic evaluation for a `block`
sDynamic env (Block decV decP statement) state = finalState where
    listOfVarToInt = map (\(var, aexp) -> (var , aVal aexp state)) decV
    state'         = keepUpdating listOfVarToInt state
    newEnv         = keepUpdating decP env
    newState       = sDynamic newEnv statement state'
    finalState     = restore newState  (map fst decV) state
-- Dynamic evaluation for `call`
sDynamic env (Call name     ) state = sDynamic env statement state where
    statement = env name


-- The main semantics for dynamic.
-- The function passes the arguments to sDynamic, along with a empty enviroment
s_dynamic :: Stm -> State -> State
s_dynamic statement s = sDynamic (\ _ -> undefined)  statement s



--------------------------------------------------------------------------------
-- Mixed Natural Semantics
--------------------------------------------------------------------------------

newtype MEnvP = MEnvP (Pname -> (Stm, MEnvP, DecP))


-- Updates a mixed enviroment
-- NOTE: The conventional `update` function cannot be used because of the
--       different type of enviroment (MEnvP)
-- Recall:
--      newtype MEnvP = MEnvP (Pname -> (Stm, MEnvP))
updateMixedEnv :: Stm -> Pname -> DecP-> MEnvP -> MEnvP
updateMixedEnv statement name dps env@(MEnvP envFunction) = MEnvP $ update (statement, env, dps) name envFunction


-- TODO Document
-- NOTE: The conventional `update` function cannot be used because of the
--       different type of enviroment (MEnvP)
keepUpdatingMixedEnv :: [(Pname, Stm)] -> MEnvP -> MEnvP
keepUpdatingMixedEnv dps menv = foldl newMEnvP menv dps  where
        newMEnvP :: MEnvP -> (Pname, Stm) -> MEnvP
        newMEnvP env (name, stm) =  updateMixedEnv stm name dps env


-- The function that initally gets called by `s_mixed` with an empty enviroment,
-- and then recursivly calls itself to get the final output state
sMixed :: MEnvP -> Stm  -> State -> State
-- Mixed evaluation for `skip`
sMixed env (Skip              ) state = state
-- Mixed evaluation for `assignment`
sMixed env (Ass var aexp      ) state = update (aVal aexp state) var state
-- Mixed evaluation for `composition`
sMixed env (Comp s1 s2        ) state = finalState where
        interimState  = sMixed env s1 state
        finalState    = sMixed env s2 interimState
-- Mixed evaluation for a `if` conditional
sMixed env (If bexp s1 s2     ) state = if (bVal bexp state) then sMixed env s1 state else sMixed env s2 state
-- Mixed evaluation for a `while` loop
sMixed env (While bexp s1     ) state = if (bVal bexp state) then sWhile3             else state  where
            sWhile2 = sMixed env s1 state
            sWhile3 = sMixed env (While bexp s1) sWhile2
-- Mixed evaluation for a `block`
sMixed env (Block decV decP statement) state = finalState where
    listOfVarToInt = map (\(var, aexp) -> (var , aVal aexp state)) decV -- [(Var , Integer)]
    state'         = keepUpdating listOfVarToInt state
    newEnv         = keepUpdatingMixedEnv decP env
    newState       = sMixed newEnv statement state'
    finalState     = restore newState  (map fst decV) state

-- Mixed evaluation for `call`
sMixed env@(MEnvP envFunction) (Call name) state =  state' where
    (body, mEnv, dps) = envFunction name
    -- mEnv' = updateMixedEnv body name  mEnv
    mEnv'  = keepUpdatingMixedEnv dps mEnv
    state' = sMixed mEnv' body state


-- The main semantics for mixed.
-- The function passes the arguments to sMixed, along with a empty enviroment
-- TODO Mutual Recursion doesnt work
s_mixed :: Stm -> State -> State
s_mixed statement s = sMixed (MEnvP (\ _ -> undefined))  statement s



--------------------------------------------------------------------------------
-- Static Natural Semantics
--
-- Not Complete!
--------------------------------------------------------------------------------

-- TODO: Finnish Static evaluation


newtype SEnvP = SEnvP (Pname -> (Stm, SEnvV, SEnvP))
type    SEnvV = Var -> Loc
type    Loc   = Integer
-- TODO Should be:
--      Store = Loc `union` {next} ->  Z
--type    Store = Store {new :: Loc, sto :: (Loc->Z)}
-- Given a `Loc`ation, returns the next one.
-- As in this example, out Locations are integers, `new` is simplly the
-- successor function on the Integers
new :: Loc -> Loc
new n = n+1

-- TODO Document
sStatic :: EnvP -> SEnvV -> Stm  -> State -> State
-- Static evaluation for `skip`
sStatic envp evnv (Skip              ) state = state
-- Static evaluation for `assignment`
sStatic envp evnv (Ass var aexp      ) state = update (aVal aexp state) var state
-- Static evaluation for `composition`
sStatic envp evnv (Comp s1 s2        ) state = finalState where
                         interimState  = sStatic envp evnv s1 state
                         finalState    = sStatic envp evnv s2 interimState
-- Static evaluation for an `if` conditional
sStatic envp evnv (If bexp s1 s2     ) state = if (bVal bexp state) then sStatic envp evnv s1 state else sStatic envp evnv s2 state
-- Static evaluation for a `while` loop
sStatic envp evnv (While bexp s1     ) state = if (bVal bexp state) then finalState              else state  where
            interimState = sStatic envp evnv s1 state
            finalState = sStatic envp  evnv(While bexp s1) interimState
-- Static evaluation for `block`
sStatic envp evnv (Block decV decP statement) state = undefined                       -- TODO undefined
-- Static evaluation for `call`
sStatic envp evnv (Call name                ) state = undefined                       -- TODO undefined


-- The main semantics for mixed.
-- The function passes the arguments to sMixed, along with a empty enviroment
s_static :: Stm -> State -> State
s_static statement s = sStatic (\ _ -> undefined) (\_ -> 0)  statement s
