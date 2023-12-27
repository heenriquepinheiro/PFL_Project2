-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
-- Part 1

import Stack

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type Variable = String
type State = [(Variable, Int)]

fetchX :: Inst -> Stack -> State -> (Inst, Stack, State)
fetchX (Fetch var) stack state =
  case lookup var storage of
    Just value -> (Push (fromIntegral value), push (Left (fromIntegral value)) stack, storage)
    Nothing    -> error $ "Variable " ++ var ++ " not found in storage"

fetchX instr stack storage = (instr, stack, storage)

fetchX :: Inst -> Stack -> State -> (Inst, Stack, State)
fetchX (Fetch var) stack state =
  case lookup var state of
    Just value -> (Push value, push value stack, state)
    Nothing    -> error $ "Variable " ++ var ++ " not found in storage"

fetchX instr stack state = (instr, stack, state)



createEmptyStack :: Stack
createEmptyStack = empty

stack2Str :: Stack -> String
stack2Str (Stk stack) = "[" ++ stackStr ++ "]"
  where
    stackStr = intercalate ", " $ map showStackItem stack

showStackItem :: Either Int String -> String
showStackItem (Left intValue) = show intValue
showStackItem (Right strValue) = strValue


createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = "{" ++ stateStr ++ "}"
  where
    stateStr = intercalate ", " $ map (\(var, value) -> var ++ ": " ++ show value) state

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- Quando não houver mais instruções, retorna o estado atual
run (code, stack, state) = run (execute (head code) (tail code, stack, state))


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, store2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")