-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
-- Part 1

import Stack
import Data.List (intercalate) -- Import the intercalate function from Data.List module

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type Variable = String
type State = [(Variable, Value)]

createEmptyStack :: Stack
createEmptyStack = empty

stack2Str :: Stack -> String
stack2Str stack = "[" ++ "stackStr" ++ "]"
{-
stack2Str stack = "[" ++ stackStr ++ "]"
  where
    stackStr = intercalate ", " $ map showStackItem stack
-}
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


execute :: Inst -> (Code, Stack, State) -> (Code, Stack, State)
execute (Push val) (code, stack, state) = (code, pushN val stack, state)
execute Tru (code, stack, state) = (code, true stack, state)
execute Fals (code, stack, state) = (code, false stack, state)
execute Add (code, stack, state) = (code, add stack, state)
execute Sub (code, stack, state) = (code, sub stack, state)
execute Mult (code, stack, state) = (code, mult stack, state)
execute Equ (code, stack, state) = (code, eq stack, state)
execute Le (code, stack, state) = (code, le stack, state)
execute And (code, stack, state) = (code, myand stack, state)
execute Neg (code, stack, state) = (code, neg stack, state)
execute Noop (code, stack, state) = (code, noop stack, state)
execute (Fetch x) (code, stack, state) = (code, fetchX x stack state, state)
execute (Branch c1 c2) (code, stack, state) = branch c1 c2 code stack state
execute (Loop c1 c2) (code, stack, state) = (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ code, stack, state)
execute (Store x) (code, stack, state) = (code, pop stack, storeX x stack state)


pushN :: Integer -> Stack -> Stack
pushN n stack = push (IntValue n) stack

true :: Stack -> Stack
true stack = push (BoolValue True) stack

false :: Stack -> Stack
false stack = push (BoolValue False) stack

add :: Stack -> Stack
add stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x + y)) finalStack
        _ -> error "add: not enough elements on the stack"
    _ -> error "add: not enough elements on the stack"

sub :: Stack -> Stack
sub stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x - y)) finalStack
        _ -> error "sub: not enough elements on the stack"
    _ -> error "sub: not enough elements on the stack"

mult :: Stack -> Stack
mult stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x * y)) finalStack
        _ -> error "mult: not enough elements on the stack"
    _ -> error "mult: not enough elements on the stack"


eq :: Stack -> Stack
eq stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> if x == y then true finalStack else false finalStack
        _ -> error "eq: not enough elements on the stack"
    (BoolValue a, newStack) ->
      case (top newStack, pop newStack) of
        (BoolValue b, finalStack) -> if a == b then true finalStack else false finalStack
        _ -> error "eq: not enough elements on the stack"
    _ -> error "eq: unsupported value types on the stack"

le :: Stack -> Stack
le stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> if x <= y then true finalStack else false finalStack
        _ -> error "le: not enough elements on the stack"
    _ -> error "le: not enough elements on the stack"


fetchX :: String -> Stack -> State -> Stack
fetchX x stack state =
  case lookup x state of
    Just value -> push value stack
    Nothing    -> error $ "fetchX: variable '" ++ x ++ "' not found in state"


branch :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 code stack state =
  case top stack of
    BoolValue True -> (c1 ++ code, pop stack, state)
    BoolValue False -> (c2 ++ code, pop stack, state)
    _ -> error "branch: top of the stack is not a boolean value"


storeX :: String -> Stack -> State -> State
storeX x stack state =
  case lookup x state of
    Just _ -> updateValue x stack state
    Nothing -> (x, top stack) : state

updateValue :: String -> Stack -> State -> State
updateValue x stack state = (x, top stack) : removeValue x state

removeValue :: String -> State -> State
removeValue x state = filter (\(key, _) -> key /= x) state


neg :: Stack -> Stack
neg stack =
  case (top stack, pop stack) of
    (BoolValue x, newStack) -> push (BoolValue (not x)) newStack
    _ -> error "neg: not enough elements on the stack"

noop :: Stack -> Stack
noop stack = stack

myand :: Stack -> Stack
myand stack = 
  case (top stack, pop stack) of
      (BoolValue a, newStack) ->
        case (top newStack, pop newStack) of
          (BoolValue b, finalStack) -> push (BoolValue (a && b)) finalStack
          _ -> error "and: not enough elements on the stack"
      _ -> error "and: unsupported value types on the stack"


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
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

main :: IO ()
main = do
  putStrLn "Hello, World!"
  putStrLn "This is a Haskell program."
  putStrLn "It doesn't do much, but it's a start."
  putStrLn "Have fun!"