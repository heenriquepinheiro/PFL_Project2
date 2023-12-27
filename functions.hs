import Stack
import Data.List (intercalate)


data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type State = [(String, Value)]


createEmptyStack :: Stack
createEmptyStack = empty

createEmptyState :: State
createEmptyState = []

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
execute (Fetch x) (code, stack, state) = (code, fetchX x stack state, state)
execute (Branch c1 c2) (code, stack, state) = branch c1 c2 stack state



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
        (IntValue y, finalStack) -> push (IntValue (y - x)) finalStack
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


branch :: Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 stack state =
  case top stack of
    BoolValue True -> (c1, pop stack, state)
    BoolValue False -> (c2, pop stack, state)
    _ -> error "branch: top of the stack is not a boolean value"


