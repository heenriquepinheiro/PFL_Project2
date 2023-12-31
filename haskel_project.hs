-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
-- Part 1

import Stack
import Data.List (intercalate, sortBy) -- Import the intercalate function from Data.List module
import Data.Ord (comparing) -- Import the comparing function from Data.Ord module
import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum) 

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type Variable = String
type State = [(Variable, Value)]

valueToStr :: Value -> String
valueToStr (IntValue x)    = show x
valueToStr (StringValue s) = s
valueToStr (BoolValue b)    = show b

popAll :: Stack -> [Value]
popAll s
  | isEmpty s = []
  | otherwise = top s : popAll (pop s)

createEmptyStack :: Stack
createEmptyStack = empty

stack2Str :: Stack -> String
stack2Str stack = stackStr
  where
    stackStr = intercalate "," $ map (\x -> valueToStr x) (popAll stack)

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = stateStr
  where
    sortedState = sortBy (comparing fst) state -- Sort by variable name
    stateStr = intercalate "," $ map (\(var, value) -> var ++ "=" ++ valueToStr value) sortedState


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
-- Arithmetic Expressions

data Aexp
  = IntLiteral Integer
  | Variable Variable
  | Addd Aexp Aexp
  | Subtract Aexp Aexp
  | Multiply Aexp Aexp
  deriving (Show, Eq)

-- Boolean Expressions
data Bexp
  = BoolLiteral Bool
  | Aexp Aexp
  | Equal Bexp Bexp
  | EqualBool Bexp Bexp
  | LessOrEqual Bexp Bexp
  | Nott Bexp
  | Andd Bexp Bexp
  deriving (Show, Eq)

data Stm
  = Assignment Variable Aexp
  | While Bexp [Stm]
  | If Bexp [Stm] [Stm]
  deriving (Show)

type Program = [Stm]

-- Statements
compile :: [Stm] -> Code
compile = concatMap compileStm

compileStm :: Stm -> Code
compileStm (Assignment var exp) = compA exp ++ [Store var]
compileStm (While cond body) = [Loop (compB cond) (compile body)]
compileStm (If cond bodyTrue bodyFalse) =  (compB cond) ++ [Branch (compile bodyTrue) (compile bodyFalse)]

compA :: Aexp -> Code
compA (IntLiteral n) = [Push n]
compA (Variable var) = [Fetch var] 
compA (Addd e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (Subtract e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (Multiply e1 e2) = compA e2 ++ compA e1 ++ [Mult]

compB :: Bexp -> Code
compB (BoolLiteral True) = [Tru]
compB (BoolLiteral False) = [Fals]
compB (Equal e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (EqualBool e1 e2) = compB e2 ++ compB e1 ++ [Equ]
compB (LessOrEqual e1 e2) = compB e2 ++ compB e1 ++ [Le]
compB (Nott b) = compB b ++ [Neg]
compB (Andd b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Aexp e) = compA e

data Token
  = PlusTok
  | MinusTok
  | TimesTok
  | OpenP
  | CloseP
  | IntTok Integer
  | VarTok Variable
  | BoolTok Bool
  | AssignTok
  | SemicolonTok
  | IfTok
  | ThenTok
  | ElseTok
  | WhileTok
  | DoTok
  | NotTok
  | AndTok
  | EqATok
  | EqBoolTok
  | LeTok
  deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('(' : restStr) = OpenP : lexer restStr
lexer (')' : restStr) = CloseP : lexer restStr
lexer (';' : restStr) = SemicolonTok : lexer restStr
lexer (':':'=' : restStr) = AssignTok : lexer restStr
lexer ('i':'f' : restStr) = IfTok : lexer restStr
lexer ('t':'h':'e':'n' : restStr) = ThenTok : lexer restStr
lexer ('e':'l':'s':'e' : restStr) = ElseTok : lexer restStr
lexer ('w':'h':'i':'l':'e' : restStr) = WhileTok : lexer restStr
lexer ('d':'o' : restStr) = DoTok : lexer restStr
lexer ('n':'o':'t' : restStr) = NotTok : lexer restStr
lexer ('a':'n':'d' : restStr) = AndTok : lexer restStr
lexer ('=':'=' : restStr) = EqATok : lexer restStr
lexer ('=' : restStr) = EqBoolTok : lexer restStr
lexer ('<':'=' : restStr) = LeTok : lexer restStr
lexer ('T':'r':'u':'e' : restStr) = BoolTok True : lexer restStr
lexer ('F':'a':'l':'s':'e' : restStr) = BoolTok False : lexer restStr

lexer (chr : restStr)
  | isSpace chr = lexer restStr
  | isAlpha chr = VarTok (chr : takeWhile isAlphaNum restStr) : lexer (dropWhile isAlphaNum restStr)

lexer str@(chr : _)
  | isDigit chr
  = IntTok (stringToInt digitStr) : lexer restStr
  where
    (digitStr, restStr) = break (not . isDigit) str
    -- convert a string to an integer
    stringToInt :: String -> Integer
    stringToInt=foldl (\acc chr->10*acc+(toInteger (digitToInt chr))) 0
  -- runtime error:
lexer (chr : restString)
  = error ("unexpected character: '" ++ show chr ++ "'")

------------------------------------------- PARSER -------------------------------------------

-------- ARITHMETIC PARSER --------
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLiteral n, restTokens)
parseIntOrParenExpr (VarTok var : restTokens)
  = Just (Variable var, restTokens)
parseIntOrParenExpr (OpenP : restTokens1)
  = case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, (CloseP : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing

parseProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseProdOrIntOrPar tokens
  = case parseIntOrParenExpr tokens of
  Just (expr1, (TimesTok : restTokens1)) ->
    case parseProdOrIntOrPar restTokens1 of
      Just (expr2, restTokens2) ->
        Just (Multiply expr1 expr2, restTokens2)
      Nothing -> Nothing
  Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)
  result -> result

parseSumOrProdOrIntOrPar::[Token] -> Maybe (Aexp, [Token])
parseSumOrProdOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Addd expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusTok : restTokens1)) ->
      case parseSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Subtract expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) ->
      Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result

-------- BOOLEAN PARSER --------

parseAexp::[Token]-> Maybe (Aexp, [Token])
parseAexp tokens = parseSumOrProdOrIntOrPar tokens

parseBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseBoolOrParenExpr (BoolTok n : restTokens)
  = Just (BoolLiteral n, restTokens)
parseBoolOrParenExpr (OpenP : restTokens1)
  = case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
    Just (expr, (CloseP : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseBoolOrParenExpr tokens = Nothing

parseAexpOrBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseAexpOrBoolOrParenExpr tokens =
  case parseBoolOrParenExpr tokens of
    Just (expr, restTokens) -> Just (expr, restTokens)
    Nothing -> case parseAexp tokens of
      Just (expr, restTokens) -> Just (Aexp expr, restTokens)
      Nothing -> Nothing

parseLeOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseLeOrBoolOrPar tokens
  = case parseAexpOrBoolOrParenExpr tokens of
    Just (expr1, (LeTok : restTokens1)) ->
      case parseLeOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (LessOrEqual expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)
    result -> result

parseEqAOrLeOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqAOrLeOrBoolOrPar tokens
  = case parseLeOrBoolOrPar tokens of
    Just (expr1, (EqATok : restTokens1)) ->
      case parseEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Equal expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)
    result -> result

parseNotOrEqAOrLeOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseNotOrEqAOrLeOrBoolOrPar (NotTok: restTokens)
  = case parseEqAOrLeOrBoolOrPar restTokens of
    Just (expr1, restTokens1) ->
      Just (Nott expr1, restTokens1)
    result -> result
parseNotOrEqAOrLeOrBoolOrPar tokens 
  = case parseEqAOrLeOrBoolOrPar tokens of
    Just (expr1, restTokens1) -> Just (expr1, restTokens1)
    result -> result

parseEqBOrNotOrEqAOrLeOrBoolOrPar::[Token] -> Maybe (Bexp, [Token])
parseEqBOrNotOrEqAOrLeOrBoolOrPar tokens
  = case parseNotOrEqAOrLeOrBoolOrPar tokens of
    Just (expr1, (EqBoolTok : restTokens1)) ->
      case parseEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (EqualBool expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result

parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar tokens =
  case parseEqBOrNotOrEqAOrLeOrBoolOrPar tokens of
    Just (expr1, (AndTok : restTokens1)) ->
      case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Andd expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result


-------- STATEMENT PARSER --------
parseSingleStatement :: [Token] -> Maybe (Program, [Token])
parseSingleStatement (VarTok var : AssignTok : restTokens1) =
  case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, SemicolonTok : restTokens2) ->
      Just ([Assignment var expr], restTokens2)
    _ -> Nothing

parseStatement :: [Token] -> Maybe (Program, [Token])
parseStatement [] = Just ([], [])
parseStatement (VarTok var : AssignTok : restTokens1) =
  case parseSumOrProdOrIntOrPar restTokens1 of
    Just (expr, SemicolonTok : restTokens2) ->
      case parseStatement restTokens2 of
        Just (stmts, restTokens3) ->
          Just ([Assignment var expr] ++ stmts, restTokens3) -- CURRENTLY NOT WORKING
        Nothing ->
          Just ([Assignment var expr], restTokens2)
    _ -> Nothing

------------------ IF STATEMENT PARSER -----------------
parseStatement (IfTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
    Just (expr, ThenTok : OpenP : restTokens2) ->
      case parseStatement restTokens2 of
        Just (stmts1, CloseP : ElseTok : OpenP : restTokens3) ->
          case parseStatement restTokens3 of
            Just (stmts2, CloseP : restTokens4) ->
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([If expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, CloseP : ElseTok : restTokens3) ->
          case parseSingleStatement restTokens3 of
            Just (stmts2, restTokens4) ->
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([If expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, restTokens3) ->
          Just ([If expr stmts1 []] , restTokens3)
        Nothing -> Nothing
    Just (expr, ThenTok : restTokens2) ->
      case parseSingleStatement restTokens2 of
        Just (stmts1, ElseTok : OpenP : restTokens3) ->
          case parseStatement restTokens3 of
            Just (stmts2, CloseP:restTokens4) ->
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([If expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, ElseTok : restTokens3) ->
          case parseSingleStatement restTokens3 of
            Just (stmts2, restTokens4) ->
              case parseStatement restTokens4 of
                Just (additionalStmts, finalRestTokens) ->
                    Just ([If expr stmts1 stmts2] ++ additionalStmts, finalRestTokens)
                Nothing -> Nothing
            Nothing -> Nothing
        Just (stmts1, restTokens3) ->
          Just ([If expr stmts1 []] , restTokens3)
        Nothing -> Nothing
    _ -> Nothing

------------------ WHILE STATEMENT PARSER -----------------
parseStatement (WhileTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
    Just (expr, DoTok : OpenP: restTokens2) ->
      case parseStatement restTokens2 of
        Just (stmts, CloseP: SemicolonTok: restTokens3) ->
          case parseStatement restTokens3 of
            Just (additionalStmts, finalRestTokens) ->
              Just ([While expr stmts] ++ additionalStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing
    Just (expr, DoTok : restTokens2) ->
      case parseSingleStatement restTokens2 of
        Just (stmts, restTokens3) ->
          case parseStatement restTokens3 of
            Just (additionalStmts, finalRestTokens) ->
              Just ([While expr stmts] ++ additionalStmts, finalRestTokens)
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing

parseStatement tokens = Just ([], tokens)
    



parser :: [Token] -> Program
parser tokens =
  case parseStatement tokens of
    Just (expr, []) -> expr
    _ -> error "Parse error"

-------------------------------------------------------------------------------------------

parse :: String -> Program
parse main_code = parser (lexer main_code)

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