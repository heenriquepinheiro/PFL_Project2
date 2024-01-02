-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
-- Part 1

import Stack
import Data.List (intercalate, sortBy) -- Import the intercalate function from Data.List module
import Data.Ord (comparing) -- Import the comparing function from Data.Ord module
import Data.Char (isDigit, isSpace, digitToInt, isAlpha, isAlphaNum, isLower)

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]


type Variable = String
type State = [(Variable, Value)]

-- Convert a value to a string
valueToStr :: Value -> String
valueToStr (IntValue x)    = show x
valueToStr (StringValue s) = s
valueToStr (BoolValue b)    = show b

-- Pop all elements from the stack, and return them in a list
popAll :: Stack -> [Value]
popAll s
  | isEmpty s = []
  | otherwise = top s : popAll (pop s)

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = empty

-- Convert a stack to a string
stack2Str :: Stack -> String
stack2Str stack = stackStr
  where
    stackStr = intercalate "," $ map (\x -> valueToStr x) (popAll stack)

-- Create an empty state
createEmptyState :: State
createEmptyState = []

-- Convert a state to a string
state2Str :: State -> String
state2Str state = stateStr
  where
    sortedState = sortBy (comparing fst) state -- Sort by variable name
    stateStr = intercalate "," $ map (\(var, value) -> var ++ "=" ++ valueToStr value) sortedState

-- Function to execute some code, starting from a given stack and state
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)  -- Quando não houver mais instruções, retorna o estado atual
run (code, stack, state) = run (execute (head code) (tail code, stack, state))

-- Function to execute an instruction, given a stack and a state
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


-- Function to push a value n to the stack
pushN :: Integer -> Stack -> Stack
pushN n stack = push (IntValue n) stack

-- Function to push a boolean value True to the stack
true :: Stack -> Stack
true stack = push (BoolValue True) stack

-- Function to push a boolean value False to the stack
false :: Stack -> Stack
false stack = push (BoolValue False) stack

-- Function to add the top two elements of the stack
add :: Stack -> Stack
add stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x + y)) finalStack
        _ -> error "Run-time error"
    _ -> error "Run-time error"

-- Function to subtract the top two elements of the stack
sub :: Stack -> Stack
sub stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x - y)) finalStack
        _ -> error "Run-time error"
    _ -> error "Run-time error"

-- Function to multiply the top two elements of the stack
mult :: Stack -> Stack
mult stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> push (IntValue (x * y)) finalStack
        _ -> error "Run-time error"
    _ -> error "Run-time error"

-- Function to compare the top two elements of the stack. If they are equal, push True to the stack, otherwise push False
eq :: Stack -> Stack
eq stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> if x == y then true finalStack else false finalStack
        _ -> error "Run-time error"
    (BoolValue a, newStack) ->
      case (top newStack, pop newStack) of
        (BoolValue b, finalStack) -> if a == b then true finalStack else false finalStack
        _ -> error "Run-time error"
    _ -> error "Run-time error"

-- Function to compare the top two elements of the stack. If the first is less than or equal to the second, push True to the stack, otherwise push False
le :: Stack -> Stack
le stack =
  case (top stack, pop stack) of
    (IntValue x, newStack) ->
      case (top newStack, pop newStack) of
        (IntValue y, finalStack) -> if x <= y then true finalStack else false finalStack
        _ -> error "Run-time error"
    _ -> error "Run-time error"

-- Function to fetch the value of a variable from the state, and push it to the stack
fetchX :: String -> Stack -> State -> Stack
fetchX x stack state =
  case lookup x state of
    Just value -> push value stack
    Nothing    -> error "Run-time error"

-- Function to execute a branch. If the top of the stack is True, execute the first code, otherwise execute the second code
branch :: Code -> Code -> Code -> Stack -> State -> (Code, Stack, State)
branch c1 c2 code stack state =
  case top stack of
    BoolValue True -> (c1 ++ code, pop stack, state)
    BoolValue False -> (c2 ++ code, pop stack, state)
    _ -> error "Run-time error"

-- Function to store the top of the stack in the state, with the given variable name
storeX :: String -> Stack -> State -> State
storeX x stack state =
  case lookup x state of
    Just _ -> updateValue x stack state
    Nothing -> (x, top stack) : state

-- Function to update the value of a variable in the state
updateValue :: String -> Stack -> State -> State
updateValue x stack state = (x, top stack) : removeValue x state

-- Function to remove a variable from the state
removeValue :: String -> State -> State
removeValue x state = filter (\(key, _) -> key /= x) state

-- Function to negate the top of the stack
neg :: Stack -> Stack
neg stack =
  case (top stack, pop stack) of
    (BoolValue x, newStack) -> push (BoolValue (not x)) newStack
    _ -> error "Run-time error"

-- No operation function
noop :: Stack -> Stack
noop stack = stack

-- Function to perform a logical and between the top two elements of the stack
myand :: Stack -> Stack
myand stack = 
  case (top stack, pop stack) of
      (BoolValue a, newStack) ->
        case (top newStack, pop newStack) of
          (BoolValue b, finalStack) -> push (BoolValue (a && b)) finalStack
          _ -> error "Run-time error"
      _ -> error "Run-time error"


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Test cases for the assembler
testCaseAssembler :: IO ()
testCaseAssembler = do
  print $ testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
  print $ testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  print $ testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
  print $ testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
  print $ testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
  print $ testAssembler [Push (-20),Push (-21), Le] == ("True","")
  print $ testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
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

-- Function to compile a program
compile :: [Stm] -> Code
compile = concatMap compileStm

-- Function to compile a statement
compileStm :: Stm -> Code
compileStm (Assignment var exp) = compA exp ++ [Store var]
compileStm (While cond body) = [Loop (compB cond) (compile body)]
compileStm (If cond bodyTrue bodyFalse) =  (compB cond) ++ [Branch (compile bodyTrue) (compile bodyFalse)]

-- Function to compile an arithmetic expression
compA :: Aexp -> Code
compA (IntLiteral n) = [Push n]
compA (Variable var) = [Fetch var] 
compA (Addd e1 e2) = compA e2 ++ compA e1 ++ [Add]
compA (Subtract e1 e2) = compA e2 ++ compA e1 ++ [Sub]
compA (Multiply e1 e2) = compA e2 ++ compA e1 ++ [Mult]

-- Function to compile a boolean expression
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

-- Function to convert a string into a list of tokens, to be used by the parser
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
  | isAlpha chr && isLower chr = VarTok (chr : takeWhile (\c -> c `notElem` reservedSymbols) restStr) : lexer (dropWhile (\c -> c `notElem` reservedSymbols) restStr)
  where
    reservedSymbols = "+-*();:=<> "

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
-- Function to parse integer or parenthesized tokens into an arithmetic expression
parseIntOrParenExpr :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenExpr (IntTok n : restTokens)
  = Just (IntLiteral n, restTokens)
parseIntOrParenExpr (VarTok var : restTokens)
  = Just (Variable var, restTokens)
parseIntOrParenExpr (OpenP : restTokens1)
  = case parseSubOrSumOrProdOrIntOrPar restTokens1 of
    Just (expr, (CloseP : restTokens2)) ->
      Just (expr, restTokens2)
    Just _ -> Nothing -- no closing paren
    Nothing -> Nothing
parseIntOrParenExpr tokens = Nothing

-- Function to parse a multiplication into an arithmetic expression. It can also parse the same tokens of the arithmetic parsers above
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

-- Function to parse a subtraction or sum into an arithmetic expression. It can also parse the same tokens of the arithmetic parsers above
parseSubOrSumOrProdOrIntOrPar :: [Token] -> Maybe (Aexp, [Token])
parseSubOrSumOrProdOrIntOrPar tokens
  = case parseProdOrIntOrPar tokens of
    Just (expr1, (PlusTok : restTokens1)) ->
      case parseSubOrSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Addd expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr1, (MinusTok : restTokens1)) ->
      case parseSubOrSumOrProdOrIntOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (Subtract expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) ->
      Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result

-------- BOOLEAN PARSER --------
-- Function that parses an arithmetic expression. It is used to parse the arithmetic expressions inside the boolean expressions
parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp tokens = parseSubOrSumOrProdOrIntOrPar tokens

-- Function to parse boolean or parenthesized tokens into a boolean expression
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

-- Function to parse a boolean expression with an arithmetic expression. It can also parse the same tokens of the boolean parsers above
parseAexpOrBoolOrParenExpr :: [Token] -> Maybe (Bexp, [Token])
parseAexpOrBoolOrParenExpr tokens =
  case parseBoolOrParenExpr tokens of
    Just (expr, restTokens) -> Just (expr, restTokens)
    Nothing -> case parseAexp tokens of
      Just (expr, restTokens) -> Just (Aexp expr, restTokens)
      Nothing -> Nothing

-- Function to parse an inequality (<=) into a boolean expression. It can also parse the same tokens of the boolean parsers above
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

-- Function to parse an arithmetic equality (==) into a boolean expression. It can also parse the same tokens of the boolean parsers above
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

-- Function to parse a negation (not) into a boolean expression. It can also parse the same tokens of the boolean parsers above
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

-- Function to parse a boolean equality (=) into a boolean expression. It can also parse the same tokens of the boolean parsers above
parseEqBOrNotOrEqAOrLeOrBoolOrPar :: [Token] -> Maybe (Bexp, [Token])
parseEqBOrNotOrEqAOrLeOrBoolOrPar tokens
  = case parseNotOrEqAOrLeOrBoolOrPar tokens of
    Just (expr1, (EqBoolTok : restTokens1)) ->
      case parseEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
        Just (expr2, restTokens2) ->
          Just (EqualBool expr1 expr2, restTokens2)
        Nothing -> Nothing
    Just (expr, (SemicolonTok : restTokens)) -> Just (expr, [SemicolonTok] ++ restTokens)  
    result -> result

-- Function to parse an and (and) into a boolean expression. It can also parse the same tokens of the boolean parsers above
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


-------- STATEMENTS PARSER --------
-- Function to parse a sequence of statements into a program
parseStatements :: [Token] -> Maybe (Program, [Token])
parseStatements [] = Just ([], [])
parseStatements (OpenP : restTokens1) =
  case parseStatements restTokens1 of
    Just (stmts, (CloseP : SemicolonTok : restTokens2)) ->
      case parseStatements restTokens2 of
        Just (stmts2, finalRestTokens) -> Just (stmts ++ stmts2, finalRestTokens)
        _ -> Nothing
    _ -> Nothing -- no closing paren
parseStatements tokens =
  case parseStatement tokens of
    Just (stmt, restTokens) ->
      case parseStatements restTokens of
        Just (stmts, finalRestTokens) -> Just (stmt : stmts, finalRestTokens)
        _ -> Just ([stmt], restTokens)
    Nothing -> Nothing

parseStatement :: [Token] -> Maybe (Stm, [Token])
-------------- ASSIGNMENT STATEMENT PARSER --------------
-- Function to parse an assignment statement into a statement
parseStatement (VarTok var : AssignTok : restTokens1) =
  case parseSubOrSumOrProdOrIntOrPar restTokens1 of
    Just (expr, SemicolonTok : restTokens2) ->
      Just (Assignment var expr, restTokens2)
    _ -> Nothing

------------------ IF STATEMENT PARSER -----------------
-- Function to parse an if statement into a statement
parseStatement (IfTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
    Just (expr, ThenTok : restTokens2) ->
      case parseThenStatements restTokens2 of
        Just (stmts1, ElseTok : restTokens3) ->
          case parseElseStatements restTokens3 of
            Just (stmts2, finalRestTokens) -> Just (If expr stmts1 stmts2, finalRestTokens)
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing
    
------------------ WHILE STATEMENT PARSER -----------------
-- Function to parse a while statement into a statement
parseStatement (WhileTok : restTokens1) =
  case parseAndOrEqBOrNotOrEqAOrLeOrBoolOrPar restTokens1 of
    Just (expr, DoTok : OpenP: restTokens2) ->
      case parseStatements restTokens2 of
        Just (stmts, CloseP: SemicolonTok: finalRestTokens) -> Just (While expr stmts, finalRestTokens)
        Nothing -> Nothing
    _ -> Nothing

parseStatement tokens = Nothing

-- Function to parse a sequence of statements inside an then statement into a list of statements (program)
parseThenStatements :: [Token] -> Maybe (Program, [Token])
parseThenStatements tokens =
  case tokens of
    OpenP : restTokens1 ->
      case parseStatements restTokens1 of
        Just (stmts, CloseP : restTokens2) -> Just (stmts, restTokens2)
        _ -> Nothing
    _ -> 
      case parseStatement tokens of
        Just (stmt, restTokens) -> Just ([stmt], restTokens)
        _ -> Nothing

-- Function to parse a sequence of statements inside an else statement into a list of statements (program)
parseElseStatements :: [Token] -> Maybe (Program, [Token])
parseElseStatements tokens =
  case tokens of
    OpenP : restTokens1 ->
      case parseStatements restTokens1 of
        Just (stmts, CloseP : SemicolonTok : restTokens2) -> Just (stmts, restTokens2)
        _ -> Nothing
    _ -> 
      case parseStatement tokens of
        Just (stmt, restTokens) -> Just ([stmt], restTokens)
        _ -> Nothing

-- Function to transform a list of tokens into a program
parser :: [Token] -> Program
parser tokens =
  case parseStatements tokens of
    Just (expr, []) -> expr
    _ -> error "Parser error"

-------------------------------------------------------------------------------------------

-- Function to parse a string into a program. It uses the lexer and parser functions
parse :: String -> Program
parse main_code = parser (lexer main_code)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Test cases for the parser
testCaseParser :: IO ()
testCaseParser = do
  -- Test Parser
  print $ testParser "x := 5; x := x - 1;" == ("","x=4")
  print $ testParser "x := 0 - 2;" == ("","x=-2")
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")

