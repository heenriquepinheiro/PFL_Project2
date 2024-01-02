module Stack (Stack, Value (IntValue, StringValue, BoolValue), push, pop, top, empty, isEmpty) where

data Value = IntValue Integer | StringValue String | BoolValue Bool deriving (Show, Eq)

data Stack = Stk [Value] deriving Show

push :: Value -> Stack -> Stack
push x (Stk xs) = Stk (x : xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> Value
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack
empty = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _)  = False
