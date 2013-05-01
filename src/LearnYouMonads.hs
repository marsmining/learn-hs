
type Stack = [Int]

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2  
