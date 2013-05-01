--
-- Scratch.hs
--

main :: IO ()
main = do c <- getChar
          putChar c

getL :: IO String
getL = do c <- getChar
          if c == '\n'
             then return ""
             else do l <- getLine
                     return (c:l)

