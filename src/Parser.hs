module Parser where

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple = letter

run :: Show a => Parser a -> String -> IO ()
run p input = case (parse p "" input) of
                   Left err -> do putStr "parse error at "
                                  print err
                   Right x -> print x

openClose :: Parser Char
openClose = char '(' >> char ')'

parens :: Parser ()
parens = do { char '('
            ; parens
            ; char ')'
            ; parens }
           <|> return ()

testOr3 = do { try (string "(a"); char ')'; return "(a)" }
          <|> string "(b)"
