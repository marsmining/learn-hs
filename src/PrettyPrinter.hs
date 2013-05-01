module PrettyPrinter where

import Data

prettyPrint :: Expr -> String
prettyPrint (Const i) = show i
prettyPrint (Add l r) =    prettyPrint l
                        ++ " + "
                        ++ prettyPrint r
