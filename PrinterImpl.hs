https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module PrinterImpl where

import AST
import ParserImpl
import Data.List

-- do not change the type!
    
printTerm :: OpTable -> Term -> String
printTerm optable term = printTerm' optable term False

optable = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])]
str2 = " 3*(x+f(y,4)+1+2-zzz)\n\t"
term2 = let (Right result) = parseStringTerm optable str2 in result

str3 = "3*(x+f(y,4)+z)\n\t"
term3 = let (Right result) = parseStringTerm optable str3 in result

-- write as few parentheses as possible，
-- check the priority of operator, 
--if the priority of outer one is equal to the inner one
-- then no needs to add parenthess
printTerm' :: OpTable -> Term -> Bool -> String
printTerm' _ (TVar x) _                 = x
printTerm' _ (TNum x) _                 = case  x > 0 of 
                                          False -> "~" ++ show (abs x)
                                          True  -> show x
printTerm' optable (TFun fname terms) b = case isInoptable optable fname of
                                      True -> case b of  
                                              True -> "(" ++ printTerm' optable ft (par fname ft) ++ fname ++ printTerm' optable st (par fname st)++ ")"
                                              _    -> printTerm' optable ft (par fname ft) ++ fname ++ printTerm' optable st (par fname st)
                                      _    -> fname ++ "(" ++ termsInTFun terms ++ ")"
                                      where 
                                            termsInTFun :: [Term] -> String
                                            termsInTFun [term]   = printTerm' optable term False
                                            termsInTFun (h:tail) = printTerm' optable h False ++ "," ++ termsInTFun tail
                                            ft = terms!!0
                                            st = terms!!1
                                            par :: String -> Term -> Bool
                                            par s (TVar _)   = False
                                            par s (TNum _)   = False 
                                            par s (TFun f _) = case isInoptable optable f of 
                                                                  True -> sv >= fv 
                                                                  _    -> False   
                                                               where  
                                                                 (Just sv) = findIndex (==s) (allOps optable)
                                                                 (Just fv) = findIndex (==f) (allOps optable)                                                                  


isInoptable :: OpTable -> FName -> Bool
isInoptable optable fname = elem fname (allOps optable)

allOps :: OpTable -> [String]
allOps (OpTable optable) = foldl (\acc x -> acc ++ snd x) [] optable

