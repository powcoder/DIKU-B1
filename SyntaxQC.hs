https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
import AST
import Syntax

import Test.QuickCheck
import Control.Monad

-- Your code here
-- Randomly generate a Term,  parseStringTerm transforms it into string
-- and use printTerm to process string into a term，
-- compare whether these two terms are the same
-- if same, then success, if otherwise it will fail.

optable = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])]
alphaFreqList = [ (26, choose ('a', 'z')) ]
digitFreqList = [ (10, choose ('0', '9')) ]

letter = frequency alphaFreqList
letterOrDigit = frequency $ alphaFreqList ++ digitFreqList

-- generate variable name whos first char is a letter
varNameGenerator  = liftM2 (:) letter (sized (\n -> replicateM n letterOrDigit))
tfunNameGenerator = oneof ([liftM2 (:) letter (sized (\n -> replicateM n letterOrDigit))] ++ map pure (allOps optable))

-- randomly choose one of TVar, TNum, TFun 
-- generate a variable name
instance Arbitrary Term where
  arbitrary = oneof 
               [ liftM TVar varNameGenerator
               , liftM TNum arbitrary
               , liftM2 TFun tfunNameGenerator (replicateM 2 arbitrary) ]
-- TFun contains some self-defined operators.
-- check whether the input term and output term is the same
checkTermParser term =
         case (parseStringTerm optable (printTerm optable term)) of
            Left  _ -> False
            Right a -> term == a
  
-- main test function, to make tests runnable as "runhaskell SyntaxQC"
main :: IO ()
main = quickCheck checkTermParser

allOps :: OpTable -> [String]
allOps (OpTable optable) = foldl (\acc x -> acc ++ snd x) [] optable


