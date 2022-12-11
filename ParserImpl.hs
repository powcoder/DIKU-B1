https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module ParserImpl where

import AST
import Text.ParserCombinators.Parsec
import Control.Monad()
import Data.Char()

--optable = OpTable [(FNone, ["<=", "<"]),(FLeft, ["+", "-"]),(FLeft, ["*"]),(FRight, ["**"])]

parseStringTerm :: OpTable -> String -> Either ErrMsg Term
parseStringTerm optable str = case pterm of 
                           Left val  -> Left (show val)
                           Right val -> Right val
                        where 
                              pterm    = parse (spaces *> (term optable) <* eof) "" str
parseStringCmds :: OpTable -> String -> Either ErrMsg [Cmd]
parseStringCmds optable str = case pcmds of 
                           Left val  -> Left (show val)
                           Right val -> Right val
                        where 
                              pcmds    = parse (spaces *> (cmds optable) <* eof) "" str

----------------------------------------------------------------------
--Process integer and string
--process positive integer, use many1 to process multiple digit
posNat :: Parser Integer
posNat = token_ (read <$> many1 digit)

--process negative integer, reservered '~' and represents a negative sign '-'
negNat :: Parser Integer
negNat = do
          reserved "~"
          num <- token_ (many1 digit)
          return (-1*(read num))
    
natural :: Parser Integer
natural = try negNat <|> try posNat

--parse variable: first number is letter, otherwise could be letter or number
var' :: Parser String
var' = do
    head <- letter
    tail <- many (letter <|> digit)
    return (head:tail) 

var :: Parser String    
var = token_ var'

--separator 
sep :: Parser String
sep =  reserved ","

token_ :: Parser a -> Parser a
token_ p = do { a <- p; 
                spaces;
                return a
              }

reserved :: String -> Parser String
reserved s = token_ (string s)

--transform parenthess
parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

----------------------------------------------------------------------

--recursively get a term
getTerm :: Parser Term -> [(Fixity,Parser (Term -> Term -> Term))] -> Parser Term
getTerm p []       = p
getTerm p (h:tail)
      | fst h == FRight = getTerm (chainl1_ p (snd h)) tail
      | otherwise       = getTerm (chainr1_ p (snd h)) tail

leftops :: OpTable -> [(Fixity,Parser (Term -> Term -> Term))]
leftops (OpTable l)= [(fix,choice $ mapOp ops)|(fix , ops) <- l]

mapOp :: [String] -> [Parser (Term -> Term -> Term)]
mapOp ops = map (\x -> infixOp x (opCal x)) ops 

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = try (reserved x) >> return f

opCal :: String -> Term -> Term -> Term
opCal op term1 term2 = TFun op [term1,term2]

chainl1_ :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1_ p op        = scan
                    where
                      scan      = do{ x <- p; rest x }
                      rest x    = do{ f <- op
                                    ; y <- scan
                                    ; return (f x y)
                                    }
                                <|> return x
    
chainr1_ :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1_` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a
---------------------------------------------------------
--use getTerm to generate a Term
term :: OpTable -> Parser Term
term optable = getTerm (factor optable) (reverse (leftops optable)) 

--anaylse Opteable except operator
factor :: OpTable -> Parser Term
factor optable= try (TFun <$> var <*> parens (termz optable))
            <|> try (TNum <$> natural)
            <|> try (TVar <$> var)
            <|> try (parens (term optable))

--termz could be null according to the gramma tree
termz :: OpTable -> Parser [Term]
termz optable = sepBy (term optable) sep

--terms could not be numm
terms :: OpTable -> Parser [Term]
terms optable = sepBy1 (term optable) sep 

---------------------------------------------------------
--Define the gramma tree
cmds :: OpTable -> Parser [Cmd]
cmds optable = try (endBy (cmd optable) (reserved "\n")) <|> try (sepBy (cmd optable) (reserved "\n"))

cmd :: OpTable -> Parser Cmd
cmd optable = try (CRule <$> (rule optable))
          <|> try (CQuery <$> (term optable) <*> bool)             

bool :: Parser Bool      
bool = (try (reserved "??") >> return True) <|>(try (reserved "?") >> return False)   
          
rule :: OpTable -> Parser Rule    
rule optable = try (rule2 optable) <|> try (rule1 optable)

-- deal with rule: Term '=' Term '.'          
rule1 :: OpTable -> Parser Rule
rule1 optable = do 
                 term1 <- term optable
                 reserved "="
                 term2 <- term optable
                 string "."
                 return (Rule term1 term2 [])

-- deal with rule: Term '=' Term '|' Conds '.'
rule2 :: OpTable -> Parser Rule
rule2 optable = do 
                 term1 <- term optable
                 reserved "="
                 term2 <- term optable
                 reserved "|"
                 conds <- conds optable
                 string "."
                 return (Rule term1 term2 conds)
    
conds :: OpTable -> Parser [Cond]
conds optable = sepBy1 (cond optable) sep

cond :: OpTable -> Parser Cond
cond optable = try (cond2 optable) <|> try (cond1 optable)

-- deal with cond: pname '(' Termz ')'
cond1 :: OpTable -> Parser Cond
cond1 optable = do
                 pname <- var
                 pterm <- parens (termz optable)
                 return (Cond pname pterm [])

-- deal with cond:  pname '(' Termz ';' Terms ')' 
cond2 :: OpTable -> Parser Cond
cond2 optable = do
                 pname  <- var
                 reserved "("
                 pterm1 <- termz optable
                 reserved ";"
                 pterm2 <- terms optable
                 reserved ")"
                 return (Cond pname pterm1 pterm2)