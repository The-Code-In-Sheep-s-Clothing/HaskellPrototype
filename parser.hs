import Text.ParserCombinators.Parsec
import Control.Monad (void)

data Program = Program Btype [Block]
               deriving (Eq, Show)

data Block = Empty |
             FunctionBlock Btype [Expr] [Expr] |
             Play [Expr] |
             Board Integer Integer Xtype
             deriving (Eq, Show)


data Btype = TBool Bool | TInt Integer | TSymbol String | TInput String |
             TBoard Block | TPlayer | TPosition (Integer, Integer) | TPositions [(Integer, Integer)]
             deriving (Eq, Show)

data Xtype = Xtype Btype [Btype]
             deriving (Eq, Show)

data Ttype = Ttype [Xtype]
             deriving (Eq, Show)

data Ptype = Xtype' Xtype |
             Ttype' Ttype
             deriving (Eq, Show)

data Ftype = Ftype Ptype Ptype
             deriving (Eq, Show)

data Type = Ptype' Ptype |
            Ftype' Ftype
            deriving (Eq, Show)

data BinaryOperator = Plus | Minus | Compare
                      deriving (Eq, Show)

data Expr = Var Btype |
            Parens Expr |
            Tuple [Expr] |
            FunctionCall Btype [Expr] |
            BinaryOp Expr BinaryOperator Expr |
            Defintion Btype Expr Expr |
            IfElse Expr Expr Expr |
            While Expr Expr
            deriving (Eq, Show)



number :: Parser Integer
number = do
  n <- many1 digit
  return (read n)

symbol :: Parser String
symbol = do
  n <- many1 (noneOf " \t\n|")
  return (n)

emptyline :: Parser ()
emptyline = spaces >> eol >> return ()


ignore :: Parser ()
ignore = (emptyline >> return ()) <|> (comment >> return ())

comment :: Parser ()
comment = string "--" >> endBy (noneOf "\n") eol >> return ()

eol :: Parser ()
eol = void $ char '\n'

program :: Parser Program
program = do
  void $ many ignore
  void $ string "game"
  spaces
  name <- symbol
  spaces
  --void $ many ignore

  b <- many block

  void $ many ignore

  return (Program (TSymbol name) b)

block :: Parser Block
block = try board
    -- <|> try function
    -- <|> try play
    <?> "ERROR?"

--function :: Parser Block
--function = do
  --void $ string "a"
  --return (FunctionBlock "function")

  --TBool Bool | TInt Integer | TSymbol String | TInput String |
  --             TBoard Block | TPlayer | TPosition (Integer, Integer) | TPositions [(Integer, Integer)]
tbool :: Parser Btype
tbool = do
        void $ string "true"
        return (TBool True)
    <|> do
        void $ string "false"
        return (TBool False)

tint :: Parser Btype
tint = do
  n <- number
  return (TInt n)

tsymbol :: Parser Btype
tsymbol = do
  s <- symbol
  return (TSymbol s)


tinput :: Parser Btype
tinput = do
  s <- symbol
  return (TInput s)

btype :: Parser Btype
btype = try tbool
    <|> try tint
    <|> try tsymbol
    <|> try tinput

xtype' :: Parser Btype
xtype' = do
  void $ char '|'
  b <- btype
  return (b)

xtype :: Parser Xtype
xtype = do
  first <- btype
  next <- many xtype'
  return(Xtype first next)


board :: Parser Block
board = do
  void $ string "type"
  spaces
  void $ string "Board"
  spaces
  void $ string "="
  spaces
  void $ string "Grid"
  spaces
  void $ string "("
  spaces
  x <- number
  spaces
  void $ string ","
  spaces
  y <- number
  spaces
  void $ string ")"
  spaces
  void $ string "of"
  spaces
  t <- xtype
  return(Board x y t)

--play :: Parser Block
--play = do
  --void $ string "test"
  --return(Play "test")

parseProgram :: String -> Either ParseError Program
parseProgram input = parse program "(unknown)" input

main :: IO ()
main = return ()
