import System.IO
import Text.Parsec hiding (token)
import Text.Parsec.String
import Prelude hiding(LT,GT,EQ)
import Data.List (sortBy)

data InputElement = Whitespace | C Comment | T Token | Unknown String
                    deriving (Eq,Ord,Show)
data Comment = EndOfLineComment (Maybe String) | TraditionalComment (Maybe String)
               deriving (Eq,Ord,Show)
data Token = Keyword String | Literal Literal | Separator String | Operator Operator |
             Identifier String
           deriving (Eq,Ord,Show)
-- FIXME implement support for other literals
data Literal = Null | Boolean Bool | StringLiteral String | Character Char | IntLiteral String
               deriving (Eq,Ord,Show)
data Operator = LT | GT | EQ | NEQ | LTEQ | GTEQ | AND | OR | INC | DEC |
                NOT | ADD | SUB | MUL | DIV | BITAND | BITOR | BITXOR |
                BITCOMPL | MODDIV | LSHIFT | RSHIFT | RSHIFTZF |
                CONDTHEN | CONDELSE | ASSIGN | ASSIGNADD | ASSIGNSUB |
                ASSIGNMUL | ASSIGNDIV | ASSIGNAND | ASSIGNOR | ASSIGNMODDIV |
                ASSIGNXOR | ASSIGNLSHIFT | ASSIGNRSHIFT | ASSIGNRSHIFTZF
              deriving (Eq,Ord,Show)

inputElement :: Parser InputElement
inputElement = whitespace <|> comment <|> token <|> unknown

unknown :: Parser InputElement
unknown = do
  result <- many1 $ noneOf "\n (){}[];,.@:"
  return $ Unknown result

whitespace :: Parser InputElement
whitespace = do
  result <- many1 space <|> eol
  return $ Whitespace

separator :: Parser InputElement
separator = do
  result <- choice ([ try (string "..."),
                     try (string "::") ] ++ (map (\c -> string (c:[])) "(){}[];,.@"))
  return $ T (Separator result)

comment :: Parser InputElement
comment = simpleComment <|> multilineComment

simpleComment :: Parser InputElement
simpleComment = do
  try (string "//")
  result <- many (noneOf "\n")
  return $ C (EndOfLineComment (Just result))

multilineComment :: Parser InputElement
multilineComment = do
  try (string "/*")
  result <- (manyTill anyChar (try $ string "*/"))
  return $ C (TraditionalComment (Just result))

token :: Parser InputElement
token = keyword <|> identifier <|> literal <|> separator <|> operator

keyword :: Parser InputElement
keyword = do
  result <- choice $ map (\k -> try $ string k) keywords
  return $ T (Keyword result)
     where keywords = ["abstract" ,"continue" ,"for" ,"new" ,"switch"
                            ,"assert" ,"default" ,"if" ,"package" ,"synchronized"
                            ,"boolean" ,"do" ,"goto" ,"private" ,"this" ,"break"
                            ,"double" ,"implements" ,"protected" ,"throw" ,"byte"
                            ,"else" ,"import" ,"public" ,"throws" ,"case" ,"enum"
                            ,"instanceof" ,"return" ,"transient" ,"catch" ,"extends"
                            ,"int" ,"short" ,"try" ,"char" ,"final" ,"interface"
                            ,"static" ,"void" ,"class" ,"finally" ,"long" ,"strictfp"
                            ,"volatile" ,"const" ,"float" ,"native" ,"super" ,"while"]

operator :: Parser InputElement
operator = do
  result <- choice $ map (\(op,s) -> (try $ string s) >> return op) sortedOperators
  return $ T (Operator result)
     where operators = [
             (LT,"<"), (GT,">"), (EQ,"=="), (NEQ,"!="), (LTEQ,"<="), (GTEQ,">="), (AND,"&&"),
             (OR,"||"), (INC,"++"), (DEC,"--"), (NOT,"!"), (ADD,"+"), (SUB,"-"), (MUL,"*"),
             (DIV,"/"), (BITAND,"&"), (BITOR,"|"), (BITXOR,"^"), (BITCOMPL,"~"),
             (MODDIV,"%"), (LSHIFT,"<<"), (RSHIFT,">>"), (RSHIFTZF,">>>"),
             (CONDTHEN,"?"), (CONDELSE,":"), (ASSIGN,"="), (ASSIGNADD,"+="), (ASSIGNSUB,"-="),
             (ASSIGNMUL,"*="), (ASSIGNDIV,"/="), (ASSIGNAND,"&="), (ASSIGNOR,"|="), (ASSIGNMODDIV,"%="),
             (ASSIGNXOR,"^="), (ASSIGNLSHIFT,"<<="), (ASSIGNRSHIFT,">>="), (ASSIGNRSHIFTZF,">>>=")]
           sortedOperators = sortBy (\(_,s1) (_,s2) -> compare (length s2) (length s1)) operators

identifier :: Parser InputElement
identifier = do
  start <- javaLetter
  rest <- many (javaLetter <|> javaDigit)
  return $ T (Identifier (start : rest))

javaLetter :: Parser Char
javaLetter = oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ "_$"

javaDigit :: Parser Char
javaDigit = digit

literal :: Parser InputElement
literal = do
  result <- nullLiteral <|> booleanLiteral <|> stringLiteral <|> charLiteral <|> numericLiteral
  return $ T (Literal result)

nullLiteral :: Parser Literal
nullLiteral = (try $ string "null") >> return Null

booleanLiteral :: Parser Literal
booleanLiteral = do
  result <- ((try  $ string ("true" :: String)) <|> (try $ string ("false" :: String)))
  return $ if result == "true" then (Boolean True) else (Boolean False)

escapes :: Parser Char
escapes = oneOf "\\\"0nrvtbf" -- all the characters which can be escaped

charNonEscape :: Parser Char
charNonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

-- escape, nonEscape, character, literal functions are taken from this SO answer
-- http://stackoverflow.com/a/24106749/258848
stringEscape :: Parser String
stringEscape = do
    d <- char '\\'
    c <- escapes
    return [d, c]

charEscape :: Parser Char
charEscape = do
  char '\\'
  c <- escapes
  return $ case c of
    '\\' -> '\\'
    '"' -> '"'
    '0' -> '\0'
    'n' -> '\n'
    'r' -> '\r'
    'v' -> '\v'
    't' -> '\t'
    'b' -> '\b'
    'f' -> '\f'


stringCharacter :: Parser String
stringCharacter = fmap return charNonEscape <|> stringEscape

stringLiteral :: Parser Literal
stringLiteral = do
    char '"'
    strings <- many stringCharacter
    char '"'
    return $ StringLiteral (concat strings)

charLiteral :: Parser Literal
charLiteral = do
  char '\''
  result <- charEscape <|> charNonEscape
  char '\''
  return $ Character result

-- it is important to prevent parsing of '0389' to something like 'octliteral 03,intliteral 89',
-- it should instead be invalid input, so make parse of int literals succeed only when sure
-- that when parsed no digit left unconsumed by the parser
numericLiteral :: Parser Literal
numericLiteral = try (do
  result <- integerLiteral
  notFollowedBy digit
  return result)

integerLiteral :: Parser Literal
integerLiteral = do
  result <- binaryLiteral <|> octLiteral <|> decimalLiteral <|>  hexLiteral
  -- suffix can be applied to all types of int literals, accroding to spec
  suffix <- optionMaybe $ oneOf "lL"
  return $ case suffix of
    Just s -> IntLiteral (result ++ [s])
    _ -> IntLiteral result

-- not supporting _ for now
decimalLiteral :: Parser String
decimalLiteral = zeroIntLiteral <|> (many1 $ oneOf ['1'..'9'])

zeroIntLiteral :: Parser String
zeroIntLiteral = do
  char '0'
  return "0"

hexLiteral :: Parser String
hexLiteral = parserZero

-- not supporting _ for now
octLiteral :: Parser String
octLiteral = try $ do
  char '0'
  rest <- many1 octDigit
  return $ '0':rest

binaryLiteral :: Parser String
binaryLiteral = parserZero

eol :: Parser String
eol =  try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

elements :: Parser [InputElement]
elements = many inputElement

parseJava input = parse elements "unknown" input

type ParseResult = Either ParseError [InputElement]

filterResult :: (InputElement -> Bool) -> ParseResult -> ParseResult
filterResult pred result = case result of
  Right elements -> Right (filter pred elements)
  Left error -> Left error

prettyPrint :: ParseResult -> IO ()
prettyPrint result = case result of
  Right elements -> mapM_ (\e -> putStrLn $ show e) elements
  Left error -> putStrLn $ show error

isComment elem = case elem of
  C _ -> True
  otherwise -> False

isKeyword elem = case elem of
  T (Keyword _) -> True
  otherwise -> False

isLiteral elem = case elem of
  T (Literal _) -> True
  otherwise -> False

isUnknown elem = case elem of
  Unknown _ -> True
  otherwise -> False

main = do
  contents <- readFile "/home/dima/projects/treto/TretoAndroid/app/src/main/java/ru/treto/tile/NewItemsManager.java"
  let parsed = parseJava contents
  prettyPrint $ filterResult isUnknown parsed
  --prettyPrint $ parsed
