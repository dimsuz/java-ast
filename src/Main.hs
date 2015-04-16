import System.IO
import Text.Parsec hiding (token)
import Text.Parsec.String

data InputElement = Whitespace | C Comment | T Token | Unknown String
                    deriving (Eq,Ord,Show)
data Comment = EndOfLineComment (Maybe String) | TraditionalComment (Maybe String)
               deriving (Eq,Ord,Show)
data Token = Keyword String
               deriving (Eq,Ord,Show)

inputElement :: Parser InputElement
inputElement = whitespace <|> comment <|> token <|> unknown

unknown :: Parser InputElement
unknown = do
  result <- many1 (noneOf "\n")
  return $ Unknown result

whitespace :: Parser InputElement
whitespace = do
  result <- many1 space <|> eol
  return $ Whitespace

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
token = do
  result <- keyword
  return $ T (Keyword result)

keyword :: Parser String
keyword = choice $ map (\k -> try $ string k) keywords
          where keywords = ["abstract" ,"continue" ,"for" ,"new" ,"switch"
                           ,"assert" ,"default" ,"if" ,"package" ,"synchronized"
                           ,"boolean" ,"do" ,"goto" ,"private" ,"this" ,"break"
                           ,"double" ,"implements" ,"protected" ,"throw" ,"byte"
                           ,"else" ,"import" ,"public" ,"throws" ,"case" ,"enum"
                           ,"instanceof" ,"return" ,"transient" ,"catch" ,"extends"
                           ,"int" ,"short" ,"try" ,"char" ,"final" ,"interface"
                           ,"static" ,"void" ,"class" ,"finally" ,"long" ,"strictfp"
                           ,"volatile" ,"const" ,"float" ,"native" ,"super" ,"while"]

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

main = do
  contents <- readFile "/home/dima/projects/treto/TretoAndroid/app/src/main/java/ru/treto/tile/NewItemsManager.java"
  let parsed = parseJava contents
  prettyPrint $ filterResult isKeyword parsed
