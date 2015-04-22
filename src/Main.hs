import System.IO
import SyntaxParser
import LexicalParser

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
  -- contents <- readFile "/home/dima/projects/treto/TretoAndroid/app/src/main/java/ru/treto/tile/NewItemsManager.java"
  contents <- readFile "/home/dima/projects/treto/TretoAndroid/app/src/main/java/ru/treto/tile/util/Utils.java"
  let parsed = parseJava contents
  prettyPrint $ parsed
  -- prettyPrint $ filterResult isLiteral parsed
  prettyPrint $ filterResult isUnknown parsed
