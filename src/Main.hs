import System.IO
import Text.Parsec
import Text.Parsec.String

line :: Parser String
line = do
  result <- many (noneOf "\n")
  eol
  return result

eol :: Parser Char
eol = char '\n'

file = do
  result <- many line
  eof
  return result

parseJava input = parse file "unknown" input

main = do
  contents <- readFile "/home/dima/projects/treto/TretoAndroid/app/src/main/java/ru/treto/tile/NewItemsManager.java"
  putStrLn (show $ parseJava contents)
