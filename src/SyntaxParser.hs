module SyntaxParser where

import LexicalParser
import Text.Parsec
import Text.Parsec.String
import Data.List

data CompilationUnit = PackageDecl TypeName | ImportDecls [ImportDecl] | TypeDecls [TypeDecl]
                     deriving (Eq,Ord,Show)
data ImportDecl = ImportDecl TypeName
                deriving (Eq,Ord,Show)
data TypeDecl = ClassDecl ClassDeclaration | EnumDecl EnumDeclaration
              deriving (Eq,Ord,Show)
data ClassDeclaration =  ClassDeclaration [ClassModifier] Ident [TypeParameter] SuperClass [SuperInterface] ClassBody
                      deriving (Eq,Ord,Show)
data EnumDeclaration = EnumDeclaration String -- TODO
                     deriving (Eq, Ord, Show)
data TypeName = TypeName String
              deriving (Eq,Ord,Show)

data ClassModifier = ClassModifier String
                 deriving (Eq, Ord, Show)
data Ident = Ident InputElement
                 deriving (Eq, Ord, Show)
data TypeParameter = TypeParameter String
                 deriving (Eq, Ord, Show)
data SuperClass = SuperClass ClassType
                 deriving (Eq, Ord, Show)
data SuperInterface = SuperInterface ClassType
                 deriving (Eq, Ord, Show)
data ClassType = ClassType Ident [TypeParameter]
                 deriving (Eq, Ord, Show)
data ClassBody = ClassBody [InputElement] -- TODO
                 deriving (Eq, Ord, Show)

type JParser = GenParser InputElement ()

satisfy' p = tokenPrim showTok nextPos testTok
  where
    showTok t = show t
    testTok t = if p t then Just t else Nothing
    nextPos pos t ts = incSourceColumn pos 1

anyElement :: Monad m => ParsecT [InputElement] u m InputElement
anyElement =  satisfy' p <?> "input element"
  where p el = True

keyword :: Monad m => String -> ParsecT [InputElement] u m InputElement
keyword kw = satisfy' p <?> ("keyword " ++ kw)
  where p el = case el of
          T (Keyword keyword) -> keyword == kw
          _ -> False

identifier :: Monad m => ParsecT [InputElement] u m InputElement
identifier = satisfy' p <?> "identifier"
  where p el = case el of
          T (Identifier _) -> True
          _ -> False

separator :: Monad m => String -> ParsecT [InputElement] u m InputElement
separator s = satisfy' p <?> ("separator '" ++ s ++ "'")
  where p el = case el of
          T (Separator sep) -> sep == s
          _ -> False

-- compilationUnit :: Parser CompilationUnit
-- compilationUnit = packageDeclaration <|> (many importDeclaration) <|> (many typeDeclaration)

packageDeclaration :: JParser CompilationUnit
packageDeclaration = do
  keyword "package"
  result <- typeName
  separator ";"
  return $ PackageDecl result

importDeclaration :: JParser ImportDecl
importDeclaration = do
  keyword "import"
  result <- typeName
  separator ";"
  return $ ImportDecl result

typeDeclaration :: JParser TypeDecl
typeDeclaration = classDeclaration <|> interfaceDeclaration

typeName :: JParser TypeName
typeName = do
  result <- identifier `sepBy` (separator ".")
  let nameParts = map (\(T (Identifier x)) -> x) result
  return $ TypeName (intercalate "." nameParts)

classDeclaration :: JParser TypeDecl
classDeclaration = normalClass <|> enumClass

interfaceDeclaration = undefined

normalClass :: JParser TypeDecl
normalClass = do
  modifiers <- many classModifier
  keyword "class"
  name <- identifier
  typeParams <- classTypeParams
  superClass <- (try $ keyword "extends") >> classType
  superInterfaces <- (try $ keyword "implements") >> (classType `sepBy` (separator ","))
  separator "{"
  classBody <- many anyElement
  separator "}"
  let classDecl = ClassDeclaration
                  modifiers (Ident name) typeParams
                  (SuperClass superClass) (map (\x -> SuperInterface x ) superInterfaces)
                  (ClassBody classBody)
  return $ ClassDecl classDecl

classModifier :: JParser ClassModifier
classModifier = undefined

classTypeParams :: JParser [TypeParameter]
classTypeParams = undefined

classType :: JParser ClassType
classType = undefined

enumClass :: JParser TypeDecl
enumClass = undefined

-- parseSyntax :: String -> Either ParseError CompilationUnit
-- parseSyntax input = parse compilationUnit "unknown" input

parseJava input = parse elements "unknown" input

parseSyntax input = parse importDeclaration "" tokens
                    where tokens = case (parseJava input) of
                            Right toks -> filter (not . isWhitespace) toks
                            _ -> []
                          isWhitespace t = case t of
                            Whitespace -> True
                            _ -> False
