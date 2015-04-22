module SyntaxParser (parseJava) where

import LexicalParser as LP
import Text.Parsec

parseJava input = parse LP.elements "unknown" input
