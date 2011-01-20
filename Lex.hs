module Lex where
import LexTypes
import Char

horizontalWhitespaceCharacters = " \t"
verticalWhitespaceCharacters   = "\n\r"

isHorizontalWhitespace :: Char -> Bool
isHorizontalWhitespace x = x `elem` horizontalWhitespaceCharacters 

isVerticalWhitespace :: Char -> Bool
isVerticalWhitespace x = x `elem` verticalWhitespaceCharacters

isWhitespace :: Char -> Bool
isWhitespace x = x `elem` (horizontalWhitespaceCharacters ++ verticalWhitespaceCharacters)

atSymbolInnerCharacters = ['A' .. 'Z', 'a' .. 'z', '_']

isAtSymbolInnerCharacter :: Char -> Bool
isAtSymbolInnerCharacter x = x `elem` atSymbolInnerCharacters

isUppercase :: Char -> Bool
isUppercase x = x `elem` ['A' .. 'Z']

isLowercase :: Char -> Bool
isLowercase x = x `elem` ['a' .. 'z']

_receiveToken :: Int -> Int -> String -> InnerToken -> String -> [Token]
_receiveToken lines chars plainToken tok s = Token lines chars tok : _lex lines (chars + n) (drop n s)
                                             where n = length plainToken

_lex :: Int -> Int -> String -> [Token]
_lex l c s | "module" `isPrefix` s  = _receiveToken l c "module"  Module s
           | ":=" `isPrefix` s      = _receiveToken l c ":="      Module s
           | "==" `isPrefix` s      = _receiveToken l c "=="      Module s
           | ">=" `isPrefix` s      = _receiveToken l c ">="      Module s
           | "<=" `isPrefix` s      = _receiveToken l c "<="      Module s
           | "!=" `isPrefix` s      = _receiveToken l c "!="      Module s
           | "and" `isPrefix` s     = _receiveToken l c "and"     Module s
           | "or" `isPrefix` s      = _receiveToken l c "or"      Module s
           | "import" `isPrefix` s  = _receiveToken l c "import"  Module s
           | "export" `isPrefix` s  = _receiveToken l c "export"  Module s
           | "return" `isPrefix` s  = _receiveToken l c "return"  Module s
           | "while" `isPrefix` s   = _receiveToken l c "while"   Module s
           | "for" `isPrefix` s     = _receiveToken l c "for"     Module s
           | "in" `isPrefix` s      = _receiveToken l c "in"      Module s
           | "do" `isPrefix` s      = _receiveToken l c "do"      Module s
           | "enum" `isPrefix` s    = _receiveToken l c "enum"    Module s
           | "variant" `isPrefix` s = _receiveToken l c "variant" Module s
           | "::" `isPrefix` s      = _receiveToken l c "::"      Module s
           | "if" `isPrefix` s      = _receiveToken l c "if"      Module s
           | "else" `isPrefix` s    = _receiveToken l c "else"    Module s
           | "case" `isPrefix` s    = _receiveToken l c "case"    Module s
           | "->" `isPrefix` s      = _receiveToken l c "->"      Module s
           | "<-" `isPrefix` s      = _receiveToken l c "<-"      Module s

_lex lines chars (x : s) | isHorizontalWhitespace x = _lex lines (chars + 1) s
                         | isVerticalWhitespace x   = _lex (lines + 1) 1 s
                         | x == '(' = Token lines chars OParen        : _lex lines (chars + 1) s
                         | x == ')' = Token lines chars CParen        : _lex lines (chars + 1) s
                         | x == '<' = Token lines chars OAngleBracket : _lex lines (chars + 1) s
                         | x == '>' = Token lines chars CAngleBracket : _lex lines (chars + 1) s
                         | x == '+' = Token lines chars Plus          : _lex lines (chars + 1) s
                         | x == '/' = Token lines chars Slash         : _lex lines (chars + 1) s
                         | x == '*' = Token lines chars Asterisk      : _lex lines (chars + 1) s
                         | x == '-' = Token lines chars Dash          : _lex lines (chars + 1) s
                         | x == '%' = Token lines chars Percent       : _lex lines (chars + 1) s
                         | x == '!' = Token lines chars Exclamation   : _lex lines (chars + 1) s
                         | x == '.' = Token lines chars Dot           : _lex lines (chars + 1) s
                         | x == ';' = Token lines chars SemiColon     : _lex lines (chars + 1) s
                         | x == '{' = Token lines chars OBrace        : _lex lines (chars + 1) s
                         | x == '}' = Token lines chars CBrace        : _lex lines (chars + 1) s
                         | x == ',' = Token lines chars Comma         : _lex lines (chars + 1) s
                         | x == '\' = Token lines chars Backslash     : _lex lines (chars + 1) s
                         | x == '|' = Token lines chars Pipe          : _lex lines (chars + 1) s
                         | x == '[' = Token lines chars OSqB          : _lex lines (chars + 1) s
                         | x == ']' = Token lines chars CSqB          : _lex lines (chars + 1) s
                         | x == ':' = Token lines chars Colon         : _lex lines (chars + 1) s
                         | x == '=' = Token lines chars Eq            : _lex lines (chars + 1) s
                         | x == '_' = Token lines chars Underscore    : _lex lines (chars + 1) s
                         | x == '@' = _receiveToken lines (chars + 1) str (AtString str) s 
                                      where str = fst $ span isAtSymbolInnerCharacter s
_lex lines chars s | (isDigit . head) s = let match = matchIntLit s 
					      matchedString = take (fst match) s
					      rest = drop (fst match) s
					  in
					      if "." `isPrefix` rest then
						
					      else _receiveToken lines chars matchedString (IntLiteral (fromJust . snd) match) s
_lex lines chars "" = []


lex :: String -> [Token]
lex a = _lex 1 1 a
