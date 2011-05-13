module LexTypes where

data InnerToken = OParen | CParen 
                         | ColonEq
                         | Eq
                         | DoubleEq
                         | GEq
                         | LEq
                         | OAngleBracket
                         | CAngleBracket
                         | NEq
                         | Plus
                         | Slash
                         | Asterisk
                         | Dash
                         | Percent
                         | And
                         | Or
                         | Exclamation
                         | IntLiteral Integer
                         | RealLiteral Double
                         | Module
                         | ModuleName [String]
                         | Import
                         | Export
                         | Dot
                         | TypeName String
                         | Return
                         | While
                         | For
                         | In
                         | Do
                         | Enum
                         | SemiColon
                         | OBrace
                         | CBrace
                         | Variant
                         | Comma
                         | Backslash
                         | StringLiteral String
                         | CharacterLiteral Char
                         | AtSymbol String
                         | Symbol String
                         | DoubleColon
                         | If
                         | Else
                         | Case
                         | Underscore
                         | Pipe
                         | RightArrow
                         | LeftArrow
                         | OSqB
                         | Colon
                         | CSqb deriving (Show, Read, Eq)
-- line number, column number, token type
data Token = Token Int Int InnerToken deriving (Show, Read, Eq)

