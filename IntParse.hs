module IntParse where

data IntLitStr = HexLit String | BinLit String | OctLit String | DecLit String | NoNumber

parseReverseIntegerString :: String -> String -> Integer
parseReverseIntegerString "" _ = 0
parseReverseIntegerString x:xs base = (fromJust . (findIndex (== x))) base +
                                      (length base) * (parseReverseIntegerString base xs)

parseIntegerString :: String -> String -> Integer
parseIntegerString base '0':xs = parseIntegerString base xs
parseIntegerString base x      = parseReverseIntegerString base (reverse x)

parseIntLit :: IntLitStr -> Maybe Integer
parseIntLit (HexLit x) = Just $ parseIntegerString "0123456789ABCDEF" x
parseIntLit (BinLit x) = Just $ parseIntegerString "01" x
parseIntLit (OctLit x) = Just $ parseIntegerString "01234567" x
parseIntLit (DecLit x) = Just $ parseIntegerString "0123456789" x
parseIntLit NoNumber = Nothing

usedCharacters :: IntLitStr -> Int
usedCharacters (HexLit x) = 2 + length x
usedCharacters (BinLit x) = 2 + length x
usedCharacters (OctLit x) = 2 + length x
usedCharacters (DecLit x) = length x
usedCharacters NoNumber = 0

isInRange :: String, Char -> Bool
isInRange r x = x `elem` r

isHex = isInRange ['0' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f']
isBin = isInRange "01"
isOct = isInRange ['0' .. '7']
isDec = isInRange ['0' .. '9']

matchIntegerLiteral :: String -> IntLitStr
matchIntegerLiteral x | "0x" `isPrefix` x = HexLit (takeWhile isHex $ drop 2 x)
                      | "0X" `isPrefix` x = HexLit (takeWhile isHex $ drop 2 x)
                      | "0b" `isPrefix` x = BinLit (takeWhile isBin $ drop 2 x)
                      | "0B" `isPrefix` x = BinLit (takeWhile isBin $ drop 2 x)
                      | "0o" `isPrefix` x = OctLit (takeWhile isOct $ drop 2 x)
                      | "0O" `isPrefix` x = OctLit (takeWhile isOct $ drop 2 x)
                      | otherwise         = let x = takeWhile isDec $ drop 2 x in
                                                if x then DecLit x
                                                     else NoNumber

matchIntLit :: String -> (Int, Maybe Integer)
matchIntLit s = (usedCharacters x, parseIntLit x)
                where x = matchIntegerLiteral s

