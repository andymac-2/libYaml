import Text.Parsec

type Parser = Parsec String ()

runPa :: Parser String -> String -> Either ParseError String
runPa parser = runParser parser () ""

divOrMod :: Parser String
divOrMod = string "div" <|> string "mod"