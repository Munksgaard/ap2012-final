module SoilParser where

import SoilAst
import Text.ParserCombinators.ReadP

import Data.Char

data Error = ParseError Program String
           | AmbiguousError [Program]
             deriving Show

type Parser a = ReadP a

parse :: ReadP a -> ReadS a
parse = readP_to_S

parseEof :: ReadP a -> ReadS a
parseEof p = parse (do { r <- p; skipSpaces; eof; return r})

reservedKeywords :: [String]
reservedKeywords = [ "let", "from", "case", "of", "if", "then", "else"
                   , "send", "self", "concat", "send", "create"
                   , "become", "with", "to"]

parseString :: String -> Either Error Program
parseString s =
    let
        parses = parse parseDefOps s
        eofParse = parseEof parseDefOps s
    in
      if null eofParse then
          let
              (prog, rest) = last parses
          in
            Left $ ParseError prog rest
      else if length eofParse == 1
           then
               Right $ fst $ head eofParse
           else
               Left $ AmbiguousError $ map fst eofParse

parseFile :: FilePath -> IO (Either Error Program)
parseFile fpath =
    do
      s <- readFile fpath
      return $ parseString s

digit           :: Parser Char
digit            = satisfy isDigit

letter          :: Parser Char
letter           = satisfy isAlpha

space           :: Parser Char
space            = satisfy isSpace

spaces          :: Parser String
spaces           = many space

token           :: Parser a -> Parser a
token p          = spaces >> p

symbol :: String -> Parser String
symbol = token . string

schar :: Char -> Parser Char
schar = token . char

var :: Parser Ident
var = token $ munch1 $ \x -> isAlpha x || '_' == x || isDigit x

parseIdent :: Parser Ident
parseIdent =
    do
      _ <- schar '#'
      var

parseSelf :: Parser Prim
parseSelf =
    do
      _ <- symbol "self"
      return Self

parseName :: Parser Name
parseName =
    do
      s <- var
      if  s `notElem` reservedKeywords && isLetter (head s)
      then
          return s
      else
          pfail

parseConcat :: Parser Prim
parseConcat =
    do
      p1 <- parsePrim
      _ <- symbol "concat"
      p2 <- parsePrim
      return $ Concat p1 p2

parsePrim :: Parser Prim
parsePrim =
    chainl1 parsePrim' (symbol "concat" >>
                        return Concat)
    where
      parsePrim' =
          do
            ident <- parseIdent
            return $ Id ident
          +++
          parseSelf
          +++
          do
            name <- parseName
            return $ Par name
parseArgs :: ReadP [Prim]
parseArgs = sepBy parsePrim (skipSpaces >> char ',' >> skipSpaces)

parseFcall :: ReadP (Prim, [Prim])
parseFcall =
    do
      f <- parsePrim
      args <- between (schar '(') (schar ')') parseArgs
      return (f, args)

parseActOp :: ReadP ActOp
parseActOp =
    do
      _ <- symbol "send"
      args <- between (schar '(') (schar ')') parseArgs
      _ <- symbol "to"
      prim <- parsePrim
      return $ SendTo args prim
    +++ do
      _ <- symbol "create"
      prim <- parsePrim
      _ <- symbol "with"
      (f, args) <- parseFcall
      return $ Create prim f args
    +++ do
      _ <- symbol "become"
      (f, args) <- parseFcall
      return $ Become f args

parseActOps :: ReadP [ActOp]
parseActOps = many parseActOp

parseParams :: ReadP [Name]
parseParams = sepBy parseName (schar ',')

parseExpr :: ReadP Expr
parseExpr =
    do
      _ <- symbol "case"
      prim <- parsePrim
      _ <- symbol "of"
      (cases, wildcard) <- parseCases
      _ <- symbol "end"
      return $ CaseOf prim cases wildcard
    +++ do
      _ <- symbol "if"
      x <- parsePrim
      _ <- symbol "=="
      y <- parsePrim
      _ <- symbol "then"
      e1 <- parseExpr
      _ <- symbol "else"
      e2 <- parseExpr
      _ <- symbol "end"
      return $ IfEq x y e1 e2
    +++ do
      acts <- parseActOps
      return $ Acts acts


parseCases :: ReadP ([([Name], Expr)], Expr)
parseCases =
    do
      parms <- between (schar '(') (schar ')') parseParams
      _ <- schar ':'
      expr <- parseExpr
      (cases, wildcard) <- parseCases
      return ((parms, expr) : cases, wildcard)
    +++ do
      _ <- schar '_'
      _ <- schar ':'
      expr <- parseExpr
      return ([], expr)

parseFunDef :: ReadP Func
parseFunDef =
    do
      _ <- symbol "let"
      ident <- parseIdent
      parms <- between (schar '(') (schar ')') parseParams
      _ <- symbol "from"
      name <- parseName
      _ <- schar '='
      expr <- parseExpr
      _ <- symbol "end"
      return Func { funcname = ident
                  , params = parms
                  , receive = name
                  , body = expr}

parseDefOps :: ReadP ([Func], [ActOp])
parseDefOps =
    do
      funs <- many parseFunDef
      skipSpaces
      acts <- parseActOps
      return (funs, acts)
