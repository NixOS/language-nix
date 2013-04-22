{- |
   Module      :  Language.Nix
   Copyright   :  (c) 2013 Peter Simons
   License     :  BSD3
   Maintainer  :  simons@cryp.to
 -}

module Language.Nix
  (
    -- * Running the Parser
    Parser, parse,

    -- * Nix Language AST
    Expr(..), Attr(..), genIdentifier,

    -- * Nix Language Parsers
    identifier,
  --   expr, listExpr, term, operatorTable, listOperatorTable, identifier, literal,
  --   nixString, literalURI, attrSet, scopedIdentifier, attribute, list, letExpr,
  --   attrSetPattern,

    -- * Parsec Language Specification
    symbol, lexeme, parens, braces, brackets, natural,
    assign, semi, dot, whitespace
  )
  where

import Text.ParserCombinators.UU hiding ( parse, Apply, optional )
import Text.ParserCombinators.UU.BasicInstances hiding ( Parser )
import Text.ParserCombinators.UU.Utils
import Text.Printf
import Data.Char
import Data.Either

-- import Prelude hiding ( lookup )
-- import Data.Functor.Identity
-- import Control.Applicative ( (<$>), (<*>), (<$), (<*), (*>) )
-- import Text.Parsec hiding ( parse )
-- import qualified Text.Parsec as Parsec
-- import qualified Text.Parsec.Language as Parsec
-- import qualified Text.Parsec.Token as Parsec
-- import Text.Parsec.Expr
import Test.QuickCheck hiding ( Str )

-- import Text.Show.Functions ( )
-- import Control.Monad.Reader
-- import qualified Control.Monad.Error as ErrT
-- import Control.Monad.Error hiding ( Error )
-- import qualified Data.Map as Map ( )
-- import Data.Map hiding ( map, foldr )
--
-- -- import Debug.Trace
-- trace :: a -> b -> b
-- trace _ b = b

----- Nix Language Definition for UU-Parsinglib -------------------------------

type Parser a = P (Str Char String LineColPos) a

-- nixLanguage :: LanguageDef
-- nixLanguage = Parsec.emptyDef
--   { Parsec.commentStart    = "/*"
--   , Parsec.commentEnd      = "*/"
--   , Parsec.commentLine     = "#"
--   , Parsec.nestedComments  = False
--   , Parsec.identStart      = letter <|> oneOf "_"
--   , Parsec.identLetter     = alphaNum <|> oneOf "-_"
--   , Parsec.opStart         = Parsec.opLetter nixLanguage
--   , Parsec.opLetter        = oneOf ".!{}[]+=?&|/:"
--   , Parsec.reservedOpNames = [".","!","+","++","&&","||","?","=","//","==","!=",":"]

reservedNames :: [String]
reservedNames = ["rec","let","in","import","with","inherit","assert","or","if","then","else"]

symbol :: String -> Parser String
symbol = pSymbol

parens :: Parser a -> Parser a
parens = pParens

braces :: Parser a -> Parser a
braces = pBraces

brackets :: Parser a -> Parser a
brackets = pBrackets

natural :: Parser Integer
natural = pNatural

assign :: Parser Char
assign = lexeme (pSym '=')

semi :: Parser Char
semi = lexeme (pSym ';')

dot :: Parser Char
dot = pDot

-- commaSep1 :: Parser a -> Parser [a]
-- commaSep1 = Parsec.commaSep1 nixLexer

whitespace :: Parser String
whitespace = pSpaces

oneOf = pAny pSym

noneOf cs = pAnySym $ filter (`notElem`cs) [ chr c | c <- [0..255] ]

string = pToken

optional a p = opt p a

----- Nix Expressions ---------------------------------------------------------

data Attr = Assign String Expr
          | Inherit String [String]
  deriving (Read, Show, Eq)

genIdentifier :: Gen String
genIdentifier = ((:) <$> elements firstChar <*> listOf (elements identChar)) `suchThat` (`notElem` reservedNames)
  where firstChar = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
        identChar = firstChar ++ ['0'..'9'] ++ "-"

-- instance Arbitrary ScopedIdent where
--   arbitrary = SIdent <$> listOf1 genIdentifier

data Expr = Null
          | Lit String
          | Ident String
          | Boolean Bool
          | AttrSet Bool [Attr]
          | AttrSetP (Maybe String) [(String, Maybe Expr)]
          | List [Expr]
          | Deref Expr Expr
          | HasAttr Expr Expr
          | DefAttr Expr Expr
          | Concat Expr Expr
          | Append Expr Expr
          | Not Expr
          | Union Expr Expr
          | Equal Expr Expr
          | Inequal Expr Expr
          | And Expr Expr
          | Or Expr Expr
          | Implies Expr Expr
          | Fun Expr Expr
          | Let [Attr] Expr
          | Apply Expr Expr
          | Import Expr
          | With Expr
          | Assert Expr
          | IfThenElse Expr Expr Expr
  deriving (Read, Show, Eq)

expr :: Parser Expr
expr = expr01
  where
    expr14 = pChainl (Deref <$ dot) term
    expr13 = msum [ DefAttr <$> (expr14 <* symbol "or") <*> expr14, expr14 ]
    expr12 = pChainl (Apply <$ whitespace) expr13 -- This rule is not supported in lists.
    expr11 = msum [ HasAttr <$> (expr12 <* symbol "?") <*> expr12, expr12 ]
    expr10 = pChainr (Concat <$ symbol "++") expr11
    expr09 = pChainl (Append <$ symbol "+") expr10
    expr08 = msum [ Not <$> (symbol "!" *> expr09), expr09 ]
    expr07 = pChainr (Union <$ symbol "//") expr08
    expr06 = msum [ Equal <$> (expr07 <* symbol "==") <*> expr07, expr07 ]
    expr05 = msum [ Inequal <$> (expr06 <* symbol "!=") <*> expr06, expr06 ]
    expr04 = pChainl (And <$ symbol "&&") expr05
    expr03 = pChainl (Or <$ symbol "||") expr04
    expr02 = msum [ Implies <$> (expr03 <* symbol "->") <*> expr03, expr03 ]
    expr01 = pChainr (Fun <$ symbol ":") expr02

term :: Parser Expr
term = msum [ Null <$ symbol "null"
            , Boolean True <$ symbol "true", Boolean False <$ symbol "false"
            , parens expr
            -- , list
            -- , attrSetPattern
            -- , attrSet
            -- , letExpr
            , symbol "import" >> Import <$> expr
            , symbol "with" >> With <$> expr <* semi
            , symbol "assert" >> Assert <$> expr <* semi
            , IfThenElse <$> (symbol "if" *> expr) <*> (symbol "then" *> expr) <*> (symbol "else" *> expr)
            -- , literal
            , identifier
            ]

identifier :: Parser Expr
identifier = (Ident <$> lexeme (pList1 (pLetter <|> pDigit <|> others))) `micro` 10
  where others = pSym '-' <|> pSym '_'

alpha = pRange ('a','z') <|> pRange ('A','Z')
digit = pRange ('0','9')
alphaNum = alpha <|> digit

-- literal :: Parser Expr
-- literal = Lit <$> (stringLiteral <|> nixString <|> natural <|> literalURI)
--
-- stringLiteral :: Parser String
-- stringLiteral = lexeme $ between (string "\"") (string "\"") (concat <$> many stringChar)
--   where
--     stringChar :: Parser String
--     stringChar = choice [ pSome (noneOf "$\\\"")
--                         , try $ char '$' >> braces expr >> return ""
--                         , return <$> char '$'
--                         , char '\\' >> anyChar >>= \c -> return ['\\',c]
--                         ]

nixString :: Parser String
nixString = lexeme $ pPacked (string "''") (string "''") (concat <$> many stringChar)
  where
    stringChar :: Parser String
    stringChar = msum   [ pSome (noneOf "$'")
                        , pSym '$' >> braces expr >> return ""
                        , return <$> pSym '$'
                        , (return <$> pSym '\'') -- <* notFollowedBy (char '\'')
                        , string "''" >> string "${"
                        ]

literalURI :: Parser String
literalURI = lexeme $ absoluteURI <|> relativeURI

absoluteURI :: Parser String
absoluteURI = (++) <$> scheme <*> ((:) <$> pSym ':' <*> (hierPart <|> opaquePart))

relativeURI :: Parser String
relativeURI = (++) <$> (absPath <|> relPath) <*> optional "" ((:) <$> pSym '?' <*> query)

absPath :: Parser String
absPath = (:) <$> pSym '/' <*> pathSegments

authority :: Parser String
authority = server <|> regName

domainlabel :: Parser String
domainlabel = (:) <$> alphaNum <*> optional "" ((++) <$> many (pSym '-') <*> domainlabel)

escapedChars :: Parser Char
escapedChars = pSym '%' *> hexDigit *> hexDigit

hexDigit = pRange ('a','f') <|> pRange ('A','F') <|> pDigit

hierPart :: Parser String
hierPart = (++) <$> (netPath <|> absPath) <*> optional "" (pSym '?' *> query)

host :: Parser String
host = hostname <|> ipv4address

hostname :: Parser String
hostname = many (domainlabel *> pSym '.') *> toplabel *> optional "" (pToken ".")

hostport :: Parser String
hostport = (++) <$> host <*> optional "" ((:) <$> pSym ':' <*> port)

ipv4address :: Parser String
ipv4address = pSome pDigit *> pSym '.' *> pSome pDigit *> pSym '.' *> pSome pDigit *> pSym '.' *> pSome pDigit

markChars :: Parser Char
markChars = pAny pSym "-_.!~*'" -- Note that "()" have been removed here!

netPath :: Parser String
netPath = (++) <$> ((++) <$> pToken "//" <*> authority) <*> optional "" absPath

opaquePart :: Parser String
opaquePart = uricNoSlash >> many uric

pathSegments :: Parser String
pathSegments = (++) <$> segment <*> (concat <$> many ((:) <$> pSym '/' <*> segment))

pchar :: Parser Char
pchar = unreservedChars <|> escapedChars <|> pAny pSym ":@&=+$,"

port :: Parser String
port = pSome digit

query :: Parser String
query = many uric

regName :: Parser String
regName = pSome (unreservedChars <|> escapedChars <|> oneOf "$,:@&=+") -- Note that ';' has been removed here!

relPath :: Parser String
relPath = (++) <$> relSegment <*> absPath

relSegment :: Parser String
relSegment = pSome (unreservedChars <|> escapedChars <|> oneOf "@&=+$,") -- Note that ';' has been removed here!

reservedChars :: Parser Char
reservedChars = oneOf "/?:@&=+$," -- Note that ';' has been removed here!

scheme :: Parser String
scheme = (:) <$> pLetter <*> many (alphaNum <|> oneOf "+-.")

segment :: Parser String
segment = {- (++) <$> -} many pchar {- <*> (concat <$> many ((:) <$> char ';' <*> param)) -}

server :: Parser String
server = optional "" (optional "" ((++) <$> userinfo <*> string "@") *> hostport)

toplabel :: Parser Char
toplabel = pLetter <|> (pLetter *> pMany (alphaNum <|> pSym '-') *> alphaNum)

unreservedChars :: Parser Char
unreservedChars = alphaNum <|> markChars

uric :: Parser Char
uric = reservedChars <|> unreservedChars <|> escapedChars

uricNoSlash :: Parser Char
uricNoSlash = unreservedChars <|> escapedChars <|> oneOf ";?:@&=+$,"

userinfo :: Parser String
userinfo = many (unreservedChars <|> escapedChars <|> oneOf ";:&=+$,")

-- attrSet :: Parser Expr
-- attrSet = AttrSet <$> option False (True <$ reserved "rec") <*> braces (attribute `endBy` semi)
--
-- scopedIdentifier :: Parser ScopedIdent
-- scopedIdentifier = SIdent <$> sepBy1 (Parsec.identifier nixLexer) dot
--
-- attribute :: Parser Attr
-- attribute =  (Assign <$> (SIdent . return <$> stringLiteral <|> scopedIdentifier) <* assign <*> expr)
--          <|> (Inherit <$> (symbol "inherit" *> option (SIdent []) (parens scopedIdentifier)) <*> pSome (Parsec.identifier nixLexer))
--
-- list :: Parser Expr
-- list = List <$> brackets (many listExpr)
--
-- attrSetPattern :: Parser Expr
-- attrSetPattern = AttrSetP <$> optionMaybe atPattern <*> setPattern
--   where
--     atPattern  = Parsec.identifier nixLexer <* reserved "@"
--     setPattern = braces $ commaSep1 $ (,) <$> Parsec.identifier nixLexer <*> optionMaybe (reservedOp "?" >> expr) <|> ellipsis
--     ellipsis   = ("...",Nothing) <$ reserved "..."
--
-- letExpr :: Parser Expr
-- letExpr = choice [ try $ Let <$> (reserved "let" *> try attribute `endBy1` semi) <*> (reserved "in" *> expr)
--                  , (`Let` Ident "body") <$> (reserved "let" *> braces (try attribute `endBy1` semi))
--                  ]
--
-- parseNixFile :: FilePath -> IO (Either ParseError Expr)
-- parseNixFile path = parse' (expr <* eof) path <$> readFile path
--
-- parseNix :: String -> Either ParseError Expr
-- parseNix = parse expr
--

parseNix :: String -> Expr
parseNix = either error id . parse expr

parse :: Parser a -> String -> Either String a
parse p s | null b    = Right a
          | otherwise = Left (pruneError s b)
  where (a,b) = execParser p s

pruneError :: String -> [Error LineColPos] -> String
pruneError _ [] = ""
pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
pruneError s (Inserted _ position expr : _) = prettyError s expr position
pruneError s (Deleted  _ position expr : _) = prettyError s expr position

prettyError :: String -> [String] -> LineColPos -> String
prettyError s exp p@(LineColPos _ _ abs) =
  let
     s' = map (\c -> if c `elem` "\n\r\t" then ' ' else c) s
     aboveString = replicate 30 ' ' ++ "v"
     belowString = replicate 30 ' ' ++ "^"
     inputFrag   = replicate (30 - abs) ' ' ++ take 71 (drop (abs - 30) s')
  in
  printf "Expected %s at %s :\n%s\n%s\n%s\n"
    (show_expecting p exp) (show p) aboveString inputFrag belowString

----- Nix Evaluation ----------------------------------------------------------

-- type VarName = String
-- type Env = Map VarName Expr
--
-- data Error = CannotCoerceToString Expr
--            | CannotCoerceToBool Expr
--            | TypeMismatch Expr
--            | UndefinedVariable VarName
--            | Unsupported Expr
--            | Unstructured String
--            | InvalidSyntax ParseError
--   deriving (Show)
--
-- instance ErrT.Error Error where
--   strMsg = Unstructured
--   noMsg  = Unstructured "no error message available"
--
-- type Eval a = ErrorT Error (Reader Env) a
--
-- getEnv :: VarName -> Eval Expr
-- getEnv v = ask >>= maybe (throwError (UndefinedVariable v)) return . lookup v
--
-- onError :: Eval a -> (Error -> Bool, Eval a) -> Eval a
-- onError f (p,g) = catchError f (\e -> if p e then g else throwError e)
--
-- isUndefinedVariable :: Error -> Bool
-- isUndefinedVariable (UndefinedVariable _) = True
-- isUndefinedVariable _                     = False
--
-- isCoerceToString :: Error -> Bool
-- isCoerceToString (CannotCoerceToString _) = True
-- isCoerceToString _                        = False
--
-- isCoerceToBool :: Error -> Bool
-- isCoerceToBool (CannotCoerceToBool _) = True
-- isCoerceToBool _                      = False
--
-- evalBool :: Expr -> Eval Bool
-- evalBool e | trace ("evalBool " ++ show e) False = undefined
-- evalBool (Boolean x)    = return x
-- evalBool (Ident v)      = getEnv v >>= evalBool
-- evalBool (And x y)      = (&&) <$> evalBool x <*> evalBool y
-- evalBool (Or x y)       = (||) <$> evalBool x <*> evalBool y
-- evalBool (Not x)        = not <$> evalBool x
-- evalBool e@(Equal x y)  = ((==) <$> evalString  x <*> evalString  y)
--                             `onError` (isCoerceToString, (==) <$> evalBool x <*> evalBool y)
--                             `onError` (isCoerceToBool, throwError (TypeMismatch e))
-- evalBool e              = throwError (CannotCoerceToBool e)
--
-- evalString :: Expr -> Eval String
-- evalString e | trace ("evalString " ++ show e) False = undefined
-- evalString (Lit x)      = return x
-- evalString (Append x y) = (++) <$> evalString x <*> evalString y
-- evalString (Ident v)    = getEnv v >>= evalString
-- evalString e            = throwError (CannotCoerceToString e)
--
-- evalAttribute :: Attr -> Eval [(VarName,Expr)]
-- evalAttribute (Assign (SIdent [k]) v)  = (return . (,) k) <$> eval v
-- evalAttribute (Inherit (SIdent []) vs) = sequence [ (,) v <$> getEnv v | v <- vs ]
-- evalAttribute e                        = throwError (Unsupported (AttrSet False [e]))
--
-- attrSetToEnv :: Attr -> Eval [(VarName,Expr)]
-- attrSetToEnv (Assign (SIdent [k]) v)  = return [(k,v)]
-- attrSetToEnv (Inherit (SIdent []) vs) = sequence [ (,) v <$> getEnv v | v <- vs ]
-- attrSetToEnv e                        = throwError (Unsupported (AttrSet True [e]))
--
-- eval :: Expr -> Eval Expr
-- eval e | trace ("eval " ++ show e) False = undefined
-- eval Null                                       = return Null
-- eval e@(Lit _)                                  = return e
-- eval e@(Boolean _)                              = return e
-- eval (Ident v)                                  = getEnv v >>= eval
-- eval e@(Append _ _)                             = Lit <$> evalString e
-- eval e@(And _ _)                                = Boolean <$> evalBool e
-- eval e@(Or _ _)                                 = Boolean <$> evalBool e
-- eval e@(Not _)                                  = Boolean <$> evalBool e
-- eval e@(Equal _ _)                              = Boolean <$> evalBool e
-- eval e@(Inequal _ _)                            = Boolean <$> evalBool e
-- eval (IfThenElse b x y)                         = evalBool b >>= \b' -> eval (if b' then x else y)
-- eval (DefAttr x y)                              = eval x `onError` (isUndefinedVariable, eval y)
-- eval (Let as e)                                 = concat <$> mapM attrSetToEnv as >>= \env -> trace ("add to env: " ++ show env) $ local (union (fromList env)) (eval e)
-- eval (Apply (Fun (Ident v) x) y)                = trace "foo" $ eval y >>= \y' -> local (insert v y') (eval x)
-- eval (Apply (Ident v) y)                        = trace "yo" $ getEnv v >>= \x' -> eval (Apply x' y)
-- eval (Apply x@(Apply _ _) y)                    = trace "yo" $ eval x >>= \x' -> eval (Apply x' y)
-- eval (AttrSet False as)                         = (AttrSet False . map (\(k,v) -> Assign (SIdent [k]) v) . concat) <$> mapM evalAttribute as
-- eval (AttrSet True as)                          = concat <$> mapM attrSetToEnv as >>= \as' -> trace ("add to env: " ++ show as') $ local (union (fromList as')) (eval (AttrSet False as))
-- eval (Deref (Ident v) y)                        = getEnv v >>= \v' -> eval (Deref v' y)
-- eval (Deref (AttrSet False as) y@(Ident _))     = concat <$> mapM evalAttribute as >>= \as' -> trace ("add to env: " ++ show as') $ local (\env -> foldr (uncurry insert) env as') (eval y)
-- eval (Deref (AttrSet True as) y@(Ident _))      = concat <$> mapM attrSetToEnv as >>= \as' -> trace ("add to env: " ++ show as') $ local (\env -> foldr (uncurry insert) env as') (eval y)
-- eval e@(Deref _ _)                              = throwError (TypeMismatch e)
-- eval e                                          = throwError (Unsupported e)
--
-- --
-- -- eval (Apply (Lambda v x) y)     = eval y >>= \y' -> trace ("add to env: " ++ show (v,y')) $ local ((v,y'):) (eval x)
-- -- eval (Apply x@(V _) y)          = eval x >>= \x' -> eval (Apply x' y)
-- -- eval (Apply x@(Apply _ _) y)    = eval x >>= \x' -> eval (Apply x' y)
-- -- eval (Let env e)                = trace ("add to env: " ++ show env) $ local (env++) (eval e)
-- -- eval e@(Lambda _ _)             = return e
-- -- eval e                          = throwError (Unsupported e)
--
--
-- -- coerceDict :: Value -> Dict
-- -- coerceDict (AttrSetV e) = e
-- -- coerceDict e            = error ("cannot coerce expression to attribute set: " ++ show e)
-- --
-- -- coerceFun :: Value -> (Value -> Value)
-- -- coerceFun (FunV f) = f
-- -- coerceFun e        = error ("cannot coerce expression to function: " ++ show e)
-- --
-- -- coerceStr :: Value -> String
-- -- coerceStr (StrV x) = x
-- -- coerceStr e        = error ("cannot coerce expression to string: " ++ show e)
-- --
-- -- -- getScopedVar :: [String] -> Eval Value
-- -- -- getScopedVar   []   = fail "invalid empty scoped variable"
-- -- -- getScopedVar (k:[]) = getEnv k
-- -- -- getScopedVar (k:ks) = getEnv k >>= \e -> local (union (coerceDict e)) (getScopedVar ks)
-- --
-- -- -- evalAttr :: Attr -> Eval Dict
-- -- -- evalAttr (Inherit (SIdent k) is)    = fromList <$> forM is (\i -> (,) i <$> getScopedVar (k++[i]))
-- -- -- evalAttr (Assign (SIdent   []) _)   = fail "invalid empty scoped identifier in assignment"
-- -- -- evalAttr (Assign (SIdent (k:[])) e) = singleton k <$> eval e
-- -- -- evalAttr (Assign (SIdent (k:ks)) e) = (singleton k . AttrSetV) <$> evalAttr (Assign (SIdent ks) e)
-- --
-- -- simplifyAttr :: Attr -> Map String Expr
-- -- simplifyAttr (Inherit (SIdent _) [])    = error "invalid empty inherit statement"
-- -- simplifyAttr (Inherit (SIdent k) is)    = unions [ singleton i (foldl1 Deref (map Ident (k++[i]))) | i <- is]
-- -- simplifyAttr (Assign (SIdent   []) _)   = error "invalid empty scoped identifier in assignment"
-- -- simplifyAttr (Assign (SIdent (k:[])) e) = singleton k e
-- -- simplifyAttr (Assign (SIdent (k:ks)) e) = singleton k (AttrSet False [Assign (SIdent ks) e])
-- --
-- -- evalAttr' :: (String, Expr) -> Eval Dict
-- -- evalAttr' (k, e) = singleton k <$> eval e
-- --
-- -- evalDict :: Map String Expr -> Eval Dict
-- -- evalDict as = unionsWith mergeDicts <$> mapM evalAttr' (assocs as)
-- --
-- -- -- -- (Inherit (SIdent k) is)    = fromList <$> forM is (\i -> (,) i <$> getScopedVar (k++[i]))
-- -- -- evalAttr' (Assign (SIdent   []) _)   = fail "invalid empty scoped identifier in assignment"
-- -- -- evalAttr' (Assign (SIdent (k:[])) e) = singleton k <$> eval e
-- -- -- evalAttr' (Assign (SIdent (k:ks)) e) = (singleton k . AttrSetV) <$> evalAttr (Assign (SIdent ks) e)
-- --
-- -- eval :: Expr -> Eval Value
-- -- eval e | trace ("eval: " ++ show e) False = undefined
-- -- eval (Lit v)                    = return (StrV v)
-- -- eval (Ident v)                  = getEnv v
-- -- eval (AttrSet False as)         = AttrSetV . unionsWith mergeDicts <$> mapM (evalDict . simplifyAttr) as
-- --
-- -- eval (AttrSet True as)          = do
-- --   env <- ask
-- --   let e :: Map String Expr
-- --       e = unionsWith mergeAttrSets (map simplifyAttr as)
-- --   return (AttrSetV (resolve env e))
-- --
-- -- -- mdo { r@(AttrSetV d) <- local (`union` d) (eval (AttrSet False as)); return r }
-- --
-- -- -- eval (AttrSet False as)         = AttrSetV . unionsWith mergeDicts <$> mapM evalAttr as
-- -- -- eval (AttrSet True as)          = mdo { r@(AttrSetV d) <- local (`union` d) (eval (AttrSet False as)); return r }
-- -- eval (Fun (Ident x) y)          = do { env <- ask; return (FunV (\v -> runEval' (eval y) (insert x v env))) }
-- -- eval (Apply x y)                = coerceFun <$> eval x <*> eval y
-- -- eval (Append x y)               = StrV <$> ((++) <$> (coerceStr <$> eval x) <*> (coerceStr <$> eval y))
-- -- eval (Deref x (Ident y))        = coerceDict <$> eval x >>= \x' -> local (const x') (getEnv y)
-- -- -- default catch-all to report the un-expected expression
-- -- eval e                          = fail ("unsupported: " ++ show e)
-- --
-- -- mergeDicts :: Value -> Value -> Value
-- -- mergeDicts x y = AttrSetV (unionWith mergeDicts (coerceDict x) (coerceDict y))
-- --
-- -- mergeAttrSets :: Expr -> Expr -> Expr
-- -- mergeAttrSets (AttrSet False x) (AttrSet False y) = AttrSet False (x++y)
-- -- mergeAttrSets x y = error ("mergeAttrSets: cannot merge expressions " ++ show x ++ " and " ++ show y)
--
-- run :: String -> Either Error Expr
-- run = either (Left . InvalidSyntax) (\e -> runEval (eval e) builtins) . parseNix
--
-- runEval :: Eval a -> Env -> Either Error a
-- runEval = runReader . runErrorT
--
-- builtins :: Env
-- builtins = fromList
--            [ ("true", Boolean True)
--            , ("false", Boolean False)
--            , ("null", Null)
--            ]
