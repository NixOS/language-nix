-- self-test.hs

{-# LANGUAGE GADTs, StandaloneDeriving, ExistentialQuantification #-}

module Main ( main ) where

import Text.ParserCombinators.UU hiding ( parse, Apply, optional )
import Text.ParserCombinators.UU.BasicInstances hiding ( Parser )
import Text.ParserCombinators.UU.Utils
import Text.Printf
import Data.Char

import Test.DocTest
import Test.QuickCheck hiding ( Str )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit.Base ( assertFailure, assertEqual )

type Parser a = P (Str Char String LineColPos) a

----- Untyped Lambda Calculus -------------------------------------------------

type Ident = String

data UExp = UInt Integer                -- ^ Integer literal
          | UBol Bool                   -- ^ Bool literal
          | UVar Ident                  -- ^ Variable
          | UApp Ident [UExp]           -- ^ Function application
          | ULet Ident UExp UExp        -- ^ Local binding
  deriving (Show, Eq)

data UFun = UFun [Ident] UExp
  deriving (Show, Eq)

pFun :: Parser UFun
pFun = UFun <$> (pSymbol "\\" *> pList1_ng pIdent) <*> (pSymbol "->" *> pExp)

pIdent :: Parser Ident
pIdent = lexeme (pIdentString `micro` 10)
  where
    pIdentString :: Parser String
    pIdentString = (:) <$> pIdentHeadLetter <*> pList pIdentTailLetter

    pIdentHeadLetter, pIdentTailLetter :: Parser Char
    pIdentHeadLetter = msum [ pLetter, pSym '-', pSym '_']
    pIdentTailLetter = msum [ pIdentHeadLetter, pDigit ]

pBool :: Parser Bool
pBool = msum [ True <$ pSymbol "true", False <$ pSymbol "false" ]

pInt :: Parser Integer
pInt = pInteger

pExp :: Parser UExp
pExp = foldr binOp pTerm ["||", "&&", ">=", "<=", "/=", "==", "-", "+", "/", "*"]
  where
    pTerm :: Parser UExp
    pTerm = msum [ UBol <$> pBool
                 , UVar <$> pIdent
                 , UInt <$> pInteger
                 , ULet <$> (pSymbol "let" *> pIdent) <*> (pSymbol "=" *> pExp) <*> (pSymbol "in" *> pExp)
                 , UApp <$> pIdent <*> pList1 pExp `micro` 5
                 , pParens pExp
                 ]

    binOp :: String -> Parser UExp -> Parser UExp
    binOp op = pChainl ((\x y -> UApp op [x,y]) <$ pSymbol op)

----- Typed Lambda Calculus ---------------------------------------------------

data TFun a where
  TBody :: TExp a                    -> TFun a
  TLam  :: Ident -> TTyp a -> TFun b -> TFun (a->b)

data TTyp a where
  TTBol ::                     TTyp Bool
  TTInt ::                     TTyp Integer
  TTArr :: TTyp a -> TTyp b -> TTyp (a->b)

data TExp a where
    TInt   :: Integer                                           -> TExp Integer
    TBol   :: Bool                                              -> TExp Bool
    TIntOp :: IntOp     -> TExp Integer -> TExp Integer         -> TExp Integer
    TBolOp :: BolOp     -> TExp Bool    -> TExp Bool            -> TExp Bool
    TCmpOp :: CmpOp     -> TExp Integer -> TExp Integer         -> TExp Bool
    TIf    :: TExp Bool -> TExp a       -> TExp a               -> TExp a
    TLet   :: Ident     -> TTyp a       -> TExp a     -> TExp b -> TExp b
    TVar   :: Ident                                             -> TExp a

deriving instance Show (TFun a)
deriving instance Show (TTyp a)
deriving instance Show (TExp a)

data IntOp = DAdd | DSub | DMul | DDiv
    deriving (Eq, Show)

data BolOp = BAnd | BOr
    deriving (Eq, Show)

data CmpOp = CEq | CLe
    deriving (Eq, Show)

----- Type Checker ------------------------------------------------------------

data ATExp = forall a . TExp a ::: TTyp a

data AFun = forall a . AFun (TFun a) (TTyp a)

data Equal a b where
    Eq :: Equal a a

deriving instance Show ATExp
deriving instance Show AFun
deriving instance Show (Equal a b)

test :: TTyp a -> TTyp b -> Maybe (Equal a b)
test TTBol TTBol = return Eq
test TTInt TTInt = return Eq
test (TTArr a b) (TTArr a' b') = do Eq <- test a a'
                                    Eq <- test b b'
                                    return Eq
test _ _ = mzero

type Env = [(Ident, ATExp)]

typeCheckExp :: Env -> UExp -> Maybe ATExp
typeCheckExp _ (UInt d) = return (TInt d ::: TTInt)
typeCheckExp _ (UBol b) = return (TBol b ::: TTBol)

typeCheckExp r (UApp op [a, b]) | Just dop <- lookup op [("+", DAdd), ("-", DSub), ("*", DMul), ("/", DDiv)] = do
    a' ::: TTInt <- typeCheckExp r a
    b' ::: TTInt <- typeCheckExp r b
    return (TIntOp dop a' b' ::: TTInt)

typeCheckExp r (UApp op [a, b]) | Just bop <- lookup op [("&&", BAnd), ("||", BOr)] = do
    a' ::: TTBol <- typeCheckExp r a
    b' ::: TTBol <- typeCheckExp r b
    return (TBolOp bop a' b' ::: TTBol)

typeCheckExp r (UApp op [a, b]) | Just cop <- lookup op [("==", CEq), ("<=", CLe)] = do
    a' ::: TTInt <- typeCheckExp r a
    b' ::: TTInt <- typeCheckExp r b
    return (TCmpOp cop a' b' ::: TTBol)

typeCheckExp r (UApp "if" [c,t,e]) = do
    c' ::: TTBol <- typeCheckExp r c
    t' ::: tt    <- typeCheckExp r t
    e' ::: te    <- typeCheckExp r e
    Eq <- test tt te
    return (TIf c' t' e' ::: tt)

typeCheckExp r (ULet i e b) = do
    e' ::: te <- typeCheckExp r e
    b' ::: tb <- typeCheckExp ((i, TVar i ::: te) : r) b
    return (TLet i te e' b' ::: tb)

typeCheckExp r (UVar i) = lookup i r

typeCheckExp _ _ = mzero

typeCheck :: UFun -> Maybe AFun
typeCheck = typeCheckFun []

typeCheckFun :: Env -> UFun -> Maybe AFun
typeCheckFun n (UFun [] b) = do
    e ::: t <- typeCheckExp n b
    return (AFun (TBody e) t)

-- typeCheckFun n (UFun ((x, typ):vts) b) =
--     let f :: TTyp a -> Maybe AFun
--         f t = do AFun e r <- typeCheckFun ((x, TVar x ::: t) : n) (UFun vts b); return $ AFun (TLam x t e) (TTArr t r)
--     in
--     case typ of
--        UTBol -> f TTBol
--        UTInt -> f TTInt

----- Test Code ---------------------------------------------------------------

gives :: (Show err, Eq a, Show a) => Either err a -> a -> Expectation
gives x y = either (assertFailure . msg) (assertEqual "" y) x
  where msg z = "expected: " ++ show y ++ "\nbut got parser error: " ++ show z

parseFail :: Show a => Parser a -> String -> Expectation
parseFail p str = parse p str `shouldSatisfy` either (const True) (const False)

main :: IO ()
main = do
  doctest [ "self-test.hs", "Language/Nix.hs" ]
  hspec $ do
    describe "pIdent" $ do
      it "parses hand-picked sample inputs" $ do
        parse pIdent "abc" `gives` "abc"
        parse pIdent "abc  " `gives` "abc"
        parse pIdent "__a-b-c-__  " `gives` "__a-b-c-__"
      prop "parses all randomly generated samples" $
        forAll gIdent $ \i -> parse pIdent i `gives` i
      it "does not swallow leading whitespace" $
        forAll gIdent $ \i -> parseFail pIdent (' ':i)
      it "does not accept a leading digit" $ do
        parseFail pIdent "1bac"
        parseFail pIdent "1"

    describe "pBool" $
      it "parses hand-picked sample inputs" $ do
        parse pBool "true" `gives` True
        parse pBool "false" `gives` False

    describe "pInt" $
      prop "parses all randomly generated samples" $
        \i -> parse pInt (show i) `gives` i

    describe "pExp" $ do
      it "parses hand-picked sample inputs" $ do
        parse pExp "abc" `gives` UVar "abc"
        parse pExp "abc  " `gives` UVar "abc"
        parse pExp "__a-b-c-__  " `gives` UVar "__a-b-c-__"
        parse pExp "let x=12 in x " `gives` ULet "x" (UInt 12) (UVar "x")
        parse pExp "f a b" `gives` UApp "f" [UVar "a", UVar "b"]
        parse pExp "true" `gives` UBol True
        parse pExp "false" `gives` UBol False

      it "does not parse reserved words as an identifier" $ do
        let noIdent (Right (UVar _)) = False
            noIdent _                = True
        forM_ reservedNames $ \i -> parse pExp (i++" ") `shouldSatisfy` noIdent
      it "accepts identifiers that are a prefix of a reserved word" $
        forM_ reservedNames $ \i -> parse pExp (i ++ "t") `gives` UVar (i ++ "t")

      it "parses boolean expressions" $ do
        parse pExp "a && b || c" `gives` UApp "||" [UApp "&&" [UVar "a",UVar "b"],UVar "c"]
        parse pExp "(a && b) || c" `gives` UApp "||" [UApp "&&" [UVar "a",UVar "b"],UVar "c"]
        parse pExp "a || b && c" `gives` UApp "||" [UVar "a",UApp "&&" [UVar "b",UVar "c"]]
        parse pExp "a || (b && c)" `gives` UApp "||" [UVar "a",UApp "&&" [UVar "b",UVar "c"]]
        parse pExp "a && (b || c)" `gives` UApp "&&" [UVar "a",UApp "||" [UVar "b",UVar "c"]]
        parse pExp "(a || b) && c" `gives` UApp "&&" [UApp "||" [UVar "a",UVar "b"],UVar "c"]

      it "parses arithmetic expressions" $ do
        parse pExp "1 + 2 * 3" `gives` UApp "+" [UInt 1,UApp "*" [UInt 2,UInt 3]]
        parse pExp "1 + (2 * 3)" `gives` UApp "+" [UInt 1,UApp "*" [UInt 2,UInt 3]]
        parse pExp "1 * 2 + 3" `gives` UApp "+" [UApp "*" [UInt 1,UInt 2],UInt 3]
        parse pExp "(1 * 2) + 3" `gives` UApp "+" [UApp "*" [UInt 1,UInt 2],UInt 3]
        parse pExp "1 * (2 + 3)" `gives` UApp "*" [UInt 1,UApp "+" [UInt 2,UInt 3]]

      it "parses simple lambda expressions" $
        parse pFun "\\x -> 0" `gives` UFun ["x"] (UInt 0)

      it "parses function application" $
        parse pExp "a b c" `gives` UApp "a" [UVar "b",UVar "c"]

type ParseError = String

parse :: Parser a -> String -> Either ParseError a
parse p s | null b    = Right a
          | otherwise = Left (pruneError s b)
  where (a,b) = parse_h ((,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) s)

-- Taken from Text.ParserCombinators.UU.Utils.
pruneError :: String -> [Error LineColPos] -> ParseError
pruneError _ [] = ""
pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
pruneError s (Inserted _ p e : _) = prettyError s e p
pruneError s (Deleted  _ p e : _) = prettyError s e p

-- Taken from Text.ParserCombinators.UU.Utils.
prettyError :: String -> [String] -> LineColPos -> String
prettyError s e p@(LineColPos _ _ abs') =
  printf "Expected %s at %s :\n%s\n%s\n%s\n"
    (show_expecting p e) (show p) aboveString inputFrag belowString
  where
    s' = map (\c -> if c `elem` "\n\r\t" then ' ' else c) s
    aboveString = replicate 30 ' ' ++ "v"
    belowString = replicate 30 ' ' ++ "^"
    inputFrag   = replicate (30 - abs') ' ' ++ take 71 (drop (abs' - 30) s')

gIdent :: Gen String
gIdent = listOf1 letter `suchThat` (not . isDigit . head)
  where letter = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_-"

reservedNames :: [String]
reservedNames = ["let", {- "in", -} "true", "false"]


data TDeriv where
  TDeriv :: TTyp a -> TDeriv

deriving instance Show TDeriv

deriveExpType :: UExp -> Either String TDeriv
deriveExpType (UInt _) = return (TDeriv TTInt)
deriveExpType (UBol _) = return (TDeriv TTBol)
deriveExpType exp      = Left (showString "*** deriveExpType failed for " (show exp))

yo :: IO ()
yo = do
  print $ deriveExpType (UApp "==" [UApp "/" [UInt 1,UApp "*" [UVar "x",UVar "y"]],UInt 2])
  -- let str = "\\ x y -> 1/(x*y) == 2"
  --     Right ufun = parse pFun str
  --     UFun vars body = ufun
  -- print body
  -- let Just texp  = typeCheckExp [] body
  -- print texp
