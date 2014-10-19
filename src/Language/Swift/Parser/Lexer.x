{

module Language.Swift.Parser.Lexer where

import qualified Data.Map as Map
import Language.Swift.Parser.SourceLocation
import Language.Swift.Parser.Token

import Debug.Trace

}

%wrapper "monad"

$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]

@identifier_head = $alpha

@identifier_character = $alnum

@identifier = @identifier_head @identifier_character*

@decimal_literal = $digits+

:-

";"    { adapt (symbolToken TokenSemicolon) }
"("    { adapt (symbolToken TokenLParen) }
")"    { adapt (symbolToken TokenRParen) }
"{"    { adapt (symbolToken TokenLBrace) }
"}"    { adapt (symbolToken TokenRBrace) }
":"    { adapt (symbolToken TokenColon) }
","    { adapt (symbolToken TokenComma) }
"<"    { adapt (symbolToken TokenLT) }
">"    { adapt (symbolToken TokenGT) }
"=="    { adapt (symbolToken TokenDoubleEq) }
"."    { adapt (symbolToken TokenDot) }
"="    { adapt (symbolToken TokenEq) }
"->"    { adapt (symbolToken TokenMinusGT) }
"..."    { adapt (symbolToken TokenDotDotDot) }
"#"    { adapt (symbolToken TokenHash) }
"_"    { adapt (symbolToken TokenUnderscore) }
"?"    { adapt (symbolToken TokenQuestionmark) }
"!"    { adapt (symbolToken TokenBang) }
"@"    { adapt (symbolToken TokenAt) }
"["    { adapt (symbolToken TokenLBracket) }
"]"    { adapt (symbolToken TokenRBracket) }
"&"    { adapt (symbolToken TokenAmp) }
"`"    { adapt (symbolToken TokenBacktick) }
"$"    { adapt (symbolToken TokenDollar) }
"+"    { adapt (symbolToken TokenPlus) }
"-"    { adapt (symbolToken TokenMinus) }
"/"    { adapt (symbolToken TokenDiv) }
"*"    { adapt (symbolToken TokenTimes) }
"%"    { adapt (symbolToken TokenPercent) }
"|"    { adapt (symbolToken TokenPipe) }
"^"    { adapt (symbolToken TokenCaret) }
"~"    { adapt (symbolToken TokenTilde) }
".."    { adapt (symbolToken TokenDotDot) }

@identifier { \(loc, _, _, str) len -> keywordOrIdent (take len str) (alexPosnToTokenSpan loc) }

@decimal_literal { adapt (mkString DecimalLiteral) }

[\ \t \n]+  ;

{

mkString :: (Monad m) => (TokenSpan -> String -> Token) -> TokenSpan -> Int -> String -> m Token
mkString toToken loc len str = do return (toToken loc (take len str))

adapt :: (TokenSpan -> Int -> String -> Alex Token) -> AlexInput -> Int -> Alex Token
adapt f loc@(p@(AlexPn offset line col),_,_,inp) len =
    (f (TokenSpan offset line col) len inp)

symbolToken :: Monad m => (TokenSpan -> Token) -> TokenSpan -> Int -> String -> m Token
symbolToken mkToken location _ _ = trace ("SYM("++show (mkToken location)++")") $ return (mkToken location)

alexPosnToTokenSpan :: AlexPosn -> TokenSpan
alexPosnToTokenSpan (AlexPn offset line col) = (TokenSpan offset line col)

alexEOF :: Alex Token
alexEOF = do return (TokenEOF tokenSpanEmpty)

keywordOrIdent :: String -> TokenSpan -> Alex Token
keywordOrIdent str location =
    return $ case Map.lookup str keywords of
        Just symbol -> trace ("KW("++show (symbol location)++")") $ symbol location
        Nothing -> trace ("IDENT("++str++")") $ TokenIdentifier location str

keywords :: Map.Map String (TokenSpan -> Token)
keywords = Map.fromList keywordNames

keywordNames :: [(String, TokenSpan -> Token)]
keywordNames =
    [ ("for", TokenFor )
    , ("in", TokenIn )
    , ("while", TokenWhile )
    , ("do", TokenDo )
    , ("if", TokenIf )
    , ("else", TokenElse )
    , ("switch", TokenSwitch )
    , ("case", TokenCase )
    , ("default", TokenDefault )
    , ("where", TokenWhere )
    , ("break", TokenBreak )
    , ("continue", TokenContinue )
    , ("fallthrough", TokenFallthrough )
    , ("return", TokenReturn )
    , ("import", TokenImport )
    , ("typealias", TokenTypealias )
    , ("struct", TokenStruct )
    , ("class", TokenClass )
    , ("enum", TokenEnum )
    , ("protocol", TokenTokenProtocolLowercase )
    , ("var", TokenVar )
    , ("func", TokenFunc )
    , ("let", TokenLet )
    , ("get", TokenGet )
    , ("set", TokenSet )
    , ("willSet", TokenWillSet )
    , ("didSet", TokenDidSet )
    , ("inout", TokenInout )
    , ("init", TokenInit )
    , ("deinit", TokenDeinit )
    , ("extension", TokenExtension )
    , ("subscript", TokenSubscript )
    , ("prefix", TokenPrefix )
    , ("operator", TokenOperator )
    , ("postfix", TokenPostfix )
    , ("infix", TokenInfix )
    , ("precedence", TokenPrecedence )
    , ("associativity", TokenAssociativity )
    , ("left", TokenLeft )
    , ("right", TokenRight )
    , ("none", TokenNone )
    , ("convenience", TokenConvenience )
    , ("dynamic", TokenDynamic )
    , ("final", TokenFinal )
    , ("lazy", TokenLazy )
    , ("mutating", TokenMutating )
    , ("nonmutating", TokenNonmutating )
    , ("optional", TokenOptional )
    , ("override", TokenOverride )
    , ("required", TokenRequired )
    , ("static", TokenStatic )
    , ("unowned", TokenUnowned )
    , ("safe", TokenSafe )
    , ("unsafe", TokenUnsafe )
    , ("weak", TokenWeak )
    , ("internal", TokenInternal )
    , ("private", TokenPrivate )
    , ("public", TokenPublic )
    , ("is", TokenIs )
    , ("as", TokenAs )
    , ("__FILE__", TokenFILE )
    , ("__LINE__", TokenLINE )
    , ("__COLUMN__", TokenCOLUMN )
    , ("__FUNCTION__", TokenFUNCTION )
    , ("self", TokenSelf )
    , ("super", TokenSuper )
    , ("dynamicType", TokenDynamicType )
    , ("true", TokenTrue )
    , ("false", TokenFalse )
    , ("nil", TokenNil )
    , ("Type", TokenType )
    , ("Protocol", TokenProtocol )
    ]

}
