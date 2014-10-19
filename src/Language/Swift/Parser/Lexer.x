{

module Language.Swift.Parser.Lexer where

import qualified Data.Map as Map
import Language.Swift.Parser.SourceLocation
import Language.Swift.Parser.Token

}

%wrapper "monad"

$alpha = [a-zA-Z]
$digits = [0-9]
$alnum = [$alpha$digits]

@identifier_head = $alpha

@identifier_character = $alnum

@identifier = @identifier_head @identifier_character*

@integer = $digits+

:-

@identifier { \(loc, _, _, str) len -> keywordOrIdent (take len str) (alexPosnToTokenSpan loc) }

{

alexPosnToTokenSpan :: AlexPosn -> TokenSpan
alexPosnToTokenSpan (AlexPn offset line col) = (TokenSpan offset line col)

alexEOF :: Alex Token
alexEOF = do return (TokenEOF tokenSpanEmpty)

keywordOrIdent :: String -> TokenSpan -> Alex Token
keywordOrIdent str location =
    return $ case Map.lookup str keywords of
        Just symbol -> symbol location
        Nothing -> TokenIdentifier location str

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
