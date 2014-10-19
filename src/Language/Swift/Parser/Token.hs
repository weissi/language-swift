module Language.Swift.Parser.Token where

import Language.Swift.Parser.SourceLocation

data Token
    = TokenIdentifier { span :: !TokenSpan, name :: !String }
    | TokenEOF { span :: !TokenSpan }
    -- auto generated below
    | TokenSemicolon { span :: !TokenSpan }    -- ;
    | TokenFor { span :: !TokenSpan }    -- for
    | TokenLParen { span :: !TokenSpan }    -- (
    | TokenRParen { span :: !TokenSpan }    -- )
    | TokenIn { span :: !TokenSpan }    -- in
    | TokenWhile { span :: !TokenSpan }    -- while
    | TokenDo { span :: !TokenSpan }    -- do
    | TokenIf { span :: !TokenSpan }    -- if
    | TokenElse { span :: !TokenSpan }    -- else
    | TokenSwitch { span :: !TokenSpan }    -- switch
    | TokenLBrace { span :: !TokenSpan }    -- {
    | TokenRBrace { span :: !TokenSpan }    -- }
    | TokenCase { span :: !TokenSpan }    -- case
    | TokenColon { span :: !TokenSpan }    -- :
    | TokenComma { span :: !TokenSpan }    -- ,
    | TokenDefault { span :: !TokenSpan }    -- default
    | TokenWhere { span :: !TokenSpan }    -- where
    | TokenBreak { span :: !TokenSpan }    -- break
    | TokenContinue { span :: !TokenSpan }    -- continue
    | TokenFallthrough { span :: !TokenSpan }    -- fallthrough
    | TokenReturn { span :: !TokenSpan }    -- return
    | TokenLT { span :: !TokenSpan }    -- <
    | TokenGT { span :: !TokenSpan }    -- >
    | TokenDoubleEq { span :: !TokenSpan }    -- ==
    | TokenImport { span :: !TokenSpan }    -- import
    | TokenTypealias { span :: !TokenSpan }    -- typealias
    | TokenStruct { span :: !TokenSpan }    -- struct
    | TokenClass { span :: !TokenSpan }    -- class
    | TokenEnum { span :: !TokenSpan }    -- enum
    | TokenTokenProtocolLowercase { span :: !TokenSpan }    -- protocol
    | TokenVar { span :: !TokenSpan }    -- var
    | TokenFunc { span :: !TokenSpan }    -- func
    | TokenDot { span :: !TokenSpan }    -- .
    | TokenLet { span :: !TokenSpan }    -- let
    | TokenEq { span :: !TokenSpan }    -- =
    | TokenGet { span :: !TokenSpan }    -- get
    | TokenSet { span :: !TokenSpan }    -- set
    | TokenWillSet { span :: !TokenSpan }    -- willSet
    | TokenDidSet { span :: !TokenSpan }    -- didSet
    | TokenMinusGT { span :: !TokenSpan }    -- ->
    | TokenDotDotDot { span :: !TokenSpan }    -- ...
    | TokenInout { span :: !TokenSpan }    -- inout
    | TokenHash { span :: !TokenSpan }    -- #
    | TokenUnderscore { span :: !TokenSpan }    -- _
    | TokenInit { span :: !TokenSpan }    -- init
    | TokenQuestionmark { span :: !TokenSpan }    -- ?
    | TokenBang { span :: !TokenSpan }    -- !
    | TokenDeinit { span :: !TokenSpan }    -- deinit
    | TokenExtension { span :: !TokenSpan }    -- extension
    | TokenSubscript { span :: !TokenSpan }    -- subscript
    | TokenPrefix { span :: !TokenSpan }    -- prefix
    | TokenOperator { span :: !TokenSpan }    -- operator
    | TokenPostfix { span :: !TokenSpan }    -- postfix
    | TokenInfix { span :: !TokenSpan }    -- infix
    | TokenPrecedence { span :: !TokenSpan }    -- precedence
    | TokenAssociativity { span :: !TokenSpan }    -- associativity
    | TokenLeft { span :: !TokenSpan }    -- left
    | TokenRight { span :: !TokenSpan }    -- right
    | TokenNone { span :: !TokenSpan }    -- none
    | TokenConvenience { span :: !TokenSpan }    -- convenience
    | TokenDynamic { span :: !TokenSpan }    -- dynamic
    | TokenFinal { span :: !TokenSpan }    -- final
    | TokenLazy { span :: !TokenSpan }    -- lazy
    | TokenMutating { span :: !TokenSpan }    -- mutating
    | TokenNonmutating { span :: !TokenSpan }    -- nonmutating
    | TokenOptional { span :: !TokenSpan }    -- optional
    | TokenOverride { span :: !TokenSpan }    -- override
    | TokenRequired { span :: !TokenSpan }    -- required
    | TokenStatic { span :: !TokenSpan }    -- static
    | TokenUnowned { span :: !TokenSpan }    -- unowned
    | TokenSafe { span :: !TokenSpan }    -- safe
    | TokenUnsafe { span :: !TokenSpan }    -- unsafe
    | TokenWeak { span :: !TokenSpan }    -- weak
    | TokenInternal { span :: !TokenSpan }    -- internal
    | TokenPrivate { span :: !TokenSpan }    -- private
    | TokenPublic { span :: !TokenSpan }    -- public
    | TokenIs { span :: !TokenSpan }    -- is
    | TokenAs { span :: !TokenSpan }    -- as
    | TokenAt { span :: !TokenSpan }    -- @
    | TokenLBracket { span :: !TokenSpan }    -- [
    | TokenRBracket { span :: !TokenSpan }    -- ]
    | TokenAmp { span :: !TokenSpan }    -- &
    | TokenFILE { span :: !TokenSpan }    -- __FILE__
    | TokenLINE { span :: !TokenSpan }    -- __LINE__
    | TokenCOLUMN { span :: !TokenSpan }    -- __COLUMN__
    | TokenFUNCTION { span :: !TokenSpan }    -- __FUNCTION__
    | TokenSelf { span :: !TokenSpan }    -- self
    | TokenSuper { span :: !TokenSpan }    -- super
    | TokenUnownedLParensafeRParen { span :: !TokenSpan }    -- unowned(safe)
    | TokenUnownedLParenunsafeRParen { span :: !TokenSpan }    -- unowned(unsafe)
    | TokenDynamicType { span :: !TokenSpan }    -- dynamicType
    | TokenBacktick { span :: !TokenSpan }    -- `
    | TokenDollar { span :: !TokenSpan }    -- $
    | TokenTrue { span :: !TokenSpan }    -- true
    | TokenFalse { span :: !TokenSpan }    -- false
    | TokenNil { span :: !TokenSpan }    -- nil
    | Token0b { span :: !TokenSpan }    -- 0b
    | Token0o { span :: !TokenSpan }    -- 0o
    | Token0x { span :: !TokenSpan }    -- 0x
    | TokenTokenELowercase { span :: !TokenSpan }    -- e
    | TokenE { span :: !TokenSpan }    -- E
    | TokenTokenPLowercase { span :: !TokenSpan }    -- p
    | TokenP { span :: !TokenSpan }    -- P
    | TokenPlus { span :: !TokenSpan }    -- +
    | TokenMinus { span :: !TokenSpan }    -- -
    | TokenDQuotes { span :: !TokenSpan }    -- "
    | TokenBSLParen { span :: !TokenSpan }    -- \(
    | TokenBS0 { span :: !TokenSpan }    -- \0
    | TokenBSBS { span :: !TokenSpan }    -- \\
    | TokenBSt { span :: !TokenSpan }    -- \t
    | TokenBSn { span :: !TokenSpan }    -- \n
    | TokenBSr { span :: !TokenSpan }    -- \r
    | TokenBSDQuotes { span :: !TokenSpan }    -- \"
    | TokenBS' { span :: !TokenSpan }    -- \'
    | TokenBSu { span :: !TokenSpan }    -- \u
    | TokenDiv { span :: !TokenSpan }    -- /
    | TokenTimes { span :: !TokenSpan }    -- *
    | TokenPercent { span :: !TokenSpan }    -- %
    | TokenPipe { span :: !TokenSpan }    -- |
    | TokenCaret { span :: !TokenSpan }    -- ^
    | TokenTilde { span :: !TokenSpan }    -- ~
    | TokenDotDot { span :: !TokenSpan }    -- ..
    | TokenType { span :: !TokenSpan }    -- Type
    | TokenProtocol { span :: !TokenSpan }    -- Protocol

spanOfToken :: Token -> TokenSpan
spanOfToken (TokenSemicolon span) = span
spanOfToken (TokenFor span) = span
spanOfToken (TokenLParen span) = span
spanOfToken (TokenRParen span) = span
spanOfToken (TokenIn span) = span
spanOfToken (TokenWhile span) = span
spanOfToken (TokenDo span) = span
spanOfToken (TokenIf span) = span
spanOfToken (TokenElse span) = span
spanOfToken (TokenSwitch span) = span
spanOfToken (TokenLBrace span) = span
spanOfToken (TokenRBrace span) = span
spanOfToken (TokenCase span) = span
spanOfToken (TokenColon span) = span
spanOfToken (TokenComma span) = span
spanOfToken (TokenDefault span) = span
spanOfToken (TokenWhere span) = span
spanOfToken (TokenBreak span) = span
spanOfToken (TokenContinue span) = span
spanOfToken (TokenFallthrough span) = span
spanOfToken (TokenReturn span) = span
spanOfToken (TokenLT span) = span
spanOfToken (TokenGT span) = span
spanOfToken (TokenDoubleEq span) = span
spanOfToken (TokenImport span) = span
spanOfToken (TokenTypealias span) = span
spanOfToken (TokenStruct span) = span
spanOfToken (TokenClass span) = span
spanOfToken (TokenEnum span) = span
spanOfToken (TokenTokenProtocolLowercase span) = span
spanOfToken (TokenVar span) = span
spanOfToken (TokenFunc span) = span
spanOfToken (TokenDot span) = span
spanOfToken (TokenLet span) = span
spanOfToken (TokenEq span) = span
spanOfToken (TokenGet span) = span
spanOfToken (TokenSet span) = span
spanOfToken (TokenWillSet span) = span
spanOfToken (TokenDidSet span) = span
spanOfToken (TokenMinusGT span) = span
spanOfToken (TokenDotDotDot span) = span
spanOfToken (TokenInout span) = span
spanOfToken (TokenHash span) = span
spanOfToken (TokenUnderscore span) = span
spanOfToken (TokenInit span) = span
spanOfToken (TokenQuestionmark span) = span
spanOfToken (TokenBang span) = span
spanOfToken (TokenDeinit span) = span
spanOfToken (TokenExtension span) = span
spanOfToken (TokenSubscript span) = span
spanOfToken (TokenPrefix span) = span
spanOfToken (TokenOperator span) = span
spanOfToken (TokenPostfix span) = span
spanOfToken (TokenInfix span) = span
spanOfToken (TokenPrecedence span) = span
spanOfToken (TokenAssociativity span) = span
spanOfToken (TokenLeft span) = span
spanOfToken (TokenRight span) = span
spanOfToken (TokenNone span) = span
spanOfToken (TokenConvenience span) = span
spanOfToken (TokenDynamic span) = span
spanOfToken (TokenFinal span) = span
spanOfToken (TokenLazy span) = span
spanOfToken (TokenMutating span) = span
spanOfToken (TokenNonmutating span) = span
spanOfToken (TokenOptional span) = span
spanOfToken (TokenOverride span) = span
spanOfToken (TokenRequired span) = span
spanOfToken (TokenStatic span) = span
spanOfToken (TokenUnowned span) = span
spanOfToken (TokenSafe span) = span
spanOfToken (TokenUnsafe span) = span
spanOfToken (TokenWeak span) = span
spanOfToken (TokenInternal span) = span
spanOfToken (TokenPrivate span) = span
spanOfToken (TokenPublic span) = span
spanOfToken (TokenIs span) = span
spanOfToken (TokenAs span) = span
spanOfToken (TokenAt span) = span
spanOfToken (TokenLBracket span) = span
spanOfToken (TokenRBracket span) = span
spanOfToken (TokenAmp span) = span
spanOfToken (TokenFILE span) = span
spanOfToken (TokenLINE span) = span
spanOfToken (TokenCOLUMN span) = span
spanOfToken (TokenFUNCTION span) = span
spanOfToken (TokenSelf span) = span
spanOfToken (TokenSuper span) = span
spanOfToken (TokenUnownedLParensafeRParen span) = span
spanOfToken (TokenUnownedLParenunsafeRParen span) = span
spanOfToken (TokenDynamicType span) = span
spanOfToken (TokenBacktick span) = span
spanOfToken (TokenDollar span) = span
spanOfToken (TokenTrue span) = span
spanOfToken (TokenFalse span) = span
spanOfToken (TokenNil span) = span
spanOfToken (Token0b span) = span
spanOfToken (Token0o span) = span
spanOfToken (Token0x span) = span
spanOfToken (TokenTokenELowercase span) = span
spanOfToken (TokenE span) = span
spanOfToken (TokenTokenPLowercase span) = span
spanOfToken (TokenP span) = span
spanOfToken (TokenPlus span) = span
spanOfToken (TokenMinus span) = span
spanOfToken (TokenDQuotes span) = span
spanOfToken (TokenBSLParen span) = span
spanOfToken (TokenBS0 span) = span
spanOfToken (TokenBSBS span) = span
spanOfToken (TokenBSt span) = span
spanOfToken (TokenBSn span) = span
spanOfToken (TokenBSr span) = span
spanOfToken (TokenBSDQuotes span) = span
spanOfToken (TokenBS' span) = span
spanOfToken (TokenBSu span) = span
spanOfToken (TokenDiv span) = span
spanOfToken (TokenTimes span) = span
spanOfToken (TokenPercent span) = span
spanOfToken (TokenPipe span) = span
spanOfToken (TokenCaret span) = span
spanOfToken (TokenTilde span) = span
spanOfToken (TokenDotDot span) = span
spanOfToken (TokenType span) = span
spanOfToken (TokenProtocol span) = span

instance Show Token where
  showsPrec _ (TokenEOF _) = showString ""
  showsPrec _ (TokenIdentifier _ name) = showString name
  -- auto-generated below
  showsPrec _ (TokenSemicolon _) = showString ";"
  showsPrec _ (TokenFor _) = showString "for"
  showsPrec _ (TokenLParen _) = showString "("
  showsPrec _ (TokenRParen _) = showString ")"
  showsPrec _ (TokenIn _) = showString "in"
  showsPrec _ (TokenWhile _) = showString "while"
  showsPrec _ (TokenDo _) = showString "do"
  showsPrec _ (TokenIf _) = showString "if"
  showsPrec _ (TokenElse _) = showString "else"
  showsPrec _ (TokenSwitch _) = showString "switch"
  showsPrec _ (TokenLBrace _) = showString "{"
  showsPrec _ (TokenRBrace _) = showString "}"
  showsPrec _ (TokenCase _) = showString "case"
  showsPrec _ (TokenColon _) = showString ":"
  showsPrec _ (TokenComma _) = showString ","
  showsPrec _ (TokenDefault _) = showString "default"
  showsPrec _ (TokenWhere _) = showString "where"
  showsPrec _ (TokenBreak _) = showString "break"
  showsPrec _ (TokenContinue _) = showString "continue"
  showsPrec _ (TokenFallthrough _) = showString "fallthrough"
  showsPrec _ (TokenReturn _) = showString "return"
  showsPrec _ (TokenLT _) = showString "<"
  showsPrec _ (TokenGT _) = showString ">"
  showsPrec _ (TokenDoubleEq _) = showString "=="
  showsPrec _ (TokenImport _) = showString "import"
  showsPrec _ (TokenTypealias _) = showString "typealias"
  showsPrec _ (TokenStruct _) = showString "struct"
  showsPrec _ (TokenClass _) = showString "class"
  showsPrec _ (TokenEnum _) = showString "enum"
  showsPrec _ (TokenTokenProtocolLowercase _) = showString "protocol"
  showsPrec _ (TokenVar _) = showString "var"
  showsPrec _ (TokenFunc _) = showString "func"
  showsPrec _ (TokenDot _) = showString "."
  showsPrec _ (TokenLet _) = showString "let"
  showsPrec _ (TokenEq _) = showString "="
  showsPrec _ (TokenGet _) = showString "get"
  showsPrec _ (TokenSet _) = showString "set"
  showsPrec _ (TokenWillSet _) = showString "willSet"
  showsPrec _ (TokenDidSet _) = showString "didSet"
  showsPrec _ (TokenMinusGT _) = showString "->"
  showsPrec _ (TokenDotDotDot _) = showString "..."
  showsPrec _ (TokenInout _) = showString "inout"
  showsPrec _ (TokenHash _) = showString "#"
  showsPrec _ (TokenUnderscore _) = showString "_"
  showsPrec _ (TokenInit _) = showString "init"
  showsPrec _ (TokenQuestionmark _) = showString "?"
  showsPrec _ (TokenBang _) = showString "!"
  showsPrec _ (TokenDeinit _) = showString "deinit"
  showsPrec _ (TokenExtension _) = showString "extension"
  showsPrec _ (TokenSubscript _) = showString "subscript"
  showsPrec _ (TokenPrefix _) = showString "prefix"
  showsPrec _ (TokenOperator _) = showString "operator"
  showsPrec _ (TokenPostfix _) = showString "postfix"
  showsPrec _ (TokenInfix _) = showString "infix"
  showsPrec _ (TokenPrecedence _) = showString "precedence"
  showsPrec _ (TokenAssociativity _) = showString "associativity"
  showsPrec _ (TokenLeft _) = showString "left"
  showsPrec _ (TokenRight _) = showString "right"
  showsPrec _ (TokenNone _) = showString "none"
  showsPrec _ (TokenConvenience _) = showString "convenience"
  showsPrec _ (TokenDynamic _) = showString "dynamic"
  showsPrec _ (TokenFinal _) = showString "final"
  showsPrec _ (TokenLazy _) = showString "lazy"
  showsPrec _ (TokenMutating _) = showString "mutating"
  showsPrec _ (TokenNonmutating _) = showString "nonmutating"
  showsPrec _ (TokenOptional _) = showString "optional"
  showsPrec _ (TokenOverride _) = showString "override"
  showsPrec _ (TokenRequired _) = showString "required"
  showsPrec _ (TokenStatic _) = showString "static"
  showsPrec _ (TokenUnowned _) = showString "unowned"
  showsPrec _ (TokenSafe _) = showString "safe"
  showsPrec _ (TokenUnsafe _) = showString "unsafe"
  showsPrec _ (TokenWeak _) = showString "weak"
  showsPrec _ (TokenInternal _) = showString "internal"
  showsPrec _ (TokenPrivate _) = showString "private"
  showsPrec _ (TokenPublic _) = showString "public"
  showsPrec _ (TokenIs _) = showString "is"
  showsPrec _ (TokenAs _) = showString "as"
  showsPrec _ (TokenAt _) = showString "@"
  showsPrec _ (TokenLBracket _) = showString "["
  showsPrec _ (TokenRBracket _) = showString "]"
  showsPrec _ (TokenAmp _) = showString "&"
  showsPrec _ (TokenFILE _) = showString "__FILE__"
  showsPrec _ (TokenLINE _) = showString "__LINE__"
  showsPrec _ (TokenCOLUMN _) = showString "__COLUMN__"
  showsPrec _ (TokenFUNCTION _) = showString "__FUNCTION__"
  showsPrec _ (TokenSelf _) = showString "self"
  showsPrec _ (TokenSuper _) = showString "super"
  showsPrec _ (TokenUnownedLParensafeRParen _) = showString "unowned(safe)"
  showsPrec _ (TokenUnownedLParenunsafeRParen _) = showString "unowned(unsafe)"
  showsPrec _ (TokenDynamicType _) = showString "dynamicType"
  showsPrec _ (TokenBacktick _) = showString "`"
  showsPrec _ (TokenDollar _) = showString "$"
  showsPrec _ (TokenTrue _) = showString "true"
  showsPrec _ (TokenFalse _) = showString "false"
  showsPrec _ (TokenNil _) = showString "nil"
  showsPrec _ (Token0b _) = showString "0b"
  showsPrec _ (Token0o _) = showString "0o"
  showsPrec _ (Token0x _) = showString "0x"
  showsPrec _ (TokenTokenELowercase _) = showString "e"
  showsPrec _ (TokenE _) = showString "E"
  showsPrec _ (TokenTokenPLowercase _) = showString "p"
  showsPrec _ (TokenP _) = showString "P"
  showsPrec _ (TokenPlus _) = showString "+"
  showsPrec _ (TokenMinus _) = showString "-"
  showsPrec _ (TokenDQuotes _) = showString "\""
  showsPrec _ (TokenBSLParen _) = showString "\\("
  showsPrec _ (TokenBS0 _) = showString "\\0"
  showsPrec _ (TokenBSBS _) = showString "\\\\"
  showsPrec _ (TokenBSt _) = showString "\\t"
  showsPrec _ (TokenBSn _) = showString "\\n"
  showsPrec _ (TokenBSr _) = showString "\\r"
  showsPrec _ (TokenBSDQuotes _) = showString "\\\""
  showsPrec _ (TokenBS' _) = showString "\\'"
  showsPrec _ (TokenBSu _) = showString "\\u"
  showsPrec _ (TokenDiv _) = showString "/"
  showsPrec _ (TokenTimes _) = showString "*"
  showsPrec _ (TokenPercent _) = showString "%"
  showsPrec _ (TokenPipe _) = showString "|"
  showsPrec _ (TokenCaret _) = showString "^"
  showsPrec _ (TokenTilde _) = showString "~"
  showsPrec _ (TokenDotDot _) = showString ".."
  showsPrec _ (TokenType _) = showString "Type"
  showsPrec _ (TokenProtocol _) = showString "Protocol"
