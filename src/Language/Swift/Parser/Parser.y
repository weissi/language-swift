
{

module Language.Swift.Parser.Parser where

import Language.Swift.Parser.SourceLocation
import Language.Swift.Parser.Lexer
import Language.Swift.Parser.Token

import Data.Char
import Debug.Trace

    

}

%tokentype { Token }
%error { parseError }
%monad { Alex }
%name parseSwift Statements
%lexer { lexerWrapper } { TokenEOF tokenSpanEmpty }
    %token
    IDENT  { TokenIdentifier {} }
    DECIMAL_LITERAL  { DecimalLiteral {} }
    BINARY_LITERAL  { BinaryLiteral {} }
    OCTAL_LITERAL  { OctalLiteral {} }
    HEXADECIMAL_LITERAL  { HexadecimalLiteral {} }
    FLOATING_POINT_LITERAL  { FloatingPointLiteral {} }
    STRING_LITERAL  { StringLiteral {} }
    EOF { TokenEOF {} }
    -- auto-gen below
    ';'    { TokenSemicolon {} }
    'for'    { TokenFor {} }
    '('    { TokenLParen {} }
    ')'    { TokenRParen {} }
    'in'    { TokenIn {} }
    'while'    { TokenWhile {} }
    'do'    { TokenDo {} }
    'if'    { TokenIf {} }
    'else'    { TokenElse {} }
    'switch'    { TokenSwitch {} }
    '{'    { TokenLBrace {} }
    '}'    { TokenRBrace {} }
    'case'    { TokenCase {} }
    ':'    { TokenColon {} }
    ','    { TokenComma {} }
    'default'    { TokenDefault {} }
    'where'    { TokenWhere {} }
    'break'    { TokenBreak {} }
    'continue'    { TokenContinue {} }
    'fallthrough'    { TokenFallthrough {} }
    'return'    { TokenReturn {} }
    '<'    { TokenLT {} }
    '>'    { TokenGT {} }
    '=='    { TokenDoubleEq {} }
    'import'    { TokenImport {} }
    'typealias'    { TokenTypealias {} }
    'struct'    { TokenStruct {} }
    'class'    { TokenClass {} }
    'enum'    { TokenEnum {} }
    'protocol'    { TokenTokenProtocolLowercase {} }
    'var'    { TokenVar {} }
    'func'    { TokenFunc {} }
    '.'    { TokenDot {} }
    'let'    { TokenLet {} }
    '='    { TokenEq {} }
    'get'    { TokenGet {} }
    'set'    { TokenSet {} }
    'willSet'    { TokenWillSet {} }
    'didSet'    { TokenDidSet {} }
    '->'    { TokenMinusGT {} }
    '...'    { TokenDotDotDot {} }
    'inout'    { TokenInout {} }
    '#'    { TokenHash {} }
    '_'    { TokenUnderscore {} }
    'init'    { TokenInit {} }
    '?'    { TokenQuestionmark {} }
    '!'    { TokenBang {} }
    'deinit'    { TokenDeinit {} }
    'extension'    { TokenExtension {} }
    'subscript'    { TokenSubscript {} }
    'prefix'    { TokenPrefix {} }
    'operator'    { TokenOperator {} }
    'postfix'    { TokenPostfix {} }
    'infix'    { TokenInfix {} }
    'precedence'    { TokenPrecedence {} }
    'associativity'    { TokenAssociativity {} }
    'left'    { TokenLeft {} }
    'right'    { TokenRight {} }
    'none'    { TokenNone {} }
    'convenience'    { TokenConvenience {} }
    'dynamic'    { TokenDynamic {} }
    'final'    { TokenFinal {} }
    'lazy'    { TokenLazy {} }
    'mutating'    { TokenMutating {} }
    'nonmutating'    { TokenNonmutating {} }
    'optional'    { TokenOptional {} }
    'override'    { TokenOverride {} }
    'required'    { TokenRequired {} }
    'static'    { TokenStatic {} }
    'unowned'    { TokenUnowned {} }
    'safe'    { TokenSafe {} }
    'unsafe'    { TokenUnsafe {} }
    'weak'    { TokenWeak {} }
    'internal'    { TokenInternal {} }
    'private'    { TokenPrivate {} }
    'public'    { TokenPublic {} }
    'is'    { TokenIs {} }
    'as'    { TokenAs {} }
    '@'    { TokenAt {} }
    '['    { TokenLBracket {} }
    ']'    { TokenRBracket {} }
    '&'    { TokenAmp {} }
    '__FILE__'    { TokenFILE {} }
    '__LINE__'    { TokenLINE {} }
    '__COLUMN__'    { TokenCOLUMN {} }
    '__FUNCTION__'    { TokenFUNCTION {} }
    'self'    { TokenSelf {} }
    'super'    { TokenSuper {} }
    'unowned(safe)'    { TokenUnownedLParensafeRParen {} }
    'unowned(unsafe)'    { TokenUnownedLParenunsafeRParen {} }
    'dynamicType'    { TokenDynamicType {} }
    '`'    { TokenBacktick {} }
    '$'    { TokenDollar {} }
    'true'    { TokenTrue {} }
    'false'    { TokenFalse {} }
    'nil'    { TokenNil {} }
    '+'    { TokenPlus {} }
    '-'    { TokenMinus {} }
    '/'    { TokenDiv {} }
    '*'    { TokenTimes {} }
    '%'    { TokenPercent {} }
    '|'    { TokenPipe {} }
    '^'    { TokenCaret {} }
    '~'    { TokenTilde {} }
    '..'    { TokenDotDot {} }
    'Type'    { TokenType {} }
    'Protocol'    { TokenProtocol {} }

%%

Statement :: { () }
Statement
    : Expression    { () }
    | Expression ';'    { () }
    | Declaration    { () }
    | Declaration ';'    { () }
    | LoopStatement    { $1 }
    | LoopStatement ';'    { $1 }
    | BranchStatement    { () }
    | BranchStatement ';'    { () }
    | LabeledStatement    { () }
    | LabeledStatement ';'    { () }
    | ControlTransferStatement    { () }
    | ControlTransferStatement ';'    { () }

Statements :: { [()] }
Statements
    : Statement    { [$1] }
    | Statements Statement    { $2 : $1 }

LoopStatement :: { () }
LoopStatement
    : ForStatement    { $1 }
    | ForInStatement    { $1 }
    | WhileStatement    { $1 }
    | DoWhileStatement    { $1 }

ForStatement :: { () }
ForStatement
    : 'for' ';' ';' CodeBlock    { trace "for1" () }
    | 'for' ForInit ';' ';' CodeBlock    { trace "for2" () }
    | 'for' ';' Expression ';' CodeBlock    { trace "for3" () }
    | 'for' ForInit ';' Expression ';' CodeBlock    { trace "for4" () }
    | 'for' ';' ';' Expression CodeBlock    { trace "for5" () }
    | 'for' ForInit ';' ';' Expression CodeBlock    { trace "for6" () }
    | 'for' ';' Expression ';' Expression CodeBlock    { trace "for7" () }
    | 'for' ForInit ';' Expression ';' Expression CodeBlock    { trace "for8" () }
    | 'for' '(' ';' ';' ')' CodeBlock    { trace "for9" () }
    | 'for' '(' ForInit ';' ';' ')' CodeBlock    { trace "for10" () }
    | 'for' '(' ';' Expression ';' ')' CodeBlock    { trace "for11" () }
    | 'for' '(' ForInit ';' Expression ';' ')' CodeBlock    { trace "for12" () }
    | 'for' '(' ';' ';' Expression ')' CodeBlock    { trace "for13" () }
    | 'for' '(' ForInit ';' ';' Expression ')' CodeBlock    { trace "for14" () }
    | 'for' '(' ';' Expression ';' Expression ')' CodeBlock    { trace "for15" () }
    | 'for' '(' ForInit ';' Expression ';' Expression ')' CodeBlock    { trace "for16" () }

ForInit :: { () }
ForInit
    : VariableDeclaration    { () }
    | ExpressionList    { () }

ForInStatement :: { () }
ForInStatement
    : 'for' Pattern 'in' Expression CodeBlock    { trace "for-in" () }

WhileStatement :: { () }
WhileStatement
    : 'while' WhileCondition CodeBlock    { () }

WhileCondition :: { () }
WhileCondition
    : Expression    { () }
    | Declaration    { () }

DoWhileStatement :: { () }
DoWhileStatement
    : 'do' CodeBlock 'while' WhileCondition    { () }

BranchStatement :: { () }
BranchStatement
    : IfStatement    { () }
    | SwitchStatement    { () }

IfStatement :: { () }
IfStatement
    : 'if' IfCondition CodeBlock    { () }
    | 'if' IfCondition CodeBlock ElseClause    { () }

IfCondition :: { () }
IfCondition
    : Expression    { () }
    | Declaration    { () }

ElseClause :: { () }
ElseClause
    : 'else' CodeBlock    { () }
    | 'else' IfStatement    { () }

SwitchStatement :: { () }
SwitchStatement
    : 'switch' Expression '{' '}'    { () }
    | 'switch' Expression '{' SwitchCases '}'    { () }

SwitchCases :: { () }
SwitchCases
    : SwitchCase    { () }
    | SwitchCase SwitchCases    { () }

SwitchCase :: { () }
SwitchCase
    : CaseLabel Statements    { () }
    | DefaultLabel Statements    { () }
    | CaseLabel ';'    { () }
    | DefaultLabel ';'    { () }

CaseLabel :: { () }
CaseLabel
    : 'case' CaseItemList ':'    { () }

CaseItemList :: { () }
CaseItemList
    : Pattern    { () }
    | Pattern GuardClause    { () }
    | Pattern ',' CaseItemList    { () }
    | Pattern GuardClause ',' CaseItemList    { () }

DefaultLabel :: { () }
DefaultLabel
    : 'default' ':'    { () }

GuardClause :: { () }
GuardClause
    : 'where' GuardExpression    { () }

GuardExpression :: { () }
GuardExpression
    : Expression    { () }

LabeledStatement :: { () }
LabeledStatement
    : StatementLabel LoopStatement    { () }
    | StatementLabel SwitchStatement    { () }

StatementLabel :: { () }
StatementLabel
    : LabelName ':'    { () }

LabelName :: { () }
LabelName
    : Identifier    { () }

ControlTransferStatement :: { () }
ControlTransferStatement
    : BreakStatement    { () }
    | ContinueStatement    { () }
    | FallthroughStatement    { () }
    | ReturnStatement    { () }

BreakStatement :: { () }
BreakStatement
    : 'break'    { () }
    | 'break' LabelName    { () }

ContinueStatement :: { () }
ContinueStatement
    : 'continue'    { () }
    | 'continue' LabelName    { () }

FallthroughStatement :: { () }
FallthroughStatement
    : 'fallthrough'    { () }

ReturnStatement :: { () }
ReturnStatement
    : 'return'    { () }
    | 'return' Expression    { () }

GenericParameterClause :: { () }
GenericParameterClause
    : '<' GenericParameterList '>'    { () }
    | '<' GenericParameterList RequirementClause '>'    { () }

GenericParameterList :: { () }
GenericParameterList
    : GenericParameter    { () }
    | GenericParameter ',' GenericParameterList    { () }

GenericParameter :: { () }
GenericParameter
    : TypeName    { () }
    | TypeName ':' TypeIdentifier    { () }
    | TypeName ':' ProtocolCompositionType    { () }

RequirementClause :: { () }
RequirementClause
    : 'where' RequirementList    { () }

RequirementList :: { () }
RequirementList
    : Requirement    { () }
    | Requirement ',' RequirementList    { () }

Requirement :: { () }
Requirement
    : ConformanceRequirement    { () }
    | SameTypeRequirement    { () }

ConformanceRequirement :: { () }
ConformanceRequirement
    : TypeIdentifier ':' TypeIdentifier    { () }
    | TypeIdentifier ':' ProtocolCompositionType    { () }

SameTypeRequirement :: { () }
SameTypeRequirement
    : TypeIdentifier '==' TypeIdentifier    { () }

GenericArgumentClause :: { () }
GenericArgumentClause
    : '<' GenericArgumentList '>'    { () }

GenericArgumentList :: { () }
GenericArgumentList
    : GenericArgument    { () }
    | GenericArgument ',' GenericArgumentList    { () }

GenericArgument :: { () }
GenericArgument
    : Type    { () }

Declaration :: { () }
Declaration
    : ImportDeclaration    { () }
    | ConstantDeclaration    { () }
    | VariableDeclaration    { () }
    | TypealiasDeclaration    { () }
    | FunctionDeclaration    { () }
    | EnumDeclaration    { () }
    | StructDeclaration    { () }
    | ClassDeclaration    { () }
    | ProtocolDeclaration    { () }
    | InitializerDeclaration    { () }
    | DeinitializerDeclaration    { () }
    | ExtensionDeclaration    { () }
    | SubscriptDeclaration    { () }
    | OperatorDeclaration    { () }

Declarations :: { () }
Declarations
    : Declaration    { () }
    | Declaration Declarations    { () }

TopLevelDeclaration :: { () }
TopLevelDeclaration
    :    { () }
    | Statements    { () }

CodeBlock :: { () }
CodeBlock
    : '{' '}'    { () }
    | '{' Statements '}'    { () }

ImportDeclaration :: { () }
ImportDeclaration
    : 'import' ImportPath    { () }
    | Attributes 'import' ImportPath    { () }
    | 'import' ImportKind ImportPath    { () }
    | Attributes 'import' ImportKind ImportPath    { () }

ImportKind :: { () }
ImportKind
    : 'typealias'    { () }
    | 'struct'    { () }
    | 'class'    { () }
    | 'enum'    { () }
    | 'protocol'    { () }
    | 'var'    { () }
    | 'func'    { () }

ImportPath :: { () }
ImportPath
    : ImportPathIdentifier    { () }
    | ImportPathIdentifier '.' ImportPath    { () }

ImportPathIdentifier :: { () }
ImportPathIdentifier
    : Identifier    { () }
    | Operator    { () }

ConstantDeclaration :: { () }
ConstantDeclaration
    : 'let' PatternInitializerList    { () }
    | Attributes 'let' PatternInitializerList    { () }
    | DeclarationModifiers 'let' PatternInitializerList    { () }
    | Attributes DeclarationModifiers 'let' PatternInitializerList    { () }

PatternInitializerList :: { () }
PatternInitializerList
    : PatternInitializer    { () }
    | PatternInitializer ',' PatternInitializerList    { () }

PatternInitializer :: { () }
PatternInitializer
    : Pattern    { () }
    | Pattern Initializer    { () }

Initializer :: { () }
Initializer
    : '=' Expression    { () }

VariableDeclaration :: { () }
VariableDeclaration
    : VariableDeclarationHead PatternInitializerList    { () }
    | VariableDeclarationHead VariableName TypeAnnotation CodeBlock    { () }
    | VariableDeclarationHead VariableName TypeAnnotation GetterSetterBlock    { () }
    | VariableDeclarationHead VariableName TypeAnnotation GetterSetterKeywordBlock    { () }
    | VariableDeclarationHead VariableName TypeAnnotation WillsetDidsetBlock    { () }
    | VariableDeclarationHead VariableName TypeAnnotation Initializer WillsetDidsetBlock    { () }

VariableDeclarationHead :: { () }
VariableDeclarationHead
    : 'var'    { () }
    | Attributes 'var'    { () }
    | DeclarationModifiers 'var'    { () }
    | Attributes DeclarationModifiers 'var'    { () }

VariableName :: { () }
VariableName
    : Identifier    { () }

GetterSetterBlock :: { () }
GetterSetterBlock
    : '{' GetterClause '}'    { () }
    | '{' GetterClause SetterClause '}'    { () }
    | '{' SetterClause GetterClause '}'    { () }

GetterClause :: { () }
GetterClause
    : 'get' CodeBlock    { () }
    | Attributes 'get' CodeBlock    { () }

SetterClause :: { () }
SetterClause
    : 'set' CodeBlock    { () }
    | Attributes 'set' CodeBlock    { () }
    | 'set' SetterName CodeBlock    { () }
    | Attributes 'set' SetterName CodeBlock    { () }

SetterName :: { () }
SetterName
    : '(' Identifier ')'    { () }

GetterSetterKeywordBlock :: { () }
GetterSetterKeywordBlock
    : '{' GetterKeywordClause '}'    { () }
    | '{' GetterKeywordClause SetterKeywordClause '}'    { () }
    | '{' SetterKeywordClause GetterKeywordClause '}'    { () }

GetterKeywordClause :: { () }
GetterKeywordClause
    : 'get'    { () }
    | Attributes 'get'    { () }

SetterKeywordClause :: { () }
SetterKeywordClause
    : 'set'    { () }
    | Attributes 'set'    { () }

WillsetDidsetBlock :: { () }
WillsetDidsetBlock
    : '{' WillsetClause '}'    { () }
    | '{' WillsetClause DidsetClause '}'    { () }
    | '{' DidsetClause WillsetClause '}'    { () }

WillsetClause :: { () }
WillsetClause
    : 'willSet' CodeBlock    { () }
    | Attributes 'willSet' CodeBlock    { () }
    | 'willSet' SetterName CodeBlock    { () }
    | Attributes 'willSet' SetterName CodeBlock    { () }

DidsetClause :: { () }
DidsetClause
    : 'didSet' CodeBlock    { () }
    | Attributes 'didSet' CodeBlock    { () }
    | 'didSet' SetterName CodeBlock    { () }
    | Attributes 'didSet' SetterName CodeBlock    { () }

TypealiasDeclaration :: { () }
TypealiasDeclaration
    : TypealiasHead TypealiasAssignment    { () }

TypealiasHead :: { () }
TypealiasHead
    : 'typealias' TypealiasName    { () }
    | Attributes 'typealias' TypealiasName    { () }
    | AccessLevelModifier 'typealias' TypealiasName    { () }
    | Attributes AccessLevelModifier 'typealias' TypealiasName    { () }

TypealiasName :: { () }
TypealiasName
    : Identifier    { () }

TypealiasAssignment :: { () }
TypealiasAssignment
    : '=' Type    { () }

FunctionDeclaration :: { () }
FunctionDeclaration
    : FunctionHead FunctionName FunctionSignature FunctionBody    { () }
    | FunctionHead FunctionName GenericParameterClause FunctionSignature FunctionBody    { () }

FunctionHead :: { () }
FunctionHead
    : 'func'    { () }
    | Attributes 'func'    { () }
    | DeclarationModifiers 'func'    { () }
    | Attributes DeclarationModifiers 'func'    { () }

FunctionName :: { () }
FunctionName
    : Identifier    { () }
    | Operator    { () }

FunctionSignature :: { () }
FunctionSignature
    : ParameterClauses    { () }
    | ParameterClauses FunctionResult    { () }

FunctionResult :: { () }
FunctionResult
    : '->' Type    { () }
    | '->' Attributes Type    { () }

FunctionBody :: { () }
FunctionBody
    : CodeBlock    { () }

ParameterClauses :: { () }
ParameterClauses
    : ParameterClause    { () }
    | ParameterClause ParameterClauses    { () }

ParameterClause :: { () }
ParameterClause
    : '(' ')'    { () }
    | '(' ParameterList ')'    { () }
    | '(' ParameterList '...' ')'    { () }

ParameterList :: { () }
ParameterList
    : Parameter    { () }
    | Parameter ',' ParameterList    { () }

Parameter :: { () }
Parameter
    : LocalParameterName TypeAnnotation    { () }
    | 'inout' LocalParameterName TypeAnnotation    { () }
    | 'let' LocalParameterName TypeAnnotation    { () }
    | 'inout' 'let' LocalParameterName TypeAnnotation    { () }
    | '#' LocalParameterName TypeAnnotation    { () }
    | 'inout' '#' LocalParameterName TypeAnnotation    { () }
    | 'let' '#' LocalParameterName TypeAnnotation    { () }
    | 'inout' 'let' '#' LocalParameterName TypeAnnotation    { () }
    | ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'let' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' 'let' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'let' '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' 'let' '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'let' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'let' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'let' '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'let' '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'let' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'let' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'let' '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'let' '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'var' LocalParameterName TypeAnnotation    { () }
    | 'inout' 'var' LocalParameterName TypeAnnotation    { () }
    | 'var' '#' LocalParameterName TypeAnnotation    { () }
    | 'inout' 'var' '#' LocalParameterName TypeAnnotation    { () }
    | 'var' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' 'var' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'var' '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'inout' 'var' '#' ExternalParameterName LocalParameterName TypeAnnotation    { () }
    | 'var' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'var' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'var' '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'var' '#' LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'var' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'var' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'var' '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | 'inout' 'var' '#' ExternalParameterName LocalParameterName TypeAnnotation DefaultArgumentClause    { () }
    | Type    { () }
    | Attributes Type    { () }

ExternalParameterName :: { () }
ExternalParameterName
    : Identifier    { () }
    | '_'    { () }

LocalParameterName :: { () }
LocalParameterName
    : Identifier    { () }
    | '_'    { () }

DefaultArgumentClause :: { () }
DefaultArgumentClause
    : '=' Expression    { () }

EnumDeclaration :: { () }
EnumDeclaration
    : UnionStyleEnum    { () }
    | Attributes UnionStyleEnum    { () }
    | AccessLevelModifier UnionStyleEnum    { () }
    | Attributes AccessLevelModifier UnionStyleEnum    { () }
    | RawValueStyleEnum    { () }
    | Attributes RawValueStyleEnum    { () }
    | AccessLevelModifier RawValueStyleEnum    { () }
    | Attributes AccessLevelModifier RawValueStyleEnum    { () }

UnionStyleEnum :: { () }
UnionStyleEnum
    : 'enum' EnumName '{' '}'    { () }
    | 'enum' EnumName GenericParameterClause '{' '}'    { () }
    | 'enum' EnumName TypeInheritanceClause '{' '}'    { () }
    | 'enum' EnumName GenericParameterClause TypeInheritanceClause '{' '}'    { () }
    | 'enum' EnumName '{' UnionStyleEnumMembers '}'    { () }
    | 'enum' EnumName GenericParameterClause '{' UnionStyleEnumMembers '}'    { () }
    | 'enum' EnumName TypeInheritanceClause '{' UnionStyleEnumMembers '}'    { () }
    | 'enum' EnumName GenericParameterClause TypeInheritanceClause '{' UnionStyleEnumMembers '}'    { () }

UnionStyleEnumMembers :: { () }
UnionStyleEnumMembers
    : UnionStyleEnumMember    { () }
    | UnionStyleEnumMember UnionStyleEnumMembers    { () }

UnionStyleEnumMember :: { () }
UnionStyleEnumMember
    : Declaration    { () }
    | UnionStyleEnumCaseClause    { () }

UnionStyleEnumCaseClause :: { () }
UnionStyleEnumCaseClause
    : 'case' UnionStyleEnumCaseList    { () }
    | Attributes 'case' UnionStyleEnumCaseList    { () }

UnionStyleEnumCaseList :: { () }
UnionStyleEnumCaseList
    : UnionStyleEnumCase    { () }
    | UnionStyleEnumCase ',' UnionStyleEnumCaseList    { () }

UnionStyleEnumCase :: { () }
UnionStyleEnumCase
    : EnumCaseName    { () }
    | EnumCaseName TupleType    { () }

EnumName :: { () }
EnumName
    : Identifier    { () }

EnumCaseName :: { () }
EnumCaseName
    : Identifier    { () }

RawValueStyleEnum :: { () }
RawValueStyleEnum
    : 'enum' EnumName TypeInheritanceClause '{' RawValueStyleEnumMembers '}'    { () }
    | 'enum' EnumName GenericParameterClause TypeInheritanceClause '{' RawValueStyleEnumMembers '}'    { () }

RawValueStyleEnumMembers :: { () }
RawValueStyleEnumMembers
    : RawValueStyleEnumMember    { () }
    | RawValueStyleEnumMember RawValueStyleEnumMembers    { () }

RawValueStyleEnumMember :: { () }
RawValueStyleEnumMember
    : Declaration    { () }
    | RawValueStyleEnumCaseClause    { () }

RawValueStyleEnumCaseClause :: { () }
RawValueStyleEnumCaseClause
    : 'case' RawValueStyleEnumCaseList    { () }
    | Attributes 'case' RawValueStyleEnumCaseList    { () }

RawValueStyleEnumCaseList :: { () }
RawValueStyleEnumCaseList
    : RawValueStyleEnumCase    { () }
    | RawValueStyleEnumCase ',' RawValueStyleEnumCaseList    { () }

RawValueStyleEnumCase :: { () }
RawValueStyleEnumCase
    : EnumCaseName    { () }
    | EnumCaseName RawValueAssignment    { () }

RawValueAssignment :: { () }
RawValueAssignment
    : '=' Literal    { () }

StructDeclaration :: { () }
StructDeclaration
    : 'struct' StructName StructBody    { () }
    | Attributes 'struct' StructName StructBody    { () }
    | AccessLevelModifier 'struct' StructName StructBody    { () }
    | Attributes AccessLevelModifier 'struct' StructName StructBody    { () }
    | 'struct' StructName GenericParameterClause StructBody    { () }
    | Attributes 'struct' StructName GenericParameterClause StructBody    { () }
    | AccessLevelModifier 'struct' StructName GenericParameterClause StructBody    { () }
    | Attributes AccessLevelModifier 'struct' StructName GenericParameterClause StructBody    { () }
    | 'struct' StructName TypeInheritanceClause StructBody    { () }
    | Attributes 'struct' StructName TypeInheritanceClause StructBody    { () }
    | AccessLevelModifier 'struct' StructName TypeInheritanceClause StructBody    { () }
    | Attributes AccessLevelModifier 'struct' StructName TypeInheritanceClause StructBody    { () }
    | 'struct' StructName GenericParameterClause TypeInheritanceClause StructBody    { () }
    | Attributes 'struct' StructName GenericParameterClause TypeInheritanceClause StructBody    { () }
    | AccessLevelModifier 'struct' StructName GenericParameterClause TypeInheritanceClause StructBody    { () }
    | Attributes AccessLevelModifier 'struct' StructName GenericParameterClause TypeInheritanceClause StructBody    { () }

StructName :: { () }
StructName
    : Identifier    { () }

StructBody :: { () }
StructBody
    : '{' '}'    { () }
    | '{' Declarations '}'    { () }

ClassDeclaration :: { () }
ClassDeclaration
    : 'class' ClassName ClassBody    { () }
    | Attributes 'class' ClassName ClassBody    { () }
    | AccessLevelModifier 'class' ClassName ClassBody    { () }
    | Attributes AccessLevelModifier 'class' ClassName ClassBody    { () }
    | 'class' ClassName GenericParameterClause ClassBody    { () }
    | Attributes 'class' ClassName GenericParameterClause ClassBody    { () }
    | AccessLevelModifier 'class' ClassName GenericParameterClause ClassBody    { () }
    | Attributes AccessLevelModifier 'class' ClassName GenericParameterClause ClassBody    { () }
    | 'class' ClassName TypeInheritanceClause ClassBody    { () }
    | Attributes 'class' ClassName TypeInheritanceClause ClassBody    { () }
    | AccessLevelModifier 'class' ClassName TypeInheritanceClause ClassBody    { () }
    | Attributes AccessLevelModifier 'class' ClassName TypeInheritanceClause ClassBody    { () }
    | 'class' ClassName GenericParameterClause TypeInheritanceClause ClassBody    { () }
    | Attributes 'class' ClassName GenericParameterClause TypeInheritanceClause ClassBody    { () }
    | AccessLevelModifier 'class' ClassName GenericParameterClause TypeInheritanceClause ClassBody    { () }
    | Attributes AccessLevelModifier 'class' ClassName GenericParameterClause TypeInheritanceClause ClassBody    { () }

ClassName :: { () }
ClassName
    : Identifier    { () }

ClassBody :: { () }
ClassBody
    : '{' '}'    { () }
    | '{' Declarations '}'    { () }

ProtocolDeclaration :: { () }
ProtocolDeclaration
    : 'protocol' ProtocolName ProtocolBody    { () }
    | Attributes 'protocol' ProtocolName ProtocolBody    { () }
    | AccessLevelModifier 'protocol' ProtocolName ProtocolBody    { () }
    | Attributes AccessLevelModifier 'protocol' ProtocolName ProtocolBody    { () }
    | 'protocol' ProtocolName TypeInheritanceClause ProtocolBody    { () }
    | Attributes 'protocol' ProtocolName TypeInheritanceClause ProtocolBody    { () }
    | AccessLevelModifier 'protocol' ProtocolName TypeInheritanceClause ProtocolBody    { () }
    | Attributes AccessLevelModifier 'protocol' ProtocolName TypeInheritanceClause ProtocolBody    { () }

ProtocolName :: { () }
ProtocolName
    : Identifier    { () }

ProtocolBody :: { () }
ProtocolBody
    : '{' '}'    { () }
    | '{' ProtocolMemberDeclarations '}'    { () }

ProtocolMemberDeclaration :: { () }
ProtocolMemberDeclaration
    : ProtocolPropertyDeclaration    { () }
    | ProtocolMethodDeclaration    { () }
    | ProtocolInitializerDeclaration    { () }
    | ProtocolSubscriptDeclaration    { () }
    | ProtocolAssociatedTypeDeclaration    { () }

ProtocolMemberDeclarations :: { () }
ProtocolMemberDeclarations
    : ProtocolMemberDeclaration    { () }
    | ProtocolMemberDeclaration ProtocolMemberDeclarations    { () }

ProtocolPropertyDeclaration :: { () }
ProtocolPropertyDeclaration
    : VariableDeclarationHead VariableName TypeAnnotation GetterSetterKeywordBlock    { () }

ProtocolMethodDeclaration :: { () }
ProtocolMethodDeclaration
    : FunctionHead FunctionName FunctionSignature    { () }
    | FunctionHead FunctionName GenericParameterClause FunctionSignature    { () }

ProtocolInitializerDeclaration :: { () }
ProtocolInitializerDeclaration
    : InitializerHead ParameterClause    { () }
    | InitializerHead GenericParameterClause ParameterClause    { () }

ProtocolSubscriptDeclaration :: { () }
ProtocolSubscriptDeclaration
    : SubscriptHead SubscriptResult GetterSetterKeywordBlock    { () }

ProtocolAssociatedTypeDeclaration :: { () }
ProtocolAssociatedTypeDeclaration
    : TypealiasHead    { () }
    | TypealiasHead TypeInheritanceClause    { () }
    | TypealiasHead TypealiasAssignment    { () }
    | TypealiasHead TypeInheritanceClause TypealiasAssignment    { () }

InitializerDeclaration :: { () }
InitializerDeclaration
    : InitializerHead ParameterClause InitializerBody    { () }
    | InitializerHead GenericParameterClause ParameterClause InitializerBody    { () }

InitializerHead :: { () }
InitializerHead
    : 'init'    { () }
    | Attributes 'init'    { () }
    | DeclarationModifiers 'init'    { () }
    | Attributes DeclarationModifiers 'init'    { () }
    | 'init' '?'    { () }
    | Attributes 'init' '?'    { () }
    | DeclarationModifiers 'init' '?'    { () }
    | Attributes DeclarationModifiers 'init' '?'    { () }
    | 'init' '!'    { () }
    | Attributes 'init' '!'    { () }
    | DeclarationModifiers 'init' '!'    { () }
    | Attributes DeclarationModifiers 'init' '!'    { () }

InitializerBody :: { () }
InitializerBody
    : CodeBlock    { () }

DeinitializerDeclaration :: { () }
DeinitializerDeclaration
    : 'deinit' CodeBlock    { () }
    | Attributes 'deinit' CodeBlock    { () }

ExtensionDeclaration :: { () }
ExtensionDeclaration
    : 'extension' TypeIdentifier ExtensionBody    { () }
    | AccessLevelModifier 'extension' TypeIdentifier ExtensionBody    { () }
    | 'extension' TypeIdentifier TypeInheritanceClause ExtensionBody    { () }
    | AccessLevelModifier 'extension' TypeIdentifier TypeInheritanceClause ExtensionBody    { () }

ExtensionBody :: { () }
ExtensionBody
    : '{' '}'    { () }
    | '{' Declarations '}'    { () }

SubscriptDeclaration :: { () }
SubscriptDeclaration
    : SubscriptHead SubscriptResult CodeBlock    { () }
    | SubscriptHead SubscriptResult GetterSetterBlock    { () }
    | SubscriptHead SubscriptResult GetterSetterKeywordBlock    { () }

SubscriptHead :: { () }
SubscriptHead
    : 'subscript' ParameterClause    { () }
    | Attributes 'subscript' ParameterClause    { () }
    | DeclarationModifiers 'subscript' ParameterClause    { () }
    | Attributes DeclarationModifiers 'subscript' ParameterClause    { () }

SubscriptResult :: { () }
SubscriptResult
    : '->' Type    { () }
    | '->' Attributes Type    { () }

OperatorDeclaration :: { () }
OperatorDeclaration
    : PrefixOperatorDeclaration    { () }
    | PostfixOperatorDeclaration    { () }
    | InfixOperatorDeclaration    { () }

PrefixOperatorDeclaration :: { () }
PrefixOperatorDeclaration
    : 'prefix' 'operator' Operator '{' '}'    { () }

PostfixOperatorDeclaration :: { () }
PostfixOperatorDeclaration
    : 'postfix' 'operator' Operator '{' '}'    { () }

InfixOperatorDeclaration :: { () }
InfixOperatorDeclaration
    : 'infix' 'operator' Operator '{' '}'    { () }
    | 'infix' 'operator' Operator '{' InfixOperatorAttributes '}'    { () }

InfixOperatorAttributes :: { () }
InfixOperatorAttributes
    :    { () }
    | PrecedenceClause    { () }
    | AssociativityClause    { () }
    | PrecedenceClause AssociativityClause    { () }

PrecedenceClause :: { () }
PrecedenceClause
    : 'precedence' PrecedenceLevel    { () }

PrecedenceLevel :: { () }
PrecedenceLevel
    : DECIMAL_LITERAL {- officially only 0 to 255 -}    { () }

AssociativityClause :: { () }
AssociativityClause
    : 'associativity' Associativity    { () }

Associativity :: { () }
Associativity
    : 'left'    { () }
    | 'right'    { () }
    | 'none'    { () }

DeclarationModifier :: { () }
DeclarationModifier
    : 'class'    { () }
    | 'convenience'    { () }
    | 'dynamic'    { () }
    | 'final'    { () }
    | 'infix'    { () }
    | 'lazy'    { () }
    | 'mutating'    { () }
    | 'nonmutating'    { () }
    | 'optional'    { () }
    | 'override'    { () }
    | 'postfix'    { () }
    | 'prefix'    { () }
    | 'required'    { () }
    | 'static'    { () }
    | 'unowned'    { () }
    | 'unowned' '(' 'safe' ')'    { () }
    | 'unowned' '(' 'unsafe' ')'    { () }
    | 'weak'    { () }
    | AccessLevelModifier    { () }

DeclarationModifiers :: { () }
DeclarationModifiers
    : DeclarationModifier    { () }
    | DeclarationModifier DeclarationModifiers    { () }

AccessLevelModifier :: { () }
AccessLevelModifier
    : 'internal'    { () }
    | 'internal' '(' 'set' ')'    { () }
    | 'private'    { () }
    | 'private' '(' 'set' ')'    { () }
    | 'public'    { () }
    | 'public' '(' 'set' ')'    { () }

AccessLevelModifiers :: { () }
AccessLevelModifiers
    : AccessLevelModifier    { () }
    | AccessLevelModifier AccessLevelModifiers    { () }

Pattern :: { () }
Pattern
    : WildcardPattern    { () }
    | WildcardPattern TypeAnnotation    { () }
    | IdentifierPattern    { () }
    | IdentifierPattern TypeAnnotation    { () }
    | ValueBindingPattern    { () }
    | TuplePattern    { () }
    | TuplePattern TypeAnnotation    { () }
    | EnumCasePattern    { () }
    | TypeCastingPattern    { () }
    | ExpressionPattern    { () }

WildcardPattern :: { () }
WildcardPattern
    : '_'    { () }

IdentifierPattern :: { () }
IdentifierPattern
    : Identifier    { () }

ValueBindingPattern :: { () }
ValueBindingPattern
    : 'var' Pattern    { () }
    | 'let' Pattern    { () }

TuplePattern :: { () }
TuplePattern
    : '(' ')'    { () }
    | '(' TuplePatternElementList ')'    { () }

TuplePatternElementList :: { () }
TuplePatternElementList
    : TuplePatternElement    { () }
    | TuplePatternElement ',' TuplePatternElementList    { () }

TuplePatternElement :: { () }
TuplePatternElement
    : Pattern    { () }

EnumCasePattern :: { () }
EnumCasePattern
    : '.' EnumCaseName    { () }
    | TypeIdentifier '.' EnumCaseName    { () }
    | '.' EnumCaseName TuplePattern    { () }
    | TypeIdentifier '.' EnumCaseName TuplePattern    { () }

TypeCastingPattern :: { () }
TypeCastingPattern
    : IsPattern    { () }
    | AsPattern    { () }

IsPattern :: { () }
IsPattern
    : 'is' Type    { () }

AsPattern :: { () }
AsPattern
    : Pattern 'as' Type    { () }

ExpressionPattern :: { () }
ExpressionPattern
    : Expression    { () }

Attribute :: { () }
Attribute
    : '@' AttributeName    { () }
    | '@' AttributeName AttributeArgumentClause    { () }

AttributeName :: { () }
AttributeName
    : Identifier    { () }

AttributeArgumentClause :: { () }
AttributeArgumentClause
    : '(' ')'    { () }
    | '(' BalancedTokens ')'    { () }

Attributes :: { () }
Attributes
    : Attribute    { () }
    | Attribute Attributes    { () }

BalancedTokens :: { () }
BalancedTokens
    : BalancedToken    { () }
    | BalancedToken BalancedTokens    { () }

BalancedToken :: { () }
BalancedToken
    : '(' ')'    { () }
    | '(' BalancedTokens ')'    { () }
    | '[' ']'    { () }
    | '[' BalancedTokens ']'    { () }
    | '{' '}'    { () }
    | '{' BalancedTokens '}'    { () }
    | Literal    { () }
    | Identifier    { () }
    | Operator    { () }

{- FIXME: WORK needed: official:
    | TEXT-DESCRIPTION (Any identifier, keyword, literal, or operator)
    | TEXT-DESCRIPTION (Any punctuation except)
-}

Expression :: { () }
Expression
    : PrefixExpression    { () }
    | PrefixExpression BinaryExpressions    { () }

ExpressionList :: { () }
ExpressionList
    : Expression    { () }
    | Expression ',' ExpressionList    { () }

PrefixExpression :: { () }
PrefixExpression
    : PostfixExpression    { () }
    | PrefixOperator PostfixExpression    { () }
    | InOutExpression    { () }

InOutExpression :: { () }
InOutExpression
    : '&' Identifier    { () }

BinaryExpression :: { () }
BinaryExpression
    : BinaryOperator PrefixExpression    { () }
    | AssignmentOperator PrefixExpression    { () }
    | ConditionalOperator PrefixExpression    { () }
    | TypeCastingOperator    { () }

BinaryExpressions :: { () }
BinaryExpressions
    : BinaryExpression    { () }
    | BinaryExpression BinaryExpressions    { () }

AssignmentOperator :: { () }
AssignmentOperator
    : '='    { () }

ConditionalOperator :: { () }
ConditionalOperator
    : '?' Expression ':'    { () }

TypeCastingOperator :: { () }
TypeCastingOperator
    : 'is' Type    { () }
    | 'as' Type    { () }
    | 'as' '?' Type    { () }

PrimaryExpression :: { () }
PrimaryExpression
    : Identifier    { () }
    | Identifier GenericArgumentClause    { () }
    | LiteralExpression    { () }
    | SelfExpression    { () }
    | SuperclassExpression    { () }
    | ClosureExpression    { () }
    | ParenthesizedExpression    { () }
    | ImplicitMemberExpression    { () }
    | WildcardExpression    { () }

LiteralExpression :: { () }
LiteralExpression
    : Literal    { () }
    | ArrayLiteral    { () }
    | DictionaryLiteral    { () }
    | '__FILE__'    { () }
    | '__LINE__'    { () }
    | '__COLUMN__'    { () }
    | '__FUNCTION__'    { () }

ArrayLiteral :: { () }
ArrayLiteral
    : '[' ']'    { () }
    | '[' ArrayLiteralItems ']'    { () }

ArrayLiteralItems :: { () }
ArrayLiteralItems
    : ArrayLiteralItem    { () }
    | ArrayLiteralItem ','    { () }
    | ArrayLiteralItem ',' ArrayLiteralItems    { () }

ArrayLiteralItem :: { () }
ArrayLiteralItem
    : Expression    { () }

DictionaryLiteral :: { () }
DictionaryLiteral
    : '[' DictionaryLiteralItems ']'    { () }
    | '[' ':' ']'    { () }

DictionaryLiteralItems :: { () }
DictionaryLiteralItems
    : DictionaryLiteralItem    { () }
    | DictionaryLiteralItem ','    { () }
    | DictionaryLiteralItem ',' DictionaryLiteralItems    { () }

DictionaryLiteralItem :: { () }
DictionaryLiteralItem
    : Expression ':' Expression    { () }

SelfExpression :: { () }
SelfExpression
    : 'self'    { () }
    | 'self' '.' Identifier    { () }
    | 'self' '[' Expression ']'    { () }
    | 'self' '.' 'init'    { () }

SuperclassExpression :: { () }
SuperclassExpression
    : SuperclassMethodExpression    { () }
    | SuperclassSubscriptExpression    { () }
    | SuperclassInitializerExpression    { () }

SuperclassMethodExpression :: { () }
SuperclassMethodExpression
    : 'super' '.' Identifier    { () }

SuperclassSubscriptExpression :: { () }
SuperclassSubscriptExpression
    : 'super' '[' Expression ']'    { () }

SuperclassInitializerExpression :: { () }
SuperclassInitializerExpression
    : 'super' '.' 'init'    { () }

ClosureExpression :: { () }
ClosureExpression
    : '{' Statements '}'    { () }
    | '{' ClosureSignature Statements '}'    { () }

ClosureSignature :: { () }
ClosureSignature
    : ParameterClause 'in'    { () }
    | ParameterClause FunctionResult 'in'    { () }
    | IdentifierList 'in'    { () }
    | IdentifierList FunctionResult 'in'    { () }
    | CaptureList ParameterClause 'in'    { () }
    | CaptureList ParameterClause FunctionResult 'in'    { () }
    | CaptureList IdentifierList 'in'    { () }
    | CaptureList IdentifierList FunctionResult 'in'    { () }
    | CaptureList 'in'    { () }

CaptureList :: { () }
CaptureList
    : '[' CaptureSpecifier Expression ']'    { () }

CaptureSpecifier :: { () }
CaptureSpecifier
    : 'weak'    { () }
    | 'unowned'    { () }
    | 'unowned(safe)'    { () }
    | 'unowned(unsafe)'    { () }

ImplicitMemberExpression :: { () }
ImplicitMemberExpression
    : '.' Identifier    { () }

ParenthesizedExpression :: { () }
ParenthesizedExpression
    : '(' ')'    { () }
    | '(' ExpressionElementList ')'    { () }

ExpressionElementList :: { () }
ExpressionElementList
    : ExpressionElement    { () }
    | ExpressionElement ',' ExpressionElementList    { () }

ExpressionElement :: { () }
ExpressionElement
    : Expression    { () }
    | Identifier ':' Expression    { () }

WildcardExpression :: { () }
WildcardExpression
    : '_'    { () }

PostfixExpression :: { () }
PostfixExpression
    : PrimaryExpression    { () }
    | PostfixExpression PostfixOperator    { () }
    | FunctionCallExpression    { () }
    | InitializerExpression    { () }
    | ExplicitMemberExpression    { () }
    | PostfixSelfExpression    { () }
    | DynamicTypeExpression    { () }
    | SubscriptExpression    { () }
    | ForcedValueExpression    { () }
    | OptionalChainingExpression    { () }

FunctionCallExpression :: { () }
FunctionCallExpression
    : PostfixExpression ParenthesizedExpression    { () }
    | PostfixExpression TrailingClosure    { () }
    | PostfixExpression ParenthesizedExpression TrailingClosure    { () }

TrailingClosure :: { () }
TrailingClosure
    : ClosureExpression    { () }

InitializerExpression :: { () }
InitializerExpression
    : PostfixExpression '.' 'init'    { () }

ExplicitMemberExpression :: { () }
ExplicitMemberExpression
    : PostfixExpression '.' DECIMAL_LITERAL    { () }
    | PostfixExpression '.' Identifier    { () }
    | PostfixExpression '.' Identifier GenericArgumentClause    { () }

PostfixSelfExpression :: { () }
PostfixSelfExpression
    : PostfixExpression '.' 'self'    { () }

DynamicTypeExpression :: { () }
DynamicTypeExpression
    : PostfixExpression '.' 'dynamicType'    { () }

SubscriptExpression :: { () }
SubscriptExpression
    : PostfixExpression '[' ExpressionList ']'    { () }

ForcedValueExpression :: { () }
ForcedValueExpression
    : PostfixExpression '!'    { () }

OptionalChainingExpression :: { () }
OptionalChainingExpression
    : PostfixExpression '?'    { () }

Identifier :: { () }
Identifier
    : IDENT    { () }

IdentifierList :: { () }
IdentifierList
    : Identifier    { () }
    | Identifier ',' IdentifierList    { () }

ImplicitParameterName :: { () }
ImplicitParameterName
    : '$' DECIMAL_LITERAL    { () }

Literal :: { () }
Literal
    : IntegerLiteral    { () }
    | FloatingPointLiteral    { () }
    | StringLiteral    { () }
    | 'true'    { () }
    | 'false'    { () }
    | 'nil'    { () }

IntegerLiteral :: { () }
IntegerLiteral
    : BinaryLiteral    { () }
    | OctalLiteral    { () }
    | DecimalLiteral    { () }
    | HexadecimalLiteral    { () }

BinaryLiteral :: { () }
BinaryLiteral
    : BINARY_LITERAL { () }

OctalLiteral :: { () }
OctalLiteral
    : OCTAL_LITERAL    { () }

DecimalLiteral :: { () }
DecimalLiteral
    : DECIMAL_LITERAL    { () }

HexadecimalLiteral :: { () }
HexadecimalLiteral
    : HEXADECIMAL_LITERAL    { () }

FloatingPointLiteral :: { () }
FloatingPointLiteral
    : FLOATING_POINT_LITERAL    { () }

StringLiteral :: { () }
StringLiteral
    : STRING_LITERAL    { () }

Operator :: { () }
Operator
    : OperatorHead    { () }
    | OperatorHead OperatorCharacters    { () }
    | DotOperatorHead    { () }
    | DotOperatorHead DotOperatorCharacters    { () }

OperatorHead :: { () }
OperatorHead
    : '/'    { () }
    | '='    { () }
    | '-'    { () }
    | '+'    { () }
    | '!'    { () }
    | '*'    { () }
    | '%'    { () }
    | '<'    { () }
    | '>'    { () }
    | '&'    { () }
    | '|'    { () }
    | '^'    { () }
    | '~'    { () }
    | '?'    { () }

OperatorCharacter :: { () }
OperatorCharacter
    : OperatorHead    { () }

OperatorCharacters :: { () }
OperatorCharacters
    : OperatorCharacter    { () }
    | OperatorCharacter OperatorCharacters    { () }

DotOperatorHead :: { () }
DotOperatorHead
    : '..'    { () }

DotOperatorCharacter :: { () }
DotOperatorCharacter
    : '.'    { () }
    | OperatorCharacter    { () }

DotOperatorCharacters :: { () }
DotOperatorCharacters
    : DotOperatorCharacter    { () }
    | DotOperatorCharacter DotOperatorCharacters    { () }

BinaryOperator :: { () }
BinaryOperator
    : Operator    { () }

PrefixOperator :: { () }
PrefixOperator
    : Operator    { () }

PostfixOperator :: { () }
PostfixOperator
    : Operator    { () }

Type :: { () }
Type
    : ArrayType    { () }
    | DictionaryType    { () }
    | FunctionType    { () }
    | TypeIdentifier    { () }
    | TupleType    { () }
    | OptionalType    { () }
    | ImplicitlyUnwrappedOptionalType    { () }
    | ProtocolCompositionType    { () }
    | MetatypeType    { () }

TypeAnnotation :: { () }
TypeAnnotation
    : ':' Type    { () }
    | ':' Attributes Type    { () }

TypeIdentifier :: { () }
TypeIdentifier
    : TypeName    { () }
    | TypeName GenericArgumentClause    { () }
    | TypeName '.' TypeIdentifier    { () }
    | TypeName GenericArgumentClause '.' TypeIdentifier    { () }

TypeName :: { () }
TypeName
    : Identifier    { () }

TupleType :: { () }
TupleType
    : '(' ')'    { () }
    | '(' TupleTypeBody ')'    { () }

TupleTypeBody :: { () }
TupleTypeBody
    : TupleTypeElementList    { () }
    | TupleTypeElementList '...'    { () }

TupleTypeElementList :: { () }
TupleTypeElementList
    : TupleTypeElement    { () }
    | TupleTypeElement ',' TupleTypeElementList    { () }

TupleTypeElement :: { () }
TupleTypeElement
    : Type    { () }
    | Attributes Type    { () }
    | 'inout' Type    { () }
    | Attributes 'inout' Type    { () }
    | ElementName TypeAnnotation    { () }
    | 'inout' ElementName TypeAnnotation    { () }

ElementName :: { () }
ElementName
    : Identifier    { () }

FunctionType :: { () }
FunctionType
    : Type '->' Type    { () }

ArrayType :: { () }
ArrayType
    : '[' Type ']'    { () }

DictionaryType :: { () }
DictionaryType
    : '[' Type ':' Type ']'    { () }

OptionalType :: { () }
OptionalType
    : Type '?'    { () }

ImplicitlyUnwrappedOptionalType :: { () }
ImplicitlyUnwrappedOptionalType
    : Type '!'    { () }

ProtocolCompositionType :: { () }
ProtocolCompositionType
    : 'protocol' '<' '>'    { () }
    | 'protocol' '<' ProtocolIdentifierList '>'    { () }

ProtocolIdentifierList :: { () }
ProtocolIdentifierList
    : ProtocolIdentifier    { () }
    | ProtocolIdentifier ',' ProtocolIdentifierList    { () }

ProtocolIdentifier :: { () }
ProtocolIdentifier
    : TypeIdentifier    { () }

MetatypeType :: { () }
MetatypeType
    : Type '.' 'Type'    { () }
    | Type '.' 'Protocol'    { () }

TypeInheritanceClause :: { () }
TypeInheritanceClause
    : ':' ClassRequirement ',' TypeInheritanceList    { () }
    | ':' ClassRequirement    { () }
    | ':' TypeInheritanceList    { () }

TypeInheritanceList :: { () }
TypeInheritanceList
    : TypeIdentifier    { () }
    | TypeIdentifier ',' TypeInheritanceList    { () }

ClassRequirement :: { () }
ClassRequirement
    : 'class'    { () }


{

lexerWrapper :: (Token -> Alex a) -> Alex a
lexerWrapper = (alexMonadScan >>=)

parseError :: Token -> a
parseError t = error $ "Parse error, error token: " ++ show t

parse s = runAlex s parseSwift

main =
    do s <- getContents
       print s
       print $ parse s

}
