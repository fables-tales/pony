{
module Parse where
}

%name ponyParse
%tokentype { Token }
%error { parseError }

%token
    'if' { Token _ _ If }
    'else' { Token _ _ Else }
    'module' { Token _ _ Module }
    'variant' { Token _ _ Variant }
    'case' { Token _ _ Case }
    'for' { Token _ _ For }
    'while' { Token _ _ While }
    'or' { Token _ _ Or }
    'and' { Token  _ _ And }
    'import' { Token _ _ Import }
    'export' { Token _ _ Export }    
    'return' { Token _ _ Return }
    'do' { Token _ _ Do }
    'enum' { Token _ _ Enum }
    'in' { Token _ _ In}
    ';' { Token _ _ Semicolon }
    '+' { Token _ _ Plus }
    '-' { Token _ _ Dash }
    '*' { Token _ _ Asterisk }
    '/' { Token _ _ Slash }
    '\\' { Token _ _ Backslash}
    '>=' { Token _ _ GEq }
    '<=' { Token _ _ LEq }
    '!' { Token _ _ Exclamation}
    '>' { Token _ _ CAngleBracket }
    '<' { Token _ _ OAngleBracket }
    '%' { Token _ _ Percent }
    '^' { Token _ _ Caret}
    '==' { Token _ _ DoubleEq }
    '!=' { Token _ _ NEq }
    '=' { Token _ _ Eq }
    '(' { Token _ _ OParen }
    ')' { Token _ _ CParen }
    '{' { Token _ _ OBrace }
    '}' { Token _ _ CBrace }
    '[' { Token _ _ OSqB } 
    ']' { Token _ _ CSqB } 
    ':=' { Token _ _ ColonEq }
    '_' { Token _ _ Underscore }
    '<-' { Token _ _ LeftArrow }
    '->' { Token _ _ RightArrow } 
    '::' { Token _ _ DoubleColon }
    ':'  { Token _ _ Colon }
    '.'  { Token _ _ Dot }
    ','  { Token _ _ Comma}
    '|'  { Token _ _ Pipe }
    MODULE { Token _ _ (ModuleName $$) }
    TYPENAME { Token _ _ (TypeName $$) }
    SYMBOL   { Token _ _ (Symbol $$)}
    INTLITERAL { Token _ _ (LexIntLiteral $$) }
    REALLITERAL { Token _ _ (LexRealLiteral $$) }
    ATSYMBOL { Token _ _ (LexAtSymbol $$) }
    STRING {Token _ _ (LexStringLiteral $$)}
    CHARACTER {Token _ _ (LexCharLiteral $$)}
%right '<-'
%left 'or'
%left 'and'
%nonassoc '==' '!=' '<' '<=' '>' '>='
%left '::'
%left ':'
%left '+' '-'
%left '*' '/' '%'
%right CALL
%nonassoc UNARY
%right '.' '^'
%nonassoc '(' ')' '[' ']'
%%
translationUnit :: {Module}
translationUnit : topLevelStatements { Module ["anon"] $1 } 
                | moduleDeclaration topLevelStatements { Module $1 $2 }

moduleDeclaration :: {[String]}
moduleDeclaration : 'module' MODULE ';' { $2 }

topLevelStatements :: {[TopLevelStatement]}
topLevelStatements : topLevelStatements topLevelStatement {$1 ++ [$2]}
                   | {-empty-} {[]}

topLevelStatement :: {TopLevelStatement}
topLevelStatement : exportStatement {$1}
                  | importStatement {$1}
                  | declaration {asTop $1}
                  | assignment {asTop $1}

exportStatement :: {TopLevelStatement}
exportStatement : 'export' typenameList ';' {ExportTypeName $2}
                | 'export' symbolList ';' {ExportSymbol $2}

typenameList :: {[String]}
typenameList : TYPENAME ',' typenameList {$1:$3}
             | TYPENAME {[$1]}

symbolList :: {[String]}
symbolList : SYMBOL ',' symbolList {$1 : $3}
           | SYMBOL {[$1]}

importStatement :: {TopLevelStatement}
importStatement : 'import' MODULE '.' type ';' {ImportModuleType $2 $4}
                | 'import' MODULE '.' SYMBOL ';' {ImportModuleSymbol $2 $4}
                | 'import' MODULE '.' '*' ';' {ImportModuleAll $2}

declaration :: {Statement}
declaration : qualifiedSymbol '::' type ';' {Declaration $1 $3}

qualifiedSymbol :: {QualifiedSymbol}
qualifiedSymbol : SYMBOL {UntypedQualifiedSymbol $1}
                | SYMBOL type {TypedQualifiedSymbol $1 $2}

assignment :: {Statement}
assignment : qualifiedSymbol '=' expression ';' {Assign $1 $3}
           | shortFormFunction {$1}

guardedPatternList :: {GuardedPatternList}
guardedPatternList : patternList guard {FullGuard $1 $2}
                   | patternList {PatternOnlyGuard $1}
                   | guard {GuardOnlyGuard $1}
                   | {-empty-} {NoGuard}

shortFormFunction :: {ShortFormFunction}
shortFormFunction : qualifiedSymbol '(' guardedPatternList ')' functionBody ';' {SFF $1 $3 $5}

functionBody :: {FunctionBody}
functionBody : block {BlockBody $1}
             | ':=' expression {ExpressionBody $2}

block :: {Block}
block : '{' statements '}' {Block $2}

statements :: {[Statement]}
statements : statement statements {$1 : $2}
           | {-empty-} {[]}

guard :: {Guard}
guard : '|' expression {BooleanGuard $2}


type :: {Type}
type : TYPENAME {UnqualifiedName $1}
     | TYPENAME typeQualification {QualifiedName $1 $2}
     | tupleType {$1}
     | functionType {$1}
     | enumType {$1}
     | variantType {$1}

variantType :: {Type}
variantType : 'variant' variantOptions {VariantType $2}


variantOptions :: {[VariantOption]}
variantOptions : variantOptions ',' variantOption {$1 ++ [$2]}
               | variantOption {[$1]}

variantOption :: {VariantOption}
variantOption : TYPENAME tupleType  {VariantOption $1 $2}

functionType :: {Type}
functionType : type '->' type {FunctionType $1 $3}


enumType :: {Type}
enumType : 'enum' '{' enumList '}' {EnumType $3}

enumList :: {[AtSymbol]}
enumList : atSymbols {EnumList $1}

atSymbols :: {[AtSymbol]}
atSymbols : atSymbol ',' atSymbols {$1 : $3}
          | atSymbol {[$1]}

tupleType :: {Type}
tupleType : '(' typeList ')' {TupleType $2}

typeList :: {[Type]}
typeList : type ',' typeList {$1 : $3}
         | type {[$1]}

typeQualification :: {TypeQualification}
typeQualification : '<' typeParameterList '>' {TypeQual $2}

typeParameterList :: {[TypeParameter]}
typeParameterList : typeParameter ',' typeParameterList {$1 : $3}
                  | typeParameter {[$1]}

typeParameter :: {TypeParameter}
typeParameter : literal {TypeParameterLiteral $1}
              | SYMBOL  {TypeParameterSymbol $1}
              | type {TypeParameterType $1}

statement :: {Statement}
statement : assignment {$1}
          | declaration {$1}
          | functionCallExpression %prec CALL {$1}
          | block {$1}
          | whileBlock {$1}
          | forBlock {$1}
          | doWhileBlock {$1}
          | returnStatement {$1}
          | ifBlock {$1}
          | caseBlock {$1}
          | ';' {EmptyStatement}


returnStatement :: {Statement}
returnStatement : 'return' expression ';' {ReturnStatement $2}

whileBlock :: {Statement}
whileBlock : 'while' expression block {WhileStatement $2 $3}

forBlock :: {Statement}
forBlock : 'for' pattern 'in' expression block {ForStatement $2 $4 $5}

doWhileBlock :: {Statement}
doWhileBlock : 'do' block 'while' expression ';' {DoWhileStatement $4 $2}

ifBlock :: {Statement}
ifBlock : 'if' expression block {IfStatement $2 $3}
        | 'if' expression block 'else' block {IfElseStatement $2 $3 $4}


caseBlock :: {Statement}
caseblock : 'case' expression '{' caseBodyList '}' {CaseStatement $1 $2}

caseBodyList :: {[CaseBody]}
caseBodyList : caseBody caseBodyList {$1 : $2}
             | {-empty-} {[]}

caseBody :: {CaseBody}
caseBody : patternList block {FirmCaseBody $1 $2}

patternList :: {[Pattern]}
patternList : pattern ',' patternList {$1 : $3}
            | pattern {[$1]}

pattern :: {Pattern}
pattern : SYMBOL {SymbolPattern $1}
        | '_' {WildCardPattern}
        | pattern ':' pattern {ConcatPattern $1 $2}
        | '[' patternList ']'  {ArrayPattern $2} 
        | '(' patternList ')'  {TuplePattern $2}
        | '[' ']' {ArrayPattern []}
        | '(' ')' {TuplePattern []}
        | literal {LiteralPattern $1}
        | variantMatchPattern {$1}
 
variantMatchPattern :: {Pattern}
variantMatchPattern : type {VariantMatchPattern $1 []}
                    | type '(' patternList ')' {VariantMatchPattern $1 $2}
                    | type '(' ')' {VariantMatchPattern $1 []}

expression :: {Expression}
expression : literal {LiteralExp $1}
           | functionCallExpression %prec CALL {$1}
           | listExpression {$1}
           | tupleExpression {$1}
           | hintExpression {$1}
           | unaryExpression {$1}
           | binaryExpression {$1}
           | lambda {$1}
           | variantConstructorExpression {$1}
           | SYMBOL {SymbolExpression $1}

functionCallExpression :: {Expression}
functionCallExpression : expression '(' ')' {FunctionCall $1 []}
                       | expression '(' expressionList ')' {FunctionCall $1 $3}

lambda :: {Expression}
lambda : '\\' '(' guardedPatternList ')' functionBody {Lambda $3 $5}
       | '\\' functionBody {Lambda [] $2}

listExpression :: {Expression}
listExpression : '[' ']' {ListExpression []}
               | '[' expressionList ']' {ListExpression $2}

expressionList :: {[Expression]}
expressionList : expression ',' expressionList {$1 : $3}
               | expression {[$1]}

tupleExpression :: {Expression}
tupleExpression : '(' expressionList ')' {TupleExpression $1}

hintExpression :: {Expression}
hintExpression : expression '::' type {HintExpression $1 $3}

unaryExpression :: {Expression}
unaryExpression : unaryOperator expression {UnaryExpression $1 $2}

binaryExpression :: {Expression}
binaryExpression : expression binaryOperator expression {BinaryExpression $2 $1 $3}

literal :: {Literal}
literal : numericLiteral {$1}
        | stringLiteral {$1}
        | characterLiteral {$1}
        | symbolLiteral {$1}

numericLiteral :: {Literal}
numericLiteral : INTLITERAL {IntegerLiteral $1}
               | REALLITERAL {RealLiteral $1}



stringLiteral :: {Literal}
stringLiteral : STRING {StringLiteral $1}

characterLiteral :: {Literal}
characterLiteral : CHARACTER {CharacterLiteral $1}

symbolLiteral :: {Literal}
symbolLiteral : ATSYMBOL {SymbolLiteral $1}

atSymbol :: {AtSymbol}
atSymbol : ATSYMBOL {FirmAtSymbol $1}

variantConstructorExpression :: {Expression}
variantConstructorExpression : type {VariantConstructorExpA $1}
                             | type expression {VariantConstructorExpB $1 $2}

unaryOperator :: {UnaryOp}
unaryOperator : '!' %prec UNARY {UnaryNot}
              | '*' %prec UNARY {UnaryDeref}
              | '-' %prec UNARY {UnaryNegate}
              | '+' %prec UNARY {UnaryIdentity}

binaryOperator :: {BinOp}
binaryOperator : '+' {BinaryAddition}
               | '-' {BinarySubtraction}
               | '*' {BinaryMultiplication}
               | '/' {BinaryDivision}
               | '%' {BinaryModulus}
               | 'and' {BinaryLogicalAnd}
               | 'or' {BinaryLogicalOr}
               | '==' {BinaryEquality}
               | '!=' {BinaryInequality}
               | '<' {BinaryLessThan}
               | '<=' {BinaryLessThanEquals}
               | '>' {BinaryGreaterThan}
               | '>=' {BinaryGreaterThanEquals}
               | ':'  {BinaryConcatenation}
               | '<-' {BinaryStore}
               | '.'  {BinaryPartialApplication}
               | '^' {BinaryComposition}
