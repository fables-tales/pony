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
    ';' { Token _ _ Semicolon }
    '+' { Token _ _ Plus }
    '*' { Token _ _ Asterisk }
    '/' { Token _ _ Slash }
    '>=' { Token _ _ GEq }
    '<=' { Token _ _ LEq }
    '>' { Token _ _ CAngleBracket }
    '<' { Token _ _ OAngleBracket }
    '==' { Token _ _ DoubleEq }
    '!=' { Token _ _ NEq }
    '=' { Token _ _ Eq }
    '(' { Token _ _ OParen }
    ')' { Token _ _ CParen }
    '[' { Token _ _ OSqB } 
    ':=' { Token _ _ ColonEq }
    '_' { Token _ _ Underscore }
    '<-' { Token _ _ LeftArrow }
    '->' { Token _ _ RightArrow } 
    '::' { Token _ _ DoubleColon }
    ':'  { Token _ _ Colon }
    '.'  { Token _ _ Dot }
    '|'  { Token _ _ Pipe }
    MODULE { Token _ _ (ModuleName $$) }
    TYPENAME { Token _ _ (TypeName $$) }

%%
translationUnit :: {Module}
translationUnit : {-nothing-} { Module ["anon"] [] }
                | topLevelStatements { Module ["anon"] $1 } 
                | moduleDeclaration topLevelStatements { Module $1 $2 }

moduleDeclaration :: {[String]}
moduleDeclaration : 'module' MODULE ';' { $2 }

topLevelStatements :: {[TopLevelStatement]}
topLevelStatements : topLevelStatements topLevelStatement {$1 ++ [$2]}
                   | {-empty-} {[]}

topLevelStatement :: {TopLevelStatement}
topLevelStatement : exportStatement {$1}
                  | importStatement {$1}
                  | declaration {$1}
                  | assignment {$1}

exportStatement :: {TopLevelStatement}
exportStatement : 'export' typenameList ';' {ExportTypeName $2}
                | 'export' symbolList ';' {ExportSymbol $2}

typenameList :: {[String]}
typenameList : TYPENAME ',' typenameList {$1:$3}
             | TYPENAME {[$1]}

symbolList :: {[String]}
symbolList : SYMBOL ',' sybmolList {$1 : $3}
           | SYMBOL {[$1]}

importStatement :: {TopLevelStatement}
importStatement : 'import' MODULE '.' type ';' {ImportModuleType $2 $4}
                | 'import' MODULE '.' SYMBOL ';' {ImportModuleSymbol $2 $4}
                | 'import' MODULE '.' '*' ';' {ImportModuleAll $2}

declaration :: {TopLevelStatement}
declaration : qualifiedSymbol '::' type ';' {Declaration $1 $3}

qualifiedSymbol :: {QualifiedSymbol}
qualifiedSymbol : SYMBOL {UntypedQualifiedSymbol $1}
                | SYMBOL type {TypedQualifiedSymbol $1 $2}

assignment :: {TopLevelStatement}
assignment : qualifiedSymbol '=' expression ';' {Assign $1 $3}
           | shortFormFunction {$1}

guardedPatternList :: {GuardedPatternList}
guardedPatternList : patternList guard {FullGuard $1 $2}
                   | patternList {PatternOnlyGuard $1}
                   | guard {GuardOnlyGuard $1}
                   | {-empty-} {NoGuard}

shortFormFunction :: {ShortFormFunction}
shortFormFunction : qualifiedSymbol '(' guaredPatternList ')' functionBody ';' {SFF $1 $3 $5}

functionBody :: {FunctionBody}
functionBody : block {BlockBody $1}
             | ':=' expression {ExpressionBody $2}

block :: {Block}
block : '{' statements '}' {Block $2}

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
variantType : "variant" variantOptions {VariantType $2}


variantOptions :: {[VariantOption]}
variantOptions : variantOptions ',' variantOption {$1 ++ [$2]}
               | variantOption {[$1]}

variantOption :: {VariantOption}
variantOption : TYPENAME tupleType  {VariantOption $1 $2}

functionType :: {Type}
functionType : pureFunctionType {$1}
             | impureFunctionType {$1}

pureFunctionType :: {Type}
pureFunctionType : type '->' type {PureFunctionType $1 $3}

impureFunctionType :: {Type}
impureFunctionType : type '=>' type {ImpureFunctionType $1 $3}

enumType :: {Type}
enumType : "enum" '{' enumList '}' {EnumType $3}
