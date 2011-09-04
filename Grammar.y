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
    'as' { Token _ _ As }
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
    '!!' { Token _ _ DoubleExclamation }
    MODULE { Token _ _ (ModuleName $$) }
    TYPENAME { Token _ _ (TypeName $$) }
    SYMBOL   { Token _ _ (Symbol $$)}
    INTLITERAL { Token _ _ (LexIntLiteral $$) }
    REALLITERAL { Token _ _ (LexRealLiteral $$) }
    ATSYMBOL { Token _ _ (LexAtSymbol $$) }
    STRING {Token _ _ (LexStringLiteral $$)}
    CHARACTER {Token _ _ (LexCharLiteral $$)}
%left ','
%right '<-'
%left 'or'
%left 'and'
%left '==' '!=' '<' '<=' '>' '>='
%nonassoc '::'
%left ':'
%left '+' '-'
%left '*' '/' '%'
%left '!!'
%nonassoc UNARYNOT UNARYDEREF UNARYMIN UNARYPLUS
%right CALL VCONS
%right '.' '^'
%nonassoc '(' ')' '{' '}'

%%
translationUnit :: {TranslationUnit}
translationUnit : moduleDeclaration topLevelStatements {DeclaredUnit $1 $2}
                | topLevelStatements                   {AnonymousUnit $1}

moduleDeclaration :: {[String]}
moduleDeclaration : 'module' MODULE ';' { $2 }

topLevelStatements :: {[TopLevelStatement]}
topLevelStatements : topLevelStatement topLevelStatements {$1 : $2}
                   | {-empty-}                            {[]}

topLevelStatement :: {TopLevelStatement}
topLevelStatement : exportStatement         { $1 }
                  | importStatement         { $1 }
                  | variableMutationStatement  { $1 }
                  | typeDefinitionStatement { $1 }

exportStatement :: {TopLevelStatement}
exportStatement :  'export' exportee ';' {Export $2}

exportee :: {Exportee}
exportee :  typenameList {TypenameExportee $1}
         |  symbolList   {SymbolExportee $1}

typenameList :: {[String]}
typenameList : TYPENAME typenameListRest {$1 : $2}

typenameListRest :: {[String]}
typenameListRest :  ',' typenameList {$2}
                 | {-empty-}         {[]}

symbolList :: {[String]}
symbolList : SYMBOL symbolListRest {$1 : $2} 

symbolListRest :: {[String]}
symbolListRest : ',' symbolList {$2}
               | {-empty-}      {[]}


importStatement :: {TopLevelStatement}
importStatement : 'import' MODULE '.' importee ';' {ImportStatement $2 $4}

importee :: {Importee}
importee : TYPENAME optionalTypeRename {TypeNameImportee $1 $2}
         | SYMBOL   optionalSymbolRename {SymbolNameImportee $1 $2}
         | '*' {StarImportee}

optionalTypeRename :: {Maybe Rename}
optionalTypeRename : 'as' TYPENAME {Just (TRename $2)}
                   | {-empty-}     {Nothing}

optionalSymbolRename :: {Maybe SymbolRename}
optionalSymbolRename :  'as' SYMBOL {Just (SRename $2)}
                     |  {-empty-}   {Nothing}


qualifiedSymbol :: {QualifiedSymbol}
qualifiedSymbol : SYMBOL optionalTypeQualification {ConcreteQualifiedSymbol $1 $2}

variableMutationStatement :: {TopLevelStatement}
variableMutationStatement : qualifiedSymbol variableMutationSuffix ';' {VariableMutation $1 $2}

variableMutationSuffix :: {VariableMutationSuffix}
variableMutaitonSuffix : '::' type {TypeQualificationSuffix $2}
                       | '='  expr {AssignExpressionSuffix $2}
                       | '(' optionalGuardedPatternList ')' functionBody {FunctionBodySuffix $2 $4}

optionalGuardedPatternList :: {Maybe GuardedPatternList}
optionalGuardedPatternList : guardedPatternList {Just ($1)}
                           | {-empty-}          {Nothing} 

guardedPatternList :: {GuardedPatternList}
guardedPatternList : patternList guard {GPLWithGuard $2 $1}

functionBody :: {FunctionBody}
functionBody : block {BlockBody $1}
             | ':=' expr {ExprBody $2}

block :: {Block}
block : '{' statements '}' {StatementBlock $1}

statements :: {[Statement]}
statements : statement statements {$1 : $2}
           | {-empty-}            { [] }

guard :: {Guard}
guard : '|' expr {ExprGuard $2}
      | {-empty-} {EmptyGuard}

typeDefinitionStatement :: {TopLevelStatement}
typeDefinitionStatement : possiblyQualifiedTypeName '=' type ';' {TypeDefinition $1 $3}

possiblyQualifiedTypeName :: {PQualTypeName}
possiblyQualifiedTypeName : TYPENAME optionalTypeQualification {PossibleQualifier $1 $2}

type :: {Type}
type : functionType {$1}

parameterType :: {ParameterType}
parameterType :  parameterFunctionType {$1}

functionType :: {Type}
functionType : baseType functionTypeSuffix {FunctionType $1 $2}

functionTypeSuffix :: {FunctionTypeSuffix}
functionTypeSuffix : '->' baseType {OutputTypeSuffix $1}
                   | ',' functionType {FunctionTypeSuffix $1}
                   | {-empty-} {EmptyFTSuffix}

parameterFunctionType :: {ParameterType}
parameterFunctionType : parameterBaseType parameterFunctionTypeSuffix {ParameterFunctionType $1 $2}

parameterFunctionTypeSuffix :: {ParameterFunctionTypeSuffix}
parameterFunctionTypeSuffix : '->' parameterBaseType {PFTSuffix $2}
                            | {-empty-} {EmptyPFTSuffix}

baseType :: {BaseType}
baseType : possiblyQualifiedTypeName {BasePossQual $1}
         | tupleType {BaseTupleType $1}
         | enumType  {BaseEnumType  $1}
         | variantType {BaseVariantType $1}

parameterBaseType :: {ParameterBaseType}
parameterBaseType : possiblyQualifiedTypeName {PBasePossQual $1}
                  | tupleType                 {PBaseTupleType $1}
                  | enumType                  {PBaseEnumType $1}


variantType :: {VariantType}
variantType : 'variant' variantOptions {VT $2}

variantOptions :: {[VariantOption]}
variantOptions : variantOption variantOptionsRest {$1 : $2}

variantOptionsRest :: {[VariantOption]}
variantOptionsRest : ',' variantOptions {$2}
                   | {-empty-} {[]}

variantOption :: {VariantOption}
variantOption : TYPENAME variantOptionData {VO $1 $2}

variantOptionData :: {VariantOptionData}
variantOptionData : tupleType {VODTupleType $1}
                  | {-empty-} {VODEmpty} 


enumType :: {EnumType}
enumType : 'enum' '{' enumList '}' {EnumListType $3}

enumList :: {[AtSymbol]}
enumList : ATSYMBOL enumListRest {(AtSymbol $1) : $2} 

enumListRest :: {[AtSymbol]}
enumListRest : ',' enumList {$2}
             | {-empty-} {[]}

tupleType :: {TupleType}
tupleType : '(' typeList ')' {TT $2}

typeList :: {[Type]}
typeList : type typeListRest {$1 : $2}

typeListRest :: {[Type]}
typeListRest : ',' typeList {$2}
             | {-empty-} {[]}

optionalTypeQualification :: {Maybe TypeQualification}
optionalTypeQualification : typeQualification {Just ($1)}
                          | {-empty-}         {Nothing}

typeQualification :: {TypeQualification}
typeQualification : '<' typeParameterList '>' {ListQualification $2}

typeParameterList :: {[TypeParameter]}
typeParameterList : typeParameter typeParameterListRest {$1 : $2}

typeParameterListRest :: {[TypeParameter]}
typeParameterListRest : ',' typeParameterList {$2}
                      | {-empty-} {[]}

typeParameter :: {TypeParameter}
typeParameter : literal {LiteralTypeParam $1}
              | SYMBOL  {SymbolTypeParam $1}
              | parameterType {ParameterTypeTypeParam $1}

statement :: {Statement}
statement : variablePrefixedStatement {$1}
          | block {$1}
          | whileBlock {$1}
          | forBlock {$1}
          | doWhileBlock {$1}
          | returnStatement {$1}
          | ifBlock {$1}
          | caseBlock {$1}
          | ';' {EmptyStatement}

variablePrefixedStatement :: {Statement}
variablePrefixedStatement : SYMBOL variablePrefixedStatementSuffix ';' {VariablePrefixedSTMT $1 $2}

variablePrefixedStatementSuffix :: {VPSS}
variablePrefixedStatementSuffix : '::' type {VPSSTypeSuffix $2}
                                | '=' expr  {VPSSAssignSuffix $2}
                                | callFamilySuffix {VPSSCFS $1}


returnStatement :: {Statement}
returnStatement : 'return' expr ';' {ReturnStatement $2}

whileBlock :: {Statement}
whileBlock : 'while' expr block {WhileBlock $2 $3}

forBlock :: {Statement}
forBlock : 'for' pattern 'in' expr block {ForBlock $2 $4 $5}

doWhileBlock :: {Statement}
doWhileBlock : 'do' block 'while' expr {DoWhileBlock $4 $2}

ifBlock :: {Statement}
ifBlock : 'if' expr block elseSuffix {IfBlock $2 $3 $4}

elseSuffix :: {ElseSuffix}
elseSuffix : 'else' block {BlockElseSuffix $2}
           | {-empty-} {EmptyElseSuffix}

caseBlock :: {Statement}
caseBlock : 'case' expr '{' caseInnards '}' {CaseBlock $2 $4}

caseInnards :: {[CaseBody]}
caseInnards : caseBody caseInnards {$1 : $2}
            | {-empty-} {[]}

caseBody :: {CaseBody}
caseBody : patternList block {CB $1 $2}

patternList :: {[Pattern]}
patternList : pattern patternListRest {$1 : $2}

patternListRest :: {[Pattern]}
patternListRest : ',' patternList {$2}
                | {-empty-} {[]}

optionalPatternList :: {Maybe [Pattern]}
optionalPatternList : patternList {Just $1}
                    | {-empty-}   {Nothing}


pattern :: {Pattern}
pattern : appendPattern {$1}

appendPattern :: {Pattern}
appendPattern : appendPattern ':' basePattern {AppendBase $1 $3}
              | basePattern {BaseOnly $1}

basePattern :: {BasePattern}
basePattern : SYMBOL {SymbolBasePattern $1}
            | '_'    {WildcardBasePattern $1}
            | '[' optionalPatternList ']'  {PatternListBasePattern $2}
            | '(' optionalPatternList ')'  {PatternTupleBasePattern $2}
            | literal {LiteralBasePattern $1}
            | variantMatchPattern {VariantMatchBasePattern $1}

variantMatchPattern :: {VariantMatchPattern}
variantMatchPattern : TYPENAME variantMatchPatternData {VMP $1 $2}

variantMatchPatternData :: {VariantMatchPatternData}
variantMatchPatternData : '(' optionalPatternList ')' {VMPD $2}
                        | {-empty-} {NoVMPD}

expr :: {Expression}
expr : tupleExpr {Expr $1}

tupleExpr :: {[SubExpr]}
tupleExpr : tupleExpr ',' storeExpr {$1 : $3}
          | storeExpr {[$1]}

storeExpr :: {SubExpr}
storeExpr : orExpr storeSuffix {StoreExpr $1 $2}

storeSuffix :: {StoreSuffix}
storeSuffix : '<-' storeExpr {StoreSuff $1 $2}
            | {-empty-} {EmptyStoreSuff}

orExpr :: {OrExpr}
orExpr : orExpr 'or' andExpr {WithOrExpr $1 $2}
        | andExpr {WithoutOrExpr $1}

andExpr :: {AndExpr}
andExpr : andExpr 'and' relExpr {WithAndExpr $1 $2}
        | relExpr {WithoutAndExpr $1}

relExpr :: {RelExpr}
relExpr : hintExpr relSuffix {RelExpr $1 $2}

relSuffix :: {RelSuffix}
relSuffix : relOperator hintExpr {NonEmptyRelSuffix $1 $2}
          | {-empty-} {EmptyRelSuffix}

relOperator :: {RelOperator}
relOperator : '==' {EqualityCMPOperator $1}
            | '!=' {InequalityCMPOperator $1}
            | '<'  {LessThanCMPOperator $1}
            | '<=' {LessThanEqualsCMPOperator $1}
            | '>'  {GreaterThanCMPOperator $1}
            | '>=' {GreaterThanCMPOperator $1}

hintExpr :: {HintExpr}
hintExpr : appendExpr hintSuffix {Hint $1 $2}

hintSuffix :: {HintSuffix}
hintSuffix : '::' type {HintSuffixType $2}
           | {-empty-} {EmptyHintSuffix}

appendExpr :: {[ArithmeticExpr]}
appendExpr : appendExpr ':' arithmeticExpr {$1 ++ $3}
           | arithmeticExpr {[$1]}

arithmeticExpr :: {ArithmeticExpr}
arithmeticExpr : arithmeticExpr arithmeticOperator multiplicativeExpr {ArithExp $1 $2 $3}
               | multiplicativeExpr {ArithWrapMul $1}

arithmeticOperator :: {ArithmeticOperator}
arithmeticOperator : '+' {AddOperator}
                   | '-' {SubtractOperator}

multiplicativeExpr :: {MultiplicativeExpr}
multiplicativeExpr : multiplicativeExpr multiplicativeOperator arrayIndexExpr {MulExp $1 $2 $3}
                 | arrayIndexExpr {MulWrapArr $1}

multiplicativeOperator :: {MulOperator}
multiplicativeOperator : '*' {MultiplyOperator}
                       | '/' {DivideOperator}
                       | '%' {ModuloOperator}

arrayIndexExpr :: {ArrayIndexExpr}
arrayIndexExpr : arrayIndexExpr '!!' unaryExpr {ArrayIndex $1 $3}
               | unaryExpr {UnaryArrayExpr $1}

unaryExpr :: {UnaryExpr}
unaryExpr : callFamilyExpr {CallUnaryExpr $1}
          | unaryOperator unaryExpr {UnaryOpExpr $1 $2}

callFamilyExpr :: {CallFamilyExpr}
callFamilyExpr : closureOperatorExpr callFamilySuffix {ClosureCallExpr $1 $2}
               | variantConstructor callFamilySuffix  {VariantCallExpr $1 $2}

callFamilySuffix :: {[CallArguments]}
callFamilySuffix : callArguments callFamilySuffix {$1 : $2}
                 | {-empty-} {[]}

callArguments :: {CallArguments}
callArguments : '(' optionalExpr ')' {CallArg $1}

closureOperatorExpr :: {ClosureOperatorExpr}
closureOperatorExpr :  baseExpr closureOperatorSuffix {COE $1 $2}

closureOperatorSuffix :: {ClosureOperatorSuffix}
closureOperatorSuffix : closureOperator closureOperatorExpr {COS $1 $2}
                      | {-empty-} {EmptyClosureOperatorSuffix}

closureOperator :: {ClosureOperator}
closureOperator : '.' {PartialApplicationOperator}
                | '^' {SailingBoatOperator}

baseExpr :: {BaseExpr}
baseExpr : lambda {LambdaBaseExpr $1}
         | literal {LiteralBaseExpr $1}
         | list {ListBaseExpr $1}
         | SYMBOL {SymbolBaseExpr $1}

lambda :: {Lambda}
lambda : '\\' lambdaArguments functionBody {L $2 $3}

lambdaArguments :: {LambdaArguments}
lambdaArguments : '(' optionalGuardedPatternList ')' {WithLambdaArgs $2}
                | {-empty-} {WithoutLambdaArgs}

list :: {ParseList}
list : '[' expr ']' {PL $2}

literal :: {Literal}
literal : numericLiteral {NumLiteral $1}
        | stringLiteral  {StringLiteral $1}
        | characterLiteral {CharLiteral $1}
        | symbolLiteral {SymbolLiteral $1}

stringLiteral :: {String}
stringLiteral : STRING {$1}

numericLiteral :: {Either Float Integer}
numericLiteral : INTLITERAL {Right $1}
               | REALLITERAL {Left $1}

characterLiteral :: {Char}
characterLiteral : CHARACTER {$1}

symbolLiteral :: {Symbol}
symbolLiteral : ATSYMBOL {AtSymbol $1}

variantConstructor :: {VariantConstructor}
variantConstructor : TYPENAME variantConstructorData %prec VCONS {VC $1 $2}

variantConstructorData :: {VariantConstructorData}
variantConstructorData : parenthesisedExpr {PEVCD $1}
                       | {-empty-} {EmptyVCD}

parenthesisedExpr :: {Expr}                      
parenthesisedExpr : '(' expr ')' {$2}

unaryOperator :: {UnaryOperator}
unaryOperator : '!' {NotOperator}
              | '*' {Dereference}
              | '-' {Negation}
              | '+' {Positation}

optionalExpr :: {Maybe Expr}
optionalExpr : expr {Just $1}
             | {-empty-} {Nothing}
