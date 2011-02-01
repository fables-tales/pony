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
	'_' { Token _ _ Underscore }
	'<-' { Token _ _ LeftArrow }
	'->' { Token _ _ RightArrow } 
	'::' { Token _ _ DoubleColon }
	':'  { Token _ _ Colon }
  '.'  { Token _ _ Dot }
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
topLevelStatements : topLevelStatements topLevelStatement {$1 : $2}
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
              | {-empty-} {[]}
