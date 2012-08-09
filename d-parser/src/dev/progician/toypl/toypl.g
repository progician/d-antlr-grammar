grammar toypl;

options {
  language = Java;
  output = AST;
}

tokens {
  MODULE ;
  INFIX ;
  DEF_FUNC ;
  DEF_VAR ;
  AUTO_VAR ;
  DEF_SYMBOL ;
  REF_IDENTIFIER ;
  REF_QUALIFIED ;
  TEMPLATE_INSTANCE ;
  AST_NEONODE_ARRAYVIEW ;
  INITIALIZER_VOID ;
  INITIALIZER_EXP ;
  INTEGER_LITERAL ;
  FLOAT_LITERAL ;
  EXP_THIS = 'this' ;
  EXP_SUPER = 'super' ;
  EXP_LITERAL_NULL = 'null' ;
  EXP_LITERAL_FILE = '__FILE__' ;
  EXP_LITERAL_LINE = '__LINE__' ;
  TYPE_POINTER ;
  TYPE_DYN_ARRAY ;
  TYPE_STATIC_ARRAY ;
  TYPE_MAP_ARRAY ;
  REF_TYPE_SLICE ;
  PARAMETER_LIST ;
  FUNCTION_BODY ;
  MODULE_DECLARATION ;
  MODULE_SYMBOL ;
  FUNC_PARAM ;
  DECL_IMPORT = 'import' ;
  IMPORT_LIST ;
  IMPORT_CONTENT ;
  IMPORT_ALIAS ;
}

@lexer::header {
package dev.progician.toypl;
}

@parser::header {
package dev.progician.toypl;
}

// The white space blocks
WHITESPACE : ( ' ' | '\t' | '\u000B'| '\u000C' | '\r' | '\n' )+  { $channel = HIDDEN; } ;

// Comments
LINE_COMMENT  : '//' (~('\n'|'\r'))* '\r'? '\n' { $channel = HIDDEN; } ;
MULTILINE_COMMENT : ('/*'  ( options { greedy=false; } : . )* '*/') { $channel = HIDDEN; } ;
NESTING_COMMENT 
  : ('/+'  ( options {greedy=false;} : NESTING_COMMENT | . )* '+/') { $channel = HIDDEN; } ;

BooleanLiteral
  : 'true' | 'false'
  ;
  
Range
  : '..'
  ;
  
PropertyIdentifier
  : 'property'
  | 'safe'
  | 'trusted'
  | 'system'
  | 'disable'
  ;
  
StorageClass
  : 'abstract'
  | 'auto'
  | 'const'
  | 'deprecated'
  | 'enum'
  | 'extern'
  | 'final'
  | 'immutable'
  | 'inout'
  | 'shared'
  | 'nothrow'
  | 'override'
  | 'pure'
  | '__gshared'
  | 'scope'
  | 'static'
  | 'synchronized'
  ;
  
ParameterAttribute
  : 'auto'
  | 'const'
  | 'final'
  | 'immutable'
  | 'in' 
  | 'inout'
  | 'lazy'
  | 'out'
  | 'ref'
  | 'scope'
  | 'shared'
  ;

// Identity tokens
Identifier :  IdStartChar (IdStartChar | '0'..'9')* ;
fragment IdStartChar :  '_' | 'a'..'z' | 'A'..'Z' ; // | '\u0080'..'\ufffe' ;

// Strings and chars
CharacterLiteral : '\'' (~('\''|'\\') | EscapeChar) '\'' ;
fragment EscapeChar
    :   '\\' ('\''|'\"'|'?'|'\\'|'a'|'b'|'f'|'n'|'r'|'t'|'v')
    |   OctalEscape 
    |   HexEscape 
    | '\\' '&' //TODO \& NamedCharacterEntity ;
    ;
    
fragment OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ;

fragment HexEscape
    :   '\\U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
    |   '\\u' HexDigit HexDigit HexDigit HexDigit
    |   '\\x' HexDigit HexDigit ;

StringLiteral  : (RawString | RawStringAlt | DqString ) StringPostfix? ;
fragment RawString :   ('r' | 'x' ) '"' (~'"')* '"' ;
fragment RawStringAlt :   '`' (~'`')* '`' ;
fragment DqString   :  '"' (~('"'|'\\') | EscapeChar)* '"' ;
fragment StringPostfix  : 'c' | 'w' | 'd';

/*--- Numbers ---*/

Number options { backtrack=true; }
  : (IntegerLiteral Range)=> IntegerLiteral { $type = IntegerLiteral; }
  | (FloatLiteral)=> FloatLiteral { $type = FloatLiteral; }
  | IntegerLiteral { $type = IntegerLiteral; }
  ;

/*--- INTEGERS ---*/
fragment IntegerLiteral : Integer IntSuffix? ;
fragment IntSuffix :  'L'|'u'|'U'|'Lu'|'LU'|'uL'|'UL' ;
fragment Integer :  Decimal| Binary| Octal| Hexadecimal ;

fragment Decimal :  '0' | '1'..'9' (DecimalDigit | '_')* ;
fragment Binary :  ('0b' | '0B') ('0' | '1' | '_')+ ;
fragment Octal :  '0' (OctalDigit | '_')+ ;
fragment Hexadecimal :  ('0x' | '0X') (HexDigit | '_')+;  

fragment DecimalDigit :  '0'..'9' ;
fragment OctalDigit :  '0'..'7' ;

fragment HexDigit :  ('0'..'9'|'a'..'f'|'A'..'F') ;

/*--- FLOATS ---*/
fragment FloatLiteral : Float ImaginarySuffix?  ;
fragment DecimalExponent :  'e' | 'E' | 'e+' | 'E+' | 'e-' | 'E-' DecimalDigits;
fragment DecimalDigits :  ('0'..'9'| '_')+ ;
fragment FloatTypeSuffix :  'f' | 'F' | 'L' ;
fragment ImaginarySuffix :  'i' ;
fragment Float
  : DecimalDigits ( options { greedy = true; } : FloatTypeSuffix 
                  | '.' DecimalDigits DecimalExponent?
                  )
  | '.' DecimalDigits DecimalExponent?
  ;
  
// ================ PARSER ================ 

prog
  : moduleDeclaration? declDef* EOF
      -> ^(MODULE moduleDeclaration? declDef* EOF)
  ;
  
moduleDeclaration
  : 'module' moduleFullyQualifiedName ';' -> ^(MODULE_DECLARATION moduleFullyQualifiedName)
  ;
  
moduleFullyQualifiedName
  : Identifier ('.' Identifier)* -> ^(MODULE_SYMBOL Identifier*)
  ;
  
declarationBlock
  : declDef
  | '{'! declDef* '}'!
  ;

declDef
  : importDeclaration
  | declaration
  ;
  
declaration
  : storageClass
  | decl
  ;
  
storageClass
  : StorageClass
    ( decl -> ^(StorageClass decl)
    | defSymbol initializer -> ^(AUTO_VAR defSymbol initializer)
    | storageClass -> ^(StorageClass storageClass)
    )
  ;
  
decl
  : type defSymbol
    ( '[' ']' initializer? ->  ^(DEF_VAR ^(TYPE_DYN_ARRAY type) defSymbol initializer?)
    | '[' primaryExpression ']' initializer? -> ^(DEF_VAR ^(TYPE_STATIC_ARRAY primaryExpression type) defSymbol initializer?)
    | initializer? ';' -> ^(DEF_VAR type defSymbol initializer?)
    | parameters functionBody -> ^(DEF_FUNC defSymbol type parameters functionBody) 
    )
  ;
  
defSymbol
  : Identifier -> ^(DEF_SYMBOL[$Identifier])
  ;
  
initializer
  : '='
    ( voidInitializer
    | nonVoidInitializer
    )
  ;
  
voidInitializer
  : 'void' -> ^(INITIALIZER_VOID)
  ;
  
nonVoidInitializer
  : primaryExpression -> ^(INITIALIZER_EXP primaryExpression)
  ;
  
primaryExpression
  : IntegerLiteral
  | FloatLiteral
  | CharacterLiteral
  | StringLiteral
  | EXP_THIS
  | EXP_SUPER
  | EXP_LITERAL_NULL
  | refIdentifier
  | '.' refIdentifier
  ;
  
basicType
  : basicTypeX -> ^(REF_IDENTIFIER basicTypeX)
  | identifierList
  | '.' identifierList -> ^(REF_QUALIFIED identifierList)
  ;
  
basicTypeX
  : 'bool' | 'byte' | 'ubyte' | 'short' | 'ushort' | 'int' | 'uint' | 'long' | 'ulong'
  | 'char' | 'wchar' | 'dchar'
  | 'float' | 'double' | 'real'
  | 'ifloat' | 'idouble' | 'ireal'
  | 'cfloat' | 'cdouble' | 'creal'
  | 'void'
  ;

reference
  : Identifier -> ^(REF_IDENTIFIER Identifier)
  ;
  
refIdentifier
  : Identifier -> ^(REF_IDENTIFIER Identifier)
  ;
  
templateInstance
  : Identifier '!' templateSingleArgument
      -> ^(TEMPLATE_INSTANCE Identifier templateSingleArgument)
  | Identifier '!' '(' templateArgumentList ')'
      -> ^(TEMPLATE_INSTANCE Identifier templateArgumentList)
  ;
  
templateSingleArgument
  : basicTypeX
  | refIdentifier
  | CharacterLiteral
  | StringLiteral
  | IntegerLiteral
  | FloatLiteral
  | BooleanLiteral
  | EXP_LITERAL_NULL
  | EXP_LITERAL_FILE
  | EXP_LITERAL_LINE
  ;
  
templateArgumentList
  : templateArgument (',' templateArgument)* -> ^(AST_NEONODE_ARRAYVIEW templateArgument*)
  ;
  
templateArgument
  : type
  ;
  
identifierList
  : (refIdentifier -> refIdentifier
    | templateInstance -> templateInstance
    )
    
    ( '.' refIdentifier -> ^(REF_QUALIFIED $identifierList refIdentifier)
    | '.' templateInstance -> ^(REF_QUALIFIED $identifierList templateInstance)
    )*
  ;
  
type
  : ( basicType -> basicType )
	  ( '*' -> ^(TYPE_POINTER basicType)
	  | '[' ']' -> ^(TYPE_DYN_ARRAY basicType)
	  | '[' primaryExpression ']' -> ^(TYPE_STATIC_ARRAY IntegerLiteral basicType)
	  | '[' primaryExpression '..' primaryExpression ']' -> ^(REF_TYPE_SLICE primaryExpression primaryExpression basicType)
	  // | '[' type ']' -> ^(TYPE_MAP_ARRAY type basicType)
	  // FIX: Ambuiguity between TYPE_MAP_ARRAY and TYPE_STATIC_ARRAY, as the primaryExpression and the type can both be Identifier.
	  )?
  ;
  
property
  : '@' PropertyIdentifier -> PropertyIdentifier
  ;
  
parameters
  : '(' ( parameter (',' parameter)* )? ')' -> ^(PARAMETER_LIST parameter*)  
  ;
  
parameter
  : ParameterAttribute? type defSymbol defaultInitializerExpression?
      -> ^(FUNC_PARAM type ParameterAttribute? defaultInitializerExpression?)
  ;
  
functionBody
  : '{' declDef* '}' -> ^(FUNCTION_BODY declDef*)
  ;
  
defaultInitializerExpression
  : primaryExpression
  | '__FILE__'
  | '__LINE__'
  ;
  
// ================ DECLARATIONS ================

importDeclaration
  : 'static'? DECL_IMPORT importList -> ^(DECL_IMPORT importList 'static'?) 
  ;
  
importList
  : importEntry* -> ^(IMPORT_LIST importEntry*)
  ;
  
importEntry
  : Identifier '=' moduleFullyQualifiedName -> ^(IMPORT_ALIAS Identifier moduleFullyQualifiedName)
  | moduleFullyQualifiedName -> ^(IMPORT_CONTENT moduleFullyQualifiedName)
  ;