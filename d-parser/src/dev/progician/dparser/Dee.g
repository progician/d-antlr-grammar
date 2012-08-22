grammar Dee;

options {
  language = Java;
  output = AST;
}

tokens {
  NULL ;
  MODULE ;
  DEF_FUNC ;
  DEF_VAR ;
  AUTO_VAR ;
  STORAGE_CLASS ;
  DEF_SYMBOL ;
  REF_IDENTIFIER ;
  REF_QUALIFIED ;
  REF_MODULE ;
  TEMPLATE_INSTANCE ;
  AST_NEONODE_ARRAYVIEW ;
  INITIALIZER_VOID ;
  INITIALIZER_EXP ;
  EXP_THIS = 'this' ;
  EXP_SUPER = 'super' ;
  EXP_LITERAL_BOOL ;
  EXP_LITERAL_INTEGER ;
  EXP_LITERAL_REAL ;
  EXP_LITERAL_CHAR ;
  EXP_LITERAL_STRING ;
  EXP_LITERAL_NULL = 'null' ;
  EXP_LITERAL_FILE = '__FILE__' ;
  EXP_LITERAL_LINE = '__LINE__' ;
  EXP_LITERAL_FUNCTION ;
  EXP_LITERAL_IMPORTEDSTRING ;
  TYPE_POINTER ;
  TYPE_DYN_ARRAY ;
  TYPE_STATIC_ARRAY ;
  TYPE_MAP_ARRAY ;
  REF_TYPE_SLICE ;
  PARAMETER_LIST ;
  FUNCTION_BODY ;
  MODULE_DECLARATION = 'module' ;
  MODULE_SYMBOL ;
  FUNC_PARAM ;
  DECL_IMPORT ;
  IMPORT_FRAGMENT ;
  IMPORT_CONTENT ;
  IMPORT_ALIAS ;
  IMPORT_BINDING ;
  ENUM_DECLARATION = 'enum' ;
  ENUM_MEMBER ;
  CLASS_DECLARATION = 'class' ;
  BASE_CLASSES ;
  BASE_CLASS ;
  INVARIANT = 'invariant' ;
  INTERFACE_DECLARATION = 'interface' ;
  STRUCT_DECLARATION = 'struct' ;
  UNION_DECLARATION = 'union' ;
  DEF_CTOR ;
  UNITTEST_DECLARATION = 'unittest' ;
  CONDITIONAL_DECL ;
  CONDITIONAL_COMPILATION_CONDITION ;
  DECLARATION_BLOCK ;
  SYMBOL ;
  DV_SPEC ;
  STATIC_ASSERT ;
  TEMPLATE_DECLARATION = 'template' ;
  TEMPLATE_PARAM_LIST ;
  TEMPLATE_PARAM_TYPE ;
  TEMPLATE_PARAM_VALUE ;
  TEMPLATE_PARAM_ALIAS ;
  TEMPLATE_PARAM_TUPLE ;
  TEMPLATE_PARAM_THIS ;
  TEMPLATE_MIXIN_DECLARATION ;
  TEMPLATE_MIXIN ;
  MIXIN_DECLARATION ;
  INFIX ;
  PREFIX ;
  POSTFIX ;
  EXP_REFERENCE ;
  EXP_CONDITIONAL ;
  EXP_NEW = 'new' ;
  EXP_DELETE = 'delete' ;
  EXP_CAST = 'cast' ;
  EXP_CALL ;
  EXP_INDEX ;
  EXP_SLICE ;
  EXP_DOLLAR = '$' ;
  EXP_ASSERT = 'assert' ;
  EXP_MIXIN = 'mixin' ;
  EXP_TYPEID = 'typeid' ;
  EXP_TYPEOF = 'typeof' ;
  EXP_TRAITS = '__traits' ;
  EXP_IS = 'is' ;
  CAST_QUALIFIER ;
  ARG_LIST ;
  STMT_BLOCK ;
  STMT_EXPRESSION ;
  STMT_RETURN = 'return' ;
  STMT_CASE = 'case' ;
  STMT_DEFAULT = 'default';
  STMT_LABELED ;
  STMT_DECLARATION ;
  STMT_IF = 'if' ;
  STMT_WHILE = 'while' ;
  STMT_DO = 'do' ;
  STMT_FOR = 'for' ;
  STMT_FOREACH = 'foreach' ;
  STMT_FOREACH_RANGE = 'foreach_range' ;
  STMT_SWITCH = 'switch' ;
  STMT_FINALSWITCH ;
  STMT_CONTINUE = 'continue' ;
  STMT_BREAK = 'break' ;
  STMT_GOTO = 'goto' ;
  STMT_WITH = 'with' ;
  STMT_SYNCHRONIZED = 'synchronized' ;
  STMT_TRY = 'try' ;
  STMT_CATCH = 'catch' ;
  STMT_FINALLY = 'finally' ;
  STMT_THROW = 'throw' ;
  STMT_SCOPE = 'scope' ;
  STMT_ASM = 'asm' ;
  STMT_PRAGMA = 'pragma' ;
  STMT_MIXIN ;
  STMT_CONDITIONAL ;
}

@lexer::header {
package dev.progician.dparser;
}

@parser::header {
package dev.progician.dparser;
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
      -> ^(MODULE moduleDeclaration? declDef*)
  ;
  
moduleDeclaration
  : MODULE_DECLARATION^ moduleFullyQualifiedName ';'!
  ;
  
moduleFullyQualifiedName
  : Identifier -> ^(MODULE_SYMBOL Identifier)
  ;
  
declarationBlock
  : (declDef | '{' declDef* '}') -> ^(DECLARATION_BLOCK declDef*)
  ;

declDef
  : ('static' 'import')=> (importDeclaration)
  | importDeclaration
  | enumDeclaration
  | classDeclaration
  | interfaceDeclaration
  | aggregateDeclaration
  | declaration
  | ctorDeclaration
  | unittestDeclaration
  | conditionalDeclaration
  | dvSpecification
  | staticAssert
  | templateDeclaration
  | templateMixinDeclaration
  | mixinDeclaration
  ;
  
declaration
  : storageClass
  | decl
  ;
  
sc
  : ('abstract' | 'auto' | 'const'  | 'deprecated' | 'enum' | 'extern' | 'final'
  | 'immutable' | 'inout' | 'shared' | 'nothrow' | 'override' | 'pure'
  | '__gshared' | 'scope' | 'static' | 'synchronized') -> STORAGE_CLASS[$start]
  ;
  
storageClass
  : sc^
    ( decl | autoVariable | storageClass )
  ;
  
autoVariable
  : defSymbol initializer ';' -> ^(AUTO_VAR defSymbol initializer)
  ;
  
decl
  : type defSymbol
    ( '[' ']' initializer? ->  ^(DEF_VAR ^(TYPE_DYN_ARRAY type) defSymbol initializer?)
    | '[' assignExpression ']' initializer? -> ^(DEF_VAR ^(TYPE_STATIC_ARRAY assignExpression type) defSymbol initializer?)
    | initializer? ';' -> ^(DEF_VAR type defSymbol initializer?)
    | parameters memberFunctionAttribute* (functionBody | ';') -> ^(DEF_FUNC defSymbol type parameters functionBody?) 
    )
  ;
  
memberFunctionAttribute
  : 'const' | 'immutable' | 'inout' | 'shared'
  | functionAttribute
  ;
  
functionAttribute
  : 'nothrow' | 'pure'
  | property
  ;
  
defSymbol
  : Identifier -> ^(DEF_SYMBOL[$Identifier])
  ;
  
symbol
  : Identifier -> ^(SYMBOL[$Identifier])
  ;
  
initializer
  : '='!
    ( voidInitializer
    | nonVoidInitializer
    )
  ;
  
voidInitializer
  : 'void' -> ^(INITIALIZER_VOID)
  ;
  
nonVoidInitializer
  : assignExpression -> ^(INITIALIZER_EXP assignExpression)
  ;
  
basicType
  : basicTypeX -> ^(REF_IDENTIFIER basicTypeX)
  | '.' identifierList -> ^(REF_QUALIFIED identifierList)
  | identifierList 
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
  : refIdentifier '!' templateSingleArgument
      -> ^(TEMPLATE_INSTANCE refIdentifier templateSingleArgument)
  | refIdentifier '!' '(' templateArgumentList ')'
      -> ^(TEMPLATE_INSTANCE refIdentifier templateArgumentList)
  ;
  
templateSingleArgument
  : basicTypeX -> ^(REF_IDENTIFIER basicTypeX)
  | refIdentifier
  | CharacterLiteral -> ^(EXP_LITERAL_CHAR[$CharacterLiteral])
  | StringLiteral -> ^(EXP_LITERAL_STRING[$StringLiteral])
  | IntegerLiteral -> ^(EXP_LITERAL_INTEGER[$IntegerLiteral])
  | FloatLiteral -> ^(EXP_LITERAL_REAL[$FloatLiteral])
  | BooleanLiteral -> ^(EXP_LITERAL_BOOL[$BooleanLiteral])
  | EXP_LITERAL_NULL
  | EXP_LITERAL_FILE
  | EXP_LITERAL_LINE
  ;
  
templateArgumentList
  : templateArgument (',' templateArgument)* -> ^(AST_NEONODE_ARRAYVIEW templateArgument*)
  ;
  
templateArgument
  // The first case covers basically any reference, without making it an EXP_REFERENCE expression, plus the pointer, array, map types
  : (type)=> (type)
  | assignExpression
  // This doesn't make much sense! A symbol at this place would be interpreted as a reference anyway.
  // | symbol
  ;
  
identifierList
  : ( (Identifier '!')=> (templateInstance -> templateInstance)
    | (refIdentifier -> refIdentifier)
  	  ( options { greedy = true; } : '.'
  	    ( (Identifier '!')=> (templateInstance -> ^(REF_QUALIFIED $identifierList templateInstance))
		    | refIdentifier -> ^(REF_QUALIFIED $identifierList refIdentifier)
		    )
		  )*
		)
  ;
  
type
  : ( basicType -> basicType )
  
	  ( options { greedy=true; }
	  : '*' -> ^(TYPE_POINTER $type)
	  | '[' (  -> ^(TYPE_DYN_ARRAY $type)
	         | assignExpression
	           ( -> ^(TYPE_STATIC_ARRAY assignExpression $type)
	           | '..' assignExpression -> ^(REF_TYPE_SLICE assignExpression assignExpression $type)
	           )
	         )
      ']'
	  )?
  ;
  
property
  : '@' PropertyIdentifier -> PropertyIdentifier
  ;
  
parameters
  : '('! parameterList ')'!  
  ;
  
parameterList
  : ( parameter (',' parameter)* )? -> ^(PARAMETER_LIST parameter*)
  ;
  
parameter
  : ParameterAttribute* type defSymbol defaultInitializerExpression?
      -> ^(FUNC_PARAM type defSymbol ParameterAttribute* defaultInitializerExpression?)
  ;
  
functionBody
  : blockStatement
  ;
  
defaultInitializerExpression
  : primaryExpression
   ;

  
// ================ IMPORT ================

importDeclaration
  : 'static'? 'import' importFragment (',' importFragment)* ';' -> ^(DECL_IMPORT importFragment* 'static'?) 
  ;
  
importFragment
  : importEntry -> importEntry
  | importEntry ':' Identifier -> ^(IMPORT_BINDING importEntry Identifier)
  ;
  
importEntry
  : (Identifier '=')=> (Identifier '=' refModule -> ^(IMPORT_ALIAS ^(REF_MODULE Identifier) refModule))
  | refModule -> ^(IMPORT_CONTENT refModule)
  ;
  
refModule
  : Identifier ('.' Identifier)* -> ^(REF_MODULE Identifier*)
  ;


// ================ ENUM ================

  enumDeclaration
    : ENUM_DECLARATION^ defSymbol?  (':'! type)? enumBody 
    ;
    
  enumBody
    : '{'! enumMembers '}'!
    | ';'!
    ;
    
  enumMembers
    : enumMember ( ','! enumMember)*
    ;
    
  enumMember
    : defSymbol ('=' primaryExpression)? -> ^(ENUM_MEMBER defSymbol primaryExpression?)
    ;

    
// ================ CLASS ================

 classDeclaration
  : CLASS_DECLARATION^ defSymbol baseClassList? classBody
  ;
  
baseClassList
  : ':' identifierList (',' identifierList)* -> ^(BASE_CLASSES ^(BASE_CLASS identifierList)*) 
  ;

classBody
  : '{'! classMember* '}'!
  ;
  
classMember
  : declDef
  | classAllocator
  | classDeallocator
  | invariant
  ;
  
classAllocator
  : 'new' parameters functionBody -> ^(DEF_CTOR["new"] parameters functionBody)
  ;
  
classDeallocator
  : 'delete' parameters functionBody -> ^(DEF_CTOR["delete"] parameters functionBody)
  ;
  
invariant
  : INVARIANT^ '('! ')'! blockStatement 
  ;

  
// ================ INTERFACE ================

interfaceDeclaration
  : INTERFACE_DECLARATION^ defSymbol baseClassList? interfaceBody
  ;
  
interfaceBody
  : '{'! declDef* '}'!
  ;

  
//================ STRUCT & UNION ================
  
aggregateDeclaration
  : (STRUCT_DECLARATION^ | UNION_DECLARATION^) defSymbol (';'! | structBody)
  ;
    
 structBody
  : '{'! structMember* '}'!
  ;

structMember
  : declDef
  | classAllocator
  | classDeallocator
  ;


//================ OTHER DECLARATIONS  ================

ctorDeclaration
  : 'this' parameters functionBody -> ^(DEF_CTOR["this"] parameters functionBody)
  | '~this' '(' ')' functionBody -> ^(DEF_CTOR["~this"] PARAMETER_LIST functionBody)
  ;
  

unittestDeclaration
  : UNITTEST_DECLARATION^  blockStatement
  ;
  
mixinDeclaration
  : 'mixin' '(' assignExpression ')' ';'-> ^(MIXIN_DECLARATION assignExpression)
  ;
  
//================ CONDITIONAL DECLARATIONS  ================

conditionalDeclaration
  : condition declarationBlock
    ( ('else')=> 'else' declarationBlock)?
      -> ^(CONDITIONAL_DECL condition declarationBlock+)
  ;
  
condition
  : 'version' '(' symbol ')' -> ^(CONDITIONAL_COMPILATION_CONDITION[$start] symbol)
  | 'debug' '(' symbol ')' -> ^(CONDITIONAL_COMPILATION_CONDITION[$start] symbol)
  | 'static' 'if' '(' assignExpression ')' -> ^(CONDITIONAL_COMPILATION_CONDITION["static if"] assignExpression)
  ;

dvSpecification
  : ('version' | 'debug') '=' symbol ';' -> ^(DV_SPEC[$start] symbol)
  ;
  
staticAssert
  : 'static' 'assert' '(' assignExpression (',' assignExpression)? ')' ';'
    -> ^(STATIC_ASSERT assignExpression*)
  ;


//================ TEMPLATE DECLARATIONS  ================

templateDeclaration
  : TEMPLATE_DECLARATION^
    defSymbol '('! templateParameterList ')'!
    constraint?
    templateDeclarationBlock
  ;
  
templateParameterList
  :  ( templateParameter (',' templateParameter)* )? -> ^(TEMPLATE_PARAM_LIST templateParameter*)
  ;
  
templateParameter
  : templateTypeParameter
  | templateValueParameter
  | templateAliasParameter
  | templateTupleParameter
  | templateThisParameter
  ;
  
templateTypeParameter
  : defSymbol (':' type)? ('=' type)? -> ^(TEMPLATE_PARAM_TYPE defSymbol type*)
  ;
  
templateValueParameter
  : type defSymbol
    (':' conditionalExpression)?
    ('=' assignExpression)?
    -> ^(TEMPLATE_PARAM_VALUE defSymbol type conditionalExpression? assignExpression?)
  ;
  
templateAliasParameter
  : 'alias' type? defSymbol
    (':' ( (Identifier '=>')=> (lambdaExpression)
         | (type)=> (type)
         | conditionalExpression
         )
    )?
         
    ('=' ( (Identifier '=>')=> (lambdaExpression)
         | (type)=> (type)
         | conditionalExpression
         )
    )?
    -> ^(TEMPLATE_PARAM_ALIAS defSymbol)
  ;
  
templateTupleParameter
  : defSymbol '...' -> ^(TEMPLATE_PARAM_TUPLE defSymbol)
  ;
  
templateThisParameter
  : 'this' templateTypeParameter -> ^(TEMPLATE_PARAM_THIS templateTypeParameter)
  ;
  
constraint
  : 'if'! '('! expression ')'! 
  ;
  
templateDeclarationBlock
  : '{' declDef* '}' -> ^(DECLARATION_BLOCK declDef*)
  ;
  
templateMixinDeclaration
  : 'mixin' templateDeclaration -> ^(TEMPLATE_MIXIN_DECLARATION templateDeclaration)
  ;
  
templateMixin
  : 'mixin' type (options { greedy=true; }: Identifier)? -> ^(TEMPLATE_MIXIN type Identifier?)
  ;

blockStatement
  : '{' statement* '}' -> ^(STMT_BLOCK statement*)
  ;
  
//================ EXPRESSIONS  ================

expression
  : commaExpression
  ;
  
commaExpression
  : (assignExpression -> assignExpression)
    (',' assignExpression -> ^(INFIX[","] $commaExpression assignExpression))*
  ;
  
assignExpression
  : (conditionalExpression -> conditionalExpression)
    ( options { greedy = true; }
    : op='=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='+=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='-=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='*=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='/=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='%=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='&=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='|=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='^=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='~=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='<<=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='>>=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='>>>=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    | op='^^=' assignExpression -> ^(INFIX[$op] conditionalExpression assignExpression)
    )?
  ;

conditionalExpression
  : ( ororExpression -> ororExpression )
    ( options { greedy=true; }: '?' conditionalExpression  ':' conditionalExpression
      -> ^(EXP_CONDITIONAL ororExpression conditionalExpression conditionalExpression)
    )?
  ;
  
ororExpression
  : (andandExpression -> andandExpression)
    ( options { greedy=true; }: '||' ororExpression -> ^(INFIX["||"] andandExpression ororExpression) )?
  ;
  
andandExpression
  : (orCmpExpression -> orCmpExpression)
    ( options { greedy=true; }: '&&' andandExpression -> ^(INFIX["&&"] orCmpExpression andandExpression) )?
  ;
  
orCmpExpression
  : (xorExpression -> xorExpression)
    ( options { greedy=true; }
    : '|' orCmpExpression -> ^(INFIX["|"] xorExpression orCmpExpression)
    | '==' orCmpExpression -> ^(INFIX["=="] xorExpression orCmpExpression)
    | '!=' orCmpExpression -> ^(INFIX["!="] xorExpression orCmpExpression)
    | 'is' orCmpExpression -> ^(INFIX["is"] xorExpression orCmpExpression)
    | '!is' orCmpExpression -> ^(INFIX["!is"] xorExpression orCmpExpression)
    | '<' orCmpExpression -> ^(INFIX["<"] xorExpression orCmpExpression)
    | '<=' orCmpExpression -> ^(INFIX["<="] xorExpression orCmpExpression)
    | '>' orCmpExpression -> ^(INFIX[">"] xorExpression orCmpExpression)
    | '>=' orCmpExpression -> ^(INFIX[">="] xorExpression orCmpExpression)
    | '!<>=' orCmpExpression -> ^(INFIX["!<>="] xorExpression orCmpExpression)
    | '<>' orCmpExpression -> ^(INFIX["<>"] xorExpression orCmpExpression)
    | '<>=' orCmpExpression -> ^(INFIX["<>="] xorExpression orCmpExpression)
    | '!>' orCmpExpression -> ^(INFIX["!>"] xorExpression orCmpExpression)
    | '!>=' orCmpExpression -> ^(INFIX["!>="] xorExpression orCmpExpression)
    | '!<' orCmpExpression -> ^(INFIX["!<"] xorExpression orCmpExpression)
    | '!<=' orCmpExpression -> ^(INFIX["!<="] xorExpression orCmpExpression)
    )?
  ;
  
xorExpression
  : (andExpression -> andExpression)
    ( options { greedy=true; }: '^' xorExpression -> ^(INFIX["^"] andExpression xorExpression) )?
  ;
  
andExpression
  : (shiftExpression -> shiftExpression)
    ( options { greedy=true; }: '&' andExpression -> ^(INFIX["&"] shiftExpression andExpression) )?
  ;

shiftExpression
  : (addExpression -> addExpression)
    ( options { greedy=true; }
    : '<<' shiftExpression -> ^(INFIX["<<"] addExpression shiftExpression)
    | '>>' shiftExpression -> ^(INFIX[">>"] addExpression shiftExpression)
    | '>>>' shiftExpression -> ^(INFIX[">>>"] addExpression shiftExpression)
    )?
  ;

addExpression
  : (mulExpression -> mulExpression)
    ( options { greedy=true; }
    : '+' addExpression -> ^(INFIX["+"] mulExpression addExpression)
    | '-' addExpression -> ^(INFIX["-"] mulExpression addExpression)
    | '~' addExpression -> ^(INFIX["~"] mulExpression addExpression)
    )?
  ;
  
mulExpression
  : (unaryExpression -> unaryExpression)
    ( options { greedy=true; }
    : '*' mulExpression -> ^(INFIX["*"] unaryExpression mulExpression)
    | '/' mulExpression -> ^(INFIX["/"] unaryExpression mulExpression)
    | '%' mulExpression -> ^(INFIX["\%"] unaryExpression mulExpression)
    )?
  ;
  
unaryExpression
  : ( powExpression -> powExpression
    | '&' unaryExpression -> ^(PREFIX["&"] unaryExpression)
    | '++' unaryExpression -> ^(PREFIX["++"] unaryExpression)
    | '*' unaryExpression -> ^(PREFIX["*"] unaryExpression)
    | '+' unaryExpression -> ^(PREFIX["+"] unaryExpression)
    | '-' unaryExpression -> ^(PREFIX["-"] unaryExpression)
    | '!' unaryExpression -> ^(PREFIX["!"] unaryExpression)
    | '~' unaryExpression -> ^(PREFIX["~"] unaryExpression)
    // | '(' type ')' '.' Identifier
    | newExpression -> newExpression
    | deleteExpression -> deleteExpression
    | castExpression -> castExpression
    )  
  ;
  
newExpression
  : EXP_NEW^
    allocArgs? type ( options { greedy=true; }: '('! argumentList ')'! )?
  ;

allocArgs
  : '('! argumentList ')'! 
  ;
  
deleteExpression
  :EXP_DELETE^ unaryExpression
  ;
  
castExpression
  : EXP_CAST^
    '('! (type | castQual)? ')'! unaryExpression
  ;
  
castQual
  : castQualifier -> ^(CAST_QUALIFIER castQualifier)
  ;
  
castQualifier
  : 'const' 'shared'?
  | 'shared' ('const'  | 'inout')?
  | 'inout' 'shared'?
  | 'immutable'
  ;
  
powExpression
  : (postFixExpression -> postFixExpression)
    ( options { greedy=true; }: '^^' unaryExpression -> ^(INFIX["^^"] postFixExpression unaryExpression) )?
  ;
  
postFixExpression
  : (primaryExpression -> primaryExpression)
  
	  ( options { greedy = true; }
	  : '++' -> ^(POSTFIX["++"] $postFixExpression)
	  | '--' -> ^(POSTFIX["--"] $postFixExpression)
	  | '(' argumentList ')' -> ^(EXP_CALL $postFixExpression argumentList)
	  | ( '[' assignExpression '..' )=> ('[' assignExpression '..' assignExpression ']'
	        -> ^(EXP_SLICE $postFixExpression assignExpression assignExpression))
	  | ( '[' assignExpression ',' )=> ('[' argumentList ']' -> ^(EXP_INDEX argumentList))
	  | '.' Identifier -> ^(EXP_REFERENCE ^(REF_QUALIFIED $postFixExpression ^(REF_IDENTIFIER Identifier)))
	  // Unimplemented syntax
	  // | '.' newExpression -> ^()
    | '!' ( templateSingleArgument -> ^(TEMPLATE_INSTANCE $postFixExpression templateSingleArgument)
          | '(' templateArgumentList ')'-> ^(TEMPLATE_INSTANCE $postFixExpression templateArgumentList)
          )
	  )*
  ;

primaryExpression
  : IntegerLiteral -> ^(EXP_LITERAL_INTEGER[$IntegerLiteral])
  | FloatLiteral -> ^(EXP_LITERAL_REAL[$FloatLiteral])
  | CharacterLiteral -> ^(EXP_LITERAL_CHAR[$CharacterLiteral])
  | StringLiteral -> ^(EXP_LITERAL_STRING[$StringLiteral])
  | EXP_THIS
  | EXP_SUPER
  | EXP_LITERAL_NULL
  | ('.' Identifier '.')=> ('.' refIdentifier -> ^(REF_QUALIFIED refIdentifier))
  | ('.' Identifier '!')=> ('.' refIdentifier -> ^(REF_QUALIFIED refIdentifier))
  | '.' refIdentifier -> ^(EXP_REFERENCE ^(REF_QUALIFIED refIdentifier))
  | (Identifier '.')=> (refIdentifier)
  | (Identifier '!')=> (refIdentifier)
  | (Identifier '=>')=> (lambdaExpression)
  | refIdentifier -> ^(EXP_REFERENCE refIdentifier)
  | EXP_DOLLAR
  | EXP_LITERAL_FILE
  | EXP_LITERAL_LINE
  | (parameters '=>')=> (lambdaExpression)
  | '('! assignExpression ')'!
  | assertExpression
  | mixinExpression
  | typeOfExpression
  | typeIdExpression
  | functionLiteral
  | importExpression
  | isExpression
  | traitsExpression
  ;
  
assertExpression
  : EXP_ASSERT^ '('! assignExpression (',' assignExpression)? ')'!
  ;
  
mixinExpression
  : EXP_MIXIN^ '('! assignExpression ')'!
  ;
  
typeOfExpression
  : EXP_TYPEOF '('! ( 'return' | expression) ')'!
  ;
  
typeIdExpression
  : EXP_TYPEID^ '('! expression ')'!
  ;

lambdaExpression
  : Identifier '=>' assignExpression -> ^(EXP_LITERAL_FUNCTION ^(STMT_BLOCK ^(STMT_RETURN assignExpression)))
  | parameters? functionAttribute* '=>' assignExpression
    -> ^(EXP_LITERAL_FUNCTION parameters? ^(STMT_BLOCK ^(STMT_RETURN assignExpression)))
  ;
  
functionLiteral
  : ( 'function' | 'delegate') type? (parameters functionAttribute*)? functionBody
     -> ^(EXP_LITERAL_FUNCTION type? parameters? functionBody)
  ;
  
importExpression
  : 'import' '(' assignExpression ')' -> ^(EXP_LITERAL_IMPORTEDSTRING assignExpression)
  ;
  
isExpression
  : EXP_IS^ '('! type (Identifier?)!
    (':'! typeSpecialization |  '=='! typeSpecialization)
    (',' templateParameterList)? ')'!
  ;
  
typeSpecialization
  : type
  | 'struct' | 'union' | 'class'
  | 'interface' | 'enum' | 'function'
  | 'delegate' | 'super' | 'const'
  | 'immutable' | 'inout' | 'shared'
  | 'return'
  ;
  
traitsExpression
  : EXP_TRAITS '('!
    ( 'isAbstractClass' | 'isArithmetic' | 'isAssociativeArray' | 'isFinalClass' 
    | 'isFloating' | 'isIntegral' | 'isScalar' | 'isStaticArray' | 'isUnsigned' 
    | 'isVirtualFunction' 'isAbstractFunction' | 'isFinalFunction' | 'isStaticFunction'
    | 'isRef' | 'isOut' | 'isLazy' | 'hasMember' | 'identifier' | 'getMember'
    | 'getOverloads'| 'getVirtualFunctions' | 'parent' | 'classInstanceSize' | 'allMembers'
    | 'derivedMembers' | 'isSame' | 'compiles'
    )
    ( ','!  assignExpression | type )+
    ')'!
  ;
  
//================ STATEMENTS  ================

statement
  : ';'!
  | nonEmptyStatement
  | scopeBlockStatement
  ;
  
noScopeNonEmptyStatement
  : nonEmptyStatement
  | blockStatement
  ;

noScopeStatement
  : ';'!
  | nonEmptyStatement
  | blockStatement
  ;
  
nonEmptyOrScopeBlockStatement
  : nonEmptyStatement
  | scopeBlockStatement
  ;
  
nonEmptyStatement
  : nonEmptyStatementNoCaseNoDefault
  | caseStatement
  // No need for it probably
  // | caseRangeStatement
  | defaultStatement
  ;
  
nonEmptyStatementNoCaseNoDefault
  : (Identifier ':')=> (labeledStatement)
  | ('final' 'switch')=> (finalSwitchStatement)
  | ('static' 'if')=> (conditionalStatement)
  | ('static' 'assert')=> (staticAssert)
  | (declarationStatement)=> (declarationStatement)
  | ('mixin' '(')=> (expressionStatement)
  | ('mixin' Identifier)=> (templateMixin)
  | expressionStatement
  | ifStatement
  | whileStatement
  | doStatement
  | forStatement
  | foreachStatement
  | switchStatement
  | finalSwitchStatement
  | continueStatement
  | breakStatement
  | returnStatement
  | gotoStatement
  | withStatement
  | synchronizedStatement
  | tryStatement
  | scopeGuardStatement
  | throwStatement
  | asmStatement
  | pragmaStatement
  // Big collision with the conditionalDeclaration. What's the real difference?
  // Probably they should be unified as much as possible. Declarations and statements in this respect
  // form a common set.
  | conditionalStatement
  ;
  
scopeBlockStatement
  : blockStatement
  ;
  
scopeStatementList
  : ( options {greedy=true;}: statementNoCaseNoDefault)*
  ;
  
statementNoCaseNoDefault
  : ';'!
  | nonEmptyStatementNoCaseNoDefault
  | scopeBlockStatement
  ;
  
scopeStatement
  : nonEmptyStatement
  | blockStatement
  ;
  
caseStatement
  : STMT_CASE^ argumentList ':'! scopeStatementList
  ;
  
defaultStatement
  : STMT_DEFAULT^ ':'! scopeStatementList
  ;
  
labeledStatement
  : (Identifier ':')=>
      (Identifier ':' noScopeStatement -> ^(STMT_LABELED Identifier noScopeStatement))
  ;
  
expressionStatement
  : expression ';' -> ^(STMT_EXPRESSION expression)
  ;
  
declarationStatement
  : declaration
  ;
  
ifStatement
  : STMT_IF '(' ifCondition ')' scopeStatement
    ( ('else')=> ('else' scopeStatement) )?
    -> ^(STMT_IF ifCondition scopeStatement scopeStatement?)
  ;
  
ifCondition
  options { greedy=true; }: expression
  // These cases should reflect the AST
  | 'auto'! defSymbol! '='! expression
  | type! defSymbol! '=' expression 
  ;
  
whileStatement
  : STMT_WHILE^ '('! expression ')'! scopeStatement
  ;
  
doStatement
  : STMT_DO^ scopeStatement 'while'! '('! expression ')'! ';'!
  ;
  
//================ FOR ================
  
forStatement
  : STMT_FOR^ '('! initialize forExp ';'! forExp ')'! scopeStatement
  ;
  
initialize
  : ';' -> ^(NULL)
  | noScopeNonEmptyStatement
  ;
  
forExp
  : expression
  | -> ^(NULL)
  ;
  
foreachStatement
  : ('foreach' '(' parameter ';' expression '..')=>
      ( 'foreach' '(' parameter ';' expression '..' expression ')' noScopeNonEmptyStatement
        -> ^(STMT_FOREACH_RANGE["fwd"] parameter expression expression noScopeNonEmptyStatement) )
  | ('foreach_reverse' '(' parameter ';' expression '..')=>
      ( 'foreach' '(' parameter ';' expression '..' expression ')' noScopeNonEmptyStatement
        -> ^(STMT_FOREACH_RANGE["rev"] parameter expression expression noScopeNonEmptyStatement) )
  | 'foreach' '(' parameterList ';' expression ')' noScopeNonEmptyStatement
      -> ^(STMT_FOREACH["fwd"] parameterList expression noScopeNonEmptyStatement)
  | 'foreach_reverse' '(' parameterList ';' expression ')' noScopeNonEmptyStatement
      -> ^(STMT_FOREACH["rev"] parameterList expression noScopeNonEmptyStatement)
  ;
  
//================ ... ================

switchStatement
  : STMT_SWITCH^ '('! expression ')'! scopeStatement
  ;
  
finalSwitchStatement
  : 'final' 'switch' '(' expression ')' scopeStatement
    -> ^(STMT_FINALSWITCH expression scopeStatement) 
  ;
  
continueStatement
  : STMT_CONTINUE^ Identifier? ';'!
  ;
  
breakStatement
  : STMT_BREAK^ Identifier? ';'!
  ;
  
returnStatement
  : STMT_RETURN^ expression? ';'!
  ;
  
gotoStatement
  : STMT_GOTO^
    ( symbol
    | 'default'
    | 'case' expression?
    ) ';'!
  ;
  
withStatement
  : STMT_WITH^ '('!
    ( (Identifier '!')=> (templateInstance)
    | expression
    ) ')'! scopeStatement
  ;
  
synchronizedStatement
  : STMT_SYNCHRONIZED^
    ( options { greedy=true; }: '('! expression ')'! )?
    scopeStatement
  ;
  
//================ Try/Catch ================

tryStatement
  : STMT_TRY^ scopeStatement
    catches (options { greedy=true; } : finallyStatement)?
  ;
  
catches
  : ( (STMT_CATCH '(')=> (normalCatch))* (options { greedy=true; } : lastCatch)?
  ;
  
lastCatch
  : STMT_CATCH^ noScopeNonEmptyStatement
  ;
  
normalCatch
  : STMT_CATCH^ '('! parameter ')'! noScopeNonEmptyStatement
  ;
  
finallyStatement
  : STMT_FINALLY^ noScopeNonEmptyStatement
  ;
  
//================ ... ================

throwStatement
  : STMT_THROW^ expression ';'!
  ;
  
scopeGuardStatement
  : STMT_SCOPE^ '('!
    ( 'exit' | 'success' | 'failure' )
    ')'! nonEmptyOrScopeBlockStatement
  ;
  
asmStatement
  : STMT_ASM^
    '{' /* Not parsed yet: http://dlang.org/iasm.html */ '}'
  ;
  
pragmaStatement
  : STMT_PRAGMA^ '('! symbol (','! argumentList)? ')'!
    noScopeStatement
  ;
  
mixinStatement
  : mixinExpression ';' -> ^(STMT_MIXIN mixinExpression) 
  ;
  
conditionalStatement
  : condition noScopeNonEmptyStatement
    ( ('else')=> ('else' noScopeNonEmptyStatement) )?
      -> ^(STMT_CONDITIONAL condition noScopeNonEmptyStatement+)
  ;
  
argumentList
  : (assignExpression (',' assignExpression)* )?
      -> ^(ARG_LIST assignExpression*)
  ;