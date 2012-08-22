grammar TokenTree;

options {
  language = Java;
  output = AST;
  
}

@lexer::header {
package dev.progician.dparser; 
}

@parser::header {
package dev.progician.dparser; 
}

WS
  : (' ' | '\t' | '\r' | '\n')+ { $channel = HIDDEN; }
  ;

TokenIdentity
  : '\'' (~'\'')* '\''
  | ('a' .. 'z' | 'A'..'Z' | '_') ( '0'..'9' | 'a' .. 'z' | 'A'..'Z' | '_')*
  ;
  
tokenTree
  : treeNode^ EOF!
  ;
  
StringLiteral
  : '"' (~'"')* '"'
  ;

treeNode
  : '('! tokenDeclaration^ treeNode+ ')'!
  | tokenDeclaration^
  ;

tokenDeclaration
  : TokenIdentity^
    ('['! StringLiteral ']'!)?
  ;