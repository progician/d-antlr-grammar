tree grammar toyplASTGen;

options {
  language = Java;
  output = AST;
  tokenVocab = toypl;
  ASTLabelType = CommonTree;
}

@header {
package dev.progician.toypl;
}

prog
  : stmt* EOF
  ;
  
stmt
  : assign ';'
  ;
  
assign
  : ^(INFIX Identifier Integer)
  ;
