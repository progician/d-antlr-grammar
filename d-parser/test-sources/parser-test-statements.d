int main() {
// (STMT_IF (INFIX[">"] (EXP_REFERENCE (REF_IDENTIFIER Identifier["a"])) EXP_LITERAL_INTEGER["2"]) (STMT_RETURN EXP_LITERAL_INTEGER) (STMT_RETURN EXP_LITERAL_INTEGER))
if (a > 2) return 1; else return 2;
// (DEF_VAR (REF_IDENTIFIER 'int') DEF_SYMBOL["a"])
int a;
// (STMT_WHILE EXP_LITERAL_INTEGER (STMT_EXPRESSION (PREFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)))))
while (1) ++i;
// (STMT_FOR (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier["i"])) EXP_LITERAL_INTEGER)) (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER) (POSTFIX["++"] (EXP_REFERENCE (REF_IDENTIFIER Identifier["i"]))) (STMT_BLOCK STMT_CONTINUE))
for (i = 1; i < 10; i++) { continue; }
// (STMT_FOR (DEF_VAR (REF_IDENTIFIER 'int') DEF_SYMBOL) NULL NULL STMT_CONTINUE)
for (int x;;) continue;
// (STMT_THROW (EXP_NEW (REF_IDENTIFIER Identifier) ARG_LIST))
throw new Exception();
// (STMT_RETURN EXP_LITERAL_INTEGER)
return 1;
// (STMT_SWITCH (EXP_REFERENCE (REF_IDENTIFIER Identifier)) (STMT_BLOCK (STMT_CASE (ARG_LIST EXP_LITERAL_INTEGER) (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER)) STMT_BREAK) (STMT_DEFAULT (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER)) STMT_BREAK)))
switch(a) { case 2: a = 1; break; default: a = 2; break; }
// (STMT_DO (STMT_BLOCK (DEF_VAR (REF_IDENTIFIER 'int') DEF_SYMBOL) (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER))) EXP_LITERAL_INTEGER)
do { int a; a = 1; } while (1);
// (STMT_LABELED Identifier["L1"] (STMT_RETURN EXP_LITERAL_INTEGER))
L1: return 2;
// (STMT_GOTO SYMBOL["L1"])
goto L1;
// (STMT_GOTO STMT_CASE)
goto case;
// (STMT_GOTO STMT_DEFAULT)
goto default;
// (STMT_TRY (STMT_BLOCK (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["foo"])) ARG_LIST))) (STMT_CATCH (FUNC_PARAM (REF_IDENTIFIER Identifier["Exception"]) DEF_SYMBOL["e"]) STMT_BLOCK))
try { foo(); } catch (Exception e) { }
// (STMT_SYNCHRONIZED STMT_BLOCK)
synchronized { }
// (STMT_SYNCHRONIZED (EXP_REFERENCE (REF_IDENTIFIER Identifier["mtx"])) STMT_BLOCK)
synchronized (mtx) { }
// (STMT_WITH (EXP_REFERENCE (REF_IDENTIFIER Identifier["foo"])) (STMT_BLOCK (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["bar"])) (ARG_LIST EXP_LITERAL_INTEGER)))))
with (foo) { bar(200); }
// (STMT_WITH (TEMPLATE_INSTANCE (REF_IDENTIFIER Identifier["foo"]) (AST_NEONODE_ARRAYVIEW (REF_IDENTIFIER 'int'))) (STMT_BLOCK (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["bar"])) (ARG_LIST EXP_LITERAL_INTEGER)))))
with (foo!(int)) { bar(200); }
// (STMT_WITH (TEMPLATE_INSTANCE (REF_IDENTIFIER Identifier["foo"]) (AST_NEONODE_ARRAYVIEW EXP_LITERAL_INTEGER)) (STMT_BLOCK (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["bar"])) (ARG_LIST EXP_LITERAL_INTEGER)))))
with (foo!(4)) { bar(200); }
// (STMT_FOREACH["fwd"] (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER 'int') DEF_SYMBOL["i"]) (FUNC_PARAM (REF_IDENTIFIER 'char') DEF_SYMBOL["c"])) (EXP_REFERENCE (REF_IDENTIFIER Identifier["a"])) STMT_BLOCK)
foreach (int i, char c; a) { }
// (STMT_FOREACH["rev"] (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER 'int') DEF_SYMBOL["i"]) (FUNC_PARAM (REF_IDENTIFIER 'char') DEF_SYMBOL["c"])) (EXP_REFERENCE (REF_IDENTIFIER Identifier["a"])) STMT_BLOCK)
foreach_reverse (int i, char c; a) { }
// (STMT_FOREACH["fwd"] (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER 'int') DEF_SYMBOL["i"] ParameterAttribute["ref"]) (FUNC_PARAM (REF_IDENTIFIER 'char') DEF_SYMBOL["c"] ParameterAttribute["ref"])) (EXP_REFERENCE (REF_IDENTIFIER Identifier["a"])) STMT_BLOCK)
foreach (ref int i, ref char c; a) { }
// (STMT_FOREACH_RANGE["fwd"] (FUNC_PARAM (REF_IDENTIFIER 'int') DEF_SYMBOL["i"]) EXP_LITERAL_INTEGER (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["foo"])) ARG_LIST) (STMT_BLOCK (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["write"])) (ARG_LIST (EXP_REFERENCE (REF_IDENTIFIER Identifier["i"])))))))
foreach (int i; 0 .. foo()) { write(i); }
// (STMT_SCOPE 'exit' (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["write"])) (ARG_LIST EXP_LITERAL_STRING))))
scope(exit) write("3");
// (STMT_SCOPE 'success' (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["write"])) (ARG_LIST EXP_LITERAL_STRING))))
scope(success) write("4");
// (STMT_SCOPE 'failure' (STMT_EXPRESSION (EXP_CALL (EXP_REFERENCE (REF_IDENTIFIER Identifier["write"])) (ARG_LIST EXP_LITERAL_STRING))))
scope(failure) write("4");
// (STMT_FINALSWITCH (EXP_REFERENCE (REF_IDENTIFIER Identifier)) (STMT_BLOCK (STMT_CASE (ARG_LIST EXP_LITERAL_INTEGER) (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER)) STMT_BREAK) (STMT_CASE (ARG_LIST EXP_LITERAL_INTEGER) (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER)) STMT_BREAK)))
final switch(a) { case 2: a = 1; break; case 3: a = 2; break; }
// (STMT_BLOCK (DEF_VAR (REF_IDENTIFIER 'int') DEF_SYMBOL["y"]))
{ int y; }
// (STMT_CONDITIONAL (CONDITIONAL_COMPILATION_CONDITION["version"] SYMBOL["Foo"]) (STMT_BLOCK (STMT_EXPRESSION (INFIX (EXP_REFERENCE (REF_IDENTIFIER Identifier)) EXP_LITERAL_INTEGER))))
version (Foo) { x = 2; }
}