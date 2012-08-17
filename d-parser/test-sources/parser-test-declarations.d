// (module (MODULE_SYMBOL x))
module x;
// (DECL_IMPORT (IMPORT_CONTENT (REF_MODULE other)))
import other;
// (DECL_IMPORT (IMPORT_CONTENT (REF_MODULE static_other)) static)
static import static_other;
// (DECL_IMPORT (IMPORT_CONTENT (REF_MODULE mod1)) (IMPORT_CONTENT (REF_MODULE mod2)))
import mod1, mod2;
// (DECL_IMPORT (IMPORT_ALIAS (REF_MODULE m) (REF_MODULE other mod)))
import m = other.mod;
// (DECL_IMPORT (IMPORT_BINDING (IMPORT_CONTENT (REF_MODULE a)) b))
import a : b;
// (DEF_VAR (REF_IDENTIFIER int) a)
int a;
// (DEF_VAR (REF_IDENTIFIER int) b = (INITIALIZER_EXP 2))
int b = 2;
// (DEF_VAR (REF_QUALIFIED (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y)) (REF_IDENTIFIER z)) d)
x.y.z d;
// (DEF_VAR (TEMPLATE_INSTANCE (REF_IDENTIFIER a) (REF_IDENTIFIER b)) d)
a!b d;
// (DEF_VAR (TEMPLATE_INSTANCE (REF_IDENTIFIER a) (AST_NEONODE_ARRAYVIEW (REF_IDENTIFIER b) (REF_IDENTIFIER c))) d)
a!(b, c) d;
// (DEF_VAR (REF_QUALIFIED (REF_IDENTIFIER a) (TEMPLATE_INSTANCE (REF_IDENTIFIER x) (AST_NEONODE_ARRAYVIEW (REF_IDENTIFIER b) (REF_IDENTIFIER c)))) d)
a.x!(b, c) d;
// (DEF_VAR (TYPE_POINTER (REF_IDENTIFIER int)) a)
int* a;
// (DEF_VAR (TYPE_DYN_ARRAY (REF_IDENTIFIER int)) a)
int[] a;
// (DEF_VAR (TYPE_STATIC_ARRAY 10 (REF_IDENTIFIER int)) d)
int[10] d;
// (DEF_VAR (REF_TYPE_SLICE 1 2 (REF_IDENTIFIER int)) d)
int[1..2] d;
// (const (DEF_VAR (REF_IDENTIFIER int) a))
const int a;
// (static (const (DEF_VAR (REF_IDENTIFIER int) a)))
static const int a;
// (DEF_FUNC main (REF_IDENTIFIER int) PARAMETER_LIST STMT_BLOCK)
int main() { }
// (const (AUTO_VAR a = (INITIALIZER_EXP 1)))
const a = 1;
// (enum X (ENUM_MEMBER A) (ENUM_MEMBER B) (ENUM_MEMBER C))
enum X { A, B, C }
// (enum B)
enum B;
// (enum (ENUM_MEMBER A 1) (ENUM_MEMBER B 4) (ENUM_MEMBER C 12))
enum { A =1, B=4, C=12 }
// (enum Enum (REF_IDENTIFIER int))
enum Enum : int;
// (class X (DEF_VAR (REF_IDENTIFIER int) b))  
class X { int b; }
// (class X (BASE_CLASSES (BASE_CLASS (REF_IDENTIFIER Y)) (BASE_CLASS (REF_IDENTIFIER Z))) (DEF_FUNC b (REF_IDENTIFIER int) PARAMETER_LIST STMT_BLOCK))  
class X : Y, Z { int b() { } }
// (class X (new PARAMETER_LIST STMT_BLOCK) (delete PARAMETER_LIST STMT_BLOCK))  
class X { new () {} delete() {} }
// (class X (DEF_FUNC b (REF_IDENTIFIER int) PARAMETER_LIST STMT_BLOCK) (invariant STMT_BLOCK))
class X { int b() { } invariant() {} }
// (interface I1 (DEF_FUNC func1 (REF_IDENTIFIER int) PARAMETER_LIST))
interface I1 { int func1(); }
// (interface I2 (BASE_CLASSES (BASE_CLASS (REF_IDENTIFIER I1))) (DEF_FUNC func2 (REF_IDENTIFIER int) PARAMETER_LIST))
interface I2 : I1 { int func2(); }
// (struct S1 (DEF_VAR (REF_IDENTIFIER int) fieldA) (new PARAMETER_LIST STMT_BLOCK))
struct S1 { int fieldA; new () {} }
// (union U1 (DEF_VAR (REF_IDENTIFIER int) fieldA) (DEF_VAR (REF_IDENTIFIER float) fieldB))
union U1 { int fieldA; float fieldB; }
// (class C1 (this (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER int) a)) STMT_BLOCK))
class C1 { this(int a) { } }
// (version X (DECLARATION_BLOCK (DEF_VAR (REF_IDENTIFIER int) a)))
version (X) { int a; }
// (debug Y (DECLARATION_BLOCK (DEF_VAR (REF_IDENTIFIER int) a)) (DECLARATION_BLOCK (DEF_VAR (REF_IDENTIFIER float) a)))
debug (Y) int a; else float a;
// (static if 1 (DECLARATION_BLOCK (DEF_VAR (REF_IDENTIFIER int) a)) (DECLARATION_BLOCK (DEF_VAR (REF_IDENTIFIER float) a)))
static if (1) int a; else float a;
// (version X (DECLARATION_BLOCK (version Y)))
version (X) { version = Y; }
// (version BAR DECLARATION_BLOCK (DECLARATION_BLOCK (STATIC_ASSERT 0)))
version (BAR) { } else { static assert(0); }
// (template TFoo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_TYPE T)) DECLARATION_BLOCK)
template TFoo(T) { }
// (template TFoo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_TYPE T (TYPE_DYN_ARRAY (REF_IDENTIFIER T)))) DECLARATION_BLOCK)
template TFoo(T : T[]) { }
// (template TFoo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_TYPE T (REF_IDENTIFIER char))) DECLARATION_BLOCK)
template TFoo(T : char) { }
// (template TFoo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_TYPE T) (TEMPLATE_PARAM_TYPE U) (TEMPLATE_PARAM_TYPE V)) DECLARATION_BLOCK)
template TFoo(T,U,V) { }
// (template foo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_VALUE s (REF_IDENTIFIER string))) DECLARATION_BLOCK)
template foo(string s) { }
// (template foo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_VALUE s (REF_IDENTIFIER string) "yeh" "hey")) DECLARATION_BLOCK)
template foo(string s : "yeh" = "hey") { }
// (template Foo (TEMPLATE_PARAM_LIST (TEMPLATE_PARAM_ALIAS X)) DECLARATION_BLOCK)
template Foo(alias X) {  }
// (TEMPLATE_MIXIN_DECLARATION (template Foo TEMPLATE_PARAM_LIST DECLARATION_BLOCK))
mixin template Foo() {}
// (MIXIN_DECLARATION "class X {}")
mixin("class X {}");
// (DEF_VAR (REF_QUALIFIED (REF_IDENTIFIER a)) b)
.a b;
// (DEF_VAR (TEMPLATE_INSTANCE (REF_IDENTIFIER a) (AST_NEONODE_ARRAYVIEW (REF_QUALIFIED (REF_IDENTIFIER b)))) c)
a!(.b) c;
