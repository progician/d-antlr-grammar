// (module (MODULE_SYMBOL     x))
module x;
// (DEF_VAR (REF_IDENTIFIER int) a)
int a;
// (DEF_VAR (REF_IDENTIFIER int) b = (INITIALIZER_EXP 2))
int b = 2;
// (DEF_VAR (REF_QUALIFIED (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y)) (REF_IDENTIFIER z)) d)
x.y.z d;
// (DEF_VAR (TEMPLATE_INSTANCE a (REF_IDENTIFIER b)) d)
a!b d;
// (DEF_VAR (TEMPLATE_INSTANCE a (AST_NEONODE_ARRAYVIEW (REF_IDENTIFIER b) (REF_IDENTIFIER c))) d)
a!(b, c) d;
// (DEF_VAR (REF_QUALIFIED (REF_IDENTIFIER a) (TEMPLATE_INSTANCE x (AST_NEONODE_ARRAYVIEW (REF_IDENTIFIER b) (REF_IDENTIFIER c)))) d)
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
// (DEF_FUNC main (REF_IDENTIFIER int) PARAMETER_LIST FUNCTION_BODY)
int main() { }
// (const (AUTO_VAR a = (INITIALIZER_EXP 1)))
const a = 1;