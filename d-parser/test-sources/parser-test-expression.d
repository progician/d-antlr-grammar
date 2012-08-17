// (DEF_VAR (REF_IDENTIFIER int) x = (INITIALIZER_EXP (*= (EXP_REFERENCE (REF_IDENTIFIER b)) (*= (EXP_REFERENCE (REF_IDENTIFIER c)) 2))))
int x = b *= c *= 2;
// (DEF_VAR (REF_IDENTIFIER int) x = (INITIALIZER_EXP (= (EXP_REFERENCE (REF_IDENTIFIER b)) (= (EXP_REFERENCE (REF_IDENTIFIER c)) 2))))
int x = b = c = 2;
// (DEF_VAR (REF_IDENTIFIER int) x = (INITIALIZER_EXP (-= (EXP_REFERENCE (REF_IDENTIFIER b)) (>>= (EXP_REFERENCE (REF_IDENTIFIER c)) 2))))
int x = b -= c >>= 2;
// (DEF_VAR (REF_IDENTIFIER int) x = (INITIALIZER_EXP (EXP_CONDITIONAL (EXP_REFERENCE (REF_IDENTIFIER b))  (EXP_REFERENCE (REF_IDENTIFIER c)) 2)))
int x = b ? c : 2;
// (DEF_VAR (REF_IDENTIFIER int) x = (INITIALIZER_EXP (= (EXP_REFERENCE (REF_IDENTIFIER a)) (EXP_CONDITIONAL (EXP_REFERENCE (REF_IDENTIFIER b))  (EXP_REFERENCE (REF_IDENTIFIER c)) 2))))
int x = a = b ?  c : 2;
// (DEF_VAR (REF_IDENTIFIER bool) r = (INITIALIZER_EXP (|| (&& (EXP_REFERENCE (REF_IDENTIFIER a)) (EXP_REFERENCE (REF_IDENTIFIER b))) (EXP_REFERENCE (REF_IDENTIFIER c)))))
bool r = a && b || c;
// BUG! The descent.compiler produces different AST but this should be right according to the reference.
// (DEF_VAR (REF_IDENTIFIER int) r = (INITIALIZER_EXP (|| (> (* 2 (EXP_REFERENCE (REF_IDENTIFIER x))) (| 7 3)) 5)))
int r = 2 * x > 7 | 3 || 5;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (! (& (EXP_REFERENCE (REF_IDENTIFIER a)))))))
auto r = !&a;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (REF_IDENTIFIER A)))))
auto r = new A;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (TYPE_STATIC_ARRAY 10 (REF_IDENTIFIER int))))))
auto r = new int[10];
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y))))))
auto r = new x.y;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (TYPE_STATIC_ARRAY 10 (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y)))))))
auto r = new x.y[10];
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (ARG_LIST 2) (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y))))))
auto r = new (2) x.y;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (new (ARG_LIST 2) (REF_QUALIFIED (REF_IDENTIFIER x) (REF_IDENTIFIER y)) (ARG_LIST 2)))))
auto r = new (2) x.y(2);
// (auto (AUTO_VAR r = (INITIALIZER_EXP (cast (CAST_QUALIFIER const shared) (EXP_REFERENCE (REF_IDENTIFIER a))))))
auto r = cast(const shared) a;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (cast (REF_IDENTIFIER int) (EXP_REFERENCE (REF_IDENTIFIER a))))))
auto r = cast(int) a;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (cast (EXP_REFERENCE (REF_IDENTIFIER a))))))
auto r = cast() a;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a) (REF_IDENTIFIER b))))))
auto r = a.b;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_CALL (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a) (REF_IDENTIFIER b))) ARG_LIST))))
auto r = a.b();
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_CALL (EXP_CALL (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a) (REF_IDENTIFIER b))) ARG_LIST) ARG_LIST))))
auto r = a.b()();
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_CALL (TEMPLATE_INSTANCE (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a) (REF_IDENTIFIER b))) 5) (ARG_LIST 3)))))
auto r = a.b!5(3);
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_CALL (TEMPLATE_INSTANCE (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a) (REF_IDENTIFIER b))) char) (ARG_LIST 'c')))))
auto r = a.b!char('c');
// (auto (AUTO_VAR r = (INITIALIZER_EXP (+ (* (+ 3 4)  2) 5))))
auto r = (3 + 4) * 2 + 5;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_LITERAL_FUNCTION (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER int) x)) STMT_BLOCK))))
auto r = function(int x) { };
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_LITERAL_IMPORTEDSTRING "foo.txt"))))
auto r = import ("foo.txt");
// BUG: (auto (AUTO_VAR r = (INITIALIZER_EXP (typeid (++ (EXP_REFERENCE (REF_IDENTIFIER i)))))))
// (auto (AUTO_VAR r = (INITIALIZER_EXP typeid)))
auto r = typeid(i++);
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_LITERAL_FUNCTION (STMT_BLOCK (STMT_RETURN 1))))))
auto r = x => 1;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_LITERAL_FUNCTION (PARAMETER_LIST (FUNC_PARAM (REF_IDENTIFIER int) x)) (STMT_BLOCK (STMT_RETURN (* (EXP_REFERENCE (REF_IDENTIFIER x)) (EXP_REFERENCE (REF_IDENTIFIER x)))))))))
auto r = (int x) => x * x;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_REFERENCE (REF_QUALIFIED (REF_IDENTIFIER a))))))
auto r = .a;
// (auto (AUTO_VAR r = (INITIALIZER_EXP (EXP_REFERENCE (REF_QUALIFIED (REF_QUALIFIED (REF_IDENTIFIER a)) (REF_IDENTIFIER v))))))
auto r = .a.v;