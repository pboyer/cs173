#lang reader "../lib/autogrammar/lalr/lang/reader.rkt"

program: prog-expr ENDMARKER

prog-expr: expr | expr-semi-list | defvar-exp | deffun-exp | try-exp

topexpr: sub-expr | seq-exp | defvar-exp | deffun-exp | try-exp

expr-semi-list: (expr ";")+
seq-exp: "{" expr-semi-list "}" | "{" expr "}"

expr: assign-exp | prim-assign-exp | pre-op-exp | post-op-exp | sub-expr
    | while-exp | for-exp | var-exp
    | if-exp | obj-exp | func-exp

sub-expr: num-exp | str-exp | lhs | prim-exp
    | true-exp | false-exp | paren-exp
    | dot-method-exp | bracket-method-exp | app-exp 
    | raise-exp

lhs: id-exp | bracket-exp | dot-exp

num-exp: NUMBER
str-exp: STRING
true-exp: "true"
false-exp: "false"
id-exp: NAME
paren-exp: '(' expr ')'

pre-op-exp: "++" NAME | "--" NAME
post-op-exp: NAME "++" | NAME "--"

expr-list: (expr ",")* [expr]
op: "+" | "-" | "==" | "print" | "<" | ">"
prim-exp: op "(" expr-list ")"

namelist: (NAME ",")* [NAME]
func-exp: "lambda" "(" namelist ")" topexpr

app-exp: sub-expr "(" expr-list ")"
dot-method-exp: sub-expr "@" NAME "(" expr-list ")"
bracket-method-exp: sub-expr "@" "[" sub-expr "]" "(" expr-list ")"

var-exp: "var" NAME "=" expr
assign-exp: lhs "=" expr
prim-assign-exp: lhs "+=" sub-expr | lhs "-=" sub-expr

bracket-exp: sub-expr "[" sub-expr "]"
dot-exp: sub-expr "." NAME

else-exp: topexpr | if-exp
if-exp: "if" expr "then" topexpr "else" else-exp

defvar-exp: "defvar" NAME "=" expr "in" topexpr
deffun-exp: "deffun" NAME "(" namelist ")" prog-expr "in" topexpr

try-exp: "try" topexpr "catch" NAME "in" topexpr
raise-exp: "raise" sub-expr

field-entry: NAME ":" expr
field-list: (field-entry ",")* [field-entry]
obj-exp: "{" field-list "}"

while-exp: "while" "(" expr ")" topexpr
for-exp: "for" "(" expr ";" expr ";" expr ")" topexpr

