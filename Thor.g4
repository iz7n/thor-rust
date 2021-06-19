grammar Thor;

statements: '\n'* statement ('\n'+ statement)* '\n'*;

statement: 'return'? expr;

expr:
	| ( IDENTIFIER (('+' | '-' | '*' | '/')? '=') expr)
	| or_expr;

or_expr: and_expr ('or' or_expr)*;

and_expr: not_expr ('and' and_expr)*;

not_expr: ('not' not_expr) | comp_expr;

comp_expr:
	arith_expr ('==' | '!=' | '>' | '>=' | '<' | '<=' comp_expr)*;

arith_expr: term (('+' | '-') arith_expr)*;

term: factor (('*' | '/') term)*;

factor: ('+' | '-') factor | call;

call: ((IDENTIFIER | TYPE) '(' expr? (',' expr)* ')') | atom;

atom:
	INT
	| FLOAT
	| BOOLEAN
	| IDENTIFIER
	| '(' expr ')'
	| if_expr
	| for_expr
	| fn_expr;

if_expr: 'if' expr (':' statement | block);

else_expr: 'else' ((':' statement) | block | if_expr);

for_expr: 'for' IDENTIFIER 'in' expr (':' statement | block);

fn_expr: (
		'fn' IDENTIFIER '(' (IDENTIFIER ':' TYPE)? (
			',' IDENTIFIER ':' TYPE
		)* ')' ('->' statement | block)
	);

block: '{' statements '}';

INT: [0-9]+;
FLOAT: [0-9]+ '.' [0-9]*;
BOOLEAN: 'true' | 'false';
TYPE: 'int' | 'float' | 'bool';
IDENTIFIER: [a-zA-Z] [a-zA-Z0-9_]*;