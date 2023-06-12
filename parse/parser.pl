:- module(parser,
          [ lex_parse/2,
            lex_parse/1,
            parse/2,
            expr/3,
            is_number/1 ]).

:- use_module(misc).

:- use_module(test,
          [ test/4 ]).

:- use_module(error_,
          [ set_error/3,
            set_error/4,
            reset_error/0,
            process_error/0,
            semantic_error/3]).

:- use_module(utils).

:- use_module(lexer,
          [ lex/2 ]).

:- use_module(des_data).

/*I want to use my_raise_exception 
but I don't know how this predicate 
functions or how to use it*/
%now it contain profix, infix, posfix.
:- use_module(des).  


% This SWI-Prolog flag makes strings delimited by double
% quotes to represent lists of character codes:
:- set_prolog_flag(double_quotes, codes).

% The operator # is used to couple the consumed token with
%   the error message corresponding to the expected token.
%   This error message includes the position
:- op(995, xfy, #).

% parse(+Tokens, -SyntaxTree) is det
% Main predicate of this module
% Parse the tokens from a SQL programa and return the corresponding list of sentences, each one represented by its syntax tree
% If parse/2 fails, an error should be asserted already,
% and then it is displayed with process_error/0
lex_parse(Input) :-
  lex(Input, Tokens),
  phrase(filter_tokens(FilteredTokens, []), Tokens),
  !,
  parse(FilteredTokens, SyntaxTrees),
  print(SyntaxTrees).

lex_parse(Input, SyntaxTrees) :-
  lex(Input, Tokens),
  phrase(filter_tokens(FilteredTokens, []), Tokens),
  !,
  parse(FilteredTokens, SyntaxTrees).

parse(Tokens, SyntaxTrees) :-
  reset_error,
  phrase(parse(SyntaxTrees/[]), Tokens),
  !.

 parse(_Tokens, _SyntaxTrees) :-
   process_error,
   !, fail.

% parse(-STs1/STs)//
parse(STs/STs) --> empty_program.
parse(STs1/STs) --> 
  statements(STs1/STs2),
  !,
  parse(STs2/STs).

empty_program --> [].

% Base case: Empty list.
filter_tokens([], []) --> 
  [].

% Ignore newline tokens.
filter_tokens(Filtered, Rest) --> 
  [punct(nl):_Pos], filter_tokens(Filtered, Rest).

% Ignore comment tokens.
filter_tokens(Filtered, Rest) --> 
  [comment(_):_Pos], filter_tokens(Filtered, Rest).

% Include non-newline and non-comment tokens.
filter_tokens([H|Filtered], Rest) --> 
  [H], 
    { H \= punct(nl):_, H \= comment(_):_ }, 
    filter_tokens(Filtered, Rest).

% statements(-STs1/STs)//
% statements ::=
%   DDLstmt[;] | DMLstmt[;] | DQLstmt[;] | ISLstmt[;] | TMLstmt[;]
statements(STs1/STs) -->
  dmlStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  dqlStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  islStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  tmlStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).
  
statements(_) -->
  set_error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)').

/*statements(STs1/STs) -->
  ddlStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).*/


% dqlStmt(-STs1/STs)//
dqlStmt([STs1|STs]/STs) --> 
  b_DQL([STs1|STs]/STs)               # 'todo',
  !.

dqlStmt([STs1|STs]/STs) --> 
  ub_DQL([STs1|STs]/STs)              # 'todo',
  !.

ub_DQL([select()|STs]/STs) --> 
  cmd(select)                         # 'SELECT',
  !.

b_DQL(SQLst) -->
  punct('(')                          # 'Opening parenthesis',
  dqlStmt(SQLst)                      # 'todo',
  punct(')')                          # 'Closing parenthesis'.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DML (Data Manipulation Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DMLstmt ::=
%   INSERT INTO TableName[(Att {,Att})] VALUES (ExprDef {,ExprDef}) {, (ExprDef {,ExprDef})}
%   |
%   INSERT INTO TableName DEFAULT VALUES
%   |
%   INSERT INTO TableName[(Att {,Att})] DQLstmt
%   |
%   DELETE FROM TableName [[AS] Identifier] [WHERE Condition]
%   |
%   UPDATE TableName [[AS] Identifier] SET Att=Expr {,Att=Expr} [WHERE Condition]

% DELETE FROM ... WHERE 
dmlStmt([delete_from(Table,WhereCondition)|STs]/STs) -->
  cmd(delete)                         # 'DELETE',
  cmd(from)                           # 'FROM',
  p_ren_tablename(Table),
  where_clause(WhereCondition),
  !.

% DELETE FROM 
dmlStmt([delete_from(Table,true)|STs]/STs) -->
  cmd(delete)                         # 'DELETE',
  cmd(from)                           # 'FROM',
  p_ren_tablename(Table),
  !.

% INSERT INTO Table(Columns) [VALUES(...) | selectStm]
dmlStmt([insert_into(TableName,Colnames, Vs)|STs]/STs) -->
  cmd(insert)                         # 'INSERT',
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  current_position(Position),
  punct('(')                          # 'opening parenthesis or DEFAULT',
  column_name_list(Colnames),
  punct(')')                          # 'closing parenthesis',
  {(my_remove_duplicates(Colnames,Colnames) -> true ;
  semantic_error('Column names must be different in ~w' , [Colnames], Position),
  !, fail)},
  {length(Colnames,L)},
  insert_values(L,Vs),
  !.

% INSERT INTO Table [VALUES(...) | selectStm]
dmlStmt([insert_into(TableName,Colnames, Vs)|STs]/STs) -->
  cmd(insert)                         # 'INSERT',
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  {(get_relation_arity(TableName,L) -> true ; true)},
  insert_values(L, Vs),
  {get_table_untyped_arguments(TableName,Colnames)},
  !.

where_clause(WhereCondition) -->
  cmd(where)                          # 'WHERE',
  opening_parentheses_star(N),
  {!},
  where_condition(WhereCondition)     # 'WHERE condition',
  closing_parentheses_star(N).
where_clause(true) -->
  [].

where_condition(C) --> 
  sql_condition(C).

on_condition(C) --> 
  sql_condition(C).

sql_having_condition(C) --> 
  sql_condition(C).

sql_condition(F) -->
  sql_condition(1200,F).
    
sql_condition(PP,To) -->
  cond_factor(L), 
  r_sql_condition(PP,0,L/To).
sql_condition(PP,To) -->
  punct('(')                          # 'opening parenthesis',
  sql_condition(1200,T)               # 'Expected valid SQL condition', 
  punct(')')                          # 'closing parenthesis',
  !,
  r_sql_condition(PP,0,T/To).
sql_condition(PP,To) -->
  {sql_operator(P,FX,SOP,OP),
    prefix(P,FX,PR),
    P=<PP},
  op(SOP)                             # OP,
  sql_condition(PR,T)                 # 'valid SQL condition', 
  {NT=..[OP,T]},
  r_sql_condition(PP,P,NT/To).

r_sql_condition(PP,Pi,Ti/To) -->
  {sql_operator(P,YFX,SOP,OP),
    infix(P,YFX,PL,PR),
    P=<PP,
    Pi=<PL,
    NT=..[OP,Ti,T]},
  op(SOP)                            # OP,
  sql_condition(PR,T), 
  r_sql_condition(PP,P,NT/To).
r_sql_condition(_,_,Ti/Ti) -->
  [].
  
sql_operator(1100,xfy, or,'or').
sql_operator(1050,xfy, xor,'xor').
sql_operator(1000,xfy, and,'and').
sql_operator( 900, fy, not,'not').

cond_factor(E) -->
  b_sql_condition(E).
cond_factor(true) --> 
  cmd(true).
cond_factor(false) --> 
  cmd(false).
cond_factor(is_null(R)) --> 
  sql_expression(R,_T), 
  cmd(is), 
  cmd(null)                           # 'NULL'.
cond_factor(not(is_null(R))) --> 
  sql_expression(R,_T),  
  cmd(is), 
  op(not)                             # 'NOT', 
  cmd(null)                           # 'NULL'.
cond_factor(exists(R)) -->
  cmd(exists),
  opening_parentheses_star(N),
  dqlStmt([R|STs]/STs)                # 'valid SELECT statement',
  my_DQL(R),
  closing_parentheses_star(N).
cond_factor(and('<='(L,C),'<='(C,R))) --> 
  sql_expression(C,CT),
  cmd(between)                        # 'BETWEEN',
  sql_expression(L,LT),
  syntax_check_same_types('BETWEEN test',CT,LT),
  op(and)                             # 'AND',
  sql_expression(R,RT),
  syntax_check_same_types('BETWEEN test',LT,RT),
  syntax_check_between(L,R).
cond_factor(or('>'(L,C),'>'(C,R))) --> 
  sql_expression(C,CT),
  op(not)                             # 'NOT',
  cmd(between)                        # 'BETWEEN',         
  sql_expression(L,LT),
  syntax_check_same_types('BETWEEN test',CT,LT),
  op(and)                             # 'AND',
  sql_expression(R,RT),
  syntax_check_same_types('BETWEEN test',LT,RT),
  syntax_check_between(L,R).
cond_factor(in(L,R)) --> 
  column_or_constant_tuple(L,A),
  cmd(in),
  opening_parentheses_star(N),
  dql_or_constant_tuples(A,R),
  closing_parentheses_star(N).
cond_factor(not_in(L,R)) --> 
  column_or_constant_tuple(L,A),
  op(not)                             # 'NOT',
  cmd(in)                             # 'IN',
  opening_parentheses_star(N),
  dql_or_constant_tuples(A,R),
  closing_parentheses_star(N).
cond_factor(F) --> 
  sql_expression(L,LT),
  syntax_check_expr_type(L,LT,string(_)),
  optional_op(not,NOT),
  cmd(like)                           # 'LIKE',
  sql_expression(R,RT),
  syntax_check_expr_type(R,RT,string(_)),
  {(NOT==true -> F='$not_like'(L,R) ; F='$like'(L,R))}.
cond_factor(F) --> 
  sql_expression(L,LT),
  syntax_check_expr_type(L,LT,string(_)),
  optional_op(not,NOT),
  cmd(like)                           # 'LIKE',
  sql_expression(R,RT),
  syntax_check_expr_type(R,RT,string(_)),
  cmd(escape)                         # 'ESCAPE',
  sql_expression(E,ET),
  syntax_check_expr_type(E,ET,string(_)),
  {(NOT==true -> F='$not_like'(L,R,E) ; F='$like'(L,R,E))}.
cond_factor(C) --> 
  sql_expression(L,LT), 
  relop(Op)                           # 'comparison operator', 
  sql_expression(R,RT),
  {sql_rel_cond_factor(Op,L,R,C)},
  syntax_check_same_types(C,LT,RT).
my_cond_factor(true) --> 
  {current_db(_,mysql)},
  sql_constant(_C).

b_sql_condition(SQLst) -->
  punct('(')                          # 'opening parenthesis',
  sql_condition(SQLst),
  punct(')')                          # 'closing parenthesis'.

relop(RO) --> 
  set_op(RO).
relop(RO) --> 
  tuple_op(RO).

set_op(SO) -->
  tuple_op(TO),
  cmd(all)                            # 'all',
  {atom_concat(TO,'_all',SO)}.
set_op(SO) -->
  my_tuple_op(TO),
  cmd(any)                            # 'any',
  {atom_concat(TO,'_any',SO)}.
  
tuple_op(RO) --> 
  {map_cond(RO,_), 
    atom_codes(RO,SRO)},
  [str(SRO):_].

%column_name_list(columnList)\\
%column_name separate with comma
column_name_list([C]) --> 
  untyped_column(C).
column_name_list([C|Cs]) -->
  untyped_column(C),
  punct(',')                          # 'comma', 
  !,
  column_name_list(Cs).

untyped_column(C) --> 
  colname(C).

p_ren_tablename(T) --> 
  ren_tablename(T).

p_ren_tablename((T)) -->
  tablename(T)                        # 'table name',
  !.

ren_tablename((T,[I])) -->
  %current_position(Position),
  tablename(T)                        # 'table name',
  optional_cmd(as),
  sql_user_identifier(I)              # 'user identifier',
  !,
  {my_table('$des',T,_)}.
  % Do not check the existence of said table for now
  /*-> true;
  semantic_error('table ~w does not exist in the $des system' , [T], Position),
  !, fail)}.*/

% tablename/viewname/colname
tablename(TableName) -->
  sql_user_identifier(TableName).

viewname(ViewName) -->
  sql_user_identifier(ViewName).

colname(ColName) -->
  sql_user_identifier(ColName).

% get table,view or col name -> quoted_id(id) | id(id) | [id(id)] | `id(id)` 
sql_user_identifier(Name) -->
  [quoted_id(Name):_Pos].

sql_user_identifier(Name) --> 
  [punct('['):_],  %no punct(']') # 'opening bracket' because it's not mandatory
  [id(Name):_Pos],
  punct(']')                          # 'closing bracket'.

sql_user_identifier(Name) --> 
  [punct('`'):_],  %no punct('`') # 'opening back quotes' because it's not mandatory
  [id(Name):_Pos],
  punct('`')                          # 'closing back quotes'.

sql_user_identifier(Name) -->
  [id(Name):_Pos].

%To obtain the arity (the number of attributes or columns) of a 
%relation (table or view) in the database.
get_relation_arity(Relation,Arity) :-
  current_db(Connection),
  get_relation_arity(Connection,Relation,Arity).

get_relation_arity('$des',Relation,Arity) :-
  !,
  my_table('$des',Relation,Arity). % Both tables and views
get_relation_arity(Connection,Relation,Arity) :-
  my_odbc_get_table_arity(Connection,Relation,Arity).

%insert_values(Arity, Values)
insert_values(L,Vs) -->
  cmd(default)                        # 'DEFAULT',
  cmd(values)                         # 'VALUES',
  {!,
  length(Vs,L),
  my_map_1('='(default),Vs)}.

insert_values(L, Ts) -->
  cmd(values)                         # 'VALUES',
  !,
  sql_ground_tuple_list(L,Ts).

insert_values(_L, select()) -->
  dqlStmt([select()|STs]/STs).

insert_values(_,_) -->
  set_error('Syntax', 'VALUES, select statement, or DEFAULT VALUES').

%get_table_untyped_arguments
get_table_untyped_arguments(TableName,Colnames) :-
  current_db(Connection),
  get_table_untyped_arguments(Connection,TableName,Colnames).
  
get_table_untyped_arguments('$des',TableName,Colnames) :-
  !,
  my_nf_setof((Pos,Colname),Type^my_attribute('$des',Pos,TableName,Colname,Type),PosColnames),
  findall(Colname,(member((Pos,Colname),PosColnames)),Colnames).

get_table_untyped_arguments(Connection,TableName,Colnames) :-
  my_odbc_get_colnames(Connection,TableName,Colnames).

get_table_untyped_arguments(_Connection,TableName,Colnames) :-
  des_sql_solving(on),
  get_table_untyped_arguments('$des',TableName,Colnames).

% sql_ground_tuple_list(+Arity, -TupleList) is det.
sql_ground_tuple_list(L,[T|Ts]) -->
  sql_ground_tuple(L,T),
  remaining_sql_ground_tuple_list(L,Ts).

remaining_sql_ground_tuple_list(L,Ts) -->
  punct(',')                          # 'comma',
  !,
  sql_ground_tuple_list(L,Ts).
remaining_sql_ground_tuple_list(_,[]) -->
  [].

sql_ground_tuple(L,Cs) -->
  current_position(Position),
  punct('(')                          # 'opening parenthesis',
  cs_expressions_def(Cs),
  punct(')')                          # 'closing parenthesis or comma',
  !,
  {length(Cs,TL),
    (L=TL -> true ;
      semantic_error('Unmatching number of values => ~w (must be ~w)' , [TL, L], Position),
      !, fail)}.

cs_expressions_def([E1|Es]) -->
  expressions_def(E1),
  punct(',')                          # 'comma',
  !,
  cs_expressions_def(Es).
cs_expressions_def([E]) -->
  expressions_def(E).

% Default
expressions_def(default) -->
  cmd(default)                        # 'DEFAULT',
  !.

%Number, String, DATE String, TIME String, TIMESTAMP, NULL
expressions_def(E) -->
  constant(E),
  !.

constant(E) -->
  value(E). %Number or String

% DATE 'String' % String in format '[BC] Int-Int-Int'
% date(Str)
constant(date(Str)) -->
  cmd(date)                           # 'DATE',
  current_position(Position),
  str(Str)                            # 'string',
  !,
  { is_date_format(Str, Position) }. % Verify Str is in format '[BC] Int-Int-Int'
 

% TIME 'String' % String in format 'Int:Int:Int'
% time(Str)
constant(time(Str)) -->
  cmd(time)                           # 'TIME',
  current_position(Position),
  str(Str)                            # 'string',
  !,
  { is_time_format(Str, Position) }.  % Verify Str is in format 'Int:Int:Int'
 
  

% TIMESTAMP 'String' % String in format '[BC] Int-Int-Int Int:Int:Int'
% timestamp(Str)
constant(timestamp(Str)) -->
  cmd(timestamp)                      # 'TIMESTAMP',
  current_position(Position),
  str(Str)                            # 'string',
  !,
  { is_timestamp_format(Str, Position) }.  % Verify Str is in format '[BC] Int-Int-Int Int:Int:Int'


% Default
constant(null) -->
  cmd(null)                           # 'NULL',
  !.

constant(_) -->
  set_error('Syntax', 'Number, String, DATE String, TIME String, TIMESTAMP String, NULL').

% cs_nat_exprs(-Ns)//
% Comma-separated naturals (0..)
cs_nat_exprs([N1, N2|Ns]) -->
  nat_expression(N1)                  # 'Natural expression',
  punct(',')                          # 'comma',
  !,
  cs_nat_exprs([N2|Ns]).
cs_nat_exprs([N]) -->
  nat_expression(N)                   # 'Natural expression'.


% cs_variables(-Vs)//
% Comma-separated atomic variables
cs_variables([id(V1), V2|Vs]) -->
  variable(V1)                        # 'Variable',
  punct(',')                          # 'Comma',
  !,
  cs_variables([V2|Vs]).
cs_variables([id(V)]) -->
  variable(V)                         # 'Variable'.


% cs_values(-Vs)//
% Comma-separated numbers and strings
cs_values([V1, V2|Vs]) -->
  value(V1)                           # 'Value (number or string)',
  punct(',')                          # 'Comma',
  !,
  cs_values([V2|Vs]).
cs_values([V]) -->
  value(V)                            # 'Value (number or string)'.

% value(-Value)//
value(int(I)) -->
  int(I),
  !.
value(frac(I, F)) -->
  frac(I, F),
  !.
value(float(I, F, Ex)) -->
  float(I, F, Ex),
  !.
value(str(Str)) -->
  [str(Str):_],
  !.

% is_date_format(+Str, +Position)//
% is_time_format(+Str, +Position)//
% is_timestamp_format(+Str, +Position)//
% If Str is not in the correct format, it raises a semantic error.

is_date_format(Str, Position) :-
  re_match("^\\d+-\\d+-\\d+$", Str) -> true; 
  semantic_error('Invalid DATE String format => must be \'Int-Int-Int\'', [], Position),
  !, fail.

is_time_format(Str, Position) :-
  re_match("^\\d+:\\d+:\\d+$", Str) -> true;
  semantic_error('Invalid TIME String format => must be \'Int:Int:Int\'', [], Position),
  !, fail.

is_timestamp_format(Str, Position) :-
  re_match("^\\d+-\\d+-\\d+ \\d+:\\d+:\\d+$", Str)  -> true;
  semantic_error('Invalid TIME String format => must be \'Int-Int-Int Int:Int:Int\'', [], Position),
  !, fail.

% is_number(+Expr)
% Succeed if Expr is a number
is_number(Expr) :-
  value(_, [Expr:_], []),
  Expr \= str(_).

% num_expression(-Expression)//
num_expression(Expression) -->
  expr(Expression),
  !,
  {is_num_expression(Expression)}.

% int_expression(-Expression)//
int_expression(Expression) -->
  expr(Expression),
  !,
  {is_int_expression(Expression)}.

% nat_expression(-Expression)//
nat_expression(Expression) -->
  expr(Expression),
  !,
  {is_nat_expression(Expression)}.

% bool_expression(-Expression)//
bool_expression(Expression) -->
  expr(Expression),
  !,
  {is_bool_expression(Expression)}.

% str_expression(-Expression)//
str_expression(Expression) -->
  expr(Expression),
  !,
  {is_str_expression(Expression)}.

% is_num_expression(+Expression)
is_num_expression(Expression) :- % REFINE. Naive test
  Expression \= str(_).

% is_str_expression(+Expression)
is_str_expression(Expression) :- % REFINE. Naive test
  Expression \= int(_),
  Expression \= frac(_, _),
  Expression \= float(_, _, _).

% is_bool_expression(+Expression)
is_bool_expression(Expression) :- % REFINE. Naive test
  is_int_expression(Expression).

% is_int_expression(+Expression)
is_int_expression(Expression) :-  % REFINE. Naive test
  Expression \= str(_),
  Expression \= frac(_, _),
  Expression \= float(_, _, _).

% is_nat_expression(+Expression)
is_nat_expression(Expression) :- % REFINE. Naive test
  is_int_expression(Expression).


% VALUES

% int(-I)//
int(I) -->
  [int(I):_Pos].

% frac(I, F)//
frac(I, F) -->
  [frac(I, F):_Pos].

% float(I, F, E)//
float(I, F, E) -->
  [float(I, F, E):_Pos].


% VARIABLES

% variable(-Id)//
% Variable (e.g., a, a$) or array element (e.g., a(0), a$(0))
variable(Id) -->
  array_variable(Id)           # 'Variable',
  !.
variable(Id) -->
  non_array_variable(Id)       # 'Variable'.

% non_array_variable(-Id)//
non_array_variable(Id) -->
    numeric_variable(Id)
  ; string_variable(Id).

% numeric_variable(-Id)//
% A function, operator or command name can play the role of a numeric
% variable (no reserved words), but not a string variable (ended in '$')
numeric_variable(Id) -->
    [id(Id):_],
    {\+ is_str_type_var(Id)}
  ; [fn(Id/_Ar):_]
  ; [cmd(Id):_]
  ; [op(Id):_].

% string_variable(-Id)//
string_variable(Id) -->
    [id(Id):_],
    {is_str_type_var(Id)}.

% array_variable(-Id, -Index)//
% Name of the array variable and its index as a list of expressions
array_variable(Id) -->
  non_array_variable(N)               # 'Variable',
  punct('(')                          # 'Opening parenthesis',
  cs_nat_exprs(Index)                 # 'Array index',
  punct(')')                          # 'Closing parenthesis',
  {Id =.. [N|Index]}.


% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % ISL (Information Schema Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ISLstmt ::=
%   SHOW TABLES
%   |
%   SHOW VIEWS
%   |
%   SHOW DATABASES
%   |
%   DESCRIBE [TableName|ViewName]

islStmt([show_tables|STs]/STs) -->
  cmd(show)                           # 'SHOW',
  cmd(tables)                         # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([show_views|STs]/STs) -->
  cmd(show)                           # 'SHOW',
  cmd(views)                          # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([show_databases|STs]/STs) -->
  cmd(show)                           # 'SHOW',
  cmd(databases)                      # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([describe(Name)|STs]/STs) -->
  cmd(describe)                       # 'DESCRIBE',
  tablename(Name)                     # 'table name',
  !. 

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % TML (Transaction Management Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TMLstmt ::=
%   COMMIT [WORK]
%   |
%   ROLLBACK [WORK] [TO SAVEPOINT SavepointName]
%   |
%   SAVEPOINT SavepointName

% COMMIT
tmlStmt([commit|STs]/STs) -->
  cmd(commit)                         # 'COMMIT',
  optional_cmd(work),
  !. 

% ROLLBACK TO SAVEPOINT
tmlStmt([rollback([SP])|STs]/STs) -->
  cmd(rollback)                       # 'ROLLBACK',
  optional_cmd(work),
  cmd(to)                             # 'TO',
  cmd(savepoint)                      # 'SAVEPOINT',
  filename(SP)                        # 'double quotes id (savepoint name)',
  !. 

% ROLLBACK
tmlStmt([rollback([])|STs]/STs) -->
  cmd(rollback)                       # 'ROLLBACK',
  optional_cmd(work),
  !. 

% SAVEPOINT
tmlStmt([savepoint([SP])|STs]/STs) -->
  cmd(savepoint)                      # 'SAVEPOINT',
  !,
  filename(FileName)                  # 'string (savepoint name)',
  {atom_concat(FileName,'.ddb',SP)}.


% filename(FileName)//
% get file name -> quoted_id()
filename(FileName) -->
  [quoted_id(FileName):_Pos].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expression parser
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adapted from: "The simple and powerful yfx operator precedence parser", E. L. Favero, 2007, Softw. Pract. Exper., 37:1451-1474, Wiley

% expr(-Expression)//
expr(E) -->
  % Start with the lowest precedence (1 is the maximum precedence)
  expr(E, 1200),
  !.

% expr(-Expression, +Precedence)//
% Op: Operator
% LP: Left Precedence, RP: Right Precedence, PP: Principal Precedence
expr(E, PP) --> % Integers/Fractionals/Floats/Strings
  value(V),
  !,
  rExpr(V, E, 0, PP).
expr(E, PP) --> % Parenthesized expressions
  [punct('('):_],
  expr(TI),
  [punct(')'):_],
  !,
  rExpr(TI, E, 0, PP).
expr(E, PP) --> % 0-Arity Functions
  [fn(N/0):_],
  !,
  rExpr(fn(N), E, 0, PP).
expr(E, PP) --> % Functions
  [fn(N/Ar):_,
   punct('('):_],
  fn_args(Ar, As),
  [punct(')'):_],
  !,
  {Fn =.. [N|As]},
  rExpr(fn(Fn), E, 0, PP).
expr(E, PP) --> % Prefix operators (no posfix in BASIC)
  [op(Op):_],
  {prefix(Op, P, RP),
   P =< PP,
   !},
   %true},
  expr(Arg, RP),
  {NE =.. [Op, Arg]},
  rExpr(NE, E, P, PP).
expr(E, PP) --> % Array element
  non_array_variable(N),
  [punct('('):_],
  cs_nat_exprs(Is), % Index
  [punct(')'):_],
  !,
  {Id =.. [N|Is]},
  rExpr(id(Id), E, 0, PP).
expr(E, PP) --> % Variables (numeric and strings)
  non_array_variable(Id),
  !,
  rExpr(id(Id), E, 0, PP).

% rExpr(+Expr, -E, +LeftP, +PP)//
rExpr(Expr, E, LeftP, PP) -->
  [op(Op):_],
  {infix(Op, P, LP, RP),
   P =< PP,
   LeftP =< LP,
   (redef(Op) -> true ; !)},
   %(redef(Op) -> true ; true)},
  expr(Arg2, RP),
  {NE =.. [Op, Expr, Arg2]},
  rExpr(NE, E, P, PP).
rExpr(Expr, E, LeftP, PP) -->
  [op(Op):_],
  {posfix(Op, P, LP),
   P =< PP,
   LeftP =< LP,
   (redef(Op) -> true ; !),
   NE =.. [Op, Expr]},
  rExpr(NE, E, P, PP).
rExpr(E, E, _, _) -->
  [].


% fn_args(+Arity, -As)//
% Function arguments
fn_args(Ar, H) -->
  expr(A1),
  ([punct(','):_]
   -> {H = [A1, A2|As],
       Ar1 is Ar - 1},
      fn_args(Ar1, [A2|As])
   ;  {H = [A1],
       Ar == 1}).


% redef(?Op)
% Redefined operators
redef(+).
redef(-).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxilliary predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% +Token # +Error//
% Set errors for expected tokens
Token # Error -->
  {terminal(Token),
   !},
  [CurrentToken:Position],
  {set_error('Syntax', Error, Position),
   Token = CurrentToken},
  !.
Goal # Error -->
  current_position(Position),
  {set_error('Syntax', Error, Position)},
  Goal.


% optional_op(-Op, true/false)//
% Optional op
optional_op(Op,true) -->
  [op(Op):_],
  !.
optional_op(_KW,false) -->
  [].

% optional_cmd(-Cmd)//
% Optional command
optional_cmd(Cmd) -->
  ([cmd(Cmd):_]
   -> !
   ;  []).

% optional_punct(-Punct)//
% optional punct
optional_punct(Punct) -->
  ([punct(Punct):_]
  -> !
  ;  []).

%opening parentesis and closing parentesis
opening_parentheses_star(N) -->
  opening_parentheses_star(0,N).
  
opening_parentheses_star(N,NN) -->
  [punct('('):_],
  {N1 is N+1},
  opening_parentheses_star(N1,NN).
opening_parentheses_star(N,N) -->
  [].
  
closing_parentheses_star(N) -->
  closing_parentheses_star(0,N),
  !. 
  
closing_parentheses_star(N,NN) -->
  [punct(')'):_],
  {N1 is N+1},
  closing_parentheses_star(N1,NN).
closing_parentheses_star(N,N) -->
  [].

% terminal(?Token)
terminal(id(_)).
terminal(quoted_id(_)).
terminal(cmd(_)).
terminal(op(_)).
terminal(fn(_)).
terminal(int(_)).
terminal(frac(_, _)).
terminal(float(_, _, _)).
terminal(str(_)).
terminal(punct(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(parser).

% Set of tests
% To test all of them:
%   ?- parser:test.

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.

%ISLstmt
test001 :-
  test(parser, lex_parse, 'test/test020.sql', 
  [show_tables,show_views,show_databases,describe(t)]).

%ISLstmt error
test002 :-
  test(parser, lex_parse, "DESCRIBE 2",
    failure(error('Syntax', 'table name', pos(1, 10)))).

test003 :-
  test(parser, lex_parse, "show t",
    failure(error('Syntax', 'TABLES, VIEWS or DATABASES', pos(1, 6)))).

%TMLstmt
test004 :-
  test(parser, lex_parse, 'test/test021.sql', 
    [commit,commit,rollback([]),rollback([]),rollback([sp1]),rollback([sp1]),savepoint(['sp2.ddb'])]).

%TMLstmt error
test005 :-
  test(parser, lex_parse, "ROLLBACK WORK TO point ""sp1""",
    failure(error('Syntax', 'SAVEPOINT', pos(1, 18)))).

test006 :-
  test(parser, lex_parse, "ROLLBACK TO point ""sp1""",
    failure(error('Syntax', 'SAVEPOINT', pos(1, 13)))).

test007 :-
  test(parser, lex_parse, "ROLLBACK WORK TO SAVEPOINT sp1",
    failure(error('Syntax', 'double quotes id (savepoint name)', pos(1, 28)))).

test008 :-
  test(parser, lex_parse, "ROLLBACK TO SAVEPOINT 'sp1'",
    failure(error('Syntax', 'double quotes id (savepoint name)', pos(1, 23)))).

%DMLstmt --select into
test009 :-
  test(parser, lex_parse, 'test/test016.sql',
    [insert_into(t1,[a1],select()),insert_into(t1,[a1],select()),insert_into(t1,[a1],select()),insert_into(t1,[a1],select()),insert_into(t1,[a1],[default]),insert_into(t3,[a3,b3,c3],[default,default,default]),insert_into(t2,[a2,b2],[[int(1),str('2')]]),insert_into(t2,[a2,b2],[[int(1),str('Ventas')],[int(2),str('Contabilidad')]]),insert_into(t3,[a3,b3,c3],[[str('1'),str(n1),str(d1)],[str('2'),str(n2),str(d2)]]),insert_into(t1,[a1],[[date('2000-060-01')]]),insert_into(t3,[a3,b3,c3],[[time('12:00:01'),frac(2,5),int(1)],[date('2012-01-01'),default,null]]),insert_into(t2,[a2,b2],[[time('12:00:01'),date('2000-0600-01')]]),insert_into(t2,[a2,b2],[[time('12:00:01'),date('2000-0600-01')]]),insert_into(t1,[a1],[[timestamp('2023-06-01 13:45:30')]]),insert_into(t1,[a1],[[int(1)]]),insert_into(t3,[a3,b3,c3],[[int(1),int(2),str(a)]]),insert_into(t2,[a3,b3,c3],[[int(1),int(2),str(a)]])]).

test024 :-
  test(parser, lex_parse, "insert into t3 values (1, '1')",
    failure(error('Semantic', 'Unmatching number of values => 2 (must be 3)', pos(1, 23)))).