:- module(parser,
          [ lex_parse/1,
            lex_parse/2,
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
  optional_semicolon,
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  dqlStmt(STs1/STs2),
  optional_semicolon,
  !,
  parse(STs2/STs).

statements(_) -->
  set_error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)').

% optional_semicolon//
% Zero or one semicolon
optional_semicolon -->
  semicolon
  -> !
  ;  [].

% semicolon//
% One semicolon
semicolon -->
  [punct(';'):_Pos].

/*statements(STs1/STs) -->
  ddlStmt(STs1/STs2)    # 'Statement',
  optional_semicolon,
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  islStmt(STs1/STs2)    # 'Statement',
  optional_semicolon,
  !,
  parse(STs2/STs).

statements(STs1/STs) -->
  tmlStmt(STs1/STs2)    # 'Statement',
  optional_semicolon,
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
  [punct('('):_]                      # 'Opening parenthesis',
  dqlStmt(SQLst)                      # 'todo',
  [punct(')'):_]                      # 'Closing parenthesis'.

% dmlStmt(-STs1/STs)//
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

dmlStmt([insert_into(TableName,Colnames, Vs)|STs]/STs) -->
  cmd(insert)                         # 'INSERT',
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  {(get_relation_arity(TableName,L) -> true ; true)},
  insert_values(L, Vs),
  {get_table_untyped_arguments(TableName,Colnames)},
  !.

% tablename(TableName)//
% get table name -> quoted_id() | id() | (id())
tablename(TableName) -->
  [quoted_id(TableName):_Pos].

tablename(TableName) -->
  [id(TableName):_Pos].

tablename(TableName) --> 
  optional_punct('['),
  [id(TableName):_Pos],
  optional_punct(']').

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

%insert_values(Arity)
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
  ub_DQL([select()|STs]/STs).


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
  punct(',')                      # 'comma',
  !,
  sql_ground_tuple_list(L,Ts).
remaining_sql_ground_tuple_list(_,[]) -->
  [].

sql_ground_tuple(L,Cs) -->
  current_position(Position),
  punct('(')                      # 'opening parenthesis',
  cs_expressions_def(Cs),
  punct(')')                      # 'closing parenthesis or comma',
  !,
  {length(Cs,TL),
    (L=TL -> true ;
      semantic_error('Unmatching number of values => ~w (must be ~w)' , [TL, L], Position),
      !, fail)}.

cs_expressions_def([E1, E2|Es]) -->
  expressions_def(E1),
  punct(',')                      # 'comma',
  !,
  cs_expressions_def([E2|Es]).
cs_expressions_def([E]) -->
  expressions_def(E).

% Default
expressions_def(default) -->
  cmd(default)                        # 'DEFAULT',
  !.

expressions_def(E) -->
  constant(E),
  !.

constant(E) -->
  value(E). %Number or String

% DATE 'String' % String in format '[BC] Int-Int-Int'
constant(DateStr) -->
  cmd(date)                           # 'DATE',
  current_position(Position),
  [str(Str):_]                        # 'string',
  !,
  { is_date_format(Str, Position) },
  {format(atom(DateStr), "DATE '~s'", [Str])}. % Verify Str is in format 'Int-Int-Int'

% TIME 'String' % String in format 'Int:Int:Int'
constant(Str) -->
  cmd(time)                           # 'TIME',
  current_position(Position),
  [str(Str):_]                        # 'string',
  !,
  { is_time_format(Str, Position) }. % Verify Str is in format 'Int:Int:Int'
  
  
% TIMESTAMP 'String' % String in format '[BC] Int-Int-Int Int:Int:Int'
constant(Str) -->
  cmd(timestamp)                      # 'TIMESTAMP',
  [str(Str):_]                        # 'string',
  !.

% Default
constant(null) -->
  cmd(null)                           # 'NULL',
  !.

constant(_) -->
  set_error('Syntax', 'Number, String, DATE String, TIME String, TIMESTAMP, NULL').

is_date_format(Str, Position) :-
  re_match("^\\d+-\\d+-\\d+$", Str) -> true; 
  semantic_error('Invalid DATE String format => must be \'Int-Int-Int\'', [], Position),
  !, fail.

is_time_format(Str, Position) :-
  re_match("^\\d+:\\d+:\\d+$", Str) -> true;
  semantic_error('Invalid TIME String format => must be \'Int:Int:Int\'', [], Position),
  !, fail.

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


% Operators
% prefix(?Op, ?P1, ?P2).
%   prefix(P, Op, P-1) :- op(P, fx, Op).
%   prefix(P, Op, P)   :- op(P, fy, Op).
prefix(+,   380, 380).
prefix(-,   380, 380).
prefix(not, 150, 150).

% infix(?Op, ?P1, ?P2, ?P3).
%   infix(P, Op, P-1, P-1)  :- op(P, xfx, Op).
infix(=,  700, 699, 699).
infix(>=, 700, 699, 699).
infix(<=, 700, 699, 699).
infix(<>, 700, 699, 699).
infix(>,  700, 699, 699).
infix(<,  700, 699, 699).

% infix(P, Op, P-1, P)    :- op(P, xfy, Op).
infix(^, 200, 199, 200).

% infix(P, Op, P,   P-1)  :- op(P, yfx, Op).
infix(or,  800, 800, 799).
infix(and, 800, 800, 799).
infix(+,   500, 500, 499).
infix(-,   500, 500, 499).
infix(*,   400, 400, 399).
infix(/,   400, 400, 399).

% posfix(?Op, ?P1, ?P2).
%   posfix(P, Op, P-1) :- op(P, xf, Op).
%   posfix(P, Op, P)   :- op(P, yf, Op).
% Example (though unsupported in Seiko Data 2000):
posfix(!, 280, 280).

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

test001 :-
  test(parser, parse, 
    [int(10):pos(1,1),cmd(for):pos(1,1),id(i):pos(1,5),op(=):pos(1,6),int(1):pos(1,7),cmd(to):pos(1,9),id(n):pos(1,12),cmd(step):pos(1,14),int(-1):pos(1,19)],
    [10-1:for(id(i),int(1),id(n),int(-1),true)]).

test002 :-
  test(parser, parse,
  [int(10):pos(1,1),cmd(let):pos(1,3),id(sin):pos(1,10),op(=):pos(1,13),fn(sin/1):pos(1,7),punct('('):pos(1,13),int(1):pos(1,1),punct(')'):pos(1,13)], 
  [10-1:let(id(sin),fn(sin(int(1))))]).

test003 :-
  test(parser, parse, [int(10):pos(1, 1), cmd(print):pos(1, 4), fn(sin/1):pos(1, 10), punct('('):pos(1, 13), int(1):pos(1, 14), punct(')'):pos(1, 15)], 
  [10-1:print([fn(sin(int(1)))])]).


