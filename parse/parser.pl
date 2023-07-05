:- module(parser,
          [ lex_parse/2,
            lex_parse/1,
            parse/2]).

:- use_module(misc).

:- use_module(test,
          [ test/4 ]).

:- use_module(error_,
          [ set_error/3,
            set_error/4,
            reset_error/0,
            process_error/0,
            set_error_with_parameter/4]).

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
  %forall(member(Tree, SyntaxTrees), pretty_print(Tree)).
  %forall(member(Tree, SyntaxTrees), writeln(Tree)).

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

pretty_print(Term) :-
  copy_term_nat(Term, Copy),
  numbervars(Copy, 0, _, [functor_name('$VAR'), singletons(true)]),
  format("~p,\n", [Copy]).

% statements(-STs1/STs)//
% statements ::=
%   DDLstmt[;] | DMLstmt[;] | DQLstmt[;] | ISLstmt[;] | TMLstmt[;]

statements(STs1/STs) -->
  ddlStmt(STs1/STs2),
  optional_punct(';'),
  !,
  parse(STs2/STs).

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

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DDL (Data Definition Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DDLstmt ::=
%   CREATE [OR REPLACE] TABLE CompleteConstrainedSchema
%   |
%   CREATE [OR REPLACE] TABLE TableName [(] LIKE TableName [)]
%   |
%   CREATE [OR REPLACE] TABLE TableName [(] AS DQLstmt [)]
%   |
%   CREATE [OR REPLACE] VIEW Schema AS DQLstmt
%   |
%   CREATE DATABASE DatabaseName   % Unsupported up to now
%   |
%   ALTER TABLE TableName [ADD|DROP] | [[COLUMN] Att | CONSTRAINT [ConstraintName] TableConstraint] 
%   |
%   ALTER TABLE TableName ALTER [COLUMN] Att [AttDefinition | SET [DATA] TYPE Type]
%   |
%   RENAME TABLE TableName TO TableName
%   |
%   RENAME VIEW ViewName TO ViewName
%   |
%   DROP TABLE DropTableClauses TableName{,TableName} DropTableClauses % Extended syntax following MySQL, SQL Server and others
%   |
%   DROP VIEW DropViewClauses ViewName DropViewClauses
%   |
%   DROP DATABASE [DatabaseName]
%   |
%   CompleteSchema := DQLstmt                    % Addition to support HR-SQL syntax 


% DDL Statements

% CREATE TABLE AS
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                          # 'TABLE or VIEW',
  create_view_schema(Schema)          # 'table schema',
  % syntax_check_redef(Schema), %  If attempting to redefine a datalog keyword, exception is thrown.
  opening_parentheses_star(N),
  cmd(as)                             # 'AS, or column name',
  (dqlStmt([(LSQLst,Schema)|STs]/STs) -> {true}; 
    set_error('Syntax', 'valid SQL DQL statement (SELECT, WITH or ASSUME)')),
  closing_parentheses_star(N),
  {atom_concat(CR,'_table_as',CRT),
   CRTSchema=..[CRT,(LSQLst,_AS),Schema]},
  !.

% CREATE TABLE LIKE
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                           # 'TABLE or VIEW',
  tablename(TableName)                 # 'table name',
  % syntax_check_redef(TableName), % If attempting to redefine a datalog keyword, exception is thrown.
  opening_parentheses_star(N),
  cmd(like)                            # 'AS, LIKE or column name',
  (tablename(ExistingTableName) -> {true}; 
    set_error('Syntax', 'valid SQL DDL statement (table name)')),
  closing_parentheses_star(N),
  {atom_concat(CR,'_table_like',CRT),
   CRTSchema=..[CRT,TableName,ExistingTableName]},
  !.

% CREATE TABLE
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                          # 'TABLE or VIEW',
  complete_constrained_typed_schema(Schema,Ctrs) # 'typed schema',
  % syntax_check_redef(Schema), % If attempting to redefine a datalog keyword, exception is thrown.
  {atom_concat(CR,'_table',CRT),
  CRTSchema=..[CRT,Schema,Ctrs]},
  !.

% CREATE VIEW
ddlStmt([CRVSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(view)                           # 'TABLE or VIEW',
  create_view_schema(Schema)          # 'view schema',
  % syntax_check_redef(Schema), % If attempting to redefine a datalog keyword, exception is thrown.
  cmd(as)                             # 'AS',
  opening_parentheses_star(N),
  dqlStmt([(LSQLst,Schema)|STs]/STs)  # 'valid SQL DQL statement (SELECT, WITH or ASSUME)',
  closing_parentheses_star(N),
  {atom_concat(CR,'_view',CRVF),
   CRVSchema =.. [CRVF,sql,(LSQLst,_AS),Schema]},
  !.

% CREATE DATABASE
ddlStmt([create_database(DBName)|STs]/STs) -->
  [cmd(create):_],
  cmd(database)                       # 'OR REPLACE, TABLE, VIEW or DATABASE',
  optional_database_name(DBName)      # 'database name',
  !.


% ALTER TABLE
ddlStmt([alter_table(TableName,AD,Element)|STs]/STs) -->
  [cmd(alter):_],         
  cmd(table)                          # 'TABLE',
  current_position(Position),
  tablename(TableName)                # 'table identifier',
  alter_table_alter_column(AD,TableName,Element),
  { exist_table(TableName) -> true; 
    set_error_with_parameter('Semantic', 'unknown_table(~w)', [TableName], Position)
  }.

% RENAME TABLE
ddlStmt([rename_table(TableName,NewTableName)|STs]/STs) -->
  [cmd(rename):_],
  cmd(table)                          # 'TABLE or VIEW',
  tablename(TableName)                # 'table name',
  cmd(to)                             # 'TO',
  tablename(NewTableName)             # 'table name',
  % syntax_check_redef(NewTableName),  % If attempting to redefine a datalog keyword, exception is thrown.
  !.

% RENAME VIEW
ddlStmt([rename_view(Viewname,NewViewname)|STs]/STs) -->
  [cmd(rename):_],
  cmd(view)                           # 'TABLE or VIEW',
  viewname(Viewname)                  # 'view identifier',
  cmd(to)                             # 'TO',
  viewname(NewViewname)               # 'view identifier',
  % syntax_check_redef(NewViewname),  % If attempting to redefine a datalog keyword, exception is thrown.
  !.

% DROP TABLE
ddlStmt([drop_table(Name,Clauses)|STs]/STs) -->
  [cmd(drop):_],
  cmd(table)                          # 'TABLE, VIEW or DATABASE',
  optional_drop_clauses(table,Clauses1),
  tablename(Name)                     # 'table name or optional drop table clauses(IF EXISTS, CASCADE or CASCADE CONSTRAINTS)',
  optional_drop_clauses(table,Clauses2),
  !,
  {append(Clauses1,Clauses2,Clauses)}.

% DROP VIEW
ddlStmt([drop_view(Name,Clauses)|STs]/STs) -->
  [cmd(drop):_],
  cmd(view)                           # 'TABLE, VIEW or DATABASE',
  optional_drop_clauses(view,Clauses1),
  viewname(Name)                      # 'view name or optional drop view clauses(IF EXISTS, CASCADE)',
  optional_drop_clauses(view,Clauses2),
  !,
  {append(Clauses1,Clauses2,Clauses)}.

% DROP SCHEMA
ddlStmt([drop_database(DBName)|STs]/STs) -->
  [cmd(drop):_],
  cmd(database)                       # 'TABLE, VIEW or DATABASE',
  optional_database_name(DBName)      # 'database name',
  !.

% HR-SQL CREATE VIEW syntax
ddlStmt([CRVSchema|STs]/STs) -->
  hrsql_typed_schema(Schema)          # 'typed schema', % No constraints
   % syntax_check_redef(Schema),  % If attempting to redefine a datalog keyword, exception is thrown.
  punct(':')                          # 'colon '':''',
  comparisonOp('=')                   # 'equals ''=''', 
  dqlStmt([(SQLst,Schema)|STs]/STs)   # 'select statement',
  %{CRVSchema = create_or_replace_view(hrsql,(SQLst,_AS),Schema)}, 
  { CRVSchema =.. [create_view,hrsql,(SQLst,_AS),Schema]},
  !.

% CREATE, CREATE OR REPLACE
create_or_replace(create) -->
  [cmd(create):_].

create_or_replace(create_or_replace) -->
  [cmd(create):_],
  op(or)                              # 'OR REPLACE, DATABASE',
  cmd(replace)                        # 'REPLACE'.


complete_constrained_typed_schema(Schema,Ctrs) -->
  sql_user_identifier(Name),
  punct('(')                          # 'opening parenthesis ''(''',
  constrained_typed_columns(Cs,CCtrs),
  optional_table_constraints(TCtrs),
  punct(')')                          # 'closing parenthesis '')''',
  {Schema =.. [Name|Cs],
    append(CCtrs,TCtrs,Ctrs)}.

constrained_typed_columns([C:T],Ctrs) --> 
  constrained_typed_column(C:T,Ctrs).
constrained_typed_columns([C:T|CTs],Ctrs) -->
  constrained_typed_column(C:T,CCtrs),
  punct(',')                          # 'comma or column constraints',
  !,
  constrained_typed_columns(CTs,RCtrs),
  {append(CCtrs,RCtrs,Ctrs)}.

constrained_typed_column(C:T,Ctrs) --> 
  typed_column(C:T),
  column_constraint_definitions(C,Ctrs) # 'column constraints'.
constrained_typed_column(C:T,[true]) --> 
  typed_column(C:T).

typed_column(C:T) -->
  colname(C)                          # 'AS, LIKE or column identifier',
  sql_type(T).
  
column_constraint_definitions(C,Ctrs) -->
  optional_constraint_name(_CtrName),
  column_constraint(C,Ctr),
  {Ctr==true -> Ctrs=[] ; Ctrs=[Ctr]}. % Some "constraints" do not constrain, as NULL.
column_constraint_definitions(C,[Ctr|Ctrs]) -->
  optional_constraint_name(_CtrName),
  column_constraint(C,Ctr),
  column_constraint_definitions(C,Ctrs).

optional_constraint_name(CtrName) -->
  [cmd(constraint):_],
  sql_user_identifier(CtrName).
optional_constraint_name('$void') -->
  [].

referenced_column(_FC,TableName,TC) -->
  tablename(TableName)                # 'table name',
  punct('(')                          # 'opening parenthesis ''(''',
  !,
  untyped_column(TC)                  # 'a column name',
  punct(')')                          # 'closing parenthesis '')'''.
referenced_column(C,TableName,C) -->
  tablename(TableName)                # 'table name',
  !.

optional_referential_triggered_action(on(Event,Action)) -->
  cmd(on)                             # 'ON',
  triggered_event(Event)              # 'DELETE or UPDATE',
  referential_action(Action)          # 'CASCADE, SET NULL, SET DEFAULT, RESTRICT or NO ACTION',
  !.
optional_referential_triggered_action('$void') -->
  [].

triggered_event(delete) -->
  [cmd(delete):_].
triggered_event(update) -->
  [cmd(update):_].
  
referential_action(cascade) -->
  [cmd(cascade):_].
referential_action(set_null) -->
  [cmd(set):_],
  cmd(null)                           # 'NULL'.
referential_action(set_default) -->
  [cmd(set):_],
  cmd(default)                        # 'DEFAULT'.
referential_action(restrict) -->
  [cmd(restrict):_].
referential_action(no_action) -->
  [cmd(no):_],
  cmd(action)                         # 'ACTION'.

check_constraint(fd(Ls,Rs)) -->
  column_tuple(Rs),
  cmd(determined)                     # 'DETERMINED',
  cmd(by)                             # 'BY',
  column_tuple(Ls)                    # 'a column sequence between parentheses'.
check_constraint(sql_check_constraint(WhereCondition)) -->
  where_condition(WhereCondition).

check_constraint(_) -->
  set_error('Syntax', 'where condition or a list of columns followed by ''DETERMINED BY'' and another list of columns').

optional_table_constraints(Ctrs) -->
  punct(',')                          # 'comma', 
  table_constraints(Ctrs).
optional_table_constraints([]) -->
  [].

table_constraints([Ctr]) -->
  optional_constraint_name(_CtrName),
  table_constraint(Ctr).  
table_constraints([Ctr|Ctrs]) -->
  optional_constraint_name(_CtrName),
  table_constraint(Ctr),
  punct(',')                          # 'comma', 
  table_constraints(Ctrs).  

create_view_schema(Schema) -->
  complete_untyped_schema(Schema),
  !.
create_view_schema(Name) -->
  sql_user_identifier(Name).

complete_untyped_schema(Schema) -->
  sql_user_identifier(Name),
  punct('(')                          # 'opening parenthesis ''(''',
  untyped_columns(Cs)                 # 'column sequence separated by commas',
  punct(')')                          # 'closing parenthesis '')''',
  {Schema =.. [Name|Cs]}.

optional_database_name(DBName) -->
  sql_user_identifier(DBName).
optional_database_name('$des') -->
  [].


% Parsing alter_table_alter_column options
alter_table_alter_column(AD,_TableName,Element) -->
  add_or_drop(AD)                     # 'ALTER, ADD or DROP',
  add_drop_table_element(AD,Element)  # 'COLUMN or CONSTRAINT',
  !.
alter_table_alter_column(alter,TableName,Element) -->
  cmd(alter)                          # 'ALTER, ADD or DROP',
  optional_cmd(column),
  current_position(Position),
  alter_column(Element,Column),
  !,
  {
    ((current_db(ConnectionName),
    exist_att(ConnectionName,TableName,TableName,Column)) 
    -> true; 
      set_error_with_parameter('Semantic', 'unknown_column(~w, ~w)', [TableName, Column], Position))
  }.

add_or_drop(add) -->
  [cmd(add):_].
add_or_drop(drop) -->
  [cmd(drop):_].

add_drop_table_element(AD,Column) -->
  optional_cmd(column),
  add_drop_column(AD,Column).
add_drop_table_element(_AD,ctr(Constraint)) -->
  cmd(constraint)                     # 'CONSTRAINT',
  optional_sql_ctr_name(_CtrName),
  table_constraint(Constraint)        # 'table constraint'.

add_drop_column(add,column(C:T,Ctrs)) -->
  constrained_typed_column(C:T,Ctrs)  # 'valid table column definition'.
add_drop_column(drop,column(Colname)) -->
  colname(Colname)                    # 'a column name'.


optional_sql_ctr_name(CtrName) -->
  sql_user_identifier(CtrName).
optional_sql_ctr_name('$void') -->
  % TODO: Generate a unique system identifier
  [].

alter_column(column(C:T,Ctrs),C) -->  
  constrained_typed_column(C:T,Ctrs)  # 'valid table column definition'.
alter_column(column(C:T),C) -->
  sql_user_identifier(C),
  cmd(set)                            # 'SET',
  optional_cmd(data),
  cmd(type)                           # 'DATA TYPE or TYPE',
  sql_type(T)                         # 'valid type name'.


optional_drop_clauses(RelType,Clauses) -->
  optional_drop_clauses(RelType,[],Clauses).
  
optional_drop_clauses(RelType,ClausesIn,ClausesOut) -->
  [cmd(if):_],
  cmd(exists)                         # 'EXISTS',
  optional_drop_clauses(RelType,[if_exists|ClausesIn],ClausesOut).
optional_drop_clauses(table,ClausesIn,ClausesOut) -->
  % This option only applies to tables
  [cmd(cascade):_],
  cmd(constraints)                    # 'CONSTRAINTS',
  optional_drop_clauses(table,[cascade_constraints|ClausesIn],ClausesOut). % Default option. Maybe a later version will change this default
optional_drop_clauses(RelType,ClausesIn,ClausesOut) -->
  [cmd(cascade):_],
  optional_drop_clauses(RelType,[cascade|ClausesIn],ClausesOut).
optional_drop_clauses(RelType,ClausesIn,ClausesOut) -->
  [cmd(restrict):_],
  optional_drop_clauses(RelType,ClausesIn,ClausesOut). % Default behaviour
optional_drop_clauses(_RelType,Clauses,Clauses) -->
  [].

hrsql_typed_schema(Schema) -->
  relname(Name)                       # 'relation name',
  [punct('('):_],
  hrsql_typed_columns(Cs),
  punct(')')                          # 'closing parenthesis '')''',
  {Schema =.. [Name|Cs]}.

hrsql_typed_columns([C:T]) --> 
  hrsql_typed_column(C:T).
hrsql_typed_columns([C:T|CTs]) -->
  hrsql_typed_column(C:T),
  punct(',')                          # 'comma',
  hrsql_typed_columns(CTs).

hrsql_typed_column(C:T) -->
  colname(C)                          # 'column identifier',
  sql_type(T).

exist_table(TableName) :-
  exist_table(TableName,_Arity).
  
exist_table(TableName,Arity) :-
  current_db('$des'),
  !,
  (des_table_exists(TableName,Arity) -> true;
    (!, fail)).
    %my_raise_exception(unknown_table(TableName),syntax(''),[])).

exist_table(TableName,_Arity) :-
  (my_odbc_exists_table(TableName) -> true;
    (!, fail)).
    %my_raise_exception(unknown_table(TableName),syntax(''),[])).
  
des_table_exists(TableName) :-
  des_table_exists(TableName,_Arity).

des_table_exists(TableName,Arity) :-
  my_table('$des',TableName,Arity),
  \+ my_view('$des',TableName,_A,_S,_La,_D,_ODLIds,_L,_SC).

/*exist_att(TableName,VarName,Att):-
  current_db(ConnectionName),
  exist_att(ConnectionName,TableName,VarName,Att).*/
  
exist_att(ConnectionName,TableName,_VarName,Att) :-
  (my_attribute(ConnectionName,_Pos,TableName,Att,_Type) -> true;
    (!, fail)).
    %my_raise_exception(unknown_column(VarName,Att),syntax(''),[])).

/*
des_relation_exists(Relationname) :-
  des_relation_exists(Relationname,_Arity).

des_relation_exists(Relationname,Arity) :-
  my_table('$des',Relationname,Arity).
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DQLstmt ::=
%   (DQLstmt) 
%   |
%   UBSQL

% UBSQL ::= 
%   SELECTstmt
%   |
%   DQLstmt UNION [ALL] DQLstmt
%   |
%   DQLstmt EXCEPT [ALL] DQLstmt
%   |
%   DQLstmt MINUS [ALL] DQLstmt
%   |
%   DQLstmt INTERSECT [ALL] DQLstmt
%   |
%   WITH LocalViewDefinition {,LocalViewDefinition} DQLstmt
%   |
%   ASSUME LocalAssumption {,LocalAssumption} DQLstmt  % Not in the standard

dqlStmt([STs1|STs]/STs) --> 
  ub_DQL([STs1|STs]/STs).              /*# 'valid SQL DQL statement'*/
  
dqlStmt([STs1|STs]/STs) --> 
  b_DQL([STs1|STs]/STs).               /*# 'valid SQL DQL statement'*/

b_DQL([STs1|STs]/STs) -->
  [punct('('):_],
  dqlStmt([STs1|STs]/STs),             /*# 'valid SQL DQL statement'*/
  punct(')')                          # 'closing parenthesis '')'''.
% ASSUME
ub_DQL([(with(SQLst,SQLsts),_AS)|STs]/STs) -->
  [cmd(assume):_],
  !,
  assume_list(SQLsts)                 # 'list of assumptions',
  dqlStmt([SQLst|STs]/STs)            # 'SELECT statement',
 % {allowed_with_schemas(SQLsts)},
  !.
% WITH
ub_DQL([(with(SQLst,SQLsts),_AS)|STs]/STs) -->
  [cmd(with):_],
  !,
  local_view_definition_list(SQLsts)  # 'list of temporary view definitions',
  dqlStmt([SQLst|STs]/STs)            # 'SELECT statement',
 % {allowed_with_schemas(SQLsts)},
  !.
% UNION
ub_DQL([(union(D,R1,R2),_AS)|STs]/STs) -->
  b_DQL([R1|STs]/STs),
  union_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N),
  !.
ub_DQL([(union(D,R1,R2),_AS)|STs]/STs) -->
  select_DQL([R1|STs]/STs),
  union_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N),
  !.
% EXCEPT
ub_DQL([(except(D,R1,R2),_AS)|STs]/STs) -->
  b_DQL([R1|STs]/STs),
  except_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N),
  !.
ub_DQL([(except(D,R1,R2),_AS)|STs]/STs) -->
  select_DQL([R1|STs]/STs),
  except_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N),
  !.
% INTERSECT
ub_DQL([(intersect(D,R1,R2),_AS)|STs]/STs) -->
  b_DQL([R1|STs]/STs),
  intersect_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N),
  !.
ub_DQL([(intersect(D,R1,R2),_AS)|STs]/STs) -->
  select_DQL([R1|STs]/STs),
  intersect_stmt(D),
  opening_parentheses_star(N),
  dqlStmt([R2|STs]/STs)               # 'SELECT statement',
  closing_parentheses_star(N), 
  !.

% SELECT
ub_DQL([STs1|STs]/STs) --> 
  select_DQL([STs1|STs]/STs).

% Atoms and clauses assumed in ASSUME
assume_list([V]) -->
  assume([V|STs]/STs).
assume_list([V|Vs]) -->
  assume([V|STs]/STs),
  punct(',')                          # 'comma', 
  assume_list(Vs).

assume([(SQLst,Schema)|STs]/STs) -->
  dqlStmt([(SQLst,Schema)|STs]/STs),
  [cmd(in):_],
  assume_schema(Schema).
assume([not((SQLst,Schema))|STs]/STs) -->
  assume_not_in([(SQLst,Schema)|STs]/STs).
  
assume_not_in([(SQLst,Schema)|STs]/STs) -->
  dqlStmt([(SQLst,Schema)|STs]/STs),
  [op(not):_],
  cmd(in)                             # 'IN after NOT',
  assume_schema(Schema).

assume_schema(Schema) -->
  complete_untyped_schema(Schema),
  !.
assume_schema(Schema) -->
  sql_user_identifier(Name),
  {get_table_untyped_arguments(Name,Colnames),
    length(Colnames,L),
    length(Types,L),
    my_zipWith(':',Colnames,Types,TypedCols),
    Schema=..[Name|TypedCols]}.

% Local view definitions in WITH
local_view_definition_list([V]) -->
  local_view_definition([V|STs]/STs).
local_view_definition_list([V|Vs]) -->
  local_view_definition([V|STs]/STs),
  punct(',')                          # 'comma', 
  local_view_definition_list(Vs).

local_view_definition([(SQLst,Schema)|STs]/STs) -->
  optional_cmd(recursive),
  assume_schema(Schema)               # 'schema',
  cmd(as)                             # 'AS',
  opening_parentheses_star(N),
  dqlStmt([(SQLst,Schema)|STs]/STs),
  closing_parentheses_star(N).
local_view_definition([(SQLst,Schema)|STs]/STs) -->
  assume_not_in([(SQLst,Schema)|STs]/STs).

union_stmt(DistinctAll) -->
  [cmd(union):_],
  distinct_all(DistinctAll).

except_stmt(DistinctAll) -->
  set_difference_kw, % EXCEPT or MINUS
  distinct_all(DistinctAll).

intersect_stmt(DistinctAll) -->
  [cmd(intersect):_],
  distinct_all(DistinctAll).

distinct_all(all) -->
  [cmd(all):_].
distinct_all(distinct) -->
  [cmd(distinct):_].
distinct_all(distinct) -->
  [].

set_difference_kw -->
  [cmd(except):_].
set_difference_kw -->
  [cmd(minus):_].

% SELECT 
select_DQL([(select(DistinctAll,TopN,Offset,ProjList,TargetList,
               from(Relations),
               where(WhereCondition),
               group_by(GroupList),
               having(HavingCondition), 
               order_by(OrderArgs,OrderSpecs)),_AS)|STs]/STs) -->
  select_stmt(DistinctAll,TopN),
  projection_list(ProjList),
  target_clause(TargetList),
  cmd(from)                           # 'FROM clause',
  opening_parentheses_star(N),
  {!}, % 23-01-2021
  relations(Relations),
  where_clause_with_cut(WhereCondition),
  group_by_clause(GroupList),
  having_clause(HavingCondition),
  order_by_clause(OrderArgs,OrderSpecs),
  optional_offset_limit(Offset),
  optional_fetch_first(TopN),
  closing_parentheses_star(N),
  {set_topN_default(TopN)},
  !.

% FROM-less SELECT
select_DQL([(select(DistinctAll,TopN,no_offset,ProjList,TargetList,
               from([(dual,_Ren)]),
               where(true),
               group_by([]),
               having(true),
               order_by([],[])),_AS)|STs]/STs) -->
  select_stmt(DistinctAll,TopN),
  projection_list(ProjList),
  {set_topN_default(TopN)},
  target_clause(TargetList),
  !.

select_stmt(DistinctAll,TopN) -->
  [cmd(select):_],                    %# 'SELECT'.
  optional_select_modifiers(DistinctAll,TopN).

optional_select_modifiers(DistinctAll,TopN) -->
  select_distinct_all(DistinctAll),
  select_top_n(TopN),
  !.
optional_select_modifiers(DistinctAll,TopN) -->
  select_top_n(TopN),
  select_distinct_all(DistinctAll),
  !.
optional_select_modifiers(DistinctAll,top(_N)) -->
  select_distinct_all(DistinctAll),
  !.
optional_select_modifiers(all,TopN) -->
  select_top_n(TopN),
  !.
optional_select_modifiers(all,top(_N)) -->
  !.

select_top_n(top(N)) -->
  [cmd(top):_],
  top_argument(N)                     # 'an integer expression'.

top_argument(expr(N,_AS,number(int))) -->
  integer(N).
top_argument(N) -->
  [punct('('):_],
  sql_proj_expression(N,_),
  punct(')')                          # 'closing parenthesis '')'''.

set_topN_default(top(all)) :-
  !.
set_topN_default(top(_N)).

select_distinct_all(all) -->
  [cmd(all):_].
select_distinct_all(distinct) -->
  [cmd(distinct):_].

projection_list(*) --> 
  [op('*'):_].
projection_list([A|As]) --> 
  p_ren_argument(A),
  punct(',')                          # 'comma', 
%  {!},  % It could be part of a WITH definition, so no cut is allowed
  projection_list(As).
projection_list([A]) --> 
  p_ren_argument(A).
projection_list(_) -->
  set_error('Syntax', 'SELECT list').

p_ren_argument(A) --> 
  ren_argument(A).
p_ren_argument(A) --> 
  sql_argument(A,_AS).

ren_argument(Arg) -->
  sql_argument(Arg,AS),
  optional_cmd(as), 
  sql_user_identifier(AS).

sql_argument((R,(*)),'$') -->  % Cannot be renamed
  relname(R)                          # 'relation name',
  punct('.')                          # 'dot ''.''',
  op('*')                             # 'a star ''*'''.
sql_argument(E,AS) -->
  sql_proj_expression(E,AS).

target_clause(TargetList) -->
  [cmd(into):_],
  symbol_list(TargetList)             # 'a comma-separated list of variable names'.
target_clause([]) -->
  [].

symbol_list([Target]) -->
  user_symbol(Target).
symbol_list([T1,T2|Targets]) -->
  user_symbol(T1),
  punct(',')                          # 'comma',
  symbol_list([T2|Targets]).

% get user_symbol -> lowercase[char]+id | str('id')
user_symbol(Name) --> 
  [str(Name):_Pos].

user_symbol(Name) --> 
  [id_lc_start(Name):_Pos].

relations([R|Rs]) --> 
  p_ren_relation(R), 
  remaining_relations(Rs).

relations(_) -->
  set_error('Syntax', 'a valid relation').

remaining_relations(Rs) -->
  [punct(','):_],
  relations(Rs).
remaining_relations([]) -->
  [].


p_ren_relation(R) --> 
  ren_relation(R).
p_ren_relation(R) --> 
  relation(R).  



ren_relation((R,[J|Args])) -->
  opening_parentheses_star(N),
  relation((R,[J|Args])),
  closing_parentheses_star(N),
  optional_cmd(as),
  sql_user_identifier(I),
  {ignore_autorenaming(R,I,J)}.

ignore_autorenaming(I,I,_) :- % Ignore user renaming
  !.
ignore_autorenaming(_,I,I).   % Use user renaming


relation(R) --> 
  opening_parentheses_star(N),
  ub_relation(R),
  closing_parentheses_star(N).

ub_relation((R,_AS)) --> 
  division_relation(R).
ub_relation((R,_AS)) --> 
  join_relation(R).
ub_relation(R) --> 
  non_join_relation(R).


%non JOINs
non_join_relation((T,_)) -->
  sql_user_identifier(T).

non_join_relation((R,AS)) -->
  dqlStmt([(R,AS)|STs]/STs).


peek_tokens(_, [Token:_|_], _) :-
  (Token = punct(_)), 
  !, 
  fail.

peek_tokens([], Rest, Rest).
peek_tokens([Token:Position|RestTokens], [Token:Position|RestInput], Output) :-
  peek_tokens(RestTokens, RestInput, Output).

% DIVISION
division_relation(DR,SIn,SOut) :-
  look_ahead_division_op(SIn,SOut1),
  list_diff(SIn,SOut1,SDiff),
  p_ren_leading_relation(LR,SDiff,[]),
  remainder_division_relation(LR,DR,SOut1,SOut).

look_ahead_division_op(SIn,SOut) :-
  peek_tokens(_Tokens,SIn,SOut),
  division_operator(SOut,_SOut2).

division_operator([cmd(division):_|_], _).

p_ren_leading_relation(R) -->
  opening_parentheses_star(N),
  p_ren_relation(R),
  closing_parentheses_star(N).

remainder_division_relation(LR,JR) -->
  [cmd(division):_],
  opening_parentheses_star(N),
  p_ren_relation(RR),
  closing_parentheses_star(N),
  {JR = division(LR,RR)}.


% JOINs
join_relation(JR,SIn,SOut) :-
  look_ahead_join_op(JOp,SIn,SOut1),
  list_diff(SIn,SOut1,SDiff),
  p_ren_leading_relation(LR,SDiff,[]),
  remainder_join_relation(LR,JOp,JR,SOut1,SOut).

look_ahead_join_op(JOp,SIn,SOut) :-
  peek_tokens(_Tokens, SIn, SOut),
  natural_token(SOut, SOut2),
  join_operator(JOp,SOut2,_SOut3).
look_ahead_join_op(JOp,SIn,SOut) :-
  peek_tokens(_Tokens, SIn, SOut),
  join_operator(JOp,SOut,_SOut2).

natural_token([cmd(natural):_|_], _).

% NATURAL
remainder_join_relation(LR,JOp,JR) -->
  [cmd(natural):_],
  join_operator(JOp),
  opening_parentheses_star(N),
  !,
  p_ren_relation(RR),
  closing_parentheses_star(N),
  {JR =.. [JOp,LR,RR,equijoin(natural)]}.
  % ON
remainder_join_relation(LR,JOp,JR) -->
  join_operator(JOp),
  opening_parentheses_star(N),
  !,
  p_ren_relation(RR),
  closing_parentheses_star(N),
  optional_join_condition(Cond),
  {JR =.. [JOp,LR,RR,Cond]}.

join_operator(inner_join) -->
  optional_cmd(inner),
  [cmd(join):_].
join_operator(Outer_join) -->
  outer_kind(Outer_join),
  optional_cmd(outer),
  [cmd(join):_].

outer_kind(left_join) -->
  [cmd(left):_].
outer_kind(right_join) -->
  [cmd(right):_].
outer_kind(full_join) -->
  [cmd(full):_].


optional_join_condition(Cond) -->
  join_condition(Cond).
optional_join_condition(true) -->
  [].

join_condition(Condition) -->
  [cmd(on):_],
  opening_parentheses_star(N),
  {!}, % 23-01-2021
  on_condition(Condition),
  closing_parentheses_star(N).

join_condition(equijoin(Atts)) -->
  [cmd(using):_],
  [punct('('):_],
  {!},
  column_list(Atts)                   # 'a column sequence between parentheses',
  punct(')')                          # 'comma or closing parenthesis '')'''.
join_condition(true) -->
  [].

on_condition(C) --> 
  sql_condition(C)                    # 'valid ON condition'.

where_clause_with_cut(WhereCondition) -->
  [cmd(where):_],
  opening_parentheses_star(N),
  !,
  where_condition(WhereCondition),
  closing_parentheses_star(N).
where_clause_with_cut(true) -->
  [].

where_condition(C) --> 
  sql_condition(C)                    # 'valid WHERE condition'.

group_by_clause(GroupList) -->
  [cmd(group):_],
  cmd(by)                             # 'BY',
  opening_parentheses_star(N),
  {!}, % 23-01-2021
  sql_proj_expression_sequence(GroupList),
  closing_parentheses_star(N).
group_by_clause([]) -->
  [].

having_clause(HavingCondition) -->
  [cmd(having):_],
  opening_parentheses_star(N),
  {!}, % 23-01-2021
  sql_having_condition(HavingCondition),
  closing_parentheses_star(N).
having_clause(true) -->
  [].

sql_having_condition(C) --> 
  sql_condition(C)                    # 'valid HAVING condition'.

order_by_clause(OrderArgs,OrderSpecs) -->
  [cmd(order):_],
  cmd(by)                             # 'BY',
  opening_parentheses_star(N),
  {!}, % 23-01-2021
  order_list(OrderArgs,OrderSpecs)    # 'valid ORDER BY criteria',
  closing_parentheses_star(N).
order_by_clause([],[]) -->
  [].
  
order_list([C,C2|Cs],[O,O2|Os]) -->
  expr_order(C,O),
  punct(',')                          # 'comma',
  order_list([C2|Cs],[O2|Os]).
order_list([C],[O]) -->
  expr_order(C,O).

expr_order(C,O) -->
  sql_proj_expression(C,_T),
  optional_order(O).

optional_order(O) -->
  order(O).
optional_order(a) -->
  [].

order(d) -->
  cmd(descending)                     # 'DESCENDING'.
order(d) -->
  cmd(desc)                           # 'DESC'.
order(a) -->
  cmd(ascending)                      # 'ASCENDING'.
order(a) -->
  cmd(asc)                            # 'ASC'.


optional_offset_limit(offset(Offset,Limit)) -->
  [cmd(offset):_],
  sql_proj_expression(Offset,_)       # 'an expression',
  [cmd(limit):_],
  {!},
  sql_proj_expression(Limit,_)        # 'an expression'.
optional_offset_limit(offset(Offset)) -->
  [cmd(offset):_],
  {!},
  sql_proj_expression(Offset,_T)      # 'an expression'.
optional_offset_limit(no_offset) -->
  [].


optional_fetch_first(top(Limit)) -->
  [cmd(limit):_],
  {!},
  sql_proj_expression(Limit,_)        # 'an integer expression'.
optional_fetch_first(top(N)) -->
  %current_position(Position),
  [cmd(fetch):_],
  {!},
  cmd(first)                          # 'FIRST',
  sql_proj_expression(N1,_)           # 'an integer expression',
  cmd(rows)                           # 'ROWS',
  cmd(only)                           # 'ONLY',
  {nonvar(N) ->
    set_error_with_parameter('Semantic', 'Only one TOP/LIMIT/FETCH specification is allowed' , [], pos(void,void));
    %my_raise_exception(generic,syntax([]),[])
    N=N1}.
optional_fetch_first(top(_N)) -->
  [].



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

% INSERT INTO Table(Columns) [VALUES(...) | selectStm]
dmlStmt([insert_into(TableName,Colnames,Vs)|STs]/STs) -->
  [cmd(insert):_],
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  current_position(Position),
  punct('(')                          # 'opening parenthesis ''('' or DEFAULT',
  column_name_list(Colnames)          # 'a sequence of columns between parentheses',
  punct(')')                          # 'comma or closing parenthesis '')''',
  {my_remove_duplicates(Colnames,Colnames) -> true ;
  set_error_with_parameter('Semantic', 'Column names must be different in ~w' , [Colnames], Position)},
  {length(Colnames,L)},
  insert_values_sql(L,Vs)             # 'VALUES, select statement, or DEFAULT VALUES',
  !.

% INSERT INTO Table [VALUES(...) | selectStm]
dmlStmt([insert_into(TableName,Colnames,Vs)|STs]/STs) -->
  [cmd(insert):_],
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  {(get_relation_arity(TableName,L) -> true ; true)},
  insert_values_sql(L, Vs)            # 'VALUES, select statement, or DEFAULT VALUES',
  {get_table_untyped_arguments(TableName,Colnames)},
  !.

% DELETE FROM ... WHERE 
dmlStmt([delete_from(Table,WhereCondition)|STs]/STs) -->
  [cmd(delete):_],
  cmd(from)                           # 'FROM',
  p_ren_tablename(Table)              # 'table name',
  where_clause(WhereCondition),
  !.

% UPDATE ... SET ... [WHERE ]
dmlStmt([update(Table,Assignments,WhereCondition)|STs]/STs) -->
  [cmd(update):_],
  p_ren_tablename(Table)              # 'table name',
  cmd(set)                            # 'SET',
  update_assignments(Assignments)     # 'sequence of column assignments Col=Expr',
  where_clause(WhereCondition),
  !.
/*dmlStmt([update(Table,Assignments,true)|STs]/STs) -->
  [cmd(update):_],
  p_ren_tablename(Table)              # 'table name',
  cmd(set)                            # 'SET',
  update_assignments(Assignments)     # 'sequence of column assignments Col=Expr',
  !.*/

%insert_values_sql(Arity, Values)
insert_values_sql(L,[Vs]) -->
  [cmd(default):_],
  cmd(values)                         # 'VALUES',
  {!,
  length(Vs,L),
  my_map_1('='(default),Vs)}.

insert_values_sql(L, Ts) -->
  [cmd(values):_],
  !,
  sql_ground_tuple_list(L,Ts)         # 'a sequence of constants between parentheses'.

insert_values_sql(_L,SQLst) -->
  dqlStmt([SQLst|STs]/STs).

%update_assignments(assignments)
update_assignments([Column,Expression|Assignments]) -->
  update_assignment(Column,Expression),
  [punct(','):_],
  update_assignments(Assignments)     # 'sequence of column assignments Col=Expr'.
update_assignments([Column,Expression]) -->
  update_assignment(Column,Expression).


update_assignment(expr(ColumnName,_,string),Expression) -->
  column(attr(_T,ColumnName,_AS)),
  comparisonOp('=')                   # 'equal ''=''', 
  sql_proj_expression(Expression,_Type) # 'an expression'.


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
  punct('(')                          # 'opening parenthesis ''(''',
  (sql_expressions(Cs); sql_constants(Cs)),
  punct(')')                          # 'comma or closing parenthesis '')''',
  !,
  {length(Cs,TL),
    (L=TL -> true ;
    set_error_with_parameter('Semantic', 'Unmatching number of values => ~w (must be ~w)' , [TL, L], Position))}.

where_clause(WhereCondition) -->
  [cmd(where):_],
  opening_parentheses_star(N),
  where_condition(WhereCondition)     # 'WHERE condition',
  closing_parentheses_star(N).
where_clause(true) -->
  [].

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
  [cmd(show):_],
  cmd(tables)                         # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([show_views|STs]/STs) -->
  [cmd(show):_],
  cmd(views)                          # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([show_databases|STs]/STs) -->
  [cmd(show):_],
  cmd(databases)                      # 'TABLES, VIEWS or DATABASES',
  !. 

islStmt([describe(Name)|STs]/STs) -->
  [cmd(describe):_],
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
  [cmd(commit):_],
  optional_cmd(work),
  !. 

% ROLLBACK TO SAVEPOINT
tmlStmt([rollback([SP])|STs]/STs) -->
  [cmd(rollback):_],
  optional_cmd(work),
  cmd(to)                             # 'TO',
  cmd(savepoint)                      # 'SAVEPOINT',
  filename(SP)                        # 'double quotes id (savepoint name)',
  !. 

% ROLLBACK
tmlStmt([rollback([])|STs]/STs) -->
  [cmd(rollback):_],
  optional_cmd(work),
  !. 

% SAVEPOINT
tmlStmt([savepoint([SP])|STs]/STs) -->
  [cmd(savepoint):_],
  !,
  filename(FileName)                  # 'string (savepoint name)',
  {atom_concat(FileName,'.ddb',SP)}.

% filename(FileName)//
% get file name -> quoted_id()
filename(FileName) -->
  [quoted_id(FileName):_Pos].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% char(n)
sql_type(string(char(N))) -->
  sql_character_type_id,
  [punct('('):_],
  !,
  positive_integer(N)                 # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
% char  
sql_type(string(char(1))) -->
  sql_character_type_id.
% varchar(n)
sql_type(string(varchar(N))) -->
  sql_varchar_type_id,
  [punct('('):_],
  !,
  positive_integer(N)                 # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.

sql_type(string(varchar)) -->
  cmd(varchar)                        # 'VARCHAR'. 
sql_type(string(varchar)) -->
  cmd(string)                         # 'STRING'.
sql_type(string(varchar)) -->
  cmd(text)                           # 'TEXT'. 

% integer
sql_type(number(integer)) -->
  sql_integer_type_id,
  optional_integer_range(_).
sql_type(number(integer)) -->
  sql_numeric_type_id,
  optional_integer_range(_).
% real and float
sql_type(number(float)) -->
  [cmd(float):_], 
  punct('(')                          # 'opening parenthesis ''(''',
  positive_integer(_Int)              # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
sql_type(number(float)) -->
  sql_float_type_id.
sql_type(number(float)) -->
  sql_numeric_type_id,
  punct('(')                          # 'opening parenthesis 3''(''',
  positive_integer(_Int)              # 'a positive integer',
  punct(',')                          # 'comma',
  positive_integer(_Frac)             # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.

sql_type(datetime(datetime)) -->
  cmd(datetime)                       # 'DATETIME'.
sql_type(datetime(datetime)) -->
  cmd(timestamp)                      # 'TIMESTAMP'.
sql_type(datetime(date)) -->
  cmd(date)                           # 'DATE'.
sql_type(datetime(time)) -->
  cmd(time)                           # 'TIME'.

sql_type(_) -->
  set_error('Syntax', 'valid type').

sql_float_type_id -->
  [cmd(real):_].
sql_float_type_id -->
  [cmd(float):_].
  
sql_varchar_type_id -->
  [cmd(varchar2):_].
sql_varchar_type_id -->
  [cmd(varchar):_].
sql_varchar_type_id -->
  [cmd(text):_].
  
sql_character_type_id -->
  [cmd(character):_].
sql_character_type_id -->
  [cmd(char):_].
  
sql_integer_type_id -->
  [cmd(integer):_].
sql_integer_type_id -->
  [cmd(int):_].
sql_integer_type_id -->
  [cmd(smallint):_].
  
sql_numeric_type_id -->
  [cmd(number):_].                       
sql_numeric_type_id -->
  [cmd(numeric):_].                      
sql_numeric_type_id -->
  [cmd(decimal):_].
  
optional_integer_range(R) -->
  [punct('('):_],
  !,
  positive_integer(R)                 # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
optional_integer_range(_R) -->
  [].

positive_integer(N) -->
  optional_sign(Sign),
  { Sign \== '-' },
  int(N),
  { N > 0 }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%constants
sql_constants([C|Cs]) -->
  sql_constant(C)                     # 'a constant',
  punct(',')                          # 'comma',
  !,
  sql_constants(Cs).
sql_constants([C]) -->
  sql_constant(C)                     # 'a constant'.

%constant number, string, default, null, date_constant
sql_constant(cte(C,number(N))) --> 
  value(C, N),
  !.
sql_constant(cte(C,string(_S))) -->
  value(C),
  !.
sql_constant(default) -->
  [cmd(default):_],
  !.
sql_constant(cte('$NULL'(N),_T)) -->
  [cmd(null):_],
  !,
  {get_null_id(N)}. % :::WARNING: Needed?
sql_constant(C) -->
  sql_date_constant(C),
  !.

/*
sql_constant(_) -->
  set_error('Syntax', 'Number, String, DATE String, TIME String, TIMESTAMP String, NULL').
*/

%date_constant
sql_date_constant(cte(date(Y,M,D),datetime(date))) -->
  [cmd(date):_],
  optional_cmd(bc,BC),
  current_position(Position),
  value(str(C))                       # 'string',
  { 
    string_chars(C, Chars),
    (phrase(valid_date_format, Chars) -> true; 
    set_error_with_parameter('Syntax', 'DATE String format must be [BC] \'Int(Year)-Int(month)-Int(day)\'' , [], Position)),
    split_string(C, "-", "", DateParts),
    maplist(number_string, [YRaw, M, D], DateParts),
    adjust_year(BC, YRaw, Y)            % Adjust year if BC is true
  }.  

sql_date_constant(cte(time(H,Mi,Se),datetime(time))) -->
  [cmd(time):_],
  current_position(Position),
  value(str(C))                       # 'string',
  { 
    string_chars(C, Chars),
    (phrase(valid_time_format, Chars) -> true; 
    set_error_with_parameter('Syntax', 'TIME String format must be \'Int(hour):Int(minute):Int(second)\'' , [], Position)),
    split_string(C, ":", "", TimeParts),
    maplist(number_string, [H, Mi, Se], TimeParts)
  }.

sql_date_constant(cte(datetime(Y,M,D,H,Mi,S),datetime(datetime))) -->
  ([cmd(datetime):_];
  [cmd(timestamp):_]),
  optional_cmd(bc,BC),
  current_position(Position),
  value(str(C))                       # 'string',
  { 
    string_chars(C, Chars),
    (phrase(valid_datetime_format, Chars) -> true; 
    set_error_with_parameter('Syntax', 'DATETIME/TIMESTAMP String format must be [BC] \'Int(Year)-Int(month)-Int(day) Int(hour):Int(minute):Int(second)\'' , [], Position)),
    split_string(C, " ", "", [DateString,TimeString]),
    split_string(DateString, "-", "", DateParts),
    split_string(TimeString, ":", "", TimeParts),
    append(DateParts, TimeParts, DateTimeParts),
    maplist(number_string, [YRaw, M, D, H, Mi, S], DateTimeParts),
    adjust_year(BC, YRaw, Y)            % Adjust year if BC is true
  }.  

% define valid_date_format
valid_date_format -->
  one_to_four_digits, ['-'], !, one_or_two_digits, ['-'], !, one_or_two_digits.

valid_time_format -->
  one_or_two_digits, [':'], !, one_or_two_digits, [':'], !, one_or_two_digits.

valid_datetime_format -->
  valid_date_format, [' '], !, valid_time_format.


one_to_four_digits --> digit.
one_to_four_digits --> digit, digit.
one_to_four_digits --> digit, digit, digit.
one_to_four_digits --> digit, digit, digit, digit.

one_or_two_digits --> digit.
one_or_two_digits --> digit, digit.

% define digit
digit --> [C], {char_type(C, digit)}.

adjust_year(true, YRaw, Y) :- Y is 1 - YRaw.
adjust_year(false, YRaw, Y) :- Y is YRaw.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Condition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BWhereCondition ::=
%   (WhereCondition)

sql_condition(F) -->
  sql_condition(1200,F).
    
sql_condition(PP,To) -->
  cond_factor(L), 
  r_sql_condition(PP,0,L/To).

sql_condition(PP,To) -->
  [punct('('):_],
  !,
  sql_condition(1200,T)               # 'valid SQL condition', 
  punct(')')                          # 'closing parenthesis '')''',
  r_sql_condition(PP,0,T/To).
sql_condition(PP,To) -->
  [op(OP):_],
  {sql_operator(P,FX,OP),
    prefix(P,FX,PR),
    P=<PP},
  sql_condition(PR,T)                 # 'valid SQL condition', 
  {NT=..[OP,T]},
  r_sql_condition(PP,P,NT/To).

r_sql_condition(PP,Pi,Ti/To) -->
  [op(OP):_],
  {sql_operator(P,YFX,OP),
    infix(P,YFX,PL,PR),
    P=<PP,
    Pi=<PL,
    NT=..[OP,Ti,T]},
  sql_condition(PR,T), 
  r_sql_condition(PP,P,NT/To).
r_sql_condition(_,_,Ti/Ti) -->
  [].

sql_operator(1100,xfy, or).
sql_operator(1050,xfy, xor).
sql_operator(1000,xfy, and).
sql_operator( 900, fy, not).

b_sql_condition(SQLst) -->
  [punct('('):_],
  !,
  sql_condition(SQLst),
  punct(')')                          # 'closing parenthesis '')'''.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Condition Factor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% BWhereCondition ::=
%   (WhereCondition)

% UBWhereCondition ::=
%   TRUE
%   |
%   FALSE
%   |
%   EXISTS DQLstmt
%   |
%   NOT (WhereCondition)
%   |
%   (AttOrCte{,AttOrCte}) [NOT] IN [DQLstmt|(Cte{,Cte})|((Cte{,Cte}){,(Cte{,Cte})})]  % Extension for lists of tuples 
%   |
%   WhereExpression IS [NOT] NULL
%   |
%   WhereExpression [NOT] IN DQLstmt
%   |
%   WhereExpression ComparisonOp [[ALL|ANY]] WhereExpression 
%   |
%   WhereCondition [AND|OR|XOR] WhereCondition
%   |
%   WhereExpression BETWEEN WhereExpression AND WhereExpression

cond_factor(E) -->
  b_sql_condition(E).
cond_factor(true) --> 
  [cmd(true):_].
cond_factor(false) --> 
  [cmd(false):_].
cond_factor(is_null(R)) --> 
  sql_expression(R,_T), 
  cmd(is)                             # 'IS', 
  cmd(null)                           # 'NULL or NOT NULL'.
cond_factor(not(is_null(R))) --> 
  sql_expression(R,_T),  
  cmd(is)                             # 'IS',
  op(not)                             # 'NULL or NOT NULL', 
  cmd(null)                           # 'NULL'.
cond_factor(exists(R)) -->
  [cmd(exists):_],
  opening_parentheses_star(N),
  dqlStmt([R|STs]/STs)                # 'valid SELECT statement',
  closing_parentheses_star(N).
/*
cond_factor(and('<='(L,C),'<='(C,R))) --> 
  sql_expression(C,_CT),
  cmd(between)                        # 'BETWEEN',
  sql_expression(L,_LT),
  %syntax_check_same_types('BETWEEN test',CT,LT),
  op(and)                             # 'AND',
  sql_expression(R,_RT),
  %syntax_check_same_types('BETWEEN test',LT,RT),
  syntax_check_between(L,R).
cond_factor(or('>'(L,C),'>'(C,R))) --> 
  sql_expression(C,_CT),
  op(not)                             # 'NOT',
  cmd(between)                        # 'BETWEEN',         
  sql_expression(L,_LT),
  %syntax_check_same_types('BETWEEN test',CT,LT),
  op(and)                             # 'AND',
  sql_expression(R,_RT),
  %syntax_check_same_types('BETWEEN test',LT,RT),
  syntax_check_between(L,R).*/
cond_factor(in(L,R)) --> 
  column_or_constant_tuple(L,A),
  cmd(in)                             # 'IN',
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
/*cond_factor(F) --> 
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
  {(NOT==true -> F='$not_like'(L,R,E) ; F='$like'(L,R,E))}.*/
cond_factor(C) --> 
  sql_expression(L,_LT), 
  relop(Op)                           # 'a comparison operator', 
  sql_expression(R,_RT)               # 'valid expression',
  {sql_rel_cond_factor(Op,L,R,C)}.
  %syntax_check_same_types(C,LT,RT).
cond_factor(true) --> 
  {current_db(_,mysql)},
  sql_constant(_C).

/*
sql_rel_cond_factor(OpMod,L,SQLst,CF) :-
  (atom_concat(_Op,'_all',OpMod), MOD='ALL'
  ;
   atom_concat(_Op,'_any',OpMod), MOD='ANY'
  ),
  !,
  (SQLst=(select(_D,_T,_Of,Es,_Ts,_F,_W,_G,_H,_O),_As)
   ->
    (Es=[expr(_E,_A,_Ty)]
     ->
      CF=..[OpMod,L,SQLst]
     ;
      my_raise_exception(generic,syntax(['Only one expression allowed the SELECT list of the subquery in the ',MOD,' condition.']),[])
    )
   ;
    my_raise_exception(generic,syntax(['Unsupported subquery in the ALL condition.']),[])
   ).*/

sql_rel_cond_factor(Op,L,R,CF) :-
  CF=..[Op,L,R].
    
column_or_constant_tuple(Cs,A) --> 
  [punct('('):_],
  sql_proj_expression_sequence(Cs),
  punct(')')                          # 'closing parenthesis'')''',
  {length(Cs,A)}.
column_or_constant_tuple([C],1) --> 
  sql_proj_expression(C,_AS).

sql_proj_expression_sequence([C,C2|Cs]) -->
  sql_proj_expression(C,_),
  punct(',')                          # 'comma',
  {!}, % 23-01-2021
  sql_proj_expression_sequence([C2|Cs]).
sql_proj_expression_sequence([C]) -->
  sql_proj_expression(C,_).


sql_proj_expression(expr(E,AS,Type),AS) -->
  sql_expression(E,Type).

relop(RO) --> 
  set_op(RO).
relop(RO) --> 
  tuple_op(RO).

set_op(SO) -->
  tuple_op(TO),
  cmd(all)                            # 'all',
  {atom_concat(TO,'_all',SO)}.
set_op(SO) -->
  tuple_op(TO),
  cmd(any)                            # 'any',
  {atom_concat(TO,'_any',SO)}.
  
tuple_op(RO) --> 
  [comparisonOp(RO):_].

dql_or_constant_tuples(_A,R) -->
  dqlStmt([R|STs]/STs).
dql_or_constant_tuples(A,R) -->
  [punct('('):_],
  sql_ground_tuple_list(A,Ts),
  punct(')')                          # 'closing parenthesis '')''',
  {in_tuples_to_DQL(Ts,R)}.
dql_or_constant_tuples(_,R) -->
  sql_ground_tuple(_,Cs),
  {my_list_to_list_of_lists(Cs,L),
    in_tuples_to_DQL(L,R)}.
dql_or_constant_tuples(1,R) -->
  sql_constant(C),
  {in_tuples_to_DQL([[C]],R)}.

in_tuples_to_DQL([Cs],(select(all,top(all),no_offset,Es,[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_)) :-
  args_to_exprs(Cs,Es).
in_tuples_to_DQL([C1,C2|Cs],(union(all,SQL1,SQL2),_)) :-
  in_tuples_to_DQL([C1],SQL1),
  in_tuples_to_DQL([C2|Cs],SQL2).
  
args_to_exprs([],[]).
args_to_exprs([C|Cs],[expr(C,_,_)|Es]) :-
  args_to_exprs(Cs,Es).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


sql_expressions([E|Es]) -->
  sql_expression(E,_)                 # 'an expression',
  remaining_sql_expressions(Es).
remaining_sql_expressions(Cs) -->
  punct(',')                          # 'comma',
  !,
  sql_expressions(Cs).
remaining_sql_expressions([]) -->
  [].


sql_expression(E,T) -->
  {current_db(_,postgresql)},
  sql_expression(1200,E,T),
  punct('::')                         # 'double colon ''::''',
  sql_type(_).
sql_expression(E,T) -->
  sql_expression(1200,E,T)            /*# 'valid expression'*/.

sql_expression(PP,Lo,To) -->
  sql_factor(L,T), 
  r_sql_expression(PP,0,L/Lo,T/To).
sql_expression(PP,Lo,To) -->
  [punct('('):_],
  !,
  sql_expression(1200,L,T), 
  punct(')')                          # 'closing parenthesis'')''',
  !, % WARNING
  r_sql_expression(PP,0,L/Lo,T/To).
sql_expression(PP,Lo,To) -->
  [op(OP):_],
  {operator(P,FX,[T,Ta],_,OP),
    prefix(P,FX,PR),
    P=<PP},
  sql_expression(PR,L,Ta), 
  {NL=..[OP,L]},
  r_sql_expression(PP,P,NL/Lo,T/To).
  
r_sql_expression(PP,Pi,Li/Lo,Ti/To) -->
  [op(OP):_],
  {operator(P,YFX,[T,Ti,RT],_,OP),
    infix(P,YFX,PL,PR),
    P=<PP,
    Pi=<PL
  },
  sql_expression(PR,L,RT), 
  {NL=..[OP,Li,L]}, 
  r_sql_expression(PP,P,NL/Lo,T/To).
r_sql_expression(_,_,Li/Li,Ti/Ti) -->
  [].

operator(P,A,Ts,_,OP) :-
  my_operator(P,A,Ts,_,OP).

operator(P,A,Ts,SOP,OP) :-
  my_infix_operator(OP,SOP,_,Ts,_,P,A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Expressions Factor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expression ::=
%   Op1 Expression
%   |
%   Expression Op2 Expression
%   |
%   Function(Expression{, Expression})
%   |
%   Att
%   |
%   RelationName.Att
%   |
%   Cte
%   |
%   DQLstmt

sql_factor(E,T) -->
  [punct('('):_],
  sql_expression(E,T),
  punct(')')                          # 'closing parenthesis'')''',
  {!}. % WARNING: This whole clause is only for improving parsing performance
sql_factor(E,_) --> % :::WARNING: Add type info
  dqlStmt([E|STs]/STs),
  !.
sql_factor(Aggr,T) -->
  sql_special_aggregate_function(Aggr,T),
  !.  % WARNING: This cut is only for improving parsing performance
sql_factor(FAs,T) --> 
  [fn(SF):_],
  {function(SF,F,_,_,[T|Ts],Arity),
    Arity>0},
  [punct('('):_],
  sql_function_arguments(Arity,As,Ts),
  punct(')')                          # 'closing parenthesis '')''',
  { FAs=..[F|As]}.
/*
sql_factor(Function,number(_)) -->
  [cmd(extract):_],
  punct('(')                          # 'opening parenthesis ''(''',
  extract_field(Field)                # 'valid datetime field (year, month, day, hour, minute, second)',
  cmd(from)                           # 'FROM',
  sql_expression(C,datetime(_))       # 'valid datetime expression',  
  punct(')')                          # 'closing parenthesis '')''',
  {Function=..[Field,C],
    !}.
sql_factor(cast(Factor,Type),Type) -->
  [fn(cast):_],
  punct('(')                          # 'opening parenthesis ''(''',
  sql_factor(Factor,_),
  cmd(as)                             # 'AS',
  sql_type(Type)                      # 'valid type name',  
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(coalesce(ExprSeq),_Type) -->
  [fn(coalesce):_],
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(greatest(ExprSeq),_Type) -->
  [fn(greatest):_],
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(least(ExprSeq),_Type) -->
  [fn(least):_],
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(iif(Cond,Expr1,Expr2),_Type) -->
  [fn(iif):_],
  punct('(')                          # 'opening parenthesis ''(''',
  sql_condition(Cond)                 # 'valid condition',
  punct(',')                          # 'comma',
  sql_expression(Expr1,_T1)           # 'valid expression', 
  punct(',')                          # 'comma',
  sql_expression(Expr2,_T2)           # 'valid expression', 
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(case(CondValList,Default),Type) -->
  [fn(case):_],
  sql_case2_when_thens(CondValList),
  sql_case_else_end(Default,Type).
sql_factor(case(Expr,ExprValList,Default),Type) -->
  [fn(case):_],
  sql_expression(Expr,_T)             # 'an expression', 
  sql_case3_when_thens(ExprValList),
  sql_case_else_end(Default,Type).*/
sql_factor(cte(C,T),T) -->
  sql_constant(cte(C,T)).
sql_factor(C,_) -->
  column(C).
/*sql_factor(F,T) --> 
  [fn(SF):_],
  {my_function(SF,F,Type,0,[T]),
    Type\==aggregate % 0-arity aggregate functions from Datalog are not allowed in SQL
    },
  optional_parentheses.*/

%sql_factor(_,_) -->
%  set_error('Syntax', 'valid SQL factor').

sql_function_arguments(1,[E],[T]) -->
  {dif(T,type(_))},
  current_position(Position),
  sql_expression(E,ET),
  {T=ET -> true;  
  set_error_with_parameter('Semantic', 'Expected argument type ~w' , [T], Position)}.
sql_function_arguments(1,[T],[type(T)]) -->
  sql_type(T).
sql_function_arguments(A,[E|Es],[T|Ts]) -->
  {A>1},
  sql_function_arguments(1,[E],[T]),
  punct(',')                          # 'comma',
  {A1 is A-1},
  sql_function_arguments(A1,Es,Ts).

% COUNT(*)
sql_special_aggregate_function(count,number(_)) -->
  [fn(count):_],
  punct('(')                          # 'opening parenthesis ''(''',
  op('*')                             # 'a star ''*''',
  punct(')')                          # 'closing parenthesis '')'''.
% MIN/MAX(DISTINCT Column) behaves as MIN/MAX(Column), as allowed by SQL2 Standard
sql_special_aggregate_function(min(C),_) -->
  [fn(min):_],
  punct('(')                          # 'opening parenthesis ''(''',
  cmd(distinct)                       # 'DISTINCT',
  column(C),
  punct(')')                          # 'closing parenthesis '')''',
  set_error('Semantic', 'DISTINCT should not be applied to the argument of MIN.').
sql_special_aggregate_function(max(C),_) -->
  [fn(max):_],
  punct('(')                          # 'opening parenthesis ''(''',
  cmd(distinct)                       # 'DISTINCT',
  column(C),
  punct(')')                          # 'closing parenthesis '')''',
  set_error('Semantic', 'DISTINCT should not be applied to the argument of MAX.').
% Aggr(DISTINCT Column)
sql_special_aggregate_function(AF,T) -->
  [fn(F):_],
  {my_aggregate_function(_,PF,T,1),
   atom_concat(F,'_distinct',PF)},
  punct('(')                          # 'opening parenthesis ''(''',
  cmd(distinct)                       # 'DISTINCT',
  column(C),
  punct(')')                          # 'closing parenthesis '')''',
  {AF=..[PF,C]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Column and Table Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% column constraint
column_constraint(C,not_nullables([C])) -->
  [op(not):_],
  cmd(null)                           # 'NULL',
  !. 
column_constraint(_C,true) -->
  [cmd(null):_],
  !.
column_constraint(C,primary_key([C])) -->
  [cmd(primary):_],
  cmd(key)                            # 'KEY',
  !.
column_constraint(C,candidate_key([C])) -->
  [cmd(unique):_],
  !.
column_constraint(C,foreign_key([C],TableName,[TC])) -->
  [cmd(references):_],
  referenced_column(C,TableName,TC)   # 'valid reference name (table name)',
  optional_referential_triggered_action(_Rule),
  !.
column_constraint(C,default(C,Expression,Type)) -->
  [cmd(default):_], 
  sql_expression(Expression,Type)     # 'expression',
  !.
column_constraint(_C,CheckCtr) -->
  [cmd(check):_],
  opening_parentheses_star(N),
  check_constraint(CheckCtr)          # 'valid check constraint',
  closing_parentheses_star(N),
  !.
column_constraint(C,candidate_key([C])) -->
  [cmd(candidate):_],
  cmd(key)                            # 'KEY',
  !.
column_constraint(C,fd([Att],[C])) -->
  [cmd(determined):_],
  cmd(by)                             # 'BY',
  untyped_column(Att)                 # 'a column name',
  !.

column_constraint(_,_) -->
  set_error('Syntax', 'valid column constraint (NOT, NULL, PRIMARY, UNIQUE, REFERENCES, DEFAULT, CHECK, CANDIDATE, DETERMINED)').


% table constraint
table_constraint(not_nullables(Cs)) -->
  [op(not):_],
  cmd(null)                           # 'NULL',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(primary_key(Cs)) -->
  [cmd(primary):_],
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(candidate_key(Cs)) -->
  [cmd(unique):_],
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(foreign_key(Cs,FTableName,FCs)) -->
  [cmd(foreign):_],
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a sequence of column names between parentheses',
  cmd(references)                     # 'REFERENCES', 
  tablename(FTableName)               # 'table name',
  [punct('('):_],
  column_name_list(FCs)               # 'a sequence of column names between parentheses',
  [punct(')'):_],
  optional_referential_triggered_action(_Rule),
  !.
table_constraint(foreign_key(Cs,FTableName,Cs)) -->
  [cmd(foreign):_],
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  cmd(references)                     # 'REFERENCES', 
  tablename(FTableName)               # 'table name',
  optional_referential_triggered_action(_Rule),
  !.
table_constraint(CheckCtr) -->
  [cmd(check):_],
  opening_parentheses_star(N),
  check_constraint(CheckCtr)          # 'valid check constraint',
  closing_parentheses_star(N),
  !.
table_constraint(candidate_key(Cs)) -->
  [cmd(candidate):_],
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.

table_constraint(_) -->
  set_error('Syntax', 'valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Column  ColumnNnameList tablename viewname colname relname 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

column_tuple(Ts) -->
  [punct('('):_],
  !,
  column_name_list(Ts),
  punct(')')                          # 'comma or closing parenthesis '')'''.

column_tuple([Ts]) -->
  column_name(Ts)                     # 'a column name'.

column_name_list([C]) --> 
  untyped_column(C).
column_name_list([C|Cs]) -->
  untyped_column(C),
  [punct(','):_], 
  column_name_list(Cs).

column_name(C) -->
  untyped_column(C).

column_list([C,C2|Cs]) -->
  column(C),
  punct(',')                          # 'comma or closing parenthesis '')''', 
  column_list([C2|Cs]).
column_list([C]) -->
  column(C).

untyped_columns([C:_T]) --> 
  untyped_column(C).
untyped_columns([C:_T|CTs]) -->
  untyped_column(C),
  punct(',')                          # 'comma or closing parenthesis '')''', 
  untyped_columns(CTs).

untyped_column(C) --> 
  colname(C).

p_ren_tablename(T) --> 
  ren_tablename(T).
p_ren_tablename((T, _R)) -->
  tablename(T).

ren_tablename((T,[I|Args])) -->
  %current_position(Position),
  tablename(T),
  optional_cmd(as),
  sql_user_identifier(I),
  {my_table('$des',T,A),
  length(Args,A)}.
  /*{my_table('$des',T,A) -> length(Args,A);
  set_error_with_parameter('Semantic', 'Table ~w does not exist in the $des system' , [T], Position)}.
  */

%column rel_id.col_id/col_id
column(attr(R,C,_AS)) --> 
  relname(R),
  [punct('.'):_],
  colname(C)                          # 'column name'. 
column(attr(_T,C,_AS)) --> 
  colname(C).
  %{\+ evaluable_symbol(C)}. I guess this is no need

% tablename/viewname/colname/relname
tablename(TableName) -->
  sql_user_identifier(TableName).

viewname(ViewName) -->
  sql_user_identifier(ViewName).

colname(ColName) -->
  sql_user_identifier(ColName).

relname(RelName) --> 
  sql_user_identifier(RelName).

% get table,view or col name -> quoted_id(id) | id(id) | [id(id)] | `id(id)` 
sql_user_identifier(Name) -->
  [quoted_id(Name):_Pos].

sql_user_identifier(Name) --> 
  [punct('['):_],  %no "punct(']') # 'opening bracket'" because it's not mandatory
  !,
  ([id(Name):_Pos]; 
    [id_lc_start(Name):_Pos]),
  punct(']')                          # 'closing bracket '']'''.

sql_user_identifier(Name) --> 
  [punct('`'):_],  %no "punct('`') # 'opening back quotes'" because it's not mandatory
  !,
  ([id(Name):_Pos]; 
    [id_lc_start(Name):_Pos]),
  punct('`')                          # 'closing back quotes ''`'''.

sql_user_identifier(Name) -->
  [id(Name):_Pos].

sql_user_identifier(Name) -->
  [id_lc_start(Name):_Pos].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Syntax check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*syntax_check_redef(Schema) -->
  {functor(Schema,F,A),
   datalog_keyword(F,A),
   my_raise_exception(generic,syntax(['Trying to redefine the builtin "',F,'"']),[])
  }.
syntax_check_redef(_Schema) -->
  [].*/

/*syntax_check_same_types(_E,_LT,_RT) -->
  {type_casting(on),
    !}.
syntax_check_same_types(E,LT,RT) -->
  {nonvar(LT),
   nonvar(RT),
   !},
  push_syntax_error(['Expected same types in ','$exec'(write_sql_cond(E,0,'$des'))],Old),
  {LT=RT}.
syntax_check_same_types(_E,_LT,_RT) -->
  [].

syntax_check_between(cte(CteL,TypeL),cte(CteR,TypeR)) -->
  {!,
    (compute_comparison_primitive(CteL=<CteR,CteL=<CteR)
    ->
      true
    ;
    my_raise_exception(generic,syntax(['First constant in BETWEEN (','$exec'(write_expr(cte(CteL,TypeL))), ') must be less than or equal to the second one (','$exec'(write_expr(cte(CteR,TypeR))),')']),[]))
  }.
syntax_check_between(_L,_R) -->
  [].

syntax_check_expr_type(L,LT,ET) -->
  {nonvar(LT),
    nonvar(ET),
    !},
  {internal_typename_to_user_typename(ET,UET)},
  push_syntax_error(['Expected ',UET,' type in ','$exec'(write_expr(L))],Old),
  {LT=ET},
  pop_syntax_error(Old).
syntax_check_expr_type(_L,_LT,_ET) -->
  [].*/

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


% value(-Value, Type)//
value(int(I), int) -->
  int(I),
  !.
value(frac(I, F), frac) -->
  frac(I, F),
  !.
value(float(I, F, Ex), float) -->
  float(I, F, Ex),
  !.
value(str(Str)) -->
  [str(Str):_],
  !.

integer(N) -->
  optional_sign(Sign),
  int(I),
  {Sign == '-'
  -> N is -I
  ;  N = I}.

% optional_op(-Op, true/false)//
% Optional op
optional_op(Op,true) -->
  [op(Op):_],
  !.
optional_op(_Op,false) -->
  [].

optional_cmd(Cmd,true) -->
  [cmd(Cmd):_],
  !.
optional_cmd(_Op,false) -->
  [].

optional_sign('+') -->
  [op('+'):_], !.
optional_sign('-') -->
  [op('-'):_], !.
optional_sign('') -->
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

optional_parentheses -->
  [punct('('):_],
  !,
  punct(')')                          # 'closing parenthesis '')'''.
optional_parentheses -->
  [].  

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
terminal(id_lc_start(_)).
terminal(cmd(_)).
terminal(comparisonOp(_)).
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

/*TEMPLATE
test0XX :-
test(parser, lex_parse, "STATEMENT",
  failure(error('Syntax', 'ERROR', pos(1, 1)))).*/

%ISLstmt
test001 :-
  test(parser, lex_parse, 'test/test023.sql', 
  [show_tables,
  show_views,
  show_databases,
  describe(t)]).

%ISLstmt error
test002 :-
  test(parser, lex_parse, "DESCRIBE 2",
    failure(error('Syntax', 'table name', pos(1, 10)))).

test003 :-
  test(parser, lex_parse, "show t",
    failure(error('Syntax', 'TABLES, VIEWS or DATABASES', pos(1, 6)))).

%TMLstmt
test004 :-
  test(parser, lex_parse, 'test/test024.sql', 
    [commit,
    commit,
    rollback([]),
    rollback([]),
    rollback([sp1]),
    rollback([sp1]),
    savepoint(['sp2.ddb'])]).

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

%DDLstmt CREATE, CREATE OR REPLACE
test009 :-
  test(parser, lex_parse, 'test/test014.sql',
    [create_table(t(a:number(integer)),[true]),
    create_table(t(a:number(integer)),[true]),
    create_table(t(a:number(integer)),[true]),
    create_table(c(a:string(varchar),b:string(varchar)),[true,true]),
    create_table(edge(origin:string(varchar),destination:string(varchar)),[true,true]),
    create_table(flights(airline:string(varchar),frm:string(varchar),to:string(varchar),departs:number(integer),arrives:number(integer)),[true,true,true,true,true]),
    create_table(employee(name:string(varchar(20)),department:string(varchar(20)),salary:number(integer)),[true,true,true]),
    create_table(emp(dni:string(varchar),numdep:number(integer)),[primary_key([dni]),foreign_key([numdep],dpto,[nd])]),
    create_table(trab(dni:string(varchar),npro:number(integer)),[foreign_key([dni],emp,[dni]),true,primary_key([dni,npro])]),
    create_table(takes(eid:string(varchar),cid:string(varchar),tyear:number(integer),tmonth:number(integer),tday:number(integer)),[true,true,true,true,true,primary_key([eid,cid])]),
    create_table(flight(origin:string(varchar),destination:string(varchar),time:number(float)),[true,true,true]),
    create_table(emp(dnisupervisor:string(varchar)),[true,sql_check_constraint(in([expr(attr(_,dnisupervisor,_),_,_)],(select(all,top(all),no_offset,[expr(attr(_,dni,_),_,_)],[],from([(emp,_)]),where(true),group_by([]),having(true),order_by([],[])),_)))]),
    create_table(t(a:number(integer)),[sql_check_constraint(attr(_,a,_)> -cte(float(1,0,-1),number(float)))]),
    create_or_replace_table(t(a:number(integer),b:number(integer)),[true,true]),
    create_or_replace_table(t(a:number(integer),b:number(integer)),[true,true,foreign_key([a],s,[a])]),
    create_or_replace_table(t(a:number(integer),b:number(integer),c:number(integer),d:number(integer)),[true,true,true,true,fd([c,d],[a,b])]),
    create_table_like(t,s),
    create_table_as((select(all,top(all),no_offset,[expr(attr(_,a,_),_,_)],[],from([(n,_)]),where(true),group_by([]),having(true),order_by([],[])),_),t3(a3:_,b3:_,c3:_)),
    create_view(sql,(select(all,top(all),no_offset,[expr(attr(_,b,_),_,_)],[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_),v(a:_)),
    create_view(sql,(select(all,top(all),no_offset,[expr(attr(_,b,_),c,_)],[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_),v(a:_)),
    create_view(sql,(select(all,top(all),no_offset,[(b,(*))],[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_),v(a:_)),
    create_view(sql,(select(all,top(all),no_offset,*,[],from([(left_join((a,_),(b,_),attr(_,x,_)=attr(_,y,_)),_)]),where(attr(_,x,_)>cte(int(1),number(int))),group_by([]),having(true),order_by([],[])),_),v(x:_,y:_)),
    create_database(x)]).

%DDLstmt CREATE, CREATE OR REPLACE error
test010 :-
  test(parser, lex_parse, "create or table t(a int)",
    failure(error('Syntax', 'REPLACE', pos(1, 11)))).

test011 :-
  test(parser, lex_parse, "create replace table t(a int);",
    failure(error('Syntax', 'OR REPLACE, TABLE, VIEW or DATABASE', pos(1, 8)))).

test012 :-
  test(parser, lex_parse, "create or replace able t(a int);",
    failure(error('Syntax', 'TABLE or VIEW', pos(1, 19)))).

test013 :-
  test(parser, lex_parse, "create table t('a' intiger)",
    failure(error('Syntax', 'AS, LIKE or column identifier', pos(1, 16)))).

test014 :-
  test(parser, lex_parse, "create table emp(check dnisupervisor in select dni from emp);",
    failure(error('Syntax', 'AS, LIKE or column identifier', pos(1, 18)))).
  
test015 :-
  test(parser, lex_parse, "create table t(a)",
    failure(error('Syntax', 'valid type', pos(1, 17)))).

test016 :-
  test(parser, lex_parse, "create table t(a intiger)",
    failure(error('Syntax', 'valid type', pos(1, 18)))).

test017 :-
  test(parser, lex_parse, "create table emp(a, null b)",
    failure(error('Syntax', 'valid type', pos(1, 19)))).

test018 :-
  test(parser, lex_parse, "create or replace table t(a integer check b DETERMINED BY c,d)",
    failure(error('Syntax', 'valid type', pos(1, 62)))).

test019 :-
  test(parser, lex_parse, "create table t",
    failure(error('Syntax', 'typed schema', pos(1, 14)))).
  
test020 :-
  test(parser, lex_parse, "create or replace table t(a char())",
    failure(error('Syntax', 'a positive integer', pos(1, 34)))).

test021 :-
  test(parser, lex_parse, "create or replace table t(a number(1,))",
    failure(error('Syntax', 'a positive integer', pos(1, 38)))).

test022 :-
  test(parser, lex_parse, "create or replace table t(a int not nul)",
    failure(error('Syntax', 'NULL', pos(1, 37)))).

test023 :-
  test(parser, lex_parse, "create or replace table t(a int primary kye)",
    failure(error('Syntax', 'KEY', pos(1, 41)))).

test024 :-
  test(parser, lex_parse, "create or replace table t(a int candidate)",
    failure(error('Syntax', 'KEY', pos(1, 42)))).
  
test025 :-
  test(parser, lex_parse, "create or replace table t(a int determined from a)",
    failure(error('Syntax', 'BY', pos(1, 44)))).

test026 :-
  test(parser, lex_parse, "create or replace table t(a int references)",
    failure(error('Syntax', 'table name', pos(1, 43)))).

test027 :-
  test(parser, lex_parse, "create or replace table t(a int references s()a)",
    failure(error('Syntax', 'a column name', pos(1, 46)))).

test028 :-
  test(parser, lex_parse, "create or replace table t(a int defaul 0)",
    failure(error('Syntax', 'comma or column constraints', pos(1, 33)))).

test029 :-
  test(parser, lex_parse, "create or replace table t(a int, foreign key s)",
    failure(error('Syntax', 'REFERENCES', pos(1, 47)))).
  
test030 :-
  test(parser, lex_parse, "create or replace table t(a int, b int, foreign key (a,b references s)",
    failure(error('Syntax', 'comma or closing parenthesis '')''', pos(1, 58)))).

test031 :-
  test(parser, lex_parse, "create or replace table t(a int references s a)",
    failure(error('Syntax', 'valid column constraint (NOT, NULL, PRIMARY, UNIQUE, REFERENCES, DEFAULT, CHECK, CANDIDATE, DETERMINED)', pos(1, 46)))).

test032 :-
  test(parser, lex_parse, "create or replace table t(a int, unique s,)",
    failure(error('Syntax', 'valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)', pos(1, 43)))).

test033 :-
  test(parser, lex_parse, "create table t(a) as",
    failure(error('Syntax', 'valid SQL DQL statement (SELECT, WITH or ASSUME)', pos(last, last)))).

test034 :-
  test(parser, lex_parse, "create table t(a) like s",
    failure(error('Syntax', 'AS, or column name', pos(1, 19)))).
  
test035 :-
  test(parser, lex_parse, "create table t(a) a",
    failure(error('Syntax', 'AS, or column name', pos(1, 19)))).

test036 :-
  test(parser, lex_parse, "create table t like 2",
    failure(error('Syntax', 'valid SQL DDL statement (table name)', pos(1, 21)))).

test037 :-
  test(parser, lex_parse, "create table t like sa)",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 23)))).

test038 :-
  test(parser, lex_parse, "create view",
    failure(error('Syntax', 'view schema', pos(last, last)))).

test039 :-
  test(parser, lex_parse, "create view v(a) s",
    failure(error('Syntax', 'AS', pos(1, 18)))).

test040 :-
  test(parser, lex_parse, "create view v(a) as",
    failure(error('Syntax', 'valid SQL DQL statement (SELECT, WITH or ASSUME)', pos(last, last)))).
  
test041 :-
  test(parser, lex_parse, "create view v() as select * from a",
    failure(error('Syntax', 'column sequence separated by commas', pos(1, 15)))).

%DDLstmt ALTER, RENAME
test042 :-
  test(parser, lex_parse, 'test/test015.sql', 
    [alter_table(t1,add,column(a:number(integer),[true])),
    alter_table(t1,add,column(a:number(integer),[not_nullables([a])])),
    alter_table(t1,add,ctr(primary_key([a,c]))),
    alter_table(t1,add,ctr(not_nullables([b]))),
    alter_table(t1,add,ctr(candidate_key([b]))),
    alter_table(t1,add,ctr(candidate_key([b]))),
    alter_table(t1,add,ctr(fd([b],[a]))),
    alter_table(t1,add,ctr(fd([a,b],[a,b]))),
    alter_table(t1,add,ctr(sql_check_constraint(attr(_,a,_)>cte(int(0),number(int))))),
    alter_table(t1,drop,column(a)),
    alter_table(t1,drop,column(a)),
    alter_table(t1,drop,ctr(primary_key([a]))),
    alter_table(t1,drop,ctr(primary_key([a]))),
    alter_table(t1,alter,column(a1:string(varchar(10)))),
    alter_table(t1,alter,column(a1:string(varchar),[default(a1,cte(str(''),string(A)),string(A))])),
    rename_table(t,s),
    rename_view(v,s)]).

%DDLstmt ALTER, RENAME error
test043 :-
  test(parser, lex_parse, "alter t1 add constraint primary key(a)",
    failure(error('Syntax', 'TABLE', pos(1, 7)))).

test044 :-
  test(parser, lex_parse, "alter table t drop a",
    failure(error('Semantic', 'unknown_table(t)', pos(1, 13)))).

test045 :-
  test(parser, lex_parse, "alter table t1 ad a int",
    failure(error('Syntax', 'ALTER, ADD or DROP', pos(1, 16)))).

test046 :-
  test(parser, lex_parse, "alter table t1 add primary key(a)",
    failure(error('Syntax', 'CONSTRAINT', pos(1, 20)))).

test047 :-
  test(parser, lex_parse, "alter table t1 add constraint far key(a)",
    failure(error('Syntax', 'valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)', pos(1, 35)))).

test048 :-
  test(parser, lex_parse, "alter table t1 add a",
    failure(error('Syntax', 'valid type', pos(last, last)))).

test049 :-
  test(parser, lex_parse, "alter table t1 add constraint primary key(a), primary key(a)",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 45)))).

test050 :-
  test(parser, lex_parse, "alter table t alter column a1 set data type varchar(10)",
    failure(error('Semantic', 'unknown_column(t, a1)', pos(1, 28)))).

test051 :-
  test(parser, lex_parse, "alter table t1 alter column a string default ''",
    failure(error('Semantic', 'unknown_column(t1, a)', pos(1, 29)))).

test052 :-
  test(parser, lex_parse, "alter table t1 alter column a1 et data type varchar(10)",
    failure(error('Syntax', 'SET', pos(1, 32)))).

test053 :-
  test(parser, lex_parse, "alter table t1 alter column a1 set ata type varchar(10)",
    failure(error('Syntax', 'DATA TYPE or TYPE', pos(1, 36)))).

test054 :-
  test(parser, lex_parse, "alter table t1 alter column a1 set data tyep varchar(10)",
    failure(error('Syntax', 'DATA TYPE or TYPE', pos(1, 41)))).

test055 :-
  test(parser, lex_parse, "rename t to s",
    failure(error('Syntax', 'TABLE or VIEW', pos(1, 8)))).

test056 :-
  test(parser, lex_parse, "rename table t to (s)",
    failure(error('Syntax', 'table name', pos(1, 19)))).

test057 :-
  test(parser, lex_parse, "rename table t(a) to s",
    failure(error('Syntax', 'TO', pos(1, 15)))).

test058 :-
  test(parser, lex_parse, "rename table v as t",
    failure(error('Syntax', 'TO', pos(1, 16)))).

test059 :-
  test(parser, lex_parse, "rename table t to s(a)",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 20)))).

test060 :-
  test(parser, lex_parse, "alter table t1 alter a1 set data type number()",
    failure(error('Syntax', 'a positive integer', pos(1, 46)))).

%DDLstmt DROP and CompleteSchema := DQLstmt
test061 :-
  test(parser, lex_parse, 'test/test016.sql', 
    [drop_table(t,[]),
    drop_view(v,[]),
    drop_table(t,[if_exists]),
    drop_table(t,[cascade]),
    drop_table(t,[cascade_constraints]),
    drop_view(v,[if_exists]),
    drop_view(v,[cascade]),
    drop_view(v,[if_exists]),
    drop_database('$des'),
    drop_database(db),
    drop_database('$des'),
    create_view(hrsql,(select(all,top(all),no_offset,[expr(attr(_,age,_),_,_)],[],from([(my_table,_)]),where(true),group_by([]),having(true),order_by([],[])),_),my_view(age:number(integer)))]).

%DDLstmt DROP and CompleteSchema := DQLstmt error
test062 :-
  test(parser, lex_parse, "drop t",
    failure(error('Syntax', 'TABLE, VIEW or DATABASE', pos(1, 6)))).

test063 :-
  test(parser, lex_parse, "drop table if exist t",
    failure(error('Syntax', 'EXISTS', pos(1, 15)))).

test064 :-
  test(parser, lex_parse, "drop table is t",
    failure(error('Syntax', 'table name or optional drop table clauses(IF EXISTS, CASCADE or CASCADE CONSTRAINTS)', pos(1, 12)))).

test065 :-
  test(parser, lex_parse, "drop view is v",
    failure(error('Syntax', 'view name or optional drop view clauses(IF EXISTS, CASCADE)', pos(1, 11)))).

test066 :-
  test(parser, lex_parse, "drop view v exist",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 13)))).

test067 :-
  test(parser, lex_parse, "my_view(age it) := SELECT age FROM my_table",
    failure(error('Syntax', 'valid type', pos(1, 13)))).

test068 :-
  test(parser, lex_parse, "my_view(age int,) := SELECT age FROM my_table",
    failure(error('Syntax', 'column identifier', pos(1, 17)))).

test069 :-
  test(parser, lex_parse, "my_view(age int) = SELECT age FROM my_table",
    failure(error('Syntax', 'colon '':''', pos(1, 18)))).

test070 :-
  test(parser, lex_parse, "my_view(age int) : SELECT age FROM my_table",
    failure(error('Syntax', 'equals ''=''', pos(1, 20)))).

%DQLstmt SELECT(I) STATEMENTS
test071 :-
  test(parser, lex_parse, 'test/test017.sql',
    [(select(all,top(all),no_offset,*,[],from([(t,_2350)]),where(true),group_by([]),having(true),order_by([],[])),_2290),(select(all,top(all),no_offset,[expr(attr(person,age,_2436),_2426,_2428)],[],from([(t,_2448)]),where(true),group_by([]),having(true),order_by([],[])),_2366),(select(distinct,top(all),no_offset,*,[],from([(t,_2524)]),where(true),group_by([]),having(true),order_by([],[])),_2464),(select(distinct,top(expr(1,_2594,number(int))),no_offset,*,[],from([(t,_2612)]),where(true),group_by([]),having(true),order_by([],[])),_2540),(select(all,top(expr(1,_2682,number(int))),no_offset,*,[],from([(t,_2700)]),where(true),group_by([]),having(true),order_by([],[])),_2628),(select(distinct,top(expr(1,_2770,number(int))),no_offset,*,[],from([(t,_2788)]),where(true),group_by([]),having(true),order_by([],[])),_2716),(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_2864,number(int))],[v],from([(t,_2900)]),where(true),group_by([]),having(true),order_by([],[])),_2804),(select(all,top(all),no_offset,[expr(attr(_2982,age,_2986),_2976,_2978)],[v],from([(t,_3004)]),where(true),group_by([]),having(true),order_by([],[])),_2916),(select(all,top(all),no_offset,[expr(attr(_3086,a,_3090),_3080,_3082)],[],from([(select(all,top(all),no_offset,[expr(attr(_3168,a,_3172),_3162,_3164)],[],from([(t,_3184)]),where(true),group_by([]),having(true),order_by([],[])),_3102)]),where(true),group_by([]),having(true),order_by([],[])),_3020),(select(all,top(all),no_offset,*,[],from([(t,_3350)]),where(attr(_3434,a,_3438)=attr(_3590,'$v$',_3594)),group_by([]),having(true),order_by([],[])),_3236),(select(all,top(all),no_offset,*,[],from([(t,_3832)]),where(true),group_by([expr(attr(_3902,'1',_3906),_3890,_3892)]),having(true),order_by([],[])),_3718),(select(all,top(all),no_offset,[expr(count,_4062,number(_4076))],[],from([(t,_4250)]),where(true),group_by([expr(attr(_4320,a,_4324),_4308,_4310)]),having(true),order_by([],[])),_3998),(select(all,top(all),no_offset,[expr(attr(_4492,a,_4496),_4480,_4482)],[],from([(taras,_4570)]),where(true),group_by([expr(attr(_4640,a,_4644),_4628,_4630)]),having(sum(attr(_4804,b,_4808))=cte(int(1),number(int))),order_by([],[])),_4416),(select(all,top(all),no_offset,[expr(attr(_5232,department,_5236),_5220,_5222)],[],from([(employee,_5310)]),where(true),group_by([expr(attr(_5380,department,_5384),_5368,_5370)]),having(count(attr(_5640,salary,_5644))>cte(int(1),number(int))),order_by([],[])),_5156),(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_6056,number(int))],[],from([(t,_6162)]),where(true),group_by([]),having('<=_all'(attr(_6250,age,_6254),(select(all,top(all),no_offset,[expr(attr(_6552,a,_6556),_6540,_6542)],[],from([(s,_6630)]),where(true),group_by([]),having(true),order_by([],[])),_6476))),order_by([],[])),_5992),(select(all,top(all),no_offset,[expr(attr(_6908,nombre,_6912),_6896,_6898),expr(attr(_6984,calle,_6988),_6972,_6974),expr(attr(_7058,'Cdigo postal',_7062),_7046,_7048)],[],from([(inner_join((empleados,_7216),(domicilios,_7244),equijoin(natural)),_7136)]),where(true),group_by([]),having(true),order_by([expr(attr(_7438,'Cdigo postal',_7442),_7426,_7428),expr(attr(_7520,nombre,_7524),_7508,_7510)],[a,a])),_6830),(select(all,top(all),offset(expr(cte(int(10),number(int)),_7790,number(int)),expr(cte(int(10),number(int)),_7898,number(int))),[expr(attr(_7688,n,_7692),_7676,_7678)],[],from([(n,_7766)]),where(true),group_by([]),having(true),order_by([],[])),_7612),(select(distinct,top(expr(cte(int(1),number(int)),_8256,number(int))),no_offset,*,[],from([(t,_8202)]),where(true),group_by([]),having(true),order_by([],[])),_8086)]).

%DQLstmt SELECT(I) STATEMENTS error
test072 :-
  test(parser, lex_parse, "select top 1 from t",
    failure(error('Syntax', 'SELECT list', pos(1, 14)))).

test073 :-
  test(parser, lex_parse, "select distinct top 1 top * from t",
    failure(error('Syntax', 'SELECT list', pos(1, 23)))).

test074 :-
  test(parser, lex_parse, "select * from t1 inner join t2 using (c b)",
    failure(error('Syntax', 'comma or closing parenthesis '')''', pos(1, 41)))).

test075 :-
  test(parser, lex_parse, "select person from t group yb a",
    failure(error('Syntax', 'BY', pos(1, 28)))).  

test076 :-
  test(parser, lex_parse, "SELECT N FROM E ORDER Yb N",
    failure(error('Syntax', 'BY', pos(1, 23)))).

test077 :-
  test(parser, lex_parse, "SELECT * FROM t WHERE",
    failure(error('Syntax', 'valid WHERE condition', pos(last, last)))).

test078 :-
  test(parser, lex_parse, "SELECT * FROM t WHERE a",
    failure(error('Syntax', 'a comparison operator', pos(last, last)))).
  
test079 :-
  test(parser, lex_parse, "SELECT * FROM t WHERE a=",
    failure(error('Syntax', 'valid expression', pos(last, last)))).  

test080 :-
  test(parser, lex_parse, "SELECT N FROM E ORDER BY by",
    failure(error('Syntax', 'valid ORDER BY criteria', pos(1, 26)))).

test081 :-
  test(parser, lex_parse, "select top 1 distinct * from t fetch first 1 rows only",
    failure(error('Semantic', 'Only one TOP/LIMIT/FETCH specification is allowed', pos(void, void)))).

%DQLstmt SELECT(II) STATEMENTS
test082 :-
  test(parser, lex_parse, 'test/test018.sql', 
    [(select(all,top(all),no_offset,*,[],from([(division((t1,_),(t2,_)),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t1,_),(t2,_),true),_),(inner_join((t3,_),(t4,_),true),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(full_join((t,_),(s,_),attr(t,a,_)=attr(s,a,_)),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t1,_),(t2,_),equijoin([attr(_,c,_)])),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(left_join((s,_),(right_join((q,_),(sp,_),attr(q,sno,_)=attr(sp,sno,_)),_),attr(s,sno,_)=attr(q,sno,_)),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(left_join((t1,_),(t,_),equijoin(natural)),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(right_join((t,_),(s,_),equijoin(natural)),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(right_join((t1,[table1|_]),(t2,_),true),[table2|_])]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_,number(int))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(cte(int(1),number(int)),a,number(int))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(cte(int(1),number(int)),a,number(int)),expr(attr(_,a,_)+cte(int(1),number(int)),_,number(_))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(attr(_,a,_)+cte(int(2),number(int)),_,number(_)),expr(cte(int(1),number(int)),a,number(int)),expr(attr(_,a,_)+cte(int(1),number(int)),_,number(_))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(attr(_,a,_)+cte(int(2),number(int)),_,number(_)),expr(cte(int(1),number(int))+cte(int(1),number(int)),a,number(_)),expr(attr(_,a,_)+cte(int(1),number(int)),_,number(_))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t,_),(inner_join((s,_),(u,_),true),_),true),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t,_),(inner_join((s,_),(u,_),true),_),true),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t,_),(inner_join((s,_),(u,_),true),_),true),_)]),where(true),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,*,[],from([(inner_join((t,_),(inner_join((s,_),(u,_),true),_),true),_)]),where(true),group_by([]),having(true),order_by([],[])),_)]).

%DQLstmt SELECT(II) STATEMENTS error
test083 :-
  test(parser, lex_parse, "select * from 1 right join t2",
    failure(error('Syntax', 'a valid relation', pos(1, 15)))).  

test084 :-
  test(parser, lex_parse, "select * from t1 t s join t2",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 20)))).

test085 :-
  test(parser, lex_parse, "select * from t1 1 join t2",
    failure(error('Syntax', 'valid SQL statement (SELECT, CREATE, DELETE, INSERT, UPDATE, DROP, RENAME, ALTER, SHOW, DESCRIBE, WITH, ASSUME, COMMIT, ROLLBACK, SAVEPOINT)', pos(1, 18)))).

%DQLstmt UNION, EXCEPT, MINUS, INTERSECT STATEMENTS
test086 :-
  test(parser, lex_parse, 'test/test019.sql', 
    [(union(distinct,(select(all,top(all),no_offset,*,[],from([(a,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(b,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),
    create_view(sql,(union(distinct,(select(all,top(all),no_offset,*,[],from([(father,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(mother,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),parent(parent:_,child:_)),
    (union(distinct,(select(all,top(all),no_offset,*,[],from([(p,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(union(distinct,(select(all,top(all),no_offset,*,[],from([(q,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(union(distinct,(select(all,top(all),no_offset,[expr(attr(pqs,x,_),_,_),expr(attr(p,y,_),_,_)],[],from([(pqs,_),(p,_)]),where(attr(pqs,y,_)=attr(p,x,_)),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,[expr(attr(pqs,x,_),_,_),expr(attr(q,y,_),_,_)],[],from([(pqs,_),(q,_)]),where(attr(pqs,y,_)=attr(q,x,_)),group_by([]),having(true),order_by([],[])),_)),_)),_)),_),
    create_view(sql,(union(all,(select(all,top(all),no_offset,[expr(cte(int(0),number(int)),_,number(int))],[],from([(dual,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,[expr(attr(_,n,_)+cte(int(1),number(int)),_,number(_))],[],from([(n,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),n(n:_)),
    (except(distinct,(select(all,top(all),no_offset,*,[],from([(a,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(b,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),
    (except(distinct,(select(all,top(all),no_offset,[expr(attr(_,dni,_),_,_)],[],from([(vista1,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(union(distinct,(select(all,top(all),no_offset,[expr(attr(_,dniemp,_),_,_)],[],from([(distribucion,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,[expr(attr(_,dnidir,_),_,_)],[],from([(proyectos,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_)),_),
    create_view(sql,(intersect(distinct,(select(all,top(all),no_offset,[expr(attr(_,dni,_),_,_)],[],from([(programadores,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,[expr(attr(_,dni,_),_,_)],[],from([(analistas,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),vista2),
    (select(all,top(all),no_offset,*,[],from([(s,_)]),where(not_in([expr(attr(s,a,_),_,_)],(select(all,top(all),no_offset,[expr(attr(_,a,_),_,_)],[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_))),group_by([]),having(true),order_by([],[])),_),
    (select(all,top(all),no_offset,[expr(attr(_,a,_),_,_)],[],from([(s,_)]),where(not_in([expr(attr(_,b,_),_,_)],(union(distinct,(select(all,top(all),no_offset,[expr(attr(_,a,_),_,_)],[],from([(t,_)]),where(attr(t,a,_)=attr(s,a,_)),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,[expr(attr(_,a,_),_,_)],[],from([(t,_)]),where(attr(_,b,_)=cte(int(1),number(int))),group_by([]),having(true),order_by([],[])),_)),_))),group_by([]),having(true),order_by([],[])),_),
    (union(distinct,(select(all,top(all),no_offset,*,[],from([(s,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),
    (intersect(distinct,(select(all,top(all),no_offset,*,[],from([(s,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_),
    (except(distinct,(select(all,top(all),no_offset,*,[],from([(s,_)]),where(true),group_by([]),having(true),order_by([],[])),_),(select(all,top(all),no_offset,*,[],from([(t,_)]),where(true),group_by([]),having(true),order_by([],[])),_)),_)]).

%DQLstmt UNION, EXCEPT, MINUS, INTERSECT STATEMENTS error  
test087 :-
  test(parser, lex_parse, "select * from a union selct * from b",
    failure(error('Syntax', 'SELECT statement', pos(1, 23)))).

test088 :-
  test(parser, lex_parse, "SELECT * FROM s UNION Al SELECT * FROM q",
    failure(error('Syntax', 'SELECT statement', pos(1, 23)))).

%DQLstmt WITH and ASSUME STATEMENTS
test089 :-
  test(parser, lex_parse, 'test/test020.sql', 
    [(with((select(all,top(all),no_offset,*,[],from([(l,_34544)]),where(true),group_by([]),having(true),order_by([],[])),_34414),[(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_34054,number(int))],[],from([(dual,_34016)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_34240))]),_33832),(with((select(all,top(all),no_offset,*,[],from([(l,_36172)]),where(true),group_by([]),having(true),order_by([],[])),_36042),[(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_34840,number(int))],[],from([(dual,_34802)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_34994)),not((select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_35256,number(int))],[],from([(dual,_35218)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_35444))),(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_35682,number(int))],[],from([(dual,_35644)]),where(true),group_by([]),having(true),order_by([],[])),l(a:_35868))]),_34616),(with((select(all,top(all),no_offset,*,[],from([(connect,_37736)]),where(true),group_by([]),having(true),order_by([],[])),_37606),[(select(all,top(all),no_offset,[expr(attr(flight,origin,_36498),_36482,_36484),expr(attr(connect,destination,_36628),_36612,_36614)],[],from([(flight,_36758),(connect,_36780)]),where(attr(flight,destination,_36868)=attr(connect,origin,_37064)),group_by([]),having(true),order_by([],[])),connect(origin:_37366,destination:_37430))]),_36244),(with((select(all,top(all),no_offset,*,[],from([(travel,_39278)]),where(true),group_by([]),having(true),order_by([],[])),_39148),[(union(distinct,(select(all,top(all),no_offset,[expr(cte(str(mad),string(_38072)),_38048,string(_38072)),expr(cte(str(lon),string(_38162)),_38138,string(_38162)),expr(cte(frac(2,0),number(frac)),_38226,number(frac))],[],from([(dual,_38008)]),where(true),group_by([]),having(true),order_by([],[])),_37970),(select(all,top(all),no_offset,[expr(cte(str(par),string(_38496)),_38472,string(_38496)),expr(cte(str(ber),string(_38586)),_38562,string(_38586)),expr(cte(frac(3,0),number(frac)),_38650,number(frac))],[],from([(dual,_38432)]),where(true),group_by([]),having(true),order_by([],[])),_38394)),flight(origin:_38842,destination:_38906,time:_38970))]),_37808),(with((select(all,top(all),no_offset,[expr(attr(_40204,a,_40208),_40192,_40194)],[],from([(p,_40298)]),where(true),group_by([]),having(true),order_by([],[])),_40128),[(select(all,top(all),no_offset,[expr(attr(_39900,a,_39904),_39888,_39890)],[],from([(t,_39994)]),where(true),group_by([]),having(true),order_by([],[])),p(a:_39666))]),_39350),(with((select(all,top(expr(10,_41658,number(int))),no_offset,[expr(attr(_41698,a,_41702),_41686,_41688)],[],from([(p,_41792)]),where(true),group_by([]),having(true),order_by([],[])),_41546),[(union(distinct,(select(all,top(all),no_offset,[expr(cte(int(1),number(int)),_40936,number(int))],[],from([(dual,_40898)]),where(true),group_by([]),having(true),order_by([],[])),_40860),(select(all,top(all),no_offset,[expr(attr(_41178,a,_41182)+cte(int(1),number(int)),_41166,number(_41228))],[],from([(p,_41408)]),where(true),group_by([]),having(true),order_by([],[])),_41102)),p(a:_40686))]),_40370),create_view(sql,(with((select(all,top(all),no_offset,*,[],from([(path,_44300)]),where(true),group_by([]),having(true),order_by([],[])),_44170),[(union(distinct,(select(all,top(all),no_offset,*,[],from([(edge,_43102)]),where(true),group_by([]),having(true),order_by([],[])),_42988),(select(all,top(all),no_offset,[expr(attr(path,origin,_43354),_43338,_43340),expr(attr(edge,destination,_43484),_43468,_43470)],[],from([(path,_43614),(edge,_43636)]),where(attr(path,destination,_43724)=attr(edge,origin,_43920)),group_by([]),having(true),order_by([],[])),_43272)),path(origin:_42710,destination:_42774))]),_44348),paths(origin:_42112,destination:_42176)),(with((select(all,top(all),no_offset,*,[],from([(reaches,_46506)]),where(true),group_by([]),having(true),order_by([],[])),_46376),[(union(distinct,(select(all,top(all),no_offset,[expr(attr(_45088,frm,_45092),_45076,_45078),expr(attr(_45162,to,_45166),_45150,_45152)],[],from([(flights,_45240)]),where(true),group_by([]),having(true),order_by([],[])),_45010),(select(all,top(all),no_offset,[expr(attr(r1,frm,_45492),_45476,_45478),expr(attr(r2,to,_45622),_45606,_45608)],[],from([(reaches,[r1|_45752]),(reaches,[r2|_45808])]),where(attr(r1,to,_45930)=attr(r2,frm,_46126)),group_by([]),having(true),order_by([],[])),_45410)),reaches(frm:_44732,to:_44796))]),_44416),create_view(sql,(with((except(distinct,(select(all,top(all),no_offset,[expr(attr(_50358,frm,_50362),_50346,_50348),expr(attr(_50432,to,_50436),_50420,_50422)],[],from([(reaches,_50510)]),where(attr(_50594,airline,_50598)=cte(str('UA'),string(_50746))),group_by([]),having(true),order_by([],[])),_50280),(select(all,top(all),no_offset,[expr(attr(_51058,frm,_51062),_51046,_51048),expr(attr(_51132,to,_51136),_51120,_51122)],[],from([(reaches,_51210)]),where(attr(_51294,airline,_51298)=cte(str('AA'),string(_51462))),group_by([]),having(true),order_by([],[])),_50980)),_50214),[(select(all,top(all),no_offset,[expr(attr(_47682,airline,_47686),_47670,_47672),expr(attr(_47758,frm,_47762),_47746,_47748),expr(attr(_47832,to,_47836),_47820,_47822)],[],from([(flights,_47910)]),where(true),group_by([]),having(true),order_by([],[])),triples(airline:_47378,frm:_47426,to:_47474)),(union(distinct,(select(all,top(all),no_offset,*,[],from([(triples,_48660)]),where(true),group_by([]),having(true),order_by([],[])),_48546),(select(all,top(all),no_offset,[expr(attr(triples,airline,_48912),_48896,_48898),expr(attr(triples,frm,_49044),_49028,_49030),expr(attr(reaches,to,_49174),_49158,_49160)],[],from([(triples,_49304),(reaches,_49326)]),where(and(attr(triples,to,_49414)=attr(reaches,frm,_49610),attr(triples,airline,_49774)=attr(reaches,airline,_49970))),group_by([]),having(true),order_by([],[])),_48830)),reaches(airline:_48202,frm:_48266,to:_48330))]),_51560),reach(frm:_46826,to:_46890)),(with((select(all,top(all),no_offset,[expr(attr(_54222,x,_54226),_54210,_54212)],[],from([(odd,_54316)]),where(true),group_by([]),having(true),order_by([],[])),_54146),[(union(all,(select(all,top(all),no_offset,[expr(cte(int(0),number(int)),_52116,number(int))],[],from([(dual,_52078)]),where(true),group_by([]),having(true),order_by([],[])),_52040),(select(all,top(all),no_offset,[expr(attr(odd,x,_52346)+cte(int(1),number(int)),_52330,number(_52448))],[],from([(odd,_52612)]),where(attr(_52696,x,_52700)<cte(int(10),number(int))),group_by([]),having(true),order_by([],[])),_52266)),even(x:_51898)),(select(all,top(all),no_offset,[expr(attr(even,x,_53446)+cte(int(1),number(int)),_53430,number(_53548))],[],from([(even,_53712)]),where(attr(_53796,x,_53800)<cte(int(10),number(int))),group_by([]),having(true),order_by([],[])),odd(x:_53208))]),_51628),(with((select(all,top(all),no_offset,*,[],from([(p,_56290)]),where(true),group_by([]),having(true),order_by([],[])),_56160),[(except(distinct,(select(all,top(all),no_offset,*,[],from([(r,_54914)]),where(true),group_by([]),having(true),order_by([],[])),_54800),(select(all,top(all),no_offset,*,[],from([(q,_55152)]),where(true),group_by([]),having(true),order_by([],[])),_55038)),p(x:_54658)),(except(distinct,(select(all,top(all),no_offset,*,[],from([(r,_55752)]),where(true),group_by([]),having(true),order_by([],[])),_55622),(select(all,top(all),no_offset,*,[],from([(p,_56022)]),where(true),group_by([]),having(true),order_by([],[])),_55892)),q(x:_55448))]),_54388),(with((select(all,top(all),no_offset,[expr(attr(_57336,a,_57340),_57324,_57326)],[],from([(t,_57414),(media,_57436)]),where(attr(_57520,a,_57524)>attr(_57676,m,_57680)),group_by([]),having(true),order_by([],[])),_57260),[(select(all,top(all),no_offset,[expr(avg(attr(_56976,a,_56980)),_56900,number(float))],[],from([(t,_57126)]),where(true),group_by([]),having(true),order_by([],[])),media(m:_56678))]),_56362),(with((select(all,top(all),no_offset,[expr(cte(int(1),number(int))/attr(_58674,a,_58678),_58568,number(float))],[],from([(v,_58790)]),where(attr(_58874,a,_58878)>cte(int(0),number(int))),group_by([]),having(true),order_by([],[])),_58504),[(select(all,top(all),no_offset,[expr(cte(int(0),number(int)),_58342,number(int))],[],from([(dual,_58304)]),where(true),group_by([]),having(true),order_by([],[])),v(a:_58108))]),_57792)]).

%DQLstmt WITH and ASSUME STATEMENTS error
test090 :-
  test(parser, lex_parse, "assume select 1 in t(a)",
    failure(error('Syntax', 'SELECT statement', pos(last, last)))).

test091 :-
  test(parser, lex_parse, "with v(a) as select 1",
    failure(error('Syntax', 'SELECT statement', pos(last, last)))).  

test092 :-
  test(parser, lex_parse, "with v(a) as select 1 from dual",
    failure(error('Syntax', 'SELECT statement', pos(last, last)))).

test093 :-
  test(parser, lex_parse, "with v(a) select 1 select * from v",
    failure(error('Syntax', 'AS', pos(1, 11)))).

test094 :-
  test(parser, lex_parse, "with 2 select 1 select * from v",
    failure(error('Syntax', 'schema', pos(1, 6)))).

%DMLstmt INSERT INTO STATEMENTS
test095 :-
  test(parser, lex_parse, 'test/test021.sql', 
    [insert_into(t1,[a1],[[default]]),
    insert_into(t3,[a3,b3,c3],[[default,default,default]]),
    insert_into(t2,[a2,b2],[[cte(float(1,5,2),number(float)),cte(str('2'),string(_))]]),
    insert_into(t2,[a2,b2],[[cte(int(1),number(int)),cte(str('Ventas'),string(_))],[cte(int(2),number(int)),cte(str('Contabilidad'),string(_))]]),
    insert_into(t3,[a3,b3,c3],[[cte(str('1'),string(_)),cte(str(n1),string(_)),cte(str(d1),string(_))],[cte(str('2'),string(_)),cte(str(n2),string(_)),cte(str(d2),string(_))]]),
    insert_into(t1,[a1],[[default]]),
    insert_into(t3,[a3,b3,c3],[[cte(time(12,0,1),datetime(time)),cte(frac(2,5),number(frac)),-cte(int(1),number(int))],[cte(date(2012,1,1),datetime(date)),default,cte(str('A'),string(_))]]),
    insert_into(t1,[a1],[[cte(datetime(-2022,6,1,13,45,30),datetime(datetime))],[cte(datetime(2023,6,17,17,35,45),datetime(datetime))]]),
    insert_into(t1,[a1],[[-cte(float(1,3,-2),number(float))]]),
    insert_into(t3,[a3,b3,c3],[[cte(int(1),number(int)),cte(int(2),number(int)),cte(str(a),string(_))]]),
    insert_into(t2,[a3,b3,c3],[[cte(int(1),number(int)),cte(int(2),number(int)),cte(str(a),string(_))]]),
    insert_into(c,[],(select(all,top(all),no_offset,[expr(attr(a,a,_),_,_),expr(attr(b,b,_),_,_)],[],from([(a,_),(b,_)]),where(or(attr(a,a,_)=attr(b,b,_),attr(b,b,_)=cte(str(a1),string(_)))),group_by([]),having(true),order_by([],[])),_))]).

%DMLstmt INSERT INTO STATEMENTS error
test096 :-
  test(parser, lex_parse, "insert into 222 values (1, '1')",
    failure(error('Syntax', 'table name', pos(1, 13)))).

test097 :-
  test(parser, lex_parse, "insert into t1 default v",
    failure(error('Syntax', 'VALUES', pos(1, 24)))).

test098 :-
  test(parser, lex_parse, "insert into t values 1",
    failure(error('Syntax', 'opening parenthesis ''(''', pos(1, 22)))).

test099 :-
  test(parser, lex_parse, "insert into t2(a3 c) values (1,'a')",
    failure(error('Syntax', 'comma or closing parenthesis '')''', pos(1, 19)))).  

test100 :-
  test(parser, lex_parse, "insert into t2 values(1 '2')",
    failure(error('Syntax', 'comma or closing parenthesis '')''', pos(1, 25)))).

test101 :-
  test(parser, lex_parse, "insert into t()",
    failure(error('Syntax', 'a sequence of columns between parentheses', pos(1, 15)))).

test102 :-
  test(parser, lex_parse, "insert into t(1,2)",
    failure(error('Syntax', 'a sequence of columns between parentheses', pos(1, 15)))).
  
test103 :-
  test(parser, lex_parse, "insert into t",
    failure(error('Syntax', 'VALUES, select statement, or DEFAULT VALUES', pos(last, last)))).

test104 :-
  test(parser, lex_parse, "insert into t(a) (1)",
    failure(error('Syntax', 'VALUES, select statement, or DEFAULT VALUES', pos(1, 18)))).

test105 :-
  test(parser, lex_parse, "insert into t1 defa values",
    failure(error('Syntax', 'VALUES, select statement, or DEFAULT VALUES', pos(1, 16)))).

test106 :-
  test(parser, lex_parse, "INSERT INTO  t1 VALUES (DATE 2000)",
    failure(error('Syntax', 'string', pos(1, 30)))).

test107 :-
  test(parser, lex_parse, "INSERT INTO t1 VALUES (TIME '122:07:01')",
    failure(error('Syntax', 'TIME String format must be ''Int(hour):Int(minute):Int(second)''', pos(1, 29)))).  

test108 :-
  test(parser, lex_parse, "INSERT INTO t1 VALUES (DATE '20116-00-02')",
    failure(error('Syntax', 'DATE String format must be [BC] ''Int(Year)-Int(month)-Int(day)''', pos(1, 29)))).

test109 :-
  test(parser, lex_parse, "INSERT INTO  t1 VALUES (TIMESTAMP BC '2023-06-01 1345:30')",
    failure(error('Syntax', 'DATETIME/TIMESTAMP String format must be [BC] ''Int(Year)-Int(month)-Int(day) Int(hour):Int(minute):Int(second)''', pos(1, 38)))).

test110 :-
  test(parser, lex_parse, "insert into t3 values (1, '1')",
    failure(error('Semantic', 'Unmatching number of values => 2 (must be 3)', pos(1, 23)))).
  
test111 :-
  test(parser, lex_parse, "insert into t3(a2,a3) values (1,2,'a')",
    failure(error('Semantic', 'Unmatching number of values => 3 (must be 2)', pos(1, 30)))).

test112 :-
  test(parser, lex_parse, "insert into t2(a3,b3,a3) values (1,2,'a')",
    failure(error('Semantic', 'Column names must be different in [a3,b3,a3]', pos(1, 15)))).

%DMLstmt DELETE and UPDATE STATEMENTS
test113 :-
  test(parser, lex_parse, 'test/test022.sql', 
    [delete_from((t1,_),true),
    delete_from((t1,[t,_]),true),
    delete_from((t1,[newTableName,_]),true),
    delete_from((t,_),attr(_,b,_)=cte(str(a1),string(_))),
    delete_from((t,_),attr(_,edad,_)>cte(int(0),number(int))),
    delete_from((t,_),in([expr(attr(_,b,_),_,_)],(select(all,top(all),no_offset,*,[],from([(a,_)]),where(true),group_by([]),having(true),order_by([],[])),_))),
    delete_from((t,_),not(exists((select(all,top(all),no_offset,*,[],from([(c,_)]),where(attr(c,a,_)=attr(a,a,_)),group_by([]),having(true),order_by([],[])),_)))),
    delete_from((t,_),and(attr(a,age,_)>cte(int(25),number(int)),attr(_,salary,_)>cte(int(50000),number(int)))),
    delete_from((t,_),and(attr(_,city,_)=cte(str('San Francisco'),string(_)),or(attr(_,age,_)>=cte(int(25),number(int)),attr(_,city,_)=cte(str('New York'),string(_))))),
    delete_from((t,_),or(and(attr(_,status,_)=cte(str(active),string(_)),attr(_,city,_)=cte(str('London'),string(_))),attr(_,age,_)<cte(int(30),number(int)))),
    update((t,_),[expr(a,_,string),expr(cte(int(1),number(int)),_,number(int))],true),
    update((t1,[d,_]),[expr(a,_,string),expr(cte(int(1),number(int)),_,number(int))],true),
    update((empleados,_),[expr(sueldo,_,string),expr(attr(_,sueldo,_)*cte(frac(1,1),number(frac)),_,number(_))],true),
    update((t1,[c,_]),[expr(a,_,string),expr((select(all,top(all),no_offset,[expr(attr(_,b,_),_,_)],[],from([(s,_)]),where(attr(s,a,_)=attr(c,a,_)),group_by([]),having(true),order_by([],[])),_),_,_)],attr(_,a,_)=cte(int(1),number(int)))]).

%DMLstmt DELETE and UPDATE STATEMENTS error
test114 :-
  test(parser, lex_parse, "delete * from t",
    failure(error('Syntax', 'FROM', pos(1, 8)))).

test115 :-
  test(parser, lex_parse, "delete from",
    failure(error('Syntax', 'table name', pos(last, last)))).

test116 :-
  test(parser, lex_parse, "delete from t where >0",
    failure(error('Syntax', 'valid WHERE condition', pos(1, 21)))).
  
test117 :-
  test(parser, lex_parse, "update t set a=1 where",
    failure(error('Syntax', 'valid WHERE condition', pos(last, last)))).

test118 :-
  test(parser, lex_parse, "delete from t WHERE ((name = 'John Doe')",
    failure(error('Syntax', 'closing parenthesis '')''', pos(1, 40)))).

test119 :-
  test(parser, lex_parse, "delete from t WHERE (((a.age > 25)) AND ((salary > 50000))",
    failure(error('Syntax', 'closing parenthesis '')''', pos(1, 58)))).

test120 :-
  test(parser, lex_parse, "update t a=1",
    failure(error('Syntax', 'SET', pos(1, 10)))).
  
test121 :-
  test(parser, lex_parse, "update t set",
    failure(error('Syntax', 'sequence of column assignments Col=Expr', pos(last, last)))).

test122 :-
  test(parser, lex_parse, "update t set a=1,",
    failure(error('Syntax', 'sequence of column assignments Col=Expr', pos(last, last)))).

test123 :-
  test(parser, lex_parse, "update t set a=1, w",
    failure(error('Syntax', 'sequence of column assignments Col=Expr', pos(1, 19)))).

test124 :-
  test(parser, lex_parse, "update t set a=1, w=",
    failure(error('Syntax', 'an expression', pos(last, last)))).