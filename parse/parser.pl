:- module(parser,
          [ lex_parse/2,
            lex_parse/1,
            parse/2,
            is_number/1 ]).

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
  forall(member(Tree, SyntaxTrees), writeln(Tree)).

/*
lex_parse(Input) :-
  lex(Input, Tokens),
  phrase(filter_tokens(FilteredTokens, []), Tokens),
  !,
  parse(FilteredTokens, SyntaxTrees),
  print(SyntaxTrees).
*/

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
% CREATE TABLE
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                          # 'TABLE or VIEW',
  complete_constrained_typed_schema(Schema,Ctrs) # 'typed schema',
  % syntax_check_redef(Schema),  If attempting to redefine a datalog keyword, exception is thrown.
  {atom_concat(CR,'_table',CRT),
  CRTSchema=..[CRT,Schema,Ctrs]},
  !.

% CREATE TABLE LIKE
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                          # 'TABLE or VIEW',
  tablename(TableName)                # 'table name',
  % syntax_check_redef(TableName),  If attempting to redefine a datalog keyword, exception is thrown.
  opening_parentheses_star(N),
  cmd(like)                           # 'LIKE',
  sql_user_identifier(ExistingTableName) # 'table identifier',
  closing_parentheses_star(N),
  {atom_concat(CR,'_table_like',CRT),
   CRTSchema=..[CRT,TableName,ExistingTableName]},
  !.

% CREATE TABLE AS
ddlStmt([CRTSchema|STs]/STs) -->
  create_or_replace(CR),
  cmd(table)                          # 'TABLE or VIEW',
  push_syntax_error(['Expected table schema'],Old2),
  create_view_schema(Schema),
  % syntax_check_redef(Schema),  If attempting to redefine a datalog keyword, exception is thrown.
  opening_parentheses_star(N),
  cmd(as)                             # 'AS',
  dqlStmt([(LSQLst,Schema)|STs]/STs)  # 'valid SQL DQL statement (SELECT, WITH or ASSUME)',
  closing_parentheses_star(N),
  {atom_concat(CR,'_table_as',CRT),
   CRTSchema=..[CRT,(LSQLst,_AS),Schema]},
  !.

% CREATE DATABASE
ddlStmt([create_database(DBName)|STs]/STs) -->
  cmd(create)                         # 'CREATE',
  cmd(database)                       # 'TABLE, VIEW or DATABASE',
  optional_database_name(DBName)      # 'database name',
  !.

create_or_replace(create) -->
  cmd(create)                         # 'CREATE'.

create_or_replace(create_or_replace) -->
  cmd(create)                         # 'CREATE',
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
  punct(',')                          # 'comma',
  constrained_typed_columns(CTs,RCtrs),
  {append(CCtrs,RCtrs,Ctrs)}.

constrained_typed_column(C:T,Ctrs) --> 
  typed_column(C:T),
  column_constraint_definitions(C,Ctrs) # 'column constraints'.
constrained_typed_column(C:T,[true]) --> 
  typed_column(C:T).

typed_column(C:T) -->
  colname(C)                          # 'a column identifier',
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
  cmd(constraint)                     # 'CONSTRAINT',
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
  cmd(delete)                         # 'DELETE'.
triggered_event(update) -->
  cmd(update)                         # 'UPDATE'.
  
referential_action(cascade) -->
  cmd(cascade)                        # 'CASCADE'.
referential_action(set_null) -->
  cmd(set)                            # 'SET',
  cmd(null)                           # 'NULL'.
referential_action(set_default) -->
  cmd(set)                            # 'SET',
  cmd(default)                        # 'DEFAULT'.
referential_action(restrict) -->
  cmd(restrict)                       # 'RESTRICT'.
referential_action(no_action) -->
  cmd(no)                             # 'NO',
  cmd(action)                         # 'ACTION'.

check_constraint(fd(Ls,Rs)) -->
  column_tuple(Rs)                    # 'a column sequence between parentheses',
  cmd(determined)                     # 'DETERMINED',
  cmd(by)                             # 'BY',
  column_tuple(Ls)                    # 'a column sequence between parentheses'.
check_constraint(sql_check_constraint(WhereCondition)) -->
  where_condition(WhereCondition).

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


/*
opening_parentheses_star(N),
cmd(as)                             # 'AS',
dqlStmt((LSQLst,Schema))            # 'valid SQL DQL statement (SELECT, WITH or ASSUME)',
closing_parentheses_star(N),
{atom_concat(CR,'_table_as',CRT),
  CRTSchema=..[CRT,(LSQLst,_AS),Schema]},
!.

create_view_schema(Schema) -->
  complete_untyped_schema(Schema),
  !.
create_view_schema(Name) -->
  sql_user_identifier(Name).

complete_untyped_schema(Schema) -->
  sql_user_identifier(Name),
  punct('(')                          # 'opening parenthesis ''(''',
  my_untyped_columns(Cs),
  punct(')')                          # 'closing parenthesis '')''',
  {Schema =.. [Name|Cs]}.
*/

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % DQL (Data Query Language) statements
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  punct('(')                          # 'opening parenthesis ''(''',
  dqlStmt(SQLst)                      # 'todo',
  punct(')')                          # 'closing parenthesis '')'''.

where_clause(WhereCondition) -->
  cmd(where)                          # 'WHERE',
  opening_parentheses_star(N),
  where_condition(WhereCondition)     # 'WHERE condition',
  closing_parentheses_star(N).
where_clause(true) -->
  [].


where_condition(C) --> 
  sql_condition(C).

/*on_condition(C) --> 
  sql_condition(C).

sql_having_condition(C) --> 
  sql_condition(C).*/

  
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
dmlStmt([insert_into(TableName,Colnames, Vs)|STs]/STs) -->
  cmd(insert)                         # 'INSERT',
  cmd(into)                           # 'INTO',
  tablename(TableName)                # 'table name',
  current_position(Position),
  punct('(')                          # 'opening parenthesis or DEFAULT',
  column_name_list(Colnames)          # 'a sequence of column names',
  punct(')')                          # 'closing parenthesis '')''',
  {(my_remove_duplicates(Colnames,Colnames) -> true ;
  set_error_with_parameter('Semantic', 'Column names must be different in ~w' , [Colnames], Position),
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

% DELETE FROM ... WHERE 
dmlStmt([delete_from(Table,WhereCondition)|STs]/STs) -->
  cmd(delete)                         # 'DELETE',
  cmd(from)                           # 'FROM',
  p_ren_tablename(Table),
  where_clause(WhereCondition),
  !.

% UPDATE ... SET ... [WHERE ]
dmlStmt([update(Table,Assignments,WhereCondition)|STs]/STs) -->
  cmd(update)                         # 'UPDATE',
  p_ren_tablename(Table),
  cmd(set)                            # 'SET',
  update_assignments(Assignments)     # 'sequence of column assignments Col=Expr',
  where_clause(WhereCondition),
  !.

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


%update_assignments(assignments)
update_assignments([Column,Expression]) -->
  update_assignment(Column,Expression).
update_assignments([Column,Expression|Assignments]) -->
  update_assignment(Column,Expression),
  punct(',')                          # 'comma', 
  update_assignments(Assignments).

update_assignment(expr(ColumnName,none,string),Expression) -->
  column(attr(_T,ColumnName,_AS)),
  comparisonOp('=')                   # 'equals ''=''', 
  sql_proj_expression(Expression,_Type).

/*
dql_or_constant_tuples(_A,R) -->
  dqlStmt(R).
dql_or_constant_tuples(A,R) -->
  punct('(')                          # 'opening parenthesis ''(''',
  sql_ground_tuple_list(A,Ts),
  punct(')')                          # 'closing parenthesis '')''',
  {in_tuples_to_DQL(Ts,R)}.
dql_or_constant_tuples(_,R) -->
  my_sql_ground_tuple(_,Cs),
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
  args_to_exprs(Cs,Es).*/

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
  sql_constants(Cs),
  punct(')')                          # 'closing parenthesis or comma',
  !,
  {length(Cs,TL),
    (L=TL -> true ;
      set_error_with_parameter('Semantic', 'Unmatching number of values => ~w (must be ~w)' , [TL, L], Position),
      !, fail)}.

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
% SQL Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% char(n)
sql_type(string(char(N))) -->
  sql_character_type_id,
  punct('(')                          # 'opening parenthesis ''(''',
  int(N)                              # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
% char  
sql_type(string(char(1))) -->
  sql_character_type_id.
% varchar(n)
sql_type(string(varchar(N))) -->
  sql_varchar_type_id,
  punct('(')                          # 'opening parenthesis ''(''',
  int(N)                              # 'a positive integer',
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
  cmd(float)                          # 'FLOAT', 
  punct('(')                          # 'opening parenthesis ''(''',
  int(_Int)                           # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
sql_type(number(float)) -->
  sql_float_type_id.
sql_type(number(float)) -->
  sql_numeric_type_id,
  punct('(')                          # 'opening parenthesis ''(''',
  int(_Int)                           # 'a positive integer',
  punct(',')                          # 'comma',
  int(_Frac)                          # 'a positive integer',
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
  cmd(real)                           # 'REAL'.
sql_float_type_id -->
  cmd(float)                          # 'FLOAT'.
  
sql_varchar_type_id -->
  cmd(varchar2)                       # 'VARCHAR2'.
sql_varchar_type_id -->
  cmd(varchar)                        # 'VARCHAR'.
sql_varchar_type_id -->
  cmd(text)                           # 'text'.
  
sql_character_type_id -->
  cmd(character)                      # 'CHARACTER'.
sql_character_type_id -->
  cmd(char)                           # 'CHAR'.
  
sql_integer_type_id -->
  cmd(integer)                        # 'INTEGER'.
sql_integer_type_id -->
  cmd(int)                            # 'INT'.
sql_integer_type_id -->
  cmd(smallint)                       # 'SMALLINT'.
  
sql_numeric_type_id -->
  cmd(number)                         # 'NUMBER'.
sql_numeric_type_id -->
  cmd(numeric)                        # 'NUMERIC'.
sql_numeric_type_id -->
  cmd(decimal)                        # 'DECIMAL'.
  
optional_integer_range(R) -->
  punct('(')                          # 'opening parenthesis ''(''',
  int(R)                              # 'a positive integer',
  punct(')')                          # 'closing parenthesis '')'''.
optional_integer_range(_R) -->
  [].


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
sql_constant(cte(C,string(none))) -->
  value(C),
  !.
sql_constant(default) -->
  cmd(default)                        # 'DEFAULT',
  !.
sql_constant(cte('$NULL'(N),none)) -->
  cmd(null)                           # 'NULL',
  !,
  {get_null_id(N)}. % :::WARNING: Needed?
sql_constant(C) -->
  sql_date_constant(C),
  !.

sql_constant(_) -->
  set_error('Syntax', 'Number, String, DATE String, TIME String, TIMESTAMP String, NULL').

%date_constant
sql_date_constant(cte(date(Y,M,D),datetime(date))) -->
  cmd(date)                           # 'DATE',
  optional_cmd(bc,BC),
  current_position(Position),
  value(str(C))                       # 'string',
  !,
  { 
    string_chars(C, Chars),
    (phrase(valid_date_format, Chars) -> true; set_error_with_parameter('Syntax', 'DATE String format must be [BC] \'Int-Int-Int\'' , [], Position),
    !, fail),
    split_string(C, "-", "", DateParts),
    maplist(number_string, [YRaw, M, D], DateParts),
    adjust_year(BC, YRaw, Y)            % Adjust year if BC is true
  }.  

sql_date_constant(cte(time(H,Mi,Se),datetime(time))) -->
  cmd(time)                           # 'time',
  current_position(Position),
  value(str(C))                       # 'string',
  { 
    string_chars(C, Chars),
    (phrase(valid_time_format, Chars) -> true; set_error_with_parameter('Syntax', 'TIME String format must be \'Int:Int:Int\'' , [], Position),
    !, fail),
    split_string(C, ":", "", TimeParts),
    maplist(number_string, [H, Mi, Se], TimeParts)
  }.

sql_date_constant(cte(datetime(Y,M,D,H,Mi,S),datetime(datetime))) -->
  (cmd(datetime)                       # 'DATETIME';
  cmd(timestamp)                       # 'TIMESTAMP'),
  optional_cmd(bc,BC),
  current_position(Position),
  value(str(C))                       # 'string',
  !,
  { 
    string_chars(C, Chars),
    (phrase(valid_datetime_format, Chars) -> true; set_error_with_parameter('Syntax', 'DATETIME/TIMESTAMP String format must be [BC] \'Int-Int-Int Int:Int:Int\'' , [], Position),
    !, fail),
    split_string(C, " ", "", [DateString,TimeString]),
    split_string(DateString, "-", "", DateParts),
    split_string(TimeString, ":", "", TimeParts),
    append(DateParts, TimeParts, DateTimeParts),
    maplist(number_string, [YRaw, M, D, H, Mi, S], DateTimeParts),
    adjust_year(BC, YRaw, Y)            % Adjust year if BC is true
  }.  

% define valid_date_format
valid_date_format -->
  one_to_four_digits, ['-'], one_or_two_digits, ['-'], one_or_two_digits.

valid_time_format -->
  one_or_two_digits, [':'], one_or_two_digits, [':'], one_or_two_digits.

valid_datetime_format -->
  valid_date_format, [' '], valid_time_format.


one_to_four_digits --> digit.
one_to_four_digits --> digit, digit.
one_to_four_digits --> digit, digit, digit.
one_to_four_digits --> digit, digit, digit, digit.

one_or_two_digits --> digit.
one_or_two_digits --> digit, digit.

% define digit
digit --> [C], { char_type(C, digit) /*-> true; set_error_with_parameter('Syntax', 'int' , [], Position),
!, fail) */}.

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
  sql_condition(1200,T)               # 'valid SQL condition', 
  punct(')')                          # 'closing parenthesis '')''',
  !,
  r_sql_condition(PP,0,T/To).
sql_condition(PP,To) -->
  {sql_operator(P,FX,OP,QOP),
    prefix(P,FX,PR),
    P=<PP},
  op(OP)                              # QOP,
  sql_condition(PR,T)                 # 'valid SQL condition', 
  {NT=..[OP,T]},
  r_sql_condition(PP,P,NT/To).

r_sql_condition(PP,Pi,Ti/To) -->
  {sql_operator(P,YFX,OP,QOP),
    infix(P,YFX,PL,PR),
    P=<PP,
    Pi=<PL,
    NT=..[OP,Ti,T]},
  op(OP)                             # QOP,
  sql_condition(PR,T), 
  r_sql_condition(PP,P,NT/To).
r_sql_condition(_,_,Ti/Ti) -->
  [].

sql_operator(1100,xfy, or,'or').
sql_operator(1050,xfy, xor,'xor').
sql_operator(1000,xfy, and,'and').
sql_operator( 900, fy, not,'not').

b_sql_condition(SQLst) -->
  punct('(')                          # 'opening parenthesis ''(''',
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
  cmd(true)                           # 'TRUE',
  !.
cond_factor(false) --> 
  cmd(false)                          # 'FALSE',
  !.
cond_factor(is_null(R)) --> 
  sql_expression(R,_T), 
  cmd(is), 
  cmd(null)                           # 'NULL',
  !.
cond_factor(not(is_null(R))) --> 
  sql_expression(R,_T),  
  cmd(is), 
  op(not)                             # 'NOT', 
  cmd(null)                           # 'NULL',
  !.
/*cond_factor(exists(select())) -->
  cmd(exists)                         # 'EXISTS',
  !,
  opening_parentheses_star(N),
  dqlStmt([select()|STs]/STs)         # 'valid SELECT statement',
  closing_parentheses_star(N).
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
  {(NOT==true -> F='$not_like'(L,R,E) ; F='$like'(L,R,E))}.*/
cond_factor(C) --> 
  sql_expression(L,_LT), 
  relop(Op)                           # 'comparison operator', 
  sql_expression(R,_RT),
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

column_or_constant_tuple(Cs,A) --> 
  punct('(')                          # 'opening parenthesis ''(''',
  sql_proj_expression_sequence(Cs),
  punct(')')                          # 'closing parenthesis '')''',
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

%TODO none
sql_proj_expression(expr(E,AS,Type),AS) -->
  sql_expression(E,Type).

sql_rel_cond_factor(Op,L,R,CF) :-
  CF=..[Op,L,R].

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
  comparisonOp(RO)                    # 'a comparison operator'.
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQL Expressions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
sql_expressions([E|Es]) -->
  sql_expression(E,_)                 # 'an expression',
  punct(',')                          # 'comma',
  !,
  sql_expressions(Es).
sql_expressions([E]) -->
  sql_expression(E,_)                 # 'an expression'.
sql_expressions([]) -->
  []. 
*/

sql_expression(E,T) -->
  {current_db(_,postgresql)},
  sql_expression(1200,E,T),
  punct('::')                         # 'double colon',
  sql_type(_).
sql_expression(E,T) -->
  sql_expression(1200,E,T)            # 'valid expression'.

sql_expression(PP,Lo,To) -->
  sql_factor(L,T), 
  r_sql_expression(PP,0,L/Lo,T/To).
sql_expression(PP,Lo,To) -->
  [punct('('):_],
  sql_expression(1200,L,T), 
  punct(')')                          # 'closing parenthesis '')''',
  !, % WARNING
  r_sql_expression(PP,0,L/Lo,T/To).
sql_expression(PP,Lo,To) -->
  {my_operator(P,FX,[T,Ta],_,OP),
    prefix(P,FX,PR),
    P=<PP},
    op(OP)                             # OP,
  sql_expression(PR,L,Ta), 
  {NL=..[OP,L]},
  r_sql_expression(PP,P,NL/Lo,T/To).
  
r_sql_expression(PP,Pi,Li/Lo,Ti/To) -->
  {my_operator(P,YFX,[T,Ti,RT],_,OP),
    infix(P,YFX,PL,PR),
    P=<PP,
    Pi=<PL
  },
  op(OP)                             # OP,
  sql_expression(PR,L,RT), 
  {NL=..[OP,Li,L]}, 
  r_sql_expression(PP,P,NL/Lo,T/To).
r_sql_expression(_,_,Li/Li,Ti/Ti) -->
  [].


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
  punct('(')                            # 'opening parenthesis ''(''',
  sql_expression(E,T),
  punct(')')                            # 'closing parenthesis '')''',
  {!}. % WARNING: This whole clause is only for improving parsing performance
sql_factor(E,_) --> % :::WARNING: Add type info
  dqlStmt([E|STs]/STs)                  # 'valid DQL statement',
  !.
/*sql_factor(Aggr,T) -->
  sql_special_aggregate_function(Aggr,T),
  !.  % WARNING: This cut is only for improving parsing performance*/
/*sql_factor(FAs,T) --> 
  { my_function(SF,F,Arity,[T|Ts]),
    Arity>0},
  fn(SF)                              # SF,
  punct('(')                          # 'opening parenthesis ''(''',
  sql_function_arguments(Arity,As,Ts),
  punct(')')                          # 'closing parenthesis '')''',
  { FAs=..[F|As]}.
sql_factor(Function,number(_)) -->
  cmd(extract)                        # 'EXTRACT',
  punct('(')                          # 'opening parenthesis ''(''',
  extract_field(Field)                # 'valid datetime field (year, month, day, hour, minute, second)',
  cmd(from)                           # 'FROM',
  sql_expression(C,datetime(_))       # 'valid datetime expression',  
  punct(')')                          # 'closing parenthesis '')''',
  {Function=..[Field,C],
    !}.
sql_factor(cast(Factor,Type),Type) -->
  fn(cast)                            # 'CAST',
  punct('(')                          # 'opening parenthesis ''(''',
  sql_factor(Factor,_),
  cmd(as)                             # 'AS',
  sql_type(Type)                      # 'valid type name',  
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(coalesce(ExprSeq),_Type) -->
  fn(coalesce)                        # 'COALESCE',
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(greatest(ExprSeq),_Type) -->
  fn(greatest)                        # 'GREATEST',
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(least(ExprSeq),_Type) -->
  fn(least)                           # 'LEAST',
  punct('(')                          # 'opening parenthesis ''(''',
  sql_expr_sequence(ExprSeq),
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(iif(Cond,Expr1,Expr2),_Type) -->
  fn(iif)                             # 'IIF',
  punct('(')                          # 'opening parenthesis ''(''',
  sql_condition(Cond)                 # 'valid condition',
  punct(',')                          # 'comma',
  sql_expression(Expr1,_T1)           # 'valid expression', 
  punct(',')                          # 'comma',
  sql_expression(Expr2,_T2)           # 'valid expression', 
  punct(')')                          # 'closing parenthesis '')'''.
sql_factor(case(CondValList,Default),Type) -->
  cmd(case)                           # 'CASE',
  sql_case2_when_thens(CondValList),
  sql_case_else_end(Default,Type).
sql_factor(case(Expr,ExprValList,Default),Type) -->
  cmd(case)                           # 'CASE',
  sql_expression(Expr,_T)             # 'expression', 
  sql_case3_when_thens(ExprValList),
  sql_case_else_end(Default,Type).*/
sql_factor(cte(C,T),T) -->
  sql_constant(cte(C,T)).
sql_factor(C,_) -->
  column(C).
/*sql_factor(F,T) --> 
  {my_function(SF,F,Type,0,[T]),
    Type\==aggregate % 0-arity aggregate functions from Datalog are not allowed in SQL
    },
  fn(SF)                              # SF,
  optional_parentheses.*/


/*  
% Aggr(DISTINCT Column)
sql_special_aggregate_function(AF,T) -->
  {my_aggregate_function(_,PF,T,1),
    atom_concat(F,'_distinct',PF),
    atom_codes(F,SF),
    to_uppercase_char_list(SF,USF)},
  my_kw(USF), 
  push_syntax_error(['Expected left bracket ''('''],Old1),
  my_sql_blanks_star,
  "(",
  pop_syntax_error(Old1),
  my_sql_blanks_star,
  my_kw("DISTINCT"),
  my_sql_blanks,
  my_column(C),
  push_syntax_error(['Expected right bracket '')'''],Old2),
  my_sql_blanks_star,
  ")",
  pop_syntax_error(Old2),
  {AF=..[PF,C]}.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Column and Table Constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% column constraint
column_constraint(C,not_nullables([C])) -->
  op(not)                             # 'NOT',
  cmd(null)                           # 'NULL',
  !. 
column_constraint(_C,true) -->
  cmd(null)                           # 'NULL',
  !.
column_constraint(C,primary_key([C])) -->
  cmd(primary)                        # 'PRIMARY',
  cmd(key)                            # 'KEY',
  !.
column_constraint(C,candidate_key([C])) -->
  cmd(unique)                         # 'UNIQUE',
  !.
column_constraint(C,foreign_key([C],TableName,[TC])) -->
  cmd(references)                     # 'REFERENCES', 
  referenced_column(C,TableName,TC),
  optional_referential_triggered_action(_Rule),
  !.
column_constraint(C,default(C,Expression,Type)) -->
  cmd(default)                        # 'DEFAULT',
  sql_expression(Expression,Type)     # 'expression',
  !.
column_constraint(_C,CheckCtr) -->
  cmd(check)                          # 'CHECK',
  opening_parentheses_star(N),
  check_constraint(CheckCtr)          # 'valid check constraint',
  closing_parentheses_star(N),
  !.
column_constraint(C,candidate_key([C])) -->
  cmd(candidate)                      # 'CANDIDATE',
  cmd(key)                            # 'KEY',
  !.
column_constraint(C,fd([Att],[C])) -->
  cmd(determined)                     # 'DETERMINED',
  cmd(by)                             # 'BY',
  untyped_column(Att)                 # 'a column name',
  !.

column_constraint(_,_) -->
  set_error('Syntax', 'valid column constraint (NOT, NULL, PRIMARY, UNIQUE, REFERENCES, DEFAULT, CHECK, CANDIDATE, DETERMINED)').


% table constraint
table_constraint(not_nullables(Cs)) -->
  op(not)                             # 'NOT',
  cmd(null)                           # 'NULL',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(primary_key(Cs)) -->
  cmd(primary)                        # 'PRIMARY',
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(candidate_key(Cs)) -->
  cmd(unique)                         # 'UNIQUE',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.
table_constraint(foreign_key(Cs,FTableName,FCs)) -->
  cmd(foreign)                        # 'FOREIGN',
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  cmd(references)                     # 'REFERENCES', 
  tablename(FTableName)               # 'table name',
  column_tuple(FCs)                    # 'a column sequence between parentheses',
  optional_referential_triggered_action(_Rule),
  !.
table_constraint(foreign_key(Cs,FTableName,Cs)) -->
  cmd(foreign)                        # 'FOREIGN',
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  cmd(references)                     # 'REFERENCES', 
  tablename(FTableName)               # 'table name',
  optional_referential_triggered_action(_Rule),
  !.
table_constraint(CheckCtr) -->
  cmd(check)                          # 'CHECK',
  opening_parentheses_star(N),
  check_constraint(CheckCtr)       # 'check constraint',
  closing_parentheses_star(N),
  !.
table_constraint(candidate_key(Cs)) -->
  cmd(candidate)                      # 'CANDIDATE',
  cmd(key)                            # 'KEY',
  column_tuple(Cs)                    # 'a column sequence between parentheses',
  !.

table_constraint(_) -->
  set_error('Syntax', 'valid table constraint (NOT, PRIMARY, UNIQUE, FOREIGN, CHECK, CANDIDATE)').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Column  ColumnNnameList tablename viewname colname relname 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

column_tuple(Ts) -->
  punct('(')                          # 'opening parenthesis ''(''',
  column_name_list(Ts)                # 'a sequence of column names',
  punct(')')                          # 'closing parenthesis '')'''.

%column_name_list(columnList)
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
  ren_tablename(T),
  !.

p_ren_tablename((T, none)) -->
  tablename(T)                        # 'table name',
  !.

ren_tablename((T,[I|Args])) -->
  current_position(Position),
  tablename(T)                        # 'table name',
  optional_cmd(as),
  sql_user_identifier(I)              # 'user identifier',
  !,
  {(my_table('$des',T,A) -> length(Args,A);
  set_error_with_parameter('Semantic', 'Table ~w does not exist in the $des system' , [T], Position),
  !, fail)}.

%column rel_id.col_id/col_id
column(attr(R,C,none)) --> 
  relname(R),
  punct('.')                          # 'dot',
  colname(C).
column(attr(none,C,none)) --> 
  colname(C).

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
  [id(Name):_Pos],
  punct(']')                          # 'closing bracket '']'''.

sql_user_identifier(Name) --> 
  [punct('`'):_],  %no "punct('`') # 'opening back quotes'" because it's not mandatory
  [id(Name):_Pos],
  punct('`')                          # 'closing back quotes ''`'''.

sql_user_identifier(Name) -->
  [id(Name):_Pos].


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

% is_number(+Expr)
% Succeed if Expr is a number
is_number(Expr) :-
  value(_, [Expr:_], []),
  Expr \= str(_).

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
  punct(')')                          # 'closing parenthesis '')''',
  {N1 is N+1},
  closing_parentheses_star(N1,NN).
closing_parentheses_star(N,N) -->
  [].

% terminal(?Token)
terminal(id(_)).
terminal(quoted_id(_)).
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