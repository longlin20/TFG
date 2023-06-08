:- module(des_data,
          [ current_db/1,
          my_table/3,
          my_attribute/5]).

% my_table(DB,RelationName,Arity)
:- dynamic(my_table/3).
% my_view(DB,RelationName,Arity,QuerySyntacticTree,Language,DatalogRules,CompiledRulesId,LocalViewDefinitions,StringConstants)
:- dynamic(my_view/9).
% my_attribute(DB,Position,RelationName,AttributeName,DataType)
:- dynamic(my_attribute/5).

:- dynamic(opened_db/4).      % Facts indicating the open DBs: one fact for each DB including the connection name and handle, DBMS, and opening user options

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Metadata from external DBMSs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Datalog DB ($des) is the default DB managed by the deductive engine '$des'. No connection data ($void)
set_initial_db :-
  close_dbs,
  set_default_db,
  set_flag(opened_db('$des','$void','$des',[])).
  
set_default_db :-
  set_flag(current_db('$des','$des')).
  
current_db(Connection) :-
  current_db(Connection,_DBMS).

opened_db(Connection) :-
  opened_db(Connection,_Handle,_DBMS,_UOptions).
  
opened_db(Connection,Handle) :-
  opened_db(Connection,Handle,_DBMS,_UOptions).
  
opened_db(Connection,Handle,DBMS) :-
  opened_db(Connection,Handle,DBMS,_UOptions).
  
close_dbs :-  
  opened_db(DB),
  (DB == '$des'
   ->
    true
   ;
    processC(close_db,[DB],_,_)    % Closing an opened RDB
  ),
  fail.
close_dbs.

current_db('$des', '$des').

% my_table(TableName,Arity) :-
%   atom(TableName),
%   current_db(ConnectionName),
%   my_table(ConnectionName,TableName,Arity).
my_table(TableName,Arity) :-
  atom(TableName),
  current_db(ConnectionName),
  (my_table(ConnectionName,TableName,Arity)
   ;
   (ConnectionName \== '$des', des_sql_solving(on)),
   my_table('$des',TableName,Arity)
  ).
my_table('$des', dual, 0).
my_table('$des', select_not_null, 3).
my_table('$des', t1, 1).
my_table('$des', t2, 2).
my_table('$des', t3, 3).
my_table(A, B, C) :-
  opened_db(A),
  A\=='$des',
  my_odbc_get_table_arity(A, B, C).
 
% my_attribute(Position,RelationName,AttributeName,DESDataType) :-
%   current_db(ConnectionName),
%   my_attribute(ConnectionName,Position,RelationName,AttributeName,DESDataType).
my_attribute(Position,RelationName,AttributeName,DESDataType) :-
  current_db(ConnectionName),
  (my_attribute(ConnectionName,Position,RelationName,AttributeName,DESDataType)
  ;
   (ConnectionName \== '$des', des_sql_solving(on)),
   my_attribute('$des',Position,RelationName,AttributeName,DESDataType)
  ).
  
my_attribute('$des', 1, select_not_null, '$a', _).
my_attribute('$des', 2, select_not_null, '$b', _).
my_attribute('$des', 3, select_not_null, '$c', _).

my_attribute('$des', 1, t1, a1, number(integer)).

my_attribute('$des', 1, t2, a2, number(integer)).
my_attribute('$des', 2, t2, b2, number(integer)).

my_attribute('$des', 1, t3, a3, number(integer)).
my_attribute('$des', 2, t3, b3, number(integer)).
my_attribute('$des', 3, t3, c3, number(integer)).

my_attribute(A, B, C, D, E) :-
        opened_db(A),
        A\=='$des',
        my_odbc_get_colnames(A, C, F),
        my_nth1_member(D, B, F),
        my_odbc_get_type(A, C, D, E).