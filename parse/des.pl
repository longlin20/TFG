/*prefix, infix and posfix from des.pl*/
:- module(des,[prefix/3, infix/4, posfix/3]).

prefix(P,fx,P-1).
prefix(P,fy,P).

infix(P,xfx,P-1,P-1).
infix(P,xfy,P-1,P).
infix(P,yfx,P,P-1).

posfix(P,xf,P-1).
posfix(P,yf,P).


/*
my_raise_exception from des.pl, write_error_log from des_common.pl
*/

/*
:- module(des,[ my_raise_exception/3]).

set_initial_status :-
set_flag(tapi(off)).

my_raise_exception(G,Mid,R_V) :-
  (seen;true),
  exception_message(Mid,Message),
  write_exception_message(G,Message,R_V),
  throw(des_exception(Message)).
  
exception_message(instantiation,        exception('Non ground argument(s) found in goal')) :- !.
exception_message(undefined,            exception('Undefined predicate')) :- !.
exception_message(basic_goal,           exception('The following is not a valid goal:')) :- !.
exception_message(exec,                 exception('Executing goal:')) :- !.
exception_message(unsupported_in_Prolog,exception('Aggregates are not supported in Prolog mode')) :- !.
exception_message(non_number,           exception('Non-numbers found in result set of')) :- !.
exception_message(type,                 exception('Type error')) :- !.
exception_message(bounds,               exception('Bounds error')) :- !.
%exception_message(fd_unsupported,exception('FD constraint solving unsupported by underlying Prolog system')) :- !.
exception_message(odbc_unsupported,     exception('ODBC connections unsupported by underlying Prolog system. Use either binaries or SWI-Prolog or SICStus Prolog sources')) :- !.
exception_message(compiling(Language),  exception(Message)) :- 
  atomic_concat_list(['When compiling ',Language,' to core Datalog. System error'],Message),
  !.
exception_message(Message,Message).

write_exception_message(generic,syntax(Message),NVs) :-
  !,
  write_error_log(['$tbc']),
  write_cond_unquoted_with_NVs_list(Message,NVs),
  nl_log,
  write_tapi_eot.
write_exception_message(unallowed_identifier(O,I,A),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Built-in identifier ''',I,''' with ',A,' arguments is not allowed as a ',O,'.']).
write_exception_message(invalid_use(I),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Invalid use of ''',I,''' in context.']).
write_exception_message(unknown_column(T,C),syntax(Message),R_V) :-
  !,
  write_exception_message(unknown_column(T,C,statement),syntax(Message),R_V),
  display_column_alternatives(T,C).
write_exception_message(unknown_column(T,C,S),syntax(_Message),_R_V) :-
  !,
  scope_error_tail_message(S,M),
  ((is_system_identifier(T) ; var(T) ; T==dual)
   ->
    write_error_log(['Unknown column ''',C,'''',M,'.',nl])
   ;
    write_error_log(['Unknown column ''',T,'.',C,'''',M,'.',nl])
  ),
  display_column_alternatives(T,C).
write_exception_message(unknown_relation(R),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown table or view ''',R,'''.',nl]),
  display_relation_alternatives(R).
write_exception_message(unknown_view(V),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown view ''',V,'''.',nl]),
  display_view_alternatives(V).
write_exception_message(unknown_table(T),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown table ''',T,'''.',nl]),
  display_table_alternatives(T).
write_exception_message(unknown_user_predicate(F/A),syntax(_Message),_R_V) :-
  !,
  write_error_log(['Unknown user predicate ',F/A,'.',nl]),
  display_user_predicate_alternatives(F).
write_exception_message(G,M,R_V) :-
  (M=exception(Message), 
   I='Exception'
  ;
   M=syntax(Message), 
   I='Error'
  ;
   M=Message, 
   I='Exception'
  ),
  !,
  (my_is_list(Message)
   ->
    DisplayMessage = Message
   ;
    DisplayMessage = [Message]
  ),
  write_log_list([I,': ']),
  write_unquoted_with_NVs_list(DisplayMessage,R_V),
  write_log_list([' ']),
  ((nonvar(R_V),R_V=datalog(R,NVs,_Rid,_CId,Ls,FId,_Rs))
   ->
    write_with_NVs(G,NVs),
    write_log_list([' in the instanced rule:',nl]),
    display_ruleNVs_list([(R,NVs)],11),
    display_rule_info(Ls,FId)
   ;
    (G\=='$void'
     ->
      (nonvar(R_V),my_is_list(R_V)
       ->
        write_with_NVs(G,R_V)
       ;
        write_with_NVs(G,[])
      )
     ;
    true
   )
  ),
  nl_log.


%
% Error, warning and info messages
%
% Write error message, formatted as display status

write_error_log(['$tbc']) :-
tapi(off),
!,
write_log('Error: ').
write_error_log(Message) :-
tapi(off),
!,
write_log('Error: '),
write_log_list(Message),
(append(_,[nl],Message)
 ->
 true
 ;
 nl_log
).
write_error_log(['$tbc']) :- % To be continued
write_log_list(['$error',nl,0,nl]).
write_error_log(Message) :-
write_log_list(['$error',nl,0,nl]),
write_log_list(Message),
(append(_,[nl],Message)
 ->
 true % For continuation error messages
 ;
 nl_log,
 write_tapi_eot % End of error report transmission
).
*/