/*prefix, infix and posfix from des.pl*/
:- module(des,[ prefix/3, 
                infix/4, 
                posfix/3,
                /*my_function/4,
                my_function/5,*/
                get_null_id/1,
                my_operator/5]).

:- dynamic(null_id/1).        % Integer identifier for nulls, represented as '$NULL'(i), where 'i' is the null identifier

prefix(P,fx,P-1).
prefix(P,fy,P).

infix(P,xfx,P-1,P-1).
infix(P,xfy,P-1,P).
infix(P,yfx,P,P-1).

posfix(P,xf,P-1).
posfix(P,yf,P).

/*
my_function(SF,PF,Type,A,Ts) :- 
  function(F,PF,_,Type,Ts,A),
  atom_codes(F,SF).

my_function(SF,PF,A,Ts) :- 
  function(F,PF,_,_,Ts,A),
  atom_codes(F,SF).

% function(Name, PrologPredefined, Description, Kind(arithmetic, arithmetic_cte, string, datetime, aggregate, conversion, fuzzy), Types(Return type and argument types), Arity)
function(sqrt,sqrt,'Square root',arithmetic,[number(float),number(_)],1).
function(ln,log,'Neperian logarithm',arithmetic,[number(float),number(_)],1).
function(log,log,'Neperian logarithm',arithmetic,[number(float),number(_)],1).
function(log,log,'Logarithm of the second argument in the base of the first one',arithmetic,[number(float),number(_),number(_)],2).
function(exp,exp,'Euler number to the power of its argument',arithmetic,[number(float),number(_)],1).
function(sin,sin,'Sine',arithmetic,[number(float),number(_)],1).
function(cos,cos,'Cosine',arithmetic,[number(float),number(_)],1).
function(tan,tan,'Tangent',arithmetic,[number(float),number(_)],1).
function(cot,cot,'Cotangent',arithmetic,[number(float),number(_)],1).
function(asin,asin,'Arc sine',arithmetic,[number(float),number(_)],1).
function(acos,acos,'Arc cosine',arithmetic,[number(float),number(_)],1).
function(atan,atan,'Arc tangent',arithmetic,[number(float),number(_)],1).
function(acot,acot,'Arc cotangent',arithmetic,[number(float),number(_)],1).
function(abs,abs,'Absolute value',arithmetic,[number(float),number(_)],1).
function(mod,mod,'Modulo. Apply to two integers and return an integer',arithmetic,[number(integer),number(integer),number(integer)],2).
function(float,float,'Float value of its argument',arithmetic,[number(float),number(_)],1).
function(integer,integer,'Closest integer between 0 and its argument',arithmetic,[number(integer),number(_)],1).
function(sign,sign,'Returns -1 if its argument is negative, 0 otherwise',arithmetic,[number(integer),number(_)],1).
function(gcd,gcd,'Greatest common divisor between two numbers',arithmetic,[number(integer),number(_),number(_)],2).
function(min,min,'Least of two numbers',arithmetic,[number(_),number(_),number(_)],2).
function(max,max,'Greatest of two numbers',arithmetic,[number(_),number(_),number(_)],2).
function(trunc,truncate,'Closest integer between 0 and its argument',arithmetic,[number(integer),number(_)],1).
function(truncate,truncate,'Closest integer between 0 and its argument',arithmetic,[number(integer),number(_)],1).
function(truncate,trunc,'Return its first argument truncated to the number of decimals specified by the second one',arithmetic,[number(_),number(_), number(integer)],2).
function(trunc,trunc,'Return its first argument truncated to the number of decimals specified by the second one',arithmetic,[number(_),number(_), number(integer)],2).
function(float_integer_part,float_integer_part,'Integer part as a float',arithmetic,[number(float),number(_)],1).
function(float_fractional_part,float_fractional_part,'Fractional part as a float',arithmetic,[number(float),number(_)],1).
function(round,round,'Closest integer',arithmetic,[number(_),number(_)],1).
function(round,round,'Round its first argument to the places indicated by its second one',arithmetic,[number(_),number(_),number(integer)],2).
function(floor,floor,'Greatest integer less or equal to its argument',arithmetic,[number(integer),number(_)],1).
function(ceiling,ceiling,'Least integer greater or equal to its argument',arithmetic,[number(integer),number(_)],1).
function(rand,rand,'Return a random float number',arithmetic,[number(float)],0).
function(rand,rand,'Return a random float number w.r.t. an integer seed',arithmetic,[number(float),number(integer)],1).
function(power,power,'Return its first argument raised to the power of the second one',arithmetic,[number(_),number(_),number(_)],2).
% Aggregate functions
function(avg,avg,'Average. Return a float',aggregate,[number(float),number(_)],1).
function(avg_distinct,avg_distinct,'Average of distinct values but nulls. Return a float',aggregate,[number(float),number(_)],1).
function(count,count,'Count all (with no argument). Return an integer',aggregate,[number(integer)],0).
function(count,count,'Count but nulls wrt. its argument. Return an integer',aggregate,[number(integer),_],1).
function(count_distinct,count_distinct,'Count all distincts (with no argument). Return an integer',aggregate,[number(integer)],0).
function(count_distinct,count_distinct,'Count distincts but nulls wrt. its argument. Return an integer',aggregate,[number(integer),_],1).
function(max,max,'Maximum. Return a value with the same type as its argument',aggregate,[Type,Type],1).
function(min,min,'Minimum. Return a value with the same type as its argument',aggregate,[Type,Type],1).
function(sum,sum,'Cumulative sum of values but nulls. Return a value with the same type as its argument',aggregate,[number(_),number(_)],1).
function(sum_distinct,sum_distinct,'Cumulative sum of distinct values but nulls. Return a value with the same type as its argument',aggregate,[number(_),number(_)],1).
function(times,times,'Cumulative product of values but nulls. Return a value with the same type as its argument',aggregate,[number(_),number(_)],1).
function(times_distinct,times_distinct,'Cumulative product of distinct values. Return a value with the same type as its argument',aggregate,[number(_),number(_)],1).
% Arithmetic constants
%function(pi,4*atan(1),'Archimedes'' constant',arithmetic_cte,[number(float)],0).
function(pi,pi,'Archimedes'' constant',arithmetic_cte,[number(float)],0).
function(e,exp(1),'Euler''s number',arithmetic_cte,[number(float)],0).
% String functions
function(length,length,'Length of its input string',string,[number(_),string(_)],1).
function(concat,concat,'String concatenation',string,[string(_),string(_),string(_)],2).
function(instr,instr,'Return the first numeric position of the searched substring',string,[number(integer),string(_),string(_)],2).
function(left,left,'Return the first characters of a string',string,[string(_),string(_),number(integer)],2).
function(lower,lower,'Convert to lower case',string,[string(_),string(_)],1).
function(lpad,lpad,'Return the given string padded to the left with spaces, with the given total length',string,[string(_),string(_),number(integer)],2).
function(lpad,lpad,'Return the given string padded to the left with the given character, with the given total length',string,[string(_),string(_),number(integer),string(_)],3).
function(ltrim,ltrim,'Remove leading spaces',string,[string(_),string(_)],1).
function(repeat,repeat,'Repeat the string several times',string,[string(_),string(_),number(integer)],2).
function(replace,replace,'Replace the second string by the third one in the given first string',string,[string(_),string(_),string(_),string(_)],3).
function(reverse,reverse,'Reverse a string',string,[string(_),string(_)],1).
function(rpad,rpad,'Return the given string padded to the right with spaces, with the given total length',string,[string(_),string(_),number(integer)],2).
function(rpad,rpad,'Return the given string padded to the right with the given character, with the given total length',string,[string(_),string(_),number(integer),string(_)],3).
function(right,right,'Return the last characters of a string',string,[string(_),string(_),number(integer)],2).
function(rtrim,rtrim,'Remove trailing spaces',string,[string(_),string(_)],1).
function(space,space,'Return a string with several spaces',string,[string(_),number(integer)],1).
%function(str,str,'Ensures string',string,[string(_),string(_)],1).
function(substr,substr,'Substring: String, offset, and length',string,[string(_),string(_),number(_),number(_)],3).
function(trim,trim,'Remove both leading and trailing spaces',string,[string(_),string(_)],1).
function(upper,upper,'Convert to upper case',string,[string(_),string(_)],1).
% Datetime functions
function(year,year,'Return the number of the year for a datetime',datetime,[number(_),datetime(_)],1).
function(month,month,'Return the number of the month for a datetime',datetime,[number(_),datetime(_)],1).
function(day,day,'Return the number of the day of the month for a datetime',datetime,[number(_),datetime(_)],1).
function(hour,hour,'Return the number of the hour field for a datetime',datetime,[number(_),datetime(_)],1).
function(minute,minute,'Return the number of the minute field for a datetime',datetime,[number(_),datetime(_)],1).
function(second,second,'Return the number of the second field for a datetime',datetime,[number(_),datetime(_)],1).
function(last_day,last_day,'Return the last day of the month for the given datetime',datetime,[number(_),datetime(_)],1).
function(to_char,to_char,'Convert a datetime to a string',datetime,[string(_),datetime(_)],1).
function(to_char,to_char,'Convert a datetime to a string for a given format',datetime,[string(_),datetime(_),string(_)],2).
function(to_date,to_date,'Convert a string to a date',datetime,[datetime(date),string(_)],1).
function(to_date,to_date,'Convert a string to a date for a given format',datetime,[datetime(date),string(_),string(_)],2).
function(sysdate,sysdate,'Return the current system date',datetime,[datetime(date)],0).
function(current_date,current_date,'Return the current system date',datetime,[datetime(date)],0).
function(current_time,current_time,'Return the current system time',datetime,[datetime(time)],0).
function(current_timestamp,current_timestamp,'Return the current system timestamp',datetime,[datetime(datetime)],0).
function(datetime_add,datetime_add,'Return the datetime increased by the number in its second argument',datetime,[datetime(_),datetime(_),number(integer)],2).
function(datetime_add,datetime_add,'Return the datetime increased by the number in its first argument',datetime,[datetime(_),number(integer),datetime(_)],2).
function(datetime_sub,datetime_sub,'Return the datetime decreased by the number in its second argument',datetime,[datetime(_),datetime(_),number(integer)],2).
function(datetime_sub,datetime_sub,'Return the number of days/seconds between both dates/times',datetime,[number(integer),datetime(_),datetime(_)],2).
function(add_months,add_months,'Add to a datetime a number of months',datetime,[datetime(_),datetime(_),number(integer)],2).
% Conversion functions
function(cast,cast,'Conversion of values',conversion,[number(N),number(_),type(number(N))],2).
function(cast,cast,'Conversion of values',conversion,[string(S),number(_),type(string(S))],2).
function(cast,cast,'Conversion of values',conversion,[number(N),string(_),type(number(N))],2).
function(cast,cast,'Conversion of values',conversion,[string(S),string(_),type(string(S))],2).
function(cast,cast,'Conversion of values',conversion,[string(S),datetime(_),type(string(S))],2).
function(cast,cast,'Conversion of values',conversion,[datetime(D),string(_),type(datetime(D))],2).
function(cast,cast,'Conversion of values',conversion,[datetime(D),datetime(_),type(datetime(D))],2).
% Selection functions
function(coalesce,coalesce,'Return the first non-null value',selection,[_Type,_Types],1).
function(greatest,greatest,'Return the greatest value',selection,[_Type,_Types],1).
function(least,least,'Return the least value',selection,[_Type,_Types],1).
function(nvl,nvl,'Return the first non-null value',selection,[_Type1,_Type2,_Type3],2).
function(nvl2,nvl2,'Return either the second argument if the first is not null value, or the third one otherwise',selection,[_Type1,_Type2,_Type3,_Type4],3).
function(nullif,nullif,'Return null if its arguments are equal',selection,[_Type1,_Type2,_Type3],2).
function(iif,iif,'Return either the second or the third argument depending on the truth value of the first one',selection,[_Type1,_Type2,_Type3,_Type4],3).
function(case,case,'Return either the first value in a true condition pair or the default value',selection,[_Type1,_Type2,_Type3],2).
function(case,case,'Return either the first value in a true condition pair or the default value',selection,[_Type1,_Type2,_Type3,_Type4],3).
*/

% Null identifiers
get_null_id(Id) :-
  (null_id(CId) -> 
   Id is CId+1,
   retract(null_id(CId))
   ; 
   Id is 0),
  assertz(null_id(Id)).

reset_null_id :-
  retractall(null_id(_Id)).

%my_priority_operator(Priority,Associativity,StringOperator,Operator)
% Infix:
my_operator(P,A,Ts,SOP,POP) :-
  my_infix_operator(_,SOP,POP,Ts,_,P,A).
% Prefix:
my_operator(200,fy,[number(N),number(N)],"+",'+').
my_operator(200,fy,[number(N),number(N)],"-",'-').
my_operator(200,fy,[number(N),number(N)],"\\",'\\').


% Built-in Binary Arithmetic Operators
% my_infix_operator(Name, StrName, PrologBuiltin, [ReturnType|ArgumentTypes], Description, Priority, Associativity)
% The priority of an operator in each priority group follows textual order of clauses (the first one has the higher priority, the last one has the lower priority)
my_infix_operator('^',"^",'**',[number(_),number(_),number(_)],'Power',200,xfx).
my_infix_operator('**',"**",'**',[number(_),number(_),number(_)],'Power',200,xfx).
my_infix_operator('/\\',"/\\",'/\\',[number(integer),number(integer),number(integer)],'Bitwise conjuntion between integers',500,yfx).
my_infix_operator('\\/',"\\/",'\\/',[number(integer),number(integer),number(integer)],'Bitwise disjunction between integers',500,yfx).
my_infix_operator('*',"*",'*',[number(_),number(_),number(_)],'Multiplication',400,yfx).
my_infix_operator('/',"/",'/',[number(float),number(_),number(_)],'Real division',400,yfx).
my_infix_operator('//',"//",'//',[number(integer),number(integer),number(integer)],'Integer quotient',400,yfx).
my_infix_operator('div',"div",'div',[number(integer),number(integer),number(integer)],'Integer quotient',400,yfx).
my_infix_operator('rem',"rem",'rem',[number(integer),number(integer),number(integer)],'Integer remainder',400,yfx).
my_infix_operator('mod',"mod",'mod',[number(integer),number(integer),number(integer)],'Modulo',400,yfx).
my_infix_operator('xor',"xor",'xor',[number(integer),number(integer),number(integer)],'Bitwise exclusive or between integers',500,yfx).
my_infix_operator('+',"+",'+',[number(_),number(_),number(_)],'Addition',500,yfx).
my_infix_operator('-',"-",'-',[number(_),number(_),number(_)],'Difference between its arguments',500,yfx).
my_infix_operator('<<',"<<",'<<',[number(integer),number(integer),number(integer)],'Shift left the first argument the number of places indicated by the second one',400,yfx).
my_infix_operator('>>',">>",'>>',[number(integer),number(integer),number(integer)],'Shift right the first argument the number of places indicated by the second one',400,yfx).
my_infix_operator('+',"+",'concat',[string(_),string(_),string(_)],'String concatenation',500,yfx).
my_infix_operator('||',"||",'concat',[string(_),string(_),string(_)],'String concatenation',500,yfx).
my_infix_operator('+',"+",'datetime_add',[datetime(DT),datetime(DT),number(integer)],'Date/time addition between datetimes',500,yfx).
my_infix_operator('+',"+",'datetime_add',[datetime(DT),number(integer),datetime(DT)],'Date/time addition between number and datetime',500,yfx).
my_infix_operator('-',"-",'datetime_sub',[datetime(DT),datetime(DT),number(integer)],'Date/time subtraction between datetime and number',500,yfx).
my_infix_operator('-',"-",'datetime_sub',[number(integer),datetime(DT),datetime(DT)],'Date/time subtraction between datetimes',500,yfx).
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