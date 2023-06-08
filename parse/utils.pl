% In this file, I will place all the clauses 
% from the des_common.pl file that are necessary for parser.pl.

:- module(utils,
          [ my_map_1/2,
            my_nf_setof/3]).


:- use_module(des_data,
          [my_attribute/5]).

% Map to exactly one argument (that can be a list)
my_map_1(_X,[]).
my_map_1(X,[Y|Ys]) :-
  my_apply(X,Y),
  my_map_1(X,Ys).

my_apply(my_apply(X,Y),Z) :-
  !,
  my_add_tup_arg(X,Y,T),
  my_apply(T,Z).
my_apply(X,Y) :-
  my_add_tup_arg(X,Y,T),
  call(T).

my_add_tup_arg(X,Y,T) :-
  X=..LX,
  append(LX,[Y],Ts),
  T=..Ts.

% Non-failing setof: Return empty list if setof fails
my_nf_setof(X,G,Xs) :-
  (setof(X,G,Xs) -> true ; Xs=[]).