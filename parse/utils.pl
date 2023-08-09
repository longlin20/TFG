% In this file, I will place all the clauses 
% from the des_common.pl and des_dl_debug.pl file that are necessary for parser.pl.

:- module(utils,
          [ my_map_1/2,
            my_nf_setof/3,
            my_remove_duplicates/2,
            my_list_to_list_of_lists/2,
            my_zipWith/4]).

:- use_module(des_data,
          [ my_attribute/5]).

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

my_remove_duplicates([], []).
my_remove_duplicates([Head|Tail1], [Head|Tail2]) :-
  my_delete(Tail1, Head, Residue),
  my_remove_duplicates(Residue, Tail2).

my_delete([], _, []).
my_delete([Head|Tail], Element, Rest) :-
  Head==Element, !,
  my_delete(Tail, Element, Rest).
my_delete([Head|Tail], Element, [Head|Rest]) :-
  my_delete(Tail, Element, Rest).


my_list_to_list_of_lists([],[]).
my_list_to_list_of_lists([A|As],[[A]|Bs]) :-
  my_list_to_list_of_lists(As,Bs).


% zipWith
% +Operator/Predicate +List(LeftOp) +List(RightOp) +List(Operator(LeftOp,RightOp))
my_zipWith(_Z,[],_Bs,[]).
my_zipWith(_Z,[_A|_As],[],[]).
my_zipWith(Z,[A|As],[B|Bs],[P|Ps]) :-
  P=..[Z,A,B],
  my_zipWith(Z,As,Bs,Ps).