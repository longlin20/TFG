/*********************************************************/
/*                                                       */
/* TEST module                                           */
/*   Developed for SWI-Prolog 8.x and above              */
/*                                                       */
/*                             Fernando Saenz-Perez 2023 */
/*                                                       */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                                       */
/*                                                       */
/* This is free software: you can redistribute it and/or */
/* modify it under the terms of the GNU Lesser General   */
/* Public License as published by the Free Software      */
/* Foundation, either version 3 of the License, or (at   */
/* your option) any later version.                       */
/*                                                       */
/* This software is distributed in the hope that it will */
/* be useful, but WITHOUT ANY WARRANTY; without even the */
/* implied warranty of MERCHANTABILITY or FITNESS FOR A  */
/* PARTICULAR PURPOSE. See the GNU Lesser General Public */
/* License for more details.                             */
/*                                                       */
/* You should have received a copy of the GNU Lesser     */
/* General Public License and GNU General Public License */
/* along with this program. If not, see:                 */
/*                                                       */
/*            http://www.gnu.org/licenses/               */
/*********************************************************/

:- module(test,
          [ test/1,
            test/4 ]).

:- use_module(error_,
          [ error/3 ]).

% Check all tests
% test/0
test(Module) :-
  test(Module, 1).

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.

% test(+Module, +Index)
% Check each test<Index> in Module, where <Index> is an increasing 
% (step +1) left-0-padded integer starting at 1.
% Stop if a test fails
test(Module, I) :-
  retractall(test_error),
  format(atom(TestI), 'test~|~`0t~d~3+', [I]),
  ( clause(Module:TestI, TestBody),
    % misc:cls,
    format('***********************************~n', []),
    format('************  ~w  ************~n', [TestI]),
    format('***********************************~n~n', []),
    (Module:TestBody
     -> true
     ;  format('ERROR: ~w failed.\n', [TestI]), 
        assertz(test_error),
        fail),
    nl
    -> I1 is I+1,
       test(Module, I1)
    ;  success ). 

success :-
  test_error,
  !.
success :-
  format('***********************************~n', []),
  format('**   SUCCESS! All tests passed   **~n', []),
  format('***********************************~n', []).


% test(+Module, +GoalName/2, +Input, +Expected) :-
test(Module, GoalName, Input, Expected) :-
  Goal =.. [GoalName, Input, Computed],
  Module:Goal,
  !,
  writeln(Computed),
  (Computed =@= Expected % Structurally equal
   -> true
   ;  format('TEST ERROR:\n  Expected: ~q\n  Computed: ~q\n', [Expected, Computed]),
      !, fail).
test(_Module, _GoalName, _Input, failure(Error)) :-
  Error,
  writeln(Error).

