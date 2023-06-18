/*********************************************************/
/*                                                       */
/* ERROR_ module                                         */
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Error handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(error_, 
          [ set_error/2,
            set_error/3,
            set_error/4,
            reset_error/0,
            process_error/0,
            error/3,
            set_error_with_parameter/4 ]).

:- use_module(misc,
          [ current_position/3]).

% error(+Token, +Position)
:- dynamic error/3.

% reset_error/0
% Reset all pending errors
reset_error :-
  retractall(error(_, _, _)).

% set_error(+Class, +Error, +PositionOrComp)
% Set the class ('Lexical', 'Syntax', 'Semantic') error for 
%   the longest consumed positions
set_error(Class, Error, Position) :-
  memberchk(Class, ['Lexical', 'Syntax', 'Semantic']),
  !,
  (error(Class, _, OldPosition)
   -> (Position @>= OldPosition
       -> reset_error,
          assert_error(Class, Error, Position)
       ;  true)
   ;  assert_error(Class, Error, Position)).
set_error(Class, Error) :-
  % Runtime error. There can be only one runtime error
  %get_runline(Comp, Line-Statement),
  assert_error(Class, Error, pos(1, 1)).

% set_error(+Class, +Error)
% Set a runtime error 
set_error(Class, Error) :-
  set_error(Class, Error, _Position),
  !,
  fail.

% assert_error(+Class, +Error, +Position)
assert_error(Class, Error, Position) :-
  assertz(error(Class, Error, Position)).

% set_error(+Class, +Error)//
% Set the error in a DCG for the current position
set_error(Class, Error) -->
  current_position(Position),
  {set_error(Class, Error, Position),
   !,
   fail}.

% process_error/0
% Look for the single possible error, if added; 
%  otherwise the error occurs at the beginning
process_error :-
  % Find the error in this order:
  member(Class, ['Runtime', 'Semantic', 'Syntax', 'Lexical']),
  error(Class, Error, Position),
  !,
  display_error(Class, Error, Position).
process_error :-
  display_error('Undefined', 'start of program', pos(1, 1)).

% display_error(+Class, +Error, +Position)
% Formatted display of the error setting
display_error(Class, Error, pos(Line, Statement)) :-
  Class == 'Runtime',
  !,
  format('~w Error: ~w ', [Class, Error]),
  display_error_location(Line, 'Statement', Statement).
display_error(Class, Error, pos(Line, Column)) :-
  Class == 'Semantic',
  !,
  format('~w Error: ~w ', [Class, Error]),
  display_error_location(Line, 'Column', Column).
display_error(Class, Error, pos(Line, Column)) :-
  Class == 'Syntax',
  !,
  format('~w Error: Expected ~w ', [Class, Error]),
  display_error_location(Line, 'Column', Column).
display_error(Class, Error, pos(Line, Column)) :-
  format('~w Error: Wrong ~w ', [Class, Error]),
  display_error_location(Line, 'Column', Column).

% display_error_location(+Line, +Location, +ColumnOrStmt)
% Display the location of the error: the line and either 
%   the column or statement numbers (base 1) 
display_error_location(void, _, void) :-
  !.
display_error_location(last, _, last) :-
  !,
  format('at the end of the program', []).
display_error_location(Line, Location, ColumnOrStmt) :-
  format('at Line ~w, ~w ~w. ', [Line, Location, ColumnOrStmt]).

set_error_with_parameter(Class, ErrorStr, Parameter, Position) :-
  format(atom(Error), ErrorStr, Parameter),
  set_error(Class, Error, Position).