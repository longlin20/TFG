:- module(lexer,
          [ lex/2,
            lex/1 ]).

:- use_module(library(edcg)).
		  
:- use_module(test,
          [ test/4 ]).

:- use_module(error_,
          [ set_error/3,
            reset_error/0,
            process_error/0]).

:- set_prolog_flag(double_quotes, codes).

%%%% Extended DCG declarations

edcg:acc_info(position, X, In, Out, acc_pos(X, In, Out)).

% Declare predicates using these hidden arguments
% pred_info(+Predicate, +Arity, +List_of_accumulators)
% 'dcg' represents the usual DCG accumulator.
% The additional 'position' accumulator is used to
% accumulate the term pos(Line, Column).

edcg:pred_info(Name, _, [position, dcg]) :-
  \+ memberchk(Name, ['!',           % Do not expand
                      fail,          % "   "   "
                      eoc,           % Only 'dcg' accumulator
                      kw,            % "     "
                      inc_line,      % Only 'position' accumulator
                      inc_col,       % "     "
                      add_col,       % "     "
                      get_pos        % "     "
                      ]).

edcg:pred_info(eoc,           0, [dcg]).
edcg:pred_info(kw,            1, [dcg]).

edcg:pred_info(inc_line, 0, [position]).
edcg:pred_info(inc_col,  0, [position]).
edcg:pred_info(add_col,  1, [position]).
edcg:pred_info(get_pos,  1, [position]).

%%%% End of extended DCG declarations

:- discontiguous punctuation/5.

% Lexical categories:
%
% - Numbers
%     int(Number). Integer
%     frac(Integer, Fractional). Fractional
%     float(Integer, Fractional, Exponent). Float
% - Strings (delimited by single quotes)
%     str(String)
% - Commands:
%     cmd(Command/Case)
% - Functions
%     fn(Function/Case)
% - Commands or Functions, word can be both cmd or fn
%     cmd_fn(C_F/Case)
% - Operators (symbolic and textual):
%     op(Operator)
%     comparisonOp(Operator)
%     textual_op(Operator/Case)
% - Punctuation: ( ) , ; : "...
% - (User) Identifiers:
%     id(Identifier/Case).
% - (User) Quoted Identifiers:
%     double_quotes_id(Identifier).
% - (User) Quoted Identifiers:
%     back_quotes_id(Identifier).
% - (User) Quoted Identifiers:
%     square_brackets_id(Identifier).

lex(Input) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens),
  print(Tokens).
  %forall(member(Token, Tokens), writeln(Token)).

lex(Input, Tokens) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens).
 
lex(_Input, _Tokens) :-
  process_error,
  !, fail.

lex_codes(Codes, Tokens) :-
  token_pos_list(Tokens, pos(1,1), _Pos, Codes, []),
  !.
 
token_pos_list(TokenPosList) -->>
  separators_star,
  get_pos(TokPos):position,
  token(Token),
  get_pos(NxtPos):position,
  separator(Token, NextToken),
  !,
  {(NextToken == no
   -> TokenPosList = [Token:TokPos|RemainingTokenPosList]
   ;  TokenPosList = [Token:TokPos, NextToken:NxtPos|RemainingTokenPosList])},
  token_pos_list(RemainingTokenPosList).
token_pos_list([]) -->>
  separators_star.

% If there are no more codes to read, there is no need for 
%   a final separator
separator(_Token, no) -->>
  eoc,
  !.
  
% If the previous token is an operator, there is no need for a separator
separator(op(_), no) -->>
  !,
  [].

separator(comparisonOp(_), no) -->>
  !,
  [].

% If the previous token is a punctuation mark, there is no need for a separator
separator(punct(_), no) -->>
  !,
  [].

separator(str(_), no) -->>
  !,
  [].

separator(double_quotes_id(_), no) -->>
  !,
  [].
/*
separator(back_quotes_id(_), no) -->>
  !,
  [].

separator(id(_), no) -->>
  !,
  [].
*/
separator(_Token, String) -->>
  string(String),
  !.

separator(_Token, QuotedID) -->>
  double_quotes_identifier(QuotedID),
  !.

separator(_Token, punct(nl)) -->>
  "\n",
  !,
  inc_line.

separator(_Token, Delimiter) -->>
  delimiter(Delimiter),
  !.

separator(_Token, no) -->>
  separator.

separators -->>
  skip_non_visible,
  separator,
  !,
  separators_star.

separators_star -->>
  separators,
  !.
  
separators_star -->>
  skip_non_visible,
  [].

separator -->>
  " ",
  inc_col:position,
  !.
  
separator -->>
  "\t",
  !.
  
separator -->>
  "end_of_file",
  !.

skip_non_visible -->>
  [C],
  {non_visible_code(C)},
  !,
  skip_non_visible.
skip_non_visible -->>
  [].

token(Number) -->>
  number(Number),
  !.
  
token(String) -->>
  string(String),
  !. 

token(Identifier) -->>
  double_quotes_identifier(Identifier),
  !.

/*
token(back_quotes_id(Identifier)) -->>
  back_quotes_identifier(Identifier),
  !.
*/

token(comment(Comment)) -->> % SQL comments: include the rest of the line as the comment
  sql_comment_start,
  !,
  comment(Comment).

% Rule for recognising multi-line comments in SQL
token(comment(Comment)) -->> 
  multi_line_comment_start,
  !,
  multi_line_comment_content(Comment, 1). % Add nesting level

%neither single quotes(string) 
% nor double quotes(refer to an id sensitive to upper and lower case)

token(Delimiter) -->>
  delimiter(Delimiter),
  { Delimiter \== punct('\''),
    Delimiter \== punct('"')/*, 
    Delimiter \== punct('`')*/}, % Excludes single quotes and double quotes
  !.

token(cmd_fn(Command/Case)) -->>
  command_function(Command/Case),
  !.

token(cmd(Command/Case)) -->>
  command(Command/Case),
  !.

token(fn(Function/Case)) -->>
  function(Function/Case),
  !.
  
token(textual_op(Operator/Case)) -->>
  textual_operator(Operator/Case),
  !.

token(id(Identifier/Case)) -->>
  identifier(Identifier/Case),
  !.

/*
token(id_but_semicolon(Identifier)) -->>
  identifier_but_semicolon(Identifier),
  %{ \+ is_separator(Identifier) },  % Verify that Identifier is not a separator
  !.*/


token(_Error) -->>
  set_error(token),
  !, fail.

/*
is_separator(C) -->>      % Tabulator
  {non_visible_code(C)}.
is_separator("end_of_file").
is_separator(" ").    % Space
is_separator("\t").   % Tab character
*/

sql_comment_start -->> % Check for SQL comment start ('--')
 "--", !, add_col(2).

comment(Comment) -->>
  comment_codes(Codes0),
  {(append([32|_], Codes, Codes0) % Remove the first blank, if it exists
    -> true
    ;  Codes = Codes0),
   atom_codes(Comment, Codes)}.

comment_codes([]) -->>
  dcg/[10|_], % Lookahead end of line
  !.
comment_codes([Code|Codes]) -->>
  [Code],
  inc_col,
  !,
  comment_codes(Codes).
comment_codes([]) -->> % No more codes are left to read
  [],
  !.

multi_line_comment_start -->> "/*", !, add_col(2).

multi_line_comment_content(Comment, Nesting) -->>
  multi_line_comment_codes(Codes, Nesting),
  {atom_codes(Comment, Codes)}.

multi_line_comment_codes([], 0) -->>
!.

multi_line_comment_codes([Code|Codes], Nesting) -->>
  set_error_Syntax('unclosed multiline comment'),
  [Code],
  ( {Code == 10} -> inc_line ; inc_col ),
  !,
  ( multi_line_comment_start -> 
    {NewNesting is Nesting + 1},
    multi_line_comment_codes(Codes, NewNesting)
  ; multi_line_comment_end -> 
      {NewNesting is Nesting - 1},
      multi_line_comment_codes(Codes, NewNesting)
  ; multi_line_comment_codes(Codes, Nesting)
  ).

multi_line_comment_end -->> "*/" , !, add_col(2).


delimiter(op(Delimiter)) -->>
  operator(Delimiter),
  !.

delimiter(comparisonOp(Delimiter)) -->>
  comparison_operator(Delimiter),
  !.

delimiter(punct(Delimiter)) -->>
  punctuation(Delimiter).

% operator(-Operator)//
% Operators 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
operator('<<')  -->> "<<",  !, add_col(2).
operator('>>')  -->> ">>",  !, add_col(2).
operator('\\/')  -->> "\\/",  !, add_col(2).
operator('+')   -->> "+",   !, inc_col.
operator('/\\')   -->> "/\\",   !, add_col(2).
operator('//')   -->> "//",   !, add_col(2).
operator('/')   -->> "/",   !, inc_col.
operator('^')   -->> "^",   !, inc_col.
operator('||')  -->> "||",  !, add_col(2). % concat
operator('**')  -->> "**",  !, add_col(2). 
operator('*')   -->> "*",   !, inc_col. 
%operator('#')   -->> "#",   !, inc_col. 
operator('-')   -->> "-",   !, inc_col.
%operator('\\')   -->> "\\", !, inc_col.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMPARISON OP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
comparison_operator('!=')  -->> "!=",  !, add_col(2). % inequality
comparison_operator('=')   -->> "=",   !, inc_col.
comparison_operator('>=')  -->> ">=",  !, add_col(2).
comparison_operator('<=')  -->> "<=",  !, add_col(2).
comparison_operator('<>')  -->> "<>",  !, add_col(2). % inequality
comparison_operator('>')   -->> ">",   !, inc_col.
comparison_operator('<')   -->> "<",   !, inc_col.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEXTUAL OP
%mod trantando como fn
%textual_operator('mod')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
textual_operator('and'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("nd"),
  not_more_char,
  !,
  add_col(3).


textual_operator('or'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("r"),
  not_more_char,
  !,
  add_col(2).


textual_operator('not'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("ot"),
  not_more_char,
  !,
  add_col(3).


textual_operator('xor'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('x', Char); char_code('X', Char)), C == Char },
  lc("or"),
  not_more_char,
  !,
  add_col(3).


textual_operator('rem'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("em"),
  not_more_char,
  !,
  add_col(3).


textual_operator('div'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("iv"),
  not_more_char,
  !,
  add_col(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUNCTUATION
%punctuation quotes simple is in last line
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
punctuation('(') -->> "(",   !, inc_col.
punctuation(')') -->> ")",   !, inc_col.
punctuation('[') -->> "[",   !, inc_col.
punctuation(']') -->> "]",   !, inc_col.
punctuation('`') -->> "`",   !, inc_col.
punctuation(',') -->> ",",   !, inc_col.
punctuation('.') -->> ".",   !, inc_col.
punctuation(';') -->> ";",   !, inc_col.
punctuation('::') -->> "::", !, add_col(2).
punctuation(':') -->> ":",   !, inc_col.
punctuation('"') -->> """",  !, inc_col.
punctuation('nl') -->> "\n", !, inc_line.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMAND or FUNCTION 
% those can be commands and functions at the same time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command_function('replace'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("eplace"),
  not_more_char,
  !,
  add_col(7).

command_function('float'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("loat"),
  not_more_char,
  !,
  add_col(5).

command_function('left'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("eft"),
  not_more_char,
  !,
  add_col(4).

command_function('right'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ight"),
  not_more_char,
  !,
  add_col(5).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMAND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command('add'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("dd"),
  not_more_char,
  !,
  add_col(3).


command('all'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("ll"),
  not_more_char,
  !,
  add_col(3).


command('alter'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("lter"),
  not_more_char,
  !,
  add_col(5).


command('any'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("ny"),
  not_more_char,
  !,
  add_col(3).


command('ascending'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("scending"),
  not_more_char,
  !,
  add_col(9).


command('asc'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("sc"),
  not_more_char,
  !,
  add_col(3).


command('assume'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("ssume"),
  not_more_char,
  !,
  add_col(6).


command('as'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("s"),
  not_more_char,
  !,
  add_col(2).


command('between'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('b', Char); char_code('B', Char)), C == Char },
  lc("etween"),
  not_more_char,
  !,
  add_col(7).


command('bc'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('b', Char); char_code('B', Char)), C == Char },
  lc("c"),
  not_more_char,
  !,
  add_col(2).


command('by'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('b', Char); char_code('B', Char)), C == Char },
  lc("y"),
  not_more_char,
  !,
  add_col(2).


command('candidate'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("andidate"),
  not_more_char,
  !,
  add_col(9).


command('cascade'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ascade"),
  not_more_char,
  !,
  add_col(7).


command('character'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("haracter"),
  not_more_char,
  !,
  add_col(9).


command('char'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("har"),
  not_more_char,
  !,
  add_col(4).


command('check'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("heck"),
  not_more_char,
  !,
  add_col(5).


command('column'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("olumn"),
  not_more_char,
  !,
  add_col(6).


command('commit'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ommit"),
  not_more_char,
  !,
  add_col(6).


command('constraints'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("onstraints"),
  not_more_char,
  !,
  add_col(11).


command('constraint'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("onstraint"),
  not_more_char,
  !,
  add_col(10).


command('create'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("reate"),
  not_more_char,
  !,
  add_col(6).


command('databases'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("atabases"),
  not_more_char,
  !,
  add_col(9).


command('database'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("atabase"),
  not_more_char,
  !,
  add_col(8).


command('data'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("ata"),
  not_more_char,
  !,
  add_col(4).


command('datetime'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("atetime"),
  not_more_char,
  !,
  add_col(8).


command('date'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("ate"),
  not_more_char,
  !,
  add_col(4).


command('decimal'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("ecimal"),
  not_more_char,
  !,
  add_col(7).


command('default'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("efault"),
  not_more_char,
  !,
  add_col(7).


command('delete'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("elete"),
  not_more_char,
  !,
  add_col(6).


command('descending'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("escending"),
  not_more_char,
  !,
  add_col(10).


command('desc'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("esc"),
  not_more_char,
  !,
  add_col(4).


command('describe'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("escribe"),
  not_more_char,
  !,
  add_col(8).


command('determined'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("etermined"),
  not_more_char,
  !,
  add_col(10).


command('distinct'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("istinct"),
  not_more_char,
  !,
  add_col(8).


command('division'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("ivision"),
  not_more_char,
  !,
  add_col(8).


command('drop'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("rop"),
  not_more_char,
  !,
  add_col(4).


command('else'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("lse"),
  not_more_char,
  !,
  add_col(4).


command('end'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("nd"),
  not_more_char,
  !,
  add_col(3).


command('escape'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("scape"),
  not_more_char,
  !,
  add_col(6).


command('except'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("xcept"),
  not_more_char,
  !,
  add_col(6).


command('exists'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("xists"),
  not_more_char,
  !,
  add_col(6).


command('extract'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("xtract"),
  not_more_char,
  !,
  add_col(7).


command('false'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("alse"),
  not_more_char,
  !,
  add_col(5).


command('fetch'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("etch"),
  not_more_char,
  !,
  add_col(5).


command('first'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("irst"),
  not_more_char,
  !,
  add_col(5).


command('foreign'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("oreign"),
  not_more_char,
  !,
  add_col(7).


command('from'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("rom"),
  not_more_char,
  !,
  add_col(4).


command('full'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("ull"),
  not_more_char,
  !,
  add_col(4).


command('group'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('g', Char); char_code('G', Char)), C == Char },
  lc("roup"),
  not_more_char,
  !,
  add_col(5).


command('having'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('h', Char); char_code('H', Char)), C == Char },
  lc("aving"),
  not_more_char,
  !,
  add_col(6).


command('if'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("f"),
  not_more_char,
  !,
  add_col(2).


command('inner'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nner"),
  not_more_char,
  !,
  add_col(5).


command('insert'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nsert"),
  not_more_char,
  !,
  add_col(6).


command('intersect'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("ntersect"),
  not_more_char,
  !,
  add_col(9).


command('into'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nto"),
  not_more_char,
  !,
  add_col(4).


command('integer'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nteger"),
  not_more_char,
  !,
  add_col(7).


command('int'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nt"),
  not_more_char,
  !,
  add_col(3).


command('in'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("n"),
  not_more_char,
  !,
  add_col(2).


command('is'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("s"),
  not_more_char,
  !,
  add_col(2).


command('join'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('j', Char); char_code('J', Char)), C == Char },
  lc("oin"),
  not_more_char,
  !,
  add_col(4).


command('key'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('k', Char); char_code('K', Char)), C == Char },
  lc("ey"),
  not_more_char,
  !,
  add_col(3).


command('like'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("ike"),
  not_more_char,
  !,
  add_col(4).


command('limit'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("imit"),
  not_more_char,
  !,
  add_col(5).


command('minus'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("inus"),
  not_more_char,
  !,
  add_col(5).


command('natural'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("atural"),
  not_more_char,
  !,
  add_col(7).


command('no'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("o"),
  not_more_char,
  !,
  add_col(2).


command('null'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("ull"),
  not_more_char,
  !,
  add_col(4).


command('number'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("umber"),
  not_more_char,
  !,
  add_col(6).


command('numeric'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("umeric"),
  not_more_char,
  !,
  add_col(7).


command('offset'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("ffset"),
  not_more_char,
  !,
  add_col(6).


command('only'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("nly"),
  not_more_char,
  !,
  add_col(4).


command('on'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("n"),
  not_more_char,
  !,
  add_col(2).


command('order'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("rder"),
  not_more_char,
  !,
  add_col(5).


command('outer'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('o', Char); char_code('O', Char)), C == Char },
  lc("uter"),
  not_more_char,
  !,
  add_col(5).


command('primary'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('p', Char); char_code('P', Char)), C == Char },
  lc("rimary"),
  not_more_char,
  !,
  add_col(7).


command('real'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("eal"),
  not_more_char,
  !,
  add_col(4).


command('recursive'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ecursive"),
  not_more_char,
  !,
  add_col(9).


command('references'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("eferences"),
  not_more_char,
  !,
  add_col(10).


command('rename'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ename"),
  not_more_char,
  !,
  add_col(6).


command('restrict'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("estrict"),
  not_more_char,
  !,
  add_col(8).


command('rollback'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ollback"),
  not_more_char,
  !,
  add_col(8).


command('rows'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ows"),
  not_more_char,
  !,
  add_col(4).


command('savepoint'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("avepoint"),
  not_more_char,
  !,
  add_col(9).


command('select'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("elect"),
  not_more_char,
  !,
  add_col(6).


command('set'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("et"),
  not_more_char,
  !,
  add_col(3).


command('show'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("how"),
  not_more_char,
  !,
  add_col(4).


command('smallint'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("mallint"),
  not_more_char,
  !,
  add_col(8).


command('some'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("ome"),
  not_more_char,
  !,
  add_col(4).


command('string'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("tring"),
  not_more_char,
  !,
  add_col(6).


command('tables'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("ables"),
  not_more_char,
  !,
  add_col(6).


command('table'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("able"),
  not_more_char,
  !,
  add_col(5).


command('text'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("ext"),
  not_more_char,
  !,
  add_col(4).


command('then'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("hen"),
  not_more_char,
  !,
  add_col(4).


command('timestamp'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("imestamp"),
  not_more_char,
  !,
  add_col(9).


command('time'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("ime"),
  not_more_char,
  !,
  add_col(4).


command('type'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("ype"),
  not_more_char,
  !,
  add_col(4).


command('top'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("op"),
  not_more_char,
  !,
  add_col(3).


command('to'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("o"),
  not_more_char,
  !,
  add_col(2).


command('true'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("rue"),
  not_more_char,
  !,
  add_col(4).


command('union'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('u', Char); char_code('U', Char)), C == Char },
  lc("nion"),
  not_more_char,
  !,
  add_col(5).


command('unique'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('u', Char); char_code('U', Char)), C == Char },
  lc("nique"),
  not_more_char,
  !,
  add_col(6).


command('update'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('u', Char); char_code('U', Char)), C == Char },
  lc("pdate"),
  not_more_char,
  !,
  add_col(6).


command('using'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('u', Char); char_code('U', Char)), C == Char },
  lc("sing"),
  not_more_char,
  !,
  add_col(5).


command('values'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('v', Char); char_code('V', Char)), C == Char },
  lc("alues"),
  not_more_char,
  !,
  add_col(6).


command('varchar2'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('v', Char); char_code('V', Char)), C == Char },
  lc("archar2"),
  not_more_char,
  !,
  add_col(8).


command('varchar'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('v', Char); char_code('V', Char)), C == Char },
  lc("archar"),
  not_more_char,
  !,
  add_col(7).


command('views'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('v', Char); char_code('V', Char)), C == Char },
  lc("iews"),
  not_more_char,
  !,
  add_col(5).


command('view'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('v', Char); char_code('V', Char)), C == Char },
  lc("iew"),
  not_more_char,
  !,
  add_col(4).


command('when'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('w', Char); char_code('W', Char)), C == Char },
  lc("hen"),
  not_more_char,
  !,
  add_col(4).


command('where'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('w', Char); char_code('W', Char)), C == Char },
  lc("here"),
  not_more_char,
  !,
  add_col(5).


command('with'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('w', Char); char_code('W', Char)), C == Char },
  lc("ith"),
  not_more_char,
  !,
  add_col(4).


command('work'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('w', Char); char_code('W', Char)), C == Char },
  lc("ork"),
  not_more_char,
  !,
  add_col(4).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%          
function('sqrt'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("qrt"),
  not_more_char,
  !,
  add_col(4).


function('ln'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("n"),
  not_more_char,
  !,
  add_col(2).


function('log'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("og"),
  not_more_char,
  !,
  add_col(3).


function('exp'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  lc("xp"),
  not_more_char,
  !,
  add_col(3).


function('sin'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("in"),
  not_more_char,
  !,
  add_col(3).


function('cos'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("os"),
  not_more_char,
  !,
  add_col(3).


function('tan'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("an"),
  not_more_char,
  !,
  add_col(3).


function('cot'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ot"),
  not_more_char,
  !,
  add_col(3).


function('asin'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("sin"),
  not_more_char,
  !,
  add_col(4).


function('acos'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("cos"),
  not_more_char,
  !,
  add_col(4).


function('atan'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("tan"),
  not_more_char,
  !,
  add_col(4).


function('acot'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("cot"),
  not_more_char,
  !,
  add_col(4).


function('abs'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("bs"),
  not_more_char,
  !,
  add_col(3).


function('mod'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("od"),
  not_more_char,
  !,
  add_col(3).


function('integer'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nteger"),
  not_more_char,
  !,
  add_col(7).


function('sign'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("ign"),
  not_more_char,
  !,
  add_col(4).


function('gcd'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('g', Char); char_code('G', Char)), C == Char },
  lc("cd"),
  not_more_char,
  !,
  add_col(3).


function('min'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("in"),
  not_more_char,
  !,
  add_col(3).


function('max'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("ax"),
  not_more_char,
  !,
  add_col(3).


function('truncate'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("runcate"),
  not_more_char,
  !,
  add_col(8).


function('trunc'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("runc"),
  not_more_char,
  !,
  add_col(5).


function('float_integer_part'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("loat_integer_part"),
  not_more_char,
  !,
  add_col(18).


function('float_fractional_part'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("loat_fractional_part"),
  not_more_char,
  !,
  add_col(21).


function('round'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("ound"),
  not_more_char,
  !,
  add_col(5).


function('floor'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('f', Char); char_code('F', Char)), C == Char },
  lc("loor"),
  not_more_char,
  !,
  add_col(5).


function('ceiling'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("eiling"),
  not_more_char,
  !,
  add_col(7).


function('rand'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("and"),
  not_more_char,
  !,
  add_col(4).


function('power'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('p', Char); char_code('P', Char)), C == Char },
  lc("ower"),
  not_more_char,
  !,
  add_col(5).


function('avg'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("vg"),
  not_more_char,
  !,
  add_col(3).


function('avg_distinct'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("vg_distinct"),
  not_more_char,
  !,
  add_col(12).


function('count'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ount"),
  not_more_char,
  !,
  add_col(5).


function('count_distinct'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ount_distinct"),
  not_more_char,
  !,
  add_col(14).


function('sum'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("um"),
  not_more_char,
  !,
  add_col(3).


function('sum_distinct'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("um_distinct"),
  not_more_char,
  !,
  add_col(12).


function('times'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("imes"),
  not_more_char,
  !,
  add_col(5).


function('times_distinct'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("imes_distinct"),
  not_more_char,
  !,
  add_col(14).


function('pi'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('p', Char); char_code('P', Char)), C == Char },
  lc("i"),
  not_more_char,
  !,
  add_col(2).


function('e'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('e', Char); char_code('E', Char)), C == Char },
  not_more_char,
  !,
  inc_col.


function('length'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("ength"),
  not_more_char,
  !,
  add_col(6).


function('concat'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("oncat"),
  not_more_char,
  !,
  add_col(6).


function('instr'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("nstr"),
  not_more_char,
  !,
  add_col(5).


function('lower'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("ower"),
  not_more_char,
  !,
  add_col(5).


function('lpad'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("pad"),
  not_more_char,
  !,
  add_col(4).


function('ltrim'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("trim"),
  not_more_char,
  !,
  add_col(5).


function('repeat'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("epeat"),
  not_more_char,
  !,
  add_col(6).


function('reverse'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("everse"),
  not_more_char,
  !,
  add_col(7).


function('rpad'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("pad"),
  not_more_char,
  !,
  add_col(4).


function('rtrim'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('r', Char); char_code('R', Char)), C == Char },
  lc("trim"),
  not_more_char,
  !,
  add_col(5).


function('space'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("pace"),
  not_more_char,
  !,
  add_col(5).


function('substr'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("ubstr"),
  not_more_char,
  !,
  add_col(6).


function('trim'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("rim"),
  not_more_char,
  !,
  add_col(4).


function('upper'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('u', Char); char_code('U', Char)), C == Char },
  lc("pper"),
  not_more_char,
  !,
  add_col(5).


function('year'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('y', Char); char_code('Y', Char)), C == Char },
  lc("ear"),
  not_more_char,
  !,
  add_col(4).


function('month'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("onth"),
  not_more_char,
  !,
  add_col(5).


function('day'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("ay"),
  not_more_char,
  !,
  add_col(3).


function('hour'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('h', Char); char_code('H', Char)), C == Char },
  lc("our"),
  not_more_char,
  !,
  add_col(4).


function('minute'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('m', Char); char_code('M', Char)), C == Char },
  lc("inute"),
  not_more_char,
  !,
  add_col(6).


function('second'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("econd"),
  not_more_char,
  !,
  add_col(6).


function('last_day'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("ast_day"),
  not_more_char,
  !,
  add_col(8).


function('to_char'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("o_char"),
  not_more_char,
  !,
  add_col(7).


function('to_date'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('t', Char); char_code('T', Char)), C == Char },
  lc("o_date"),
  not_more_char,
  !,
  add_col(7).


function('sysdate'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('s', Char); char_code('S', Char)), C == Char },
  lc("ysdate"),
  not_more_char,
  !,
  add_col(7).


function('current_date'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("urrent_date"),
  not_more_char,
  !,
  add_col(12).


function('current_time'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("urrent_time"),
  not_more_char,
  !,
  add_col(12).


function('current_datetime'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("urrent_datetime"),
  not_more_char,
  !,
  add_col(16).


function('datetime_add'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("atetime_add"),
  not_more_char,
  !,
  add_col(12).


function('datetime_sub'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('d', Char); char_code('D', Char)), C == Char },
  lc("atetime_sub"),
  not_more_char,
  !,
  add_col(12).


function('add_months'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('a', Char); char_code('A', Char)), C == Char },
  lc("dd_months"),
  not_more_char,
  !,
  add_col(10).


function('cast'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ast"),
  not_more_char,
  !,
  add_col(4).


function('coalesce'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("oalesce"),
  not_more_char,
  !,
  add_col(8).


function('greatest'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('g', Char); char_code('G', Char)), C == Char },
  lc("reatest"),
  not_more_char,
  !,
  add_col(8).


function('least'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('l', Char); char_code('L', Char)), C == Char },
  lc("east"),
  not_more_char,
  !,
  add_col(5).


function('nvl'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("vl"),
  not_more_char,
  !,
  add_col(3).


function('nvl2'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("vl2"),
  not_more_char,
  !,
  add_col(4).


function('nullif'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('n', Char); char_code('N', Char)), C == Char },
  lc("ullif"),
  not_more_char,
  !,
  add_col(6).


function('iif'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('i', Char); char_code('I', Char)), C == Char },
  lc("if"),
  not_more_char,
  !,
  add_col(3).


function('case'/Case) -->>
  [C],
  {is_lowercase_letter_code(C) -> Case = l; Case = u},
  { (Case == l -> char_code('c', Char); char_code('C', Char)), C == Char },
  lc("ase"),
  not_more_char,
  !,
  add_col(4).

%to lowercase
lc([Code|Codes]) -->>
  [C],
  {to_lowercase_code(C, Code)},
  lc(Codes).
lc([]) -->>
  [].

not_more_char -->>
    \+ is_more_char.
  
is_more_char -->>
    [C],
    { is_letter_code(C);
      is_underscore_code(C);
      is_number_code(C);
      is_dollar_sign_code(C) }.

number(NumberToken) -->>
  positive_number(NumberToken).
  
positive_number(Number) -->>
  ".", % Optional integer part in a fractional number
  rest_of_non_integer_positive_number(0, Number),
  !.
  
positive_number(Number) -->>
  positive_integer(Integer),
  ("."
   -> rest_of_non_integer_positive_number(Integer, Number)
   ;  exponent(Exponent)
      -> {Number = float(Integer, 0, Exponent)}
      ;  {Number = int(Integer)}).

rest_of_non_integer_positive_number(Integer, Number) -->>
  inc_col,
  set_error(fractional),
  positive_integer(Fractional),
  set_error(fractional),
  (exponent(Exponent)
   -> {Number = float(Integer, Fractional, Exponent)}
   ;  {Number = frac(Integer, Fractional)}).
   
exponent(Exponent) -->>
  ("e" ; "E"),
  inc_col,
  set_error(exponent),
  integer_exponent(Exponent),
  set_error(exponent).
  
integer_exponent(Exponent) -->>
  optional_sign(Sign, Cols),
  add_col(Cols),
  positive_integer(PosExponent),
  {Sign == '+'
   -> Exponent = PosExponent
   ;  Exponent is -PosExponent}.
   
optional_sign('+', 1) -->>
  "+",
  !.
optional_sign('-', 1) -->>
  "-",
  !.
optional_sign('+', 0) -->>
  [].

% positive_integer(-PositiveInteger)//
positive_integer(PositiveInteger) -->>
  digits_codes(DigitsCodes),
  {number_codes(PositiveInteger, DigitsCodes),
   length(DigitsCodes, Length)},
  add_col(Length),
  set_error(number).

% digits_codes(-DigitsCodes)//
% One or more digits
digits_codes([DigitCode|DigitsCodes]) -->>
  digit_code(DigitCode),
  !,
  more_digits_codes(DigitsCodes).
  
% more_digits_codes(-DigitsCodes)//
more_digits_codes(DigitsCodes) -->>
  digits_codes(DigitsCodes),
  !.
more_digits_codes([]) -->>
  [].
  
% digit_code(-DigitCode)//
digit_code(DigitCode) -->>
  [DigitCode],
  {is_number_code(DigitCode)}.

is_number_code(Code) :-
    "0" = [N0],
    "9" = [N9],
    N0 =< Code,
    N9 >= Code,
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% STRING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% string(-String)
% Strings (str/1). Delimited by simple quotes.
% Simple quotes inside a string are scaped as doubling them
% Or using \'
string(str(String)) -->>
  "'",
  rest_of_string(String).
  
% rest_of_string(-String)//
rest_of_string(String) -->>
  string_codes(StringCodes),
  "'",
  !,
  {atom_codes(String, StringCodes),
   length(StringCodes, Length),
   Cols is Length+2},
  add_col(Cols).
rest_of_string(_StringCodes) -->>
  set_error(string),
  {!, fail}.

% string_codes(-Codes)//
string_codes([Code|Codes]) -->>
  "''", % Escaped double quotes
  !,
  inc_col, %for ''
  {"'" = [Code]},
  string_codes(Codes).
string_codes([Code|Codes]) -->>
  "\\'", % Escaped \'
  !,
  inc_col, %for \
  {"'" = [Code]},
  string_codes(Codes).
string_codes([]) -->> % End of string
  {[C]="'"},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
string_codes([Code|Codes]) -->>
  [Code],
  string_codes(Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOUBLE QUOTES IDENTIFIER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
double_quotes_identifier(double_quotes_id(Identifier)) -->>
  """",
  rest_of_double_quotes_id(Identifier).
  

rest_of_double_quotes_id(Identifier) -->>
  double_quotes_id_codes(IdentifierCodes),
  """",
  !,
  {atom_codes(Identifier, IdentifierCodes),
   length(IdentifierCodes, Length),
   Cols is Length+2},
  add_col(Cols).
rest_of_double_quotes_id(_IdentifierCodes) -->>
  set_error(double_quotes_id),
  {!, fail}.

double_quotes_id_codes([Code|Codes]) -->>
  """""", % Escaped double quotes
  !,
  inc_col,
  {"""" = [Code]},
  double_quotes_id_codes(Codes).
double_quotes_id_codes([Code|Codes]) -->>
  "\\""", % Escaped \"
  !,
  inc_col, %for \
  {"""" = [Code]},
  double_quotes_id_codes(Codes).
double_quotes_id_codes([]) -->> % End of string
  {[C]=""""},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
double_quotes_id_codes([Code|Codes]) -->>
  [Code],
  double_quotes_id_codes(Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BACK QUOTES IDENTIFIER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
back_quotes_identifier(Identifier) -->>
  "`",
  rest_of_back_quotes_id(Identifier).


rest_of_back_quotes_id(Identifier) -->>
  back_quotes_id_codes(IdentifierCodes),
  "`",
  !,
  {atom_codes(Identifier, IdentifierCodes),
   length(IdentifierCodes, Length),
   Cols is Length+2},
  add_col(Cols).
rest_of_back_quotes_id(_IdentifierCodes) -->>
  set_error(back_quotes_id),
  {!, fail}.


back_quotes_id_codes([Code|Codes]) -->>
  "``", % Escaped double back quotes
  !,
  inc_col,
  {"`" = [Code]},
  back_quotes_id_codes(Codes).
back_quotes_id_codes([Code|Codes]) -->>
  "\`", % Escaped \`
  !,
  inc_col, %for \
  {"`" = [Code]},
  back_quotes_id_codes(Codes).
back_quotes_id_codes([]) -->> % End of string
  {[C]="`"},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
back_quotes_id_codes([Code|Codes]) -->>
  [Code],
  back_quotes_id_codes(Codes).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rule to recognize an identifier start by lowercase

identifier(Identifier/l) -->>
  [Code],
  {is_lowercase_letter_code(Code)},
  identifier_chars_star(Codes),
  {atom_codes(Identifier, [Code|Codes])},
  {length([Code|Codes], Length)},
  add_col(Length).

% Rule to recognize an identifier start by $
identifier(Identifier/u) -->>
  dollar_sign(CodeDollar), % Allow letters or $ as the first character
  letter(CodeLetter),
  identifier_chars_star(Codes),
  {append([CodeDollar, CodeLetter], Codes, AllCodes)},
  {atom_codes(Identifier, AllCodes)},
  {length(AllCodes, Length)},
  add_col(Length).

identifier(Identifier/u) -->>
  letter(Code),
  identifier_chars_star(Codes),
  {atom_codes(Identifier, [Code|Codes])},
  {length([Code|Codes], Length)},
  add_col(Length).

identifier(_Identifier) -->>
  set_error(identifier),
  {!, fail}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIER BUT SEMICOLON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
% remark(-Remark)//
identifier_but_semicolon(Remark) -->>
  identifier_but_semicolon_codes(Codes0),
  {(append([32|_], Codes, Codes0) % Remove the first blank, if it exists
    -> true
    ;  Codes = Codes0),
   atom_codes(Remark, Codes)}.

% remark_codes(-Remark)//
identifier_but_semicolon_codes([]) -->>
  dcg/[10|_], % Lookahead end of line
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[9|_], % Lookahead end of line
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[13|_], % Lookahead end of line
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[55|_], % Lookahead end of line
  !.
identifier_but_semicolon_codes([Code|Codes]) -->>
  [Code],
  inc_col,
  !,
  identifier_but_semicolon_codes(Codes).
identifier_but_semicolon_codes([]) -->> % No more codes are left to read
  [],
  !.
*/
% alphanum_star(-Codes)//
% Zero or more alphanumeric codes
identifier_chars_star([Code|Codes]) -->>
  ( letter(Code)
  ; digit_code(Code)
  ; underscore(Code)),
  identifier_chars_star(Codes).
identifier_chars_star([]) -->>
  [].


% letter(-LetterCode)//
letter(LetterCode) -->>
  [Code],
  {is_letter_code(Code),
   to_lowercase_code(Code, LetterCode)}.

% is_letter_code(-Code)
is_letter_code(Code) :-
  is_uppercase_letter_code(Code).
is_letter_code(Code) :-
  is_lowercase_letter_code(Code).

% Check if a character is a dollar sign
dollar_sign(Code) -->>
  [Code],
  {is_dollar_sign_code(Code)}.

is_dollar_sign_code(Code) :-
  "$" = [DCode],
  Code == DCode.

% Check if a character is an underscore
underscore(Code) -->>
  [Code],
  {is_underscore_code(Code)}.

is_underscore_code(Code) :-
  "_" = [UCode],
  Code == UCode.

% is_uppercase_letter_code(-Code)
is_uppercase_letter_code(Code) :-
  "A" = [UA],
  "Z" = [UZ],
  UA =< Code,
  UZ >= Code,
  !.
  
% is_lowercase_letter_code(-Code)
is_lowercase_letter_code(Code) :-
  "a" = [DA],
  "z" = [DZ],
  DA =< Code,
  DZ >= Code.

% to_lowercase_code(+Code, -DCode)
to_lowercase_code(Code, DCode) :-
  is_uppercase_letter_code(Code),
  !,
  "a" = [DA],
  "A" = [UA],
  DCode is Code + DA - UA.

to_lowercase_code(Code, Code) :-
  (is_number_code(Code); is_underscore_code(Code); is_dollar_sign_code(Code)),
  !.
to_lowercase_code(Code, Code).

inc_line -->>
  [add_line(1)]:position.

inc_col -->>
  [add_col(1)]:position.

add_col(N) -->>
  [add_col(N)]:position.

get_pos(Position) -->>
  [get_pos(Position)]:position.

acc_pos(add_col(I), pos(L, C), pos(L, C1)) :-
  C1 is C+I,
  !.
acc_pos(add_line(I), pos(L, _C), pos(L1, 1)) :-
  L1 is L+I,
  !.
acc_pos(get_pos(Position), Position, Position).

set_error(Error) -->>
  get_pos(Position):position,
  {set_error('Lexical', Error, Position)}.

set_error_Syntax(Error) -->>
  get_pos(Position):position,
  {set_error('Syntax', Error, Position)}.

non_visible_code(9).   % Tabulator
non_visible_code(13).  % carriage return

eoc([], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test :-
  test:test(lexer).

% Set of tests
% To test all of them: 
%   ?- lexer:test.

% All test names must be of the form testXXX,
% where XXX is a left-0-padded number.
test001 :-
  test(lexer, lex, "1 '2' ""3"" ", [int(1):pos(1,1),str('2'):pos(1,3),double_quotes_id('3'):pos(1,7)]). 

test002 :-
  test(lexer, lex, "1 '2' \"3\" ", [int(1):pos(1,1),str('2'):pos(1,3),double_quotes_id('3'):pos(1,7)]). 

test003 :-
  test(lexer, lex, "10 1234.34 -1 -43.0", [int(10):pos(1,1),frac(1234,34):pos(1,4),op(-):pos(1,12),int(1):pos(1,13),op(-):pos(1,15),frac(43,0):pos(1,16)]). 

test004 :-
  test(lexer, lex, ".1 -.12", [frac(0,1):pos(1,1),op(-):pos(1,4),frac(0,12):pos(1,5)]). 

test005 :-
  test(lexer, lex, "1e1 1e+1 1e-1 1.1e1 1.1e+1 1.1e-1", [float(1,0,1):pos(1,1),float(1,0,1):pos(1,5),float(1,0,-1):pos(1,10),float(1,1,1):pos(1,15),float(1,1,1):pos(1,21),float(1,1,-1):pos(1,28)]). 

test006 :-
  test(lexer, lex, " """" ""ab"" ""a""""b"" ", [double_quotes_id(''):pos(1,2),double_quotes_id(ab):pos(1,5),double_quotes_id('a"b'):pos(1,10)]).

test007 :-
  test(lexer, lex, "'ab' 1.0", [str(ab):pos(1,1),frac(1, 0):pos(1,6)]).

test008 :-
  test(lexer, lex, 'test/test001.sql', [cmd(select/u):pos(1,1),id(id/u):pos(1,8),punct(','):pos(1,10),id(age/l):pos(1,12),punct(nl):pos(1,15),cmd(from/u):pos(2,1),id(user1/l):pos(2,6),punct(nl):pos(2,11),cmd(where/u):pos(3,1),id(age/l):pos(3,7),comparisonOp(>):pos(3,11),int(18):pos(3,13),punct(;):pos(3,15)]).

test009 :-
  test(lexer, lex, 'test/test002.sql', [cmd(select/u):pos(1,1),id(nombreproducto/u):pos(1,8),punct(','):pos(1,22),id(precio/u):pos(1,24),cmd(from/u):pos(1,31),double_quotes_id('Productos'):pos(1,36),punct(nl):pos(1,47),cmd(where/u):pos(2,1),id(precio/u):pos(2,7),op(-):pos(2,14),punct('('):pos(2,16),cmd(select/l):pos(2,17),fn(avg/u):pos(2,24),punct('('):pos(2,27),id(precio/u):pos(2,28),punct(')'):pos(2,34),cmd(from/u):pos(2,36),id(productos/u):pos(2,41),punct(')'):pos(2,50),punct(;):pos(2,51)]).

test010 :-
  test(lexer, lex, 'test/test003.sql', [double_quotes_id(nOmbre):pos(1,1),double_quotes_id('no"3mbre'):pos(1,10),double_quotes_id('NOMBRE'):pos(1,22),punct(nl):pos(1,30),str(nOmbre):pos(2,1),str(nombre):pos(2,10),str('NOMBRE'):pos(2,19),punct(nl):pos(2,28),id(nombre/l):pos(3,1),id(nombre/l):pos(3,8),id(nombre/u):pos(3,15)]).

test011 :-
  test(lexer, lex, 'test/test004.sql', [str('X=\'\'\'X'):pos(1,1),id(a/l):pos(1,13),punct(nl):pos(1,14),str(s):pos(2,1),id(b/l):pos(2,5),punct(nl):pos(2,6),str('"s"'):pos(3,1),id(c/l):pos(3,7),punct(nl):pos(3,8),str('"s"s""\''):pos(4,1),punct(nl):pos(4,11),str('It\'s raining outside'):pos(5,1),id(d/l):pos(5,25),punct(nl):pos(5,26),str('O\'Connell'):pos(6,1),punct(nl):pos(6,13),str(' d '):pos(7,1),punct(nl):pos(7,6),str('O\'Connell'):pos(8,1),id(pie/l):pos(8,14),punct(nl):pos(8,17),str(' %e_ '):pos(9,1),punct(nl):pos(9,8),str(''):pos(10,1),punct(nl):pos(10,3),str('_12e'):pos(11,1)]).
      
test012 :-
  test(lexer, lex, 'test/test005.sql', [cmd(varchar2/l):pos(1,1),id(a_2/l):pos(1,10),punct(nl):pos(1,13),id(algo_/l):pos(2,1),punct(nl):pos(2,6),id('$t'/u):pos(3,1),id('$t1t'/u):pos(3,4),str('$T1.t'):pos(3,9),double_quotes_id('$T.1'):pos(3,17),punct(nl):pos(3,23),id('$v'/u):pos(4,1),str('$V$'):pos(4,4)]).

test013 :-
  test(lexer, lex, 'test/test006.sql', [cmd(select/l):pos(1,1),op(*):pos(1,8),comment('select -- Este * es  + un_ "comentario" \'de\' linea unica '):pos(1,10),punct(nl):pos(1,69),cmd(from/l):pos(2,1),id(tabla/l):pos(2,6)]).

test014 :-
  test(lexer, lex, 'test/test007.sql', [fn(times/l):pos(1,1),cmd(timestamp/l):pos(1,7),cmd(no/l):pos(1,17),textual_op(not/l):pos(1,20),fn(sign/l):pos(1,24),id(timesa/l):pos(1,29),id(times1/l):pos(1,36),punct(nl):pos(1,42),fn(substr/l):pos(2,1),id(substring/l):pos(2,8),punct(nl):pos(2,17)]).

test015 :-
  test(lexer, lex, 'test/test008.sql', [comment('\nEste es un comentario\nh\nde varias lneas\n'):pos(1,1),punct(nl):pos(5,3),int(1):pos(6,1)]).
  
test016 :-
  test(lexer, lex, 'test/test009.sql', [cmd(alter/l):pos(1,1),cmd((table)/l):pos(1,7),id(a/l):pos(1,13),cmd(add/l):pos(1,15),cmd(constraint/l):pos(1,20),cmd(primary/l):pos(1,31),cmd(key/l):pos(1,39),punct('('):pos(1,43),id(a/l):pos(1,44),punct(')'):pos(1,45),punct(;):pos(1,46),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(alter/l):pos(3,1),cmd((table)/u):pos(3,7),id(b/l):pos(3,13),cmd(drop/l):pos(3,15),cmd(constraint/l):pos(3,20),textual_op(not/l):pos(3,31),cmd(null/l):pos(3,35),id(b/l):pos(3,40),punct(;):pos(3,41),punct(nl):pos(3,42),punct(nl):pos(4,1),cmd(alter/l):pos(5,1),cmd((table)/l):pos(5,7),id(d/l):pos(5,13),cmd(add/l):pos(5,15),cmd(constraint/l):pos(5,20),cmd(check/l):pos(5,31),punct('('):pos(5,37),id(a/l):pos(5,38),comparisonOp(>):pos(5,39),int(0):pos(5,40),punct(')'):pos(5,41),punct(;):pos(5,42),punct(nl):pos(5,43),punct(nl):pos(6,1)]).

test017 :-
  test(lexer, lex, 'test/test010.sql', [cmd(select/u):pos(1,1),op(*):pos(1,8),cmd(from/u):pos(1,10),id(tabla/l):pos(1,15),cmd(where/u):pos(1,21),id(nombre/l):pos(1,27),comparisonOp(=):pos(1,34),str('Juan Prez'):pos(1,36),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(select/u):pos(3,1),op(*):pos(3,8),cmd(from/u):pos(3,10),id(customers/u):pos(3,15),punct(nl):pos(3,24),cmd(where/u):pos(4,1),id(customername/u):pos(4,7),cmd(like/u):pos(4,20),str('a%'):pos(4,25),punct(;):pos(4,29),punct(nl):pos(4,30),punct(nl):pos(5,1),cmd(insert/l):pos(6,1),cmd(into/l):pos(6,8),id(a/l):pos(6,13),cmd(values/l):pos(6,15),punct('('):pos(6,22),str(a1):pos(6,23),punct(')'):pos(6,27),punct(;):pos(6,28)]).

test018 :-
  test(lexer, lex, 'test/test011.sql', [punct(nl):pos(1,1),cmd(select/l):pos(2,1),op(*):pos(2,8),cmd(from/l):pos(2,10),id(t/l):pos(2,15),punct(','):pos(2,16),id(s/l):pos(2,17),cmd(where/l):pos(2,19),id(t/l):pos(2,25),punct('.'):pos(2,26),id(a/l):pos(2,27),comparisonOp(=):pos(2,28),id(s/l):pos(2,29),punct('.'):pos(2,30),id(a/l):pos(2,31),textual_op(and/l):pos(2,33),id(t/l):pos(2,37),punct('.'):pos(2,38),id(b/l):pos(2,39),comparisonOp(=):pos(2,40),id(s/l):pos(2,41),punct('.'):pos(2,42),id(b/l):pos(2,43),punct(;):pos(2,44),punct(nl):pos(2,45)]).  

test019 :-
  test(lexer, lex, 'test/test012.sql', [cmd(create/l):pos(1,1),textual_op(or/l):pos(1,8),cmd_fn(replace/l):pos(1,11),cmd(view/l):pos(1,19),id(v1_1/l):pos(1,24),punct('('):pos(1,28),id(a/l):pos(1,29),punct(')'):pos(1,30),cmd((as)/l):pos(1,32),cmd(select/l):pos(1,35),id(t1/l):pos(1,42),punct('.'):pos(1,44),id(a/l):pos(1,45),cmd(from/l):pos(1,47),id(v1_2/l):pos(1,52),id(t1/l):pos(1,57),punct(','):pos(1,59),id(v2_2/l):pos(1,60),id(t2/l):pos(1,65),cmd(where/l):pos(1,68),id(t1/l):pos(1,74),punct('.'):pos(1,76),id(a/l):pos(1,77),comparisonOp(=):pos(1,78),id(t2/l):pos(1,79),punct('.'):pos(1,81),id(a/l):pos(1,82),punct(nl):pos(1,83),punct(nl):pos(2,1),cmd(insert/l):pos(3,1),cmd(into/l):pos(3,8),id(t/l):pos(3,13),cmd(values/l):pos(3,15),punct('('):pos(3,22),int(1):pos(3,23),punct(','):pos(3,24),str('1'):pos(3,25),punct(')'):pos(3,28)]).  

test020 :-
  test(lexer, lex, "a1.2", [id(a1/l):pos(1,1),punct('.'):pos(1,3),int(2):pos(1,4)]).      

test021 :-
  test(lexer, lex, "1a", failure(error('Lexical', number, pos(1,2)))). 

test022 :-
  test(lexer, lex, "1.1a", failure(error('Lexical', fractional, pos(1,4)))).

test023 :-
  test(lexer, lex, "-1.a", failure(error('Lexical', fractional, pos(1,4)))).

test024 :-
  test(lexer, lex, "0.1E++2", failure(error('Lexical', exponent, pos(1,5)))).

test025 :-
  test(lexer, lex, "0.1e+2a", failure(error('Lexical', exponent, pos(1,7)))).

test026 :-
  test(lexer, lex, "10 \n 1.", failure(error('Lexical', fractional, pos(2,4)))).

test027 :-
  test(lexer, lex, "_1", failure(error('Lexical', token, pos(1, 1)))).

test028 :-
  test(lexer, lex, 'test/test013.sql',  [int(2):pos(1,1),punct(nl):pos(1,2),op(+):pos(2,1),int(2):pos(2,2),punct(nl):pos(2,3),op(-):pos(3,1),int(2):pos(3,2),punct(nl):pos(3,3),frac(2,2):pos(4,1),punct(nl):pos(4,4),op(+):pos(5,1),frac(2,2):pos(5,2),punct(nl):pos(5,5),op(-):pos(6,1),frac(2,2):pos(6,2),punct(nl):pos(6,5),float(2,0,2):pos(7,1),punct(nl):pos(7,4),float(2,0,-2):pos(8,1),punct(nl):pos(8,5),op(-):pos(9,1),float(2,0,2):pos(9,2),punct(nl):pos(9,5),op(-):pos(10,1),float(2,0,2):pos(10,2),punct(nl):pos(10,6),op(-):pos(11,1),float(2,0,-2):pos(11,2),punct(nl):pos(11,6),float(2,2,2):pos(12,1),punct(nl):pos(12,6),float(2,2,-2):pos(13,1),punct(nl):pos(13,7),op(+):pos(14,1),float(2,2,-2):pos(14,2),punct(nl):pos(14,8),op(-):pos(15,1),float(2,2,2):pos(15,2),punct(nl):pos(15,7),op(-):pos(16,1),float(2,2,-2):pos(16,2)]).

/*
test029 :-
  test(lexer, lex, "SELECT * FROM t WHERE a=$v$;", failure(error('Lexical', token, pos(1, 27)))).
*/

test029 :-
  test(lexer, lex, "delete from t1 /*", failure(error('Syntax', 'unclosed multiline comment', pos(1, 18)))).

punctuation('comilla') -->> "'",  !, inc_col.