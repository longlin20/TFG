:- module(lexer,
          [ lex/2,
            lex/1,
            is_lowercase_letter_code/1 ]).

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
% - Operators (symbolic and textual):
%     op(Operator)
%     comparison_op(Operator)
%     textual_op(Operator/Case)
% - Remark(-Remark)
% - Punctuation: ( ) , ; : ...
% - (User) Identifiers:
%     id(Identifier/Case).
%   (User) Quoted Identifiers:
%     double_quotes_id(Identifier).
%     back_quotes_id(Identifier).
%     square_brackets_id(Identifier).

lex(Input) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens),
  %print(Tokens),
  forall(member(Token, Tokens), writeln(Token)).

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

separator(comparison_op(_), no) -->>
  !,
  [].

% If the previous token is a punctuation mark, there is no need for a separator
separator(punct(_), no) -->>
  !,
  [].

% If the previous token is a string, there is no need for a separator
separator(str(_), no) -->>
  !,
  [].

% If the previous token is a quotes id, there is no need for a separator
separator(double_quotes_id(_), no) -->>
  !,
  [].


separator(back_quotes_id(_), no) -->>
  !,
  [].

% If the previous token is a square brackets id, there is no need for a separator
separator(square_brackets_id(_), no) -->>
  !,
  [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIER BUT SEMICOLON and QUOTES IDENTIFIER BUT QUOTES 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
separator(cmd(savepoint/_), Id) -->>
  quotes_identifier_but_quotes(Id),
  !.

separator(cmd(savepoint/_), Id) -->>
  identifier_but_semicolon(Id),
  !.

separator(cmd(savepoint/_), no) -->>
  !,
  [].

% If next codes are a string, there is no need for a separator
separator(_Token, String) -->>
  string(String),
  !.

/*
separator(_Token, QuotedID) -->>
  double_quotes_identifier(QuotedID),
  !.

separator(_Token, QuotedID) -->>
  back_quotes_identifier(QuotedID),
  !.

separator(_Token, QuotedID) -->>
  square_brackets_identifier(QuotedID),
  !.
*/

% If next code is a newline, there is no need for a separator
separator(_Token, punct(nl)) -->>
  "\n",
  !,
  inc_line.

% If next codes are a delimiter mark, there is no need for a separator
separator(_Token, Delimiter) -->>
  delimiter(Delimiter),
  !.

% Otherwise, a separator is needed
separator(_Token, no) -->>
  separator.

% separators//
% One or more separators.
separators -->>
  skip_non_visible,
  separator,
  !,
  separators_star.

% separators_star//: Zero or more separators.
separators_star -->>
  separators,
  !.
  
separators_star -->>
  skip_non_visible,
  [].

% separator//
% One separator (blank, tabulator, end of file)
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
separator -->>
  set_error('unclosed delimited ID or a separator or an unrecognized token'),
  !, fail.

% skip_non_visible//
% Skip non-visible characters
skip_non_visible -->>
  [C],
  {non_visible_code(C)},
  !,
  skip_non_visible.
skip_non_visible -->>
  [].

% token(-Token)//
% Tokens in the language
token(Number) -->>
  number(Number),
  !.
  
token(String) -->>
  string(String),
  !. 

token(Identifier) -->>
  double_quotes_identifier(Identifier),
  !.


token(Identifier) -->>
  back_quotes_identifier(Identifier),
  !.


token(Identifier) -->>
  square_brackets_identifier(Identifier),
  !.

token(comment(Comment)) -->> % SQL comments: include the rest of the line as the comment
  sql_comment_start,
  !,
  comment(Comment).

% Rule for recognising multi-line comments in SQL
token(comment(C)) -->> 
  multi_line_remark(C).


token(Delimiter) -->>
  delimiter(Delimiter),
  !.

token(cmd(Command/Original)) -->>
  command(Command/Chars),
  {atom_chars(Original, Chars)},
  !.

token(fn(Function/Original)) -->>
  function(Function/Chars),
  {atom_chars(Original, Chars)},
  !.
 
token(textual_op(Operator/Original)) -->>
  textual_operator(Operator/Chars),
  {atom_chars(Original, Chars)},
  !.

token(id(Identifier/Case)) -->>
  identifier(Identifier/Case),
  !.


token(_Error) -->>
  set_error(token),
  !, fail.


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

multi_line_comment_start -->>   "/*", add_col(2).
multi_line_comment_end -->> "*/" ,  add_col(2).

multi_line_remark(Comment) -->>
  multi_line_comment_start,
  remark_body(CommentCodes),
  multi_line_comment_end,
  { atom_codes(Comment, CommentCodes)}.

remark_body(CommentCodes) -->>
  chars_star_but_multi_line_remark_delimiters(Codes1),
  multi_line_remark(Comment),
  { atom_codes(Comment, CommentCodesList) },  % Convert the atom to a list of codes
  chars_star_but_multi_line_remark_delimiters(Codes2),
  { append([Codes1, CommentCodesList, Codes2], CommentCodes) }.
remark_body(CommentCodes) -->>
  chars_star_but_multi_line_remark_delimiters(CommentCodes).

chars_star_but_multi_line_remark_delimiters([]) -->>
  [].
chars_star_but_multi_line_remark_delimiters([]) -->>
  "/*",
  !,
  {fail}.
chars_star_but_multi_line_remark_delimiters([]) -->>
  "*/",
  !,
  {fail}.
chars_star_but_multi_line_remark_delimiters([C|Cs]) -->>
  [C],
  ({ C == 10 } -> inc_line ; inc_col),
  chars_star_but_multi_line_remark_delimiters(Cs).

delimiter(op(Delimiter)) -->>
  operator(Delimiter),
  !.

delimiter(comparison_op(Delimiter)) -->>
  comparison_operator(Delimiter),
  !.

delimiter(punct(Delimiter)) -->>
  punctuation(Delimiter), 
  !.

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
%mod is in function part
%function('mod')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
textual_operator('and'/Original) -->>  lc("and", Original),  not_more_char, !, add_col(3).
textual_operator('or'/Original) -->>   lc("or", Original),   not_more_char, !, add_col(2).
textual_operator('not'/Original) -->>  lc("not", Original),  not_more_char, !, add_col(3).
textual_operator('xor'/Original) -->>  lc("xor", Original),  not_more_char, !, add_col(3).
textual_operator('rem'/Original) -->>  lc("rem", Original),  not_more_char, !, add_col(3).
textual_operator('div'/Original) -->>  lc("div", Original),  not_more_char, !, add_col(3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PUNCTUATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
punctuation('(') -->> "(",   !, inc_col.
punctuation(')') -->> ")",   !, inc_col.
%punctuation('[') -->> "[",   !, inc_col.
%punctuation(']') -->> "]",   !, inc_col.
%punctuation('`') -->> "`",   !, inc_col.
punctuation(',') -->> ",",   !, inc_col.
punctuation('.') -->> ".",   !, inc_col.
punctuation(';') -->> ";",   !, inc_col.
punctuation('::') -->> "::", !, add_col(2).
punctuation(':') -->> ":",   !, inc_col.
%punctuation('"') -->> """",  !, inc_col.
punctuation('nl') -->> "\n", !, inc_line.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMAND
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
command('add'/Original) -->>          lc("add", Original),          not_more_char, !, add_col(3).
command('all'/Original) -->>          lc("all", Original),          not_more_char, !, add_col(3).
command('alter'/Original) -->>        lc("alter", Original),        not_more_char, !, add_col(5).
command('any'/Original) -->>          lc("any", Original),          not_more_char, !, add_col(3).
command('ascending'/Original) -->>    lc("ascending", Original),    not_more_char, !, add_col(9).
command('asc'/Original) -->>          lc("asc", Original),          not_more_char, !, add_col(3).
command('assume'/Original) -->>       lc("assume", Original),       not_more_char, !, add_col(6).
command('as'/Original) -->>           lc("as", Original),           not_more_char, !, add_col(2).
command('between'/Original) -->>      lc("between", Original),      not_more_char, !, add_col(7).
command('bc'/Original) -->>           lc("bc", Original),           not_more_char, !, add_col(2).
command('by'/Original) -->>           lc("by", Original),           not_more_char, !, add_col(2).
command('candidate'/Original) -->>    lc("candidate", Original),    not_more_char, !, add_col(9).
command('cascade'/Original) -->>      lc("cascade", Original),      not_more_char, !, add_col(7).
command('character'/Original) -->>    lc("character", Original),    not_more_char, !, add_col(9).
command('char'/Original) -->>         lc("char", Original),         not_more_char, !, add_col(4).
command('check'/Original) -->>        lc("check", Original),        not_more_char, !, add_col(5).
command('column'/Original) -->>       lc("column", Original),       not_more_char, !, add_col(6).
command('commit'/Original) -->>       lc("commit", Original),       not_more_char, !, add_col(6).
command('constraints'/Original) -->>  lc("constraints", Original),  not_more_char, !, add_col(11).
command('constraint'/Original) -->>   lc("constraint", Original),   not_more_char, !, add_col(10).
command('create'/Original) -->>       lc("create", Original),       not_more_char, !, add_col(6).
command('databases'/Original) -->>    lc("databases", Original),    not_more_char, !, add_col(9).
command('database'/Original) -->>     lc("database", Original),     not_more_char, !, add_col(8).
command('data'/Original) -->>         lc("data", Original),         not_more_char, !, add_col(4).
command('datetime'/Original) -->>     lc("datetime", Original),     not_more_char, !, add_col(8).
command('date'/Original) -->>         lc("date", Original),         not_more_char, !, add_col(4).
command('decimal'/Original) -->>      lc("decimal", Original),      not_more_char, !, add_col(7).
command('default'/Original) -->>      lc("default", Original),      not_more_char, !, add_col(7).
command('delete'/Original) -->>       lc("delete", Original),       not_more_char, !, add_col(6).
command('descending'/Original) -->>   lc("descending", Original),   not_more_char, !, add_col(10).
command('desc'/Original) -->>         lc("desc", Original),         not_more_char, !, add_col(4).
command('describe'/Original) -->>     lc("describe", Original),     not_more_char, !, add_col(8).
command('determined'/Original) -->>   lc("determined", Original),   not_more_char, !, add_col(10).
command('distinct'/Original) -->>     lc("distinct", Original),     not_more_char, !, add_col(8).
command('division'/Original) -->>     lc("division", Original),     not_more_char, !, add_col(8).
command('drop'/Original) -->>         lc("drop", Original),         not_more_char, !, add_col(4).
command('else'/Original) -->>         lc("else", Original),         not_more_char, !, add_col(4).
command('end'/Original) -->>          lc("end", Original),          not_more_char, !, add_col(3).
command('escape'/Original) -->>       lc("escape", Original),       not_more_char, !, add_col(6).
command('except'/Original) -->>       lc("except", Original),       not_more_char, !, add_col(6).
command('exists'/Original) -->>       lc("exists", Original),       not_more_char, !, add_col(6).
command('extract'/Original) -->>      lc("extract", Original),      not_more_char, !, add_col(7).
command('false'/Original) -->>        lc("false", Original),        not_more_char, !, add_col(5).
command('fetch'/Original) -->>        lc("fetch", Original),        not_more_char, !, add_col(5).
command('first'/Original) -->>        lc("first", Original),        not_more_char, !, add_col(5).
command('foreign'/Original) -->>      lc("foreign", Original),      not_more_char, !, add_col(7).
command('from'/Original) -->>         lc("from", Original),         not_more_char, !, add_col(4).
command('full'/Original) -->>         lc("full", Original),         not_more_char, !, add_col(4).
command('group'/Original) -->>        lc("group", Original),        not_more_char, !, add_col(5).
command('having'/Original) -->>       lc("having", Original),       not_more_char, !, add_col(6).
command('if'/Original) -->>           lc("if", Original),           not_more_char, !, add_col(2).
command('inner'/Original) -->>        lc("inner", Original),        not_more_char, !, add_col(5).
command('insert'/Original) -->>       lc("insert", Original),       not_more_char, !, add_col(6).
command('intersect'/Original) -->>    lc("intersect", Original),    not_more_char, !, add_col(9).
command('into'/Original) -->>         lc("into", Original),         not_more_char, !, add_col(4).
command('integer'/Original) -->>      lc("integer", Original),      not_more_char, !, add_col(7).
command('int'/Original) -->>          lc("int", Original),          not_more_char, !, add_col(3).
command('in'/Original) -->>           lc("in", Original),           not_more_char, !, add_col(2).
command('is'/Original) -->>           lc("is", Original),           not_more_char, !, add_col(2).
command('join'/Original) -->>         lc("join", Original),         not_more_char, !, add_col(4).
command('key'/Original) -->>          lc("key", Original),          not_more_char, !, add_col(3).
command('like'/Original) -->>         lc("like", Original),         not_more_char, !, add_col(4).
command('limit'/Original) -->>        lc("limit", Original),        not_more_char, !, add_col(5).
command('minus'/Original) -->>        lc("minus", Original),        not_more_char, !, add_col(5).
command('natural'/Original) -->>      lc("natural", Original),      not_more_char, !, add_col(7).
command('no'/Original) -->>           lc("no", Original),           not_more_char, !, add_col(2).
command('null'/Original) -->>         lc("null", Original),         not_more_char, !, add_col(4).
command('number'/Original) -->>       lc("number", Original),       not_more_char, !, add_col(6).
command('numeric'/Original) -->>      lc("numeric", Original),      not_more_char, !, add_col(7).
command('offset'/Original) -->>       lc("offset", Original),       not_more_char, !, add_col(6).
command('only'/Original) -->>         lc("only", Original),         not_more_char, !, add_col(4).
command('on'/Original) -->>           lc("on", Original),           not_more_char, !, add_col(2).
command('order'/Original) -->>        lc("order", Original),        not_more_char, !, add_col(5).
command('outer'/Original) -->>        lc("outer", Original),        not_more_char, !, add_col(5).
command('primary'/Original) -->>      lc("primary", Original),      not_more_char, !, add_col(7).
command('real'/Original) -->>         lc("real", Original),         not_more_char, !, add_col(4).
command('recursive'/Original) -->>    lc("recursive", Original),    not_more_char, !, add_col(9).
command('references'/Original) -->>   lc("references", Original),   not_more_char, !, add_col(10).
command('rename'/Original) -->>       lc("rename", Original),       not_more_char, !, add_col(6).
command('restrict'/Original) -->>     lc("restrict", Original),     not_more_char, !, add_col(8).
command('rollback'/Original) -->>     lc("rollback", Original),     not_more_char, !, add_col(8).
command('rows'/Original) -->>         lc("rows", Original),         not_more_char, !, add_col(4).
%warning special case savepoint + blanks
command('savepoint'/Original) -->>    lc("savepoint", Original),    not_more_char, blanks_add, !.
%only savepoint
command('savepoint'/Original) -->>    lc("savepoint", Original),    not_more_char, !, add_col(9).

command('select'/Original) -->>       lc("select", Original),       not_more_char, !, add_col(6).
command('set'/Original) -->>          lc("set", Original),          not_more_char, !, add_col(3).
command('show'/Original) -->>         lc("show", Original),         not_more_char, !, add_col(4).
command('smallint'/Original) -->>     lc("smallint", Original),     not_more_char, !, add_col(8).
command('some'/Original) -->>         lc("some", Original),         not_more_char, !, add_col(4).
command('string'/Original) -->>       lc("string", Original),       not_more_char, !, add_col(6).
command('tables'/Original) -->>       lc("tables", Original),       not_more_char, !, add_col(6).
command('table'/Original) -->>        lc("table", Original),        not_more_char, !, add_col(5).
command('text'/Original) -->>         lc("text", Original),         not_more_char, !, add_col(4).
command('then'/Original) -->>         lc("then", Original),         not_more_char, !, add_col(4).
command('timestamp'/Original) -->>    lc("timestamp", Original),    not_more_char, !, add_col(9).
command('time'/Original) -->>         lc("time", Original),         not_more_char, !, add_col(4).
command('type'/Original) -->>         lc("type", Original),         not_more_char, !, add_col(4).
command('top'/Original) -->>          lc("top", Original),          not_more_char, !, add_col(3).
command('to'/Original) -->>           lc("to", Original),           not_more_char, !, add_col(2).
command('true'/Original) -->>         lc("true", Original),         not_more_char, !, add_col(4).
command('union'/Original) -->>        lc("union", Original),        not_more_char, !, add_col(5).
command('unique'/Original) -->>       lc("unique", Original),       not_more_char, !, add_col(6).
command('update'/Original) -->>       lc("update", Original),       not_more_char, !, add_col(6).
command('using'/Original) -->>        lc("using", Original),        not_more_char, !, add_col(5).
command('values'/Original) -->>       lc("values", Original),       not_more_char, !, add_col(6).
command('varchar2'/Original) -->>     lc("varchar2", Original),     not_more_char, !, add_col(8).
command('varchar'/Original) -->>      lc("varchar", Original),      not_more_char, !, add_col(7).
command('views'/Original) -->>        lc("views", Original),        not_more_char, !, add_col(5).
command('view'/Original) -->>         lc("view", Original),         not_more_char, !, add_col(4).
command('when'/Original) -->>         lc("when", Original),         not_more_char, !, add_col(4).
command('where'/Original) -->>        lc("where", Original),        not_more_char, !, add_col(5).
command('with'/Original) -->>         lc("with", Original),         not_more_char, !, add_col(4).
command('work'/Original) -->>         lc("work", Original),         not_more_char, !, add_col(4).
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%          
function('sqrt'/Original) -->>                   lc("sqrt", Original),                   not_more_char, !, add_col(4).
function('ln'/Original) -->>                     lc("ln", Original),                     not_more_char, !, add_col(2).
function('log'/Original) -->>                    lc("log", Original),                    not_more_char, !, add_col(3).
function('exp'/Original) -->>                    lc("exp", Original),                    not_more_char, !, add_col(3).
function('sin'/Original) -->>                    lc("sin", Original),                    not_more_char, !, add_col(3).
function('cos'/Original) -->>                    lc("cos", Original),                    not_more_char, !, add_col(3).
function('tan'/Original) -->>                    lc("tan", Original),                    not_more_char, !, add_col(3).
function('cot'/Original) -->>                    lc("cot", Original),                    not_more_char, !, add_col(3).
function('asin'/Original) -->>                   lc("asin", Original),                   not_more_char, !, add_col(4).
function('acos'/Original) -->>                   lc("acos", Original),                   not_more_char, !, add_col(4).
function('atan'/Original) -->>                   lc("atan", Original),                   not_more_char, !, add_col(4).
function('acot'/Original) -->>                   lc("acot", Original),                   not_more_char, !, add_col(4).
function('abs'/Original) -->>                    lc("abs", Original),                    not_more_char, !, add_col(3).
function('mod'/Original) -->>                    lc("mod", Original),                    not_more_char, !, add_col(3).
function('float'/Original) -->>                  lc("float", Original),                  not_more_char, !, add_col(5).
function('integer'/Original) -->>                lc("integer", Original),                not_more_char, !, add_col(7).
function('sign'/Original) -->>                   lc("sign", Original),                   not_more_char, !, add_col(4).
function('gcd'/Original) -->>                    lc("gcd", Original),                    not_more_char, !, add_col(3).
function('min'/Original) -->>                    lc("min", Original),                    not_more_char, !, add_col(3).
function('max'/Original) -->>                    lc("max", Original),                    not_more_char, !, add_col(3).
function('truncate'/Original) -->>               lc("truncate", Original),               not_more_char, !, add_col(8).
function('trunc'/Original) -->>                  lc("trunc", Original),                  not_more_char, !, add_col(5).
function('float_integer_part'/Original) -->>     lc("float_integer_part", Original),     not_more_char, !, add_col(18).
function('float_fractional_part'/Original) -->>  lc("float_fractional_part", Original),  not_more_char, !, add_col(21).
function('round'/Original) -->>                  lc("round", Original),                  not_more_char, !, add_col(5).
function('floor'/Original) -->>                  lc("floor", Original),                  not_more_char, !, add_col(5).
function('ceiling'/Original) -->>                lc("ceiling", Original),                not_more_char, !, add_col(7).
function('rand'/Original) -->>                   lc("rand", Original),                   not_more_char, !, add_col(4).
function('power'/Original) -->>                  lc("power", Original),                  not_more_char, !, add_col(5).
function('avg'/Original) -->>                    lc("avg", Original),                    not_more_char, !, add_col(3).
function('avg_distinct'/Original) -->>           lc("avg_distinct", Original),           not_more_char, !, add_col(12).
function('count'/Original) -->>                  lc("count", Original),                  not_more_char, !, add_col(5).
function('count_distinct'/Original) -->>         lc("count_distinct", Original),         not_more_char, !, add_col(14).
function('sum'/Original) -->>                    lc("sum", Original),                    not_more_char, !, add_col(3).
function('sum_distinct'/Original) -->>           lc("sum_distinct", Original),           not_more_char, !, add_col(12).
function('times'/Original) -->>                  lc("times", Original),                  not_more_char, !, add_col(5).
function('times_distinct'/Original) -->>         lc("times_distinct", Original),         not_more_char, !, add_col(14).
function('pi'/Original) -->>                     lc("pi", Original),                     not_more_char, !, add_col(2).
function('e'/Original) -->>                      lc("e", Original),                      not_more_char, !, inc_col.
function('length'/Original) -->>                 lc("length", Original),                 not_more_char, !, add_col(6).
function('concat'/Original) -->>                 lc("concat", Original),                 not_more_char, !, add_col(6).
function('instr'/Original) -->>                  lc("instr", Original),                  not_more_char, !, add_col(5).
function('left'/Original) -->>                   lc("left", Original),                   not_more_char, !, add_col(4).
function('lower'/Original) -->>                  lc("lower", Original),                  not_more_char, !, add_col(5).
function('lpad'/Original) -->>                   lc("lpad", Original),                   not_more_char, !, add_col(4).
function('ltrim'/Original) -->>                  lc("ltrim", Original),                  not_more_char, !, add_col(5).
function('replace'/Original) -->>                lc("replace", Original),                not_more_char, !, add_col(7).
function('repeat'/Original) -->>                 lc("repeat", Original),                 not_more_char, !, add_col(6).
function('reverse'/Original) -->>                lc("reverse", Original),                not_more_char, !, add_col(7).
function('rpad'/Original) -->>                   lc("rpad", Original),                   not_more_char, !, add_col(4).
function('right'/Original) -->>                  lc("right", Original),                  not_more_char, !, add_col(5).
function('rtrim'/Original) -->>                  lc("rtrim", Original),                  not_more_char, !, add_col(5).
function('space'/Original) -->>                  lc("space", Original),                  not_more_char, !, add_col(5).
function('substr'/Original) -->>                 lc("substr", Original),                 not_more_char, !, add_col(6).
function('trim'/Original) -->>                   lc("trim", Original),                   not_more_char, !, add_col(4).
function('upper'/Original) -->>                  lc("upper", Original),                  not_more_char, !, add_col(5).
function('year'/Original) -->>                   lc("year", Original),                   not_more_char, !, add_col(4).
function('month'/Original) -->>                  lc("month", Original),                  not_more_char, !, add_col(5).
function('day'/Original) -->>                    lc("day", Original),                    not_more_char, !, add_col(3).
function('hour'/Original) -->>                   lc("hour", Original),                   not_more_char, !, add_col(4).
function('minute'/Original) -->>                 lc("minute", Original),                 not_more_char, !, add_col(6).
function('second'/Original) -->>                 lc("second", Original),                 not_more_char, !, add_col(6).
function('last_day'/Original) -->>               lc("last_day", Original),               not_more_char, !, add_col(8).
function('to_char'/Original) -->>                lc("to_char", Original),                not_more_char, !, add_col(7).
function('to_date'/Original) -->>                lc("to_date", Original),                not_more_char, !, add_col(7).
function('sysdate'/Original) -->>                lc("sysdate", Original),                not_more_char, !, add_col(7).
function('current_date'/Original) -->>           lc("current_date", Original),           not_more_char, !, add_col(12).
function('current_time'/Original) -->>           lc("current_time", Original),           not_more_char, !, add_col(12).
function('current_datetime'/Original) -->>       lc("current_datetime", Original),       not_more_char, !, add_col(16).
function('datetime_add'/Original) -->>           lc("datetime_add", Original),           not_more_char, !, add_col(12).
function('datetime_sub'/Original) -->>           lc("datetime_sub", Original),           not_more_char, !, add_col(12).
function('add_months'/Original) -->>             lc("add_months", Original),             not_more_char, !, add_col(10).
function('cast'/Original) -->>                   lc("cast", Original),                   not_more_char, !, add_col(4).
function('coalesce'/Original) -->>               lc("coalesce", Original),               not_more_char, !, add_col(8).
function('greatest'/Original) -->>               lc("greatest", Original),               not_more_char, !, add_col(8).
function('least'/Original) -->>                  lc("least", Original),                  not_more_char, !, add_col(5).
function('nvl'/Original) -->>                    lc("nvl", Original),                    not_more_char, !, add_col(3).
function('nvl2'/Original) -->>                   lc("nvl2", Original),                   not_more_char, !, add_col(4).
function('nullif'/Original) -->>                 lc("nullif", Original),                 not_more_char, !, add_col(6).
function('iif'/Original) -->>                    lc("iif", Original),                    not_more_char, !, add_col(3).
function('case'/Original) -->>                   lc("case", Original),                   not_more_char, !, add_col(4).


lc([Code|Codes], [Char|Chars]) -->>
  [C],
  { char_code(Char, C), to_lowercase_code(C, Code)},
  lc(Codes, Chars).
lc([], []) -->>
  [].

not_more_char -->>
    \+ is_more_char.
  
is_more_char -->>
    [C],
    { is_letter_code(C);
      is_underscore_code(C);
      is_number_code(C)}.

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
double_quotes_identifier(delimited_id(Identifier)) -->>
  """",
  rest_of_double_quotes_id(Identifier).
  
rest_of_double_quotes_id(Identifier) -->>
  letter_no_lc(Code),
  double_quotes_id_codes(IdentifierCodes),
  """",
  !,
  {atom_codes(Identifier, [Code|IdentifierCodes]),
   length([Code|IdentifierCodes], Length),
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
double_quotes_id_codes([]) -->>
  {[C]=""""},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
double_quotes_id_codes([Code|Codes]) -->>
  [Code],
  double_quotes_id_codes(Codes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BACK QUOTES IDENTIFIER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

back_quotes_identifier(delimited_id(Identifier)) -->>
  "`",
  rest_of_back_quotes_id(Identifier).

rest_of_back_quotes_id(Identifier) -->>
  letter_no_lc(Code),
  back_quotes_id_codes(IdentifierCodes),
  "`",
  !,
  {atom_codes(Identifier, [Code|IdentifierCodes]),
   length([Code|IdentifierCodes], Length),
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
  "\\`", % Escaped \`
  !,
  inc_col, %for \
  {"`" = [Code]},
  back_quotes_id_codes(Codes).
back_quotes_id_codes([]) -->>
  {[C]="`"},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
back_quotes_id_codes([Code|Codes]) -->>
  [Code],
  back_quotes_id_codes(Codes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SQUARE BRACKETS IDENTIFIER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

square_brackets_identifier(delimited_id(Identifier))  -->>
  "[",
  rest_of_square_brackets_id(Identifier).

rest_of_square_brackets_id(Identifier) -->>
  letter_no_lc(Code),
  square_brackets_id_codes(IdentifierCodes),
  "]",
  !,
  {atom_codes(Identifier, [Code|IdentifierCodes]),
  length([Code|IdentifierCodes], Length),
  Cols is Length+2},
  add_col(Cols).
rest_of_square_brackets_id(_IdentifierCodes) -->>
  set_error(back_quotes_id),
  {!, fail}.

square_brackets_id_codes([Code|Codes]) -->>
  "[[", % Escaped double square brackets
  !,
  inc_col,
  {"[" = [Code]},
  square_brackets_id_codes(Codes).
square_brackets_id_codes([Code|Codes]) -->>
  "]]", % Escaped double square brackets
  !,
  inc_col,
  {"]" = [Code]},
  square_brackets_id_codes(Codes).
  square_brackets_id_codes([Code|Codes]) -->>
  "\\[", % Escaped \[
  !,
  inc_col, %for \
  {"[" = [Code]},
  square_brackets_id_codes(Codes).
square_brackets_id_codes([Code|Codes]) -->>
  "\\]", % Escaped \]
  !,
  inc_col, %for \
  {"]" = [Code]},
  square_brackets_id_codes(Codes).
square_brackets_id_codes([]) -->>
  {[C]="]"},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
square_brackets_id_codes([]) -->>
  {[C]="["},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !,
  {fail}.
square_brackets_id_codes([Code|Codes]) -->>
  [Code],
  square_brackets_id_codes(Codes).


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
  letter_no_lc(CodeLetter),
  identifier_chars_star(Codes),
  {append([CodeDollar, CodeLetter], Codes, AllCodes)},
  {atom_codes(Identifier, AllCodes)},
  {length(AllCodes, Length)},
  add_col(Length).

identifier(Identifier/u) -->>
  letter_no_lc(Code),
  identifier_chars_star(Codes),
  {atom_codes(Identifier, [Code|Codes])},
  {length([Code|Codes], Length)},
  add_col(Length).

identifier(_Identifier) -->>
  set_error(identifier),
  {!, fail}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IDENTIFIER BUT SEMICOLON and QUOTES IDENTIFIER BUT QUOTES 
%return filename(ID) those is for SAVEPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% identifier_but_semicolon(-Remark)//
identifier_but_semicolon(filename(Id)) -->>
  identifier_but_semicolon_codes(Codes),
  !,
  {atom_codes(Id, Codes)}.

% remark_codes(-Remark)//
identifier_but_semicolon_codes([]) -->>
  dcg/[10|_], % Lookahead end of line
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[9|_], % Tabulator
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[13|_], % carriage return
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[32|_], % space
  !.
identifier_but_semicolon_codes([]) -->>
  dcg/[59|_], % ;
  !.
identifier_but_semicolon_codes([Code|Codes]) -->>
  [Code],
  inc_col,
  !,
  identifier_but_semicolon_codes(Codes).
identifier_but_semicolon_codes([]) -->> % No more codes are left to read
  [],
  !.

quotes_identifier_but_quotes(filename(Id)) -->>
  """",
  quotes_identifier_but_quotes_codes(Codes),
  """",
  !,
  add_col(2),
  {atom_codes(Id, Codes)}.

quotes_identifier_but_quotes_codes([]) -->>
  dcg/[10|_], % Lookahead end of line
  !.
quotes_identifier_but_quotes_codes([]) -->>
  dcg/[34|_], % " double quetes
  !.
quotes_identifier_but_quotes_codes([Code|Codes]) -->>
  [Code],
  inc_col,
  !,
  quotes_identifier_but_quotes_codes(Codes).
quotes_identifier_but_quotes_codes([]) -->> % No more codes are left to read
  [],
  !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alphanum_star(-Codes)//
% Zero or more alphanumeric codes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
identifier_chars_star([Code|Codes]) -->>
  ( letter_no_lc(Code)
  ; digit_code(Code)
  ; underscore(Code)),
  identifier_chars_star(Codes).
identifier_chars_star([]) -->>
  [].

letter_no_lc(Code) -->>
  [Code],
  {is_letter_code(Code)}.

% letter(-LetterCode)//
/*letter(LetterCode) -->>
  [Code],
  {is_letter_code(Code),
   to_lowercase_code(Code, LetterCode)}.*/

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
  DZ >= Code,
  !.

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

non_visible_code(9).   % Tabulator
non_visible_code(13).  % Carriage return

eoc([], []).

% Those predicate is for command(savepoint)
blanks_add -->>
  add_col(9), %this is for savepoint
  blanks.

blanks -->>
  skip_non_visible,
  blank,
  !,
  blanks_star.

blanks_star -->>
  blanks,
  !.
blanks_star -->>
  skip_non_visible,
  [].

blank -->>
  " ",
  inc_col:position,
  !.
blank -->>
  "\t",
  !.
blank -->>
  "end_of_file",
  !.
blank -->>
  "\n",
  !,
  inc_line.

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
  test(lexer, lex, "1 '2' ""l3"" ", [int(1):pos(1,1),str('2'):pos(1,3),delimited_id('l3'):pos(1,7)]). 

test002 :-
  test(lexer, lex, "1 '2' \"l3\" ", [int(1):pos(1,1),str('2'):pos(1,3),delimited_id('l3'):pos(1,7)]). 

test003 :-
  test(lexer, lex, "10 1234.34 -1 -43.0", [int(10):pos(1,1),frac(1234,34):pos(1,4),op(-):pos(1,12),int(1):pos(1,13),op(-):pos(1,15),frac(43,0):pos(1,16)]). 

test004 :-
  test(lexer, lex, ".1 -.12", [frac(0,1):pos(1,1),op(-):pos(1,4),frac(0,12):pos(1,5)]). 

test005 :-
  test(lexer, lex, "1e1 1e+1 1e-1 1.1e1 1.1e+1 1.1e-1", [float(1,0,1):pos(1,1),float(1,0,1):pos(1,5),float(1,0,-1):pos(1,10),float(1,1,1):pos(1,15),float(1,1,1):pos(1,21),float(1,1,-1):pos(1,28)]). 

test006 :-
  test(lexer, lex, " ""n"" ""ab"" ""a""""b"" ", [delimited_id(n):pos(1,2),delimited_id(ab):pos(1,6),delimited_id('a"b'):pos(1,11)]).

test007 :-
  test(lexer, lex, "'ab' 1.0", [str(ab):pos(1,1),frac(1, 0):pos(1,6)]).

test008 :-
  test(lexer, lex, 'test/test001.sql', [cmd(select/'SELECT'):pos(1,1),id('Id'/u):pos(1,8),punct(','):pos(1,10),id(age/l):pos(1,12),punct(nl):pos(1,15),cmd(from/'FROM'):pos(2,1),id(user1/l):pos(2,6),punct(nl):pos(2,11),cmd(where/'WHERE'):pos(3,1),id(age/l):pos(3,7),comparison_op(>):pos(3,11),int(18):pos(3,13),punct(;):pos(3,15)]).

test009 :-
  test(lexer, lex, 'test/test002.sql', [cmd(select/'SELECT'):pos(1,1),id('NombreProducto'/u):pos(1,8),punct(','):pos(1,22),id('Precio'/u):pos(1,24),cmd(from/'FROM'):pos(1,31),delimited_id('Productos'):pos(1,36),punct(nl):pos(1,47),cmd(where/'WHERE'):pos(2,1),id('Precio'/u):pos(2,7),op(-):pos(2,14),punct('('):pos(2,16),cmd(select/select):pos(2,17),fn(avg/'AVG'):pos(2,24),punct('('):pos(2,27),id('Precio'/u):pos(2,28),punct(')'):pos(2,34),cmd(from/'FROM'):pos(2,36),id('Productos'/u):pos(2,41),punct(')'):pos(2,50),punct(;):pos(2,51)]).

test010 :-
  test(lexer, lex, 'test/test003.sql', [delimited_id(nOmbre):pos(1,1),delimited_id('no"3mbre'):pos(1,10),delimited_id('NOMBRE'):pos(1,22),punct(nl):pos(1,30),str(nOmbre):pos(2,1),str(nombre):pos(2,10),str('NOMBRE'):pos(2,19),punct(nl):pos(2,28),id(nOmbre/l):pos(3,1),id(nombre/l):pos(3,8),id('NOMBRE'/u):pos(3,15)]).

test011 :-
  test(lexer, lex, 'test/test004.sql', [str('X=\'\'\'X'):pos(1,1),id(a/l):pos(1,13),punct(nl):pos(1,14),str(s):pos(2,1),id(b/l):pos(2,5),punct(nl):pos(2,6),str('"s"'):pos(3,1),id(c/l):pos(3,7),punct(nl):pos(3,8),str('"s"s""\''):pos(4,1),punct(nl):pos(4,11),str('It\'s raining outside'):pos(5,1),id(d/l):pos(5,25),punct(nl):pos(5,26),str('O\'Connell'):pos(6,1),punct(nl):pos(6,13),str(' d '):pos(7,1),punct(nl):pos(7,6),str('O\'Connell'):pos(8,1),id(pie/l):pos(8,14),punct(nl):pos(8,17),str(' %e_ '):pos(9,1),punct(nl):pos(9,8),str(''):pos(10,1),punct(nl):pos(10,3),str('_12e'):pos(11,1)]).
      
test012 :-
  test(lexer, lex, 'test/test005.sql', [cmd(varchar2/varchar2):pos(1,1),id(a_2/l):pos(1,10),punct(nl):pos(1,13),id(algo_/l):pos(2,1),punct(nl):pos(2,6),id('$T'/u):pos(3,1),id('$t1T'/u):pos(3,4),str('$T1.t'):pos(3,9),delimited_id('T.1'):pos(3,17),punct(nl):pos(3,22),id('$v'/u):pos(4,1),str('$V$'):pos(4,4)]).

test013 :-
  test(lexer, lex, 'test/test006.sql', [cmd(select/select):pos(1,1),op(*):pos(1,8),comment('select -- Este * es  + un_ "comentario" \'de\' linea unica '):pos(1,10),punct(nl):pos(1,69),cmd(from/from):pos(2,1),id(tabla/l):pos(2,6)]).

test014 :-
  test(lexer, lex, 'test/test007.sql', [fn(times/times):pos(1,1),cmd(timestamp/timestamp):pos(1,7),cmd(no/no):pos(1,17),textual_op(not/not):pos(1,20),fn(sign/sign):pos(1,24),id(timesa/l):pos(1,29),id(times1/l):pos(1,36),punct(nl):pos(1,42),fn(substr/substr):pos(2,1),id(substring/l):pos(2,8),punct(nl):pos(2,17)]).

test015 :-
  test(lexer, lex, 'test/test008.sql', [comment('\nUn comentario\nde\nvarias lineas\n'):pos(1,1),punct(nl):pos(5,3),int(1):pos(6,1)]).
  
test016 :-
  test(lexer, lex, 'test/test009.sql', [cmd(alter/alter):pos(1,1),cmd((table)/(table)):pos(1,7),id(t1/l):pos(1,13),cmd(add/add):pos(1,16),cmd(constraint/constraint):pos(1,20),cmd(primary/primary):pos(1,31),cmd(key/key):pos(1,39),punct('('):pos(1,43),id(a/l):pos(1,44),punct(')'):pos(1,45),punct(;):pos(1,46),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(alter/alter):pos(3,1),cmd((table)/'Table'):pos(3,7),id(t2/l):pos(3,13),cmd(drop/drop):pos(3,16),cmd(constraint/constraint):pos(3,21),textual_op(not/not):pos(3,32),cmd(null/null):pos(3,36),id(b/l):pos(3,41),punct(;):pos(3,42),punct(nl):pos(3,43),punct(nl):pos(4,1),cmd(alter/alter):pos(5,1),cmd((table)/(table)):pos(5,7),id(t3/l):pos(5,13),cmd(add/add):pos(5,16),cmd(constraint/constraint):pos(5,21),cmd(check/check):pos(5,32),punct('('):pos(5,38),id(a/l):pos(5,39),comparison_op(>):pos(5,40),int(0):pos(5,41),punct(')'):pos(5,42),punct(;):pos(5,43),punct(nl):pos(5,44),punct(nl):pos(6,1)]).

test017 :-
  test(lexer, lex, 'test/test010.sql', [cmd(select/'SELECT'):pos(1,1),op(*):pos(1,8),cmd(from/'FROM'):pos(1,10),id(tabla/l):pos(1,15),cmd(where/'WHERE'):pos(1,21),id(nombre/l):pos(1,27),comparison_op(=):pos(1,34),str('Juan Prez'):pos(1,36),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(select/'SELECT'):pos(3,1),op(*):pos(3,8),cmd(from/'FROM'):pos(3,10),id('Customers'/u):pos(3,15),punct(nl):pos(3,24),cmd(where/'WHERE'):pos(4,1),id('CustomerName'/u):pos(4,7),cmd(like/'LIKE'):pos(4,20),str('a%'):pos(4,25),punct(;):pos(4,29),punct(nl):pos(4,30),punct(nl):pos(5,1),cmd(insert/insert):pos(6,1),cmd(into/into):pos(6,8),id(a/l):pos(6,13),cmd(values/values):pos(6,15),punct('('):pos(6,22),str(a1):pos(6,23),punct(')'):pos(6,27),punct(;):pos(6,28)]).

test018 :-
  test(lexer, lex, 'test/test011.sql', [punct(nl):pos(1,1),cmd(select/select):pos(2,1),op(*):pos(2,8),cmd(from/from):pos(2,10),id(t/l):pos(2,15),punct(','):pos(2,16),id(s/l):pos(2,17),cmd(where/where):pos(2,19),id(t/l):pos(2,25),punct('.'):pos(2,26),id(a/l):pos(2,27),comparison_op(=):pos(2,28),id(s/l):pos(2,29),punct('.'):pos(2,30),id(a/l):pos(2,31),textual_op(and/and):pos(2,33),id(t/l):pos(2,37),punct('.'):pos(2,38),id(b/l):pos(2,39),comparison_op(=):pos(2,40),id(s/l):pos(2,41),punct('.'):pos(2,42),id(b/l):pos(2,43),punct(;):pos(2,44),punct(nl):pos(2,45)]).  

test019 :-
  test(lexer, lex, 'test/test012.sql', [cmd(create/create):pos(1,1),textual_op(or/or):pos(1,8),fn(replace/replace):pos(1,11),cmd(view/view):pos(1,19),id(v1_1/l):pos(1,24),punct('('):pos(1,28),id(a/l):pos(1,29),punct(')'):pos(1,30),cmd((as)/(as)):pos(1,32),cmd(select/select):pos(1,35),id(t1/l):pos(1,42),punct('.'):pos(1,44),id(a/l):pos(1,45),cmd(from/from):pos(1,47),id(v1_2/l):pos(1,52),id(t1/l):pos(1,57),punct(','):pos(1,59),id(v2_2/l):pos(1,60),id(t2/l):pos(1,65),cmd(where/where):pos(1,68),id(t1/l):pos(1,74),punct('.'):pos(1,76),id(a/l):pos(1,77),comparison_op(=):pos(1,78),id(t2/l):pos(1,79),punct('.'):pos(1,81),id(a/l):pos(1,82),punct(nl):pos(1,83),punct(nl):pos(2,1),cmd(insert/insert):pos(3,1),cmd(into/into):pos(3,8),id(t/l):pos(3,13),cmd(values/values):pos(3,15),punct('('):pos(3,22),int(1):pos(3,23),punct(','):pos(3,24),str('1'):pos(3,25),punct(')'):pos(3,28)]).  

test020 :-
  test(lexer, lex, "a1.2", [id(a1/l):pos(1,1),punct('.'):pos(1,3),int(2):pos(1,4)]).      

test021 :-
  test(lexer, lex, "1a", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1,2)))). 

test022 :-
  test(lexer, lex, "1.1a", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1,4)))).

test023 :-
  test(lexer, lex, "-1.a", failure(error('Lexical', fractional, pos(1,4)))).

test024 :-
  test(lexer, lex, "0.1E++2", failure(error('Lexical', exponent, pos(1,5)))).

test025 :-
  test(lexer, lex, "0.1e+2a", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1,7)))).

test026 :-
  test(lexer, lex, "10 \n 1.", failure(error('Lexical', fractional, pos(2,4)))).

test027 :-
  test(lexer, lex, "_1", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1, 1)))).

test028 :-
  test(lexer, lex, 'test/test013.sql',  [int(2):pos(1,1),punct(nl):pos(1,2),op(+):pos(2,1),int(2):pos(2,2),punct(nl):pos(2,3),op(-):pos(3,1),int(2):pos(3,2),punct(nl):pos(3,3),frac(2,2):pos(4,1),punct(nl):pos(4,4),op(+):pos(5,1),frac(2,2):pos(5,2),punct(nl):pos(5,5),op(-):pos(6,1),frac(2,2):pos(6,2),punct(nl):pos(6,5),float(2,0,2):pos(7,1),punct(nl):pos(7,4),float(2,0,-2):pos(8,1),punct(nl):pos(8,5),op(-):pos(9,1),float(2,0,2):pos(9,2),punct(nl):pos(9,5),op(-):pos(10,1),float(2,0,2):pos(10,2),punct(nl):pos(10,6),op(-):pos(11,1),float(2,0,-2):pos(11,2),punct(nl):pos(11,6),float(2,2,2):pos(12,1),punct(nl):pos(12,6),float(2,2,-2):pos(13,1),punct(nl):pos(13,7),op(+):pos(14,1),float(2,2,-2):pos(14,2),punct(nl):pos(14,8),op(-):pos(15,1),float(2,2,2):pos(15,2),punct(nl):pos(15,7),op(-):pos(16,1),float(2,2,-2):pos(16,2)]).

test029 :-
  test(lexer, lex, 'test/test029.sql', [cmd(savepoint/savepoint):pos(1,1),filename('+/&d'):pos(1,11),punct(nl):pos(1,15),cmd(savepoint/savepoint):pos(2,1),filename(e):pos(2,11),punct(;):pos(2,12),id(t/l):pos(2,13),punct(nl):pos(2,14),cmd(savepoint/savepoint):pos(3,1),filename('+/&d3'):pos(3,11),punct(nl):pos(3,18),cmd(savepoint/'Savepoint'):pos(4,1),filename(kkk):pos(4,11),punct(nl):pos(4,14),cmd(savepoint/savepoint):pos(5,1),filename(kkk):pos(5,13),punct(nl):pos(5,16),punct(nl):pos(6,1),cmd(savepoint/savepoint):pos(7,1),filename(kkk):pos(8,1),punct(;):pos(8,5),punct(nl):pos(8,6),cmd(savepoint/savepoint):pos(9,1),filename(kkk):pos(12,2),punct(;):pos(12,5)]).

test030 :-
  test(lexer, lex, "SELECT * FROM t WHERE a=$v$;", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1, 27)))).

test031 :-
  test(lexer, lex, 'test/test030.sql',  [delimited_id(t):pos(1,1),punct(nl):pos(1,4),delimited_id('t""'):pos(2,1),punct(nl):pos(2,6),delimited_id('t['):pos(3,1),punct(nl):pos(3,6),delimited_id('t['):pos(4,1),punct(nl):pos(4,6),delimited_id('t+1'):pos(5,1),punct(nl):pos(5,6),delimited_id('t%2'):pos(6,1),punct(nl):pos(6,6),delimited_id('t r w'):pos(7,1),punct(nl):pos(7,8),delimited_id('t$1'):pos(8,1),punct(nl):pos(8,6),delimited_id('no"'):pos(9,1),delimited_id('N"o'):pos(9,8),cmd(select/select):pos(9,15),punct(nl):pos(9,21),delimited_id('t'):pos(10,1),punct(nl):pos(10,4),delimited_id('t""'):pos(11,1),punct(nl):pos(11,6),delimited_id('t`'):pos(12,1),punct(nl):pos(12,6),delimited_id('t`'):pos(13,1),punct(nl):pos(13,6),delimited_id('t+1'):pos(14,1),punct(nl):pos(14,6),delimited_id('t%2'):pos(15,1),punct(nl):pos(15,6),delimited_id('t r w'):pos(16,1),punct(nl):pos(16,8),delimited_id('t$1'):pos(17,1),punct(nl):pos(17,6)]).

test032 :-
  test(lexer, lex, "[1]", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1, 1)))).

test033 :-
  test(lexer, lex, "savepoint", [cmd(savepoint/savepoint):pos(1,1)]).

test034 :-
  test(lexer, lex, "delete from t1 /*", [cmd(delete/delete):pos(1,1), cmd(from/from):pos(1,8), id(t1/l):pos(1,13), op(/):pos(1,16), op(*):pos(1,17)]).

test035 :-
  test(lexer, lex, "[t", failure(error('Lexical', 'unclosed delimited ID or a separator or an unrecognized token', pos(1, 1)))).

