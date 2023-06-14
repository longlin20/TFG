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
%     cmd(Command)
% - Functions
%     fn(Function/Arity)
% - Operators (symbolic and textual):
%     op(Operator)
%     comparisonOp(Operator)
% - Punctuation: ( ) , ; : "
% - (User) Identifiers:
%     id(Identifier). For numbers and strings

lex(Input) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens),
  print(Tokens).

lex(Input, Tokens) :-
  reset_error,
  (is_list(Input)
   -> Codes = Input
   ;  read_file_to_codes(Input, Codes, [])),
  lex_codes(Codes, Tokens).
 
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

separator(_Token, no) -->>
  eoc,
  !.
  
separator(op(_), no) -->>
  !,
  [].

separator(comparisonOp(_), no) -->>
  !,
  [].

separator(punct(_), no) -->>
  !,
  [].

separator(str(_), no) -->>
  !,
  [].

separator(quoted_id(_), no) -->>
  !,
  [].

separator(_Token, String) -->>
  string(String),
  !.

separator(_Token, QuotedID) -->>
  quoted_identifier(QuotedID),
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

token(quoted_id(Identifier)) -->>
  quoted_identifier(Identifier),
  !.

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
    Delimiter \== punct('"') }, % Excludes single quotes and double quotes
  !.

token(cmd(Command)) -->>
  command(Command),
  !.

token(fn(Function)) -->>
  function(Function),
  !.
  
token(op(Operator)) -->>
  textual_operator(Operator),
  !.

token(id(Identifier)) -->>
  identifier(Identifier),
  !.

token(_Error) -->>
  set_error(token),
  !, fail.

sql_comment_start -->> % Check for SQL comment start ('--')
 "--".

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

multi_line_comment_start -->> "/*".

multi_line_comment_content(Comment, Nesting) -->>
  multi_line_comment_codes(Codes, Nesting),
  {atom_codes(Comment, Codes)}.

multi_line_comment_codes([], 0) -->>
!.

multi_line_comment_codes([Code|Codes], Nesting) -->>
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

multi_line_comment_end -->> "*/".


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
operator('\\')   -->> "\\", !, inc_col.

comparison_operator('!=')  -->> "!=",  !, add_col(2). % inequality
comparison_operator('=')   -->> "=",   !, inc_col.
comparison_operator('>=')  -->> ">=",  !, add_col(2).
comparison_operator('<=')  -->> "<=",  !, add_col(2).
comparison_operator('<>')  -->> "<>",  !, add_col(2). % inequality
comparison_operator('>')   -->> ">",   !, inc_col.
comparison_operator('<')   -->> "<",   !, inc_col.

textual_operator('and') -->> lc("and"), not_more_char,  !, add_col(3).
textual_operator('or')  -->> lc("or"),  not_more_char,  !, add_col(2).
textual_operator('not') -->> lc("not"), not_more_char,  !, add_col(3).
textual_operator('xor') -->> lc("xor"), not_more_char,  !, add_col(3).
%textual_operator('rem') -->> lc("rem"), not_more_char,  !, add_col(3).
%textual_operator('div') -->> lc("div"), not_more_char,  !, add_col(3).

%punctuation quotes simple is in the last line
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

command('add')                              -->> lc("add"),                              not_more_char,  !,  add_col(3).       
command('all')                              -->> lc("all"),                              not_more_char,  !,  add_col(3).       
command('alter')                            -->> lc("alter"),                            not_more_char,  !,  add_col(5).       
command('any')                              -->> lc("any"),                              not_more_char,  !,  add_col(3).       
command('ascending')                        -->> lc("ascending"),                        not_more_char,  !,  add_col(9).  
command('asc')                              -->> lc("asc"),                              not_more_char,  !,  add_col(3).       
command('assume')                           -->> lc("assume"),                           not_more_char,  !,  add_col(6).      
command('as')                               -->> lc("as"),                               not_more_char,  !,  add_col(2).                  
command('between')                          -->> lc("between"),                          not_more_char,  !,  add_col(7).             
command('by')                               -->> lc("by"),                               not_more_char,  !,  add_col(2).   
command('cascade')                          -->> lc("cascade"),                          not_more_char,  !,  add_col(7).                         
command('case')                             -->> lc("case"),                             not_more_char,  !,  add_col(4).            
command('character')                        -->> lc("character"),                        not_more_char,  !,  add_col(9).           
command('char')                             -->> lc("char"),                             not_more_char,  !,  add_col(4).       
command('check')                            -->> lc("check"),                            not_more_char,  !,  add_col(5).            
command('column')                           -->> lc("column"),                           not_more_char,  !,  add_col(6).       
command('commit')                           -->> lc("commit"),                           not_more_char,  !,  add_col(6). 
command('constraints')                      -->> lc("constraints"),                      not_more_char,  !,  add_col(11).                
command('constraint')                       -->> lc("constraint"),                       not_more_char,  !,  add_col(10).       
command('create')                           -->> lc("create"),                           not_more_char,  !,  add_col(6).    
command('databases')                        -->> lc("databases"),                        not_more_char,  !,  add_col(9).                   
command('database')                         -->> lc("database"),                         not_more_char,  !,  add_col(8).    
command('datetime')                         -->> lc("datetime"),                         not_more_char,  !,  add_col(8).                  
command('date')                             -->> lc("date"),                             not_more_char,  !,  add_col(4).       
command('decimal')                          -->> lc("decimal"),                          not_more_char,  !,  add_col(7).           
command('default')                          -->> lc("default"),                          not_more_char,  !,  add_col(7).       
command('delete')                           -->> lc("delete"),                           not_more_char,  !,  add_col(6).       
command('descending')                       -->> lc("descending"),                       not_more_char,  !,  add_col(10).   
command('desc')                             -->> lc("desc"),                             not_more_char,  !,  add_col(4).        
command('describe')                         -->> lc("describe"),                         not_more_char,  !,  add_col(8).  
command('distinct')                         -->> lc("distinct"),                         not_more_char,  !,  add_col(8).             
command('drop')                             -->> lc("drop"),                             not_more_char,  !,  add_col(4).       
command('else')                             -->> lc("else"),                             not_more_char,  !,  add_col(4).       
command('end')                              -->> lc("end"),                              not_more_char,  !,  add_col(3).          
command('escape')                           -->> lc("escape"),                           not_more_char,  !,  add_col(6).              
command('except')                           -->> lc("except"),                           not_more_char,  !,  add_col(6).             
command('exists')                           -->> lc("exists"),                           not_more_char,  !,  add_col(6).                 
command('false')                            -->> lc("false"),                            not_more_char,  !,  add_col(5).       
command('fetch')                            -->> lc("fetch"),                            not_more_char,  !,  add_col(5).    
command('float')                            -->> lc("float"),                            not_more_char,  !,  add_col(5).          
command('foreign')                          -->> lc("foreign"),                          not_more_char,  !,  add_col(7).            
command('from')                             -->> lc("from"),                             not_more_char,  !,  add_col(4).       
command('full')                             -->> lc("full"),                             not_more_char,  !,  add_col(4).       
command('group')                            -->> lc("group"),                            not_more_char,  !,  add_col(5).             
command('having')                           -->> lc("having"),                           not_more_char,  !,  add_col(6).            
command('if')                               -->> lc("if"),                               not_more_char,  !,  add_col(2).           
command('inner')                            -->> lc("inner"),                            not_more_char,  !,  add_col(5).          
command('insert')                           -->> lc("insert"),                           not_more_char,  !,  add_col(6).           
command('intersect')                        -->> lc("intersect"),                        not_more_char,  !,  add_col(9).            
command('into')                             -->> lc("into"),                             not_more_char,  !,  add_col(4).       
command('integer')                          -->> lc("integer"),                          not_more_char,  !,  add_col(7).    
command('int')                              -->> lc("int"),                              not_more_char,  !,  add_col(3).       
command('in')                               -->> lc("in"),                               not_more_char,  !,  add_col(2).       
command('is')                               -->> lc("is"),                               not_more_char,  !,  add_col(2).            
command('join')                             -->> lc("join"),                             not_more_char,  !,  add_col(4).        
command('like')                             -->> lc("like"),                             not_more_char,  !,  add_col(4).            
command('natural')                          -->> lc("natural"),                          not_more_char,  !,  add_col(7).           
command('no')                               -->> lc("no"),                               not_more_char,  !,  add_col(2).       
command('null')                             -->> lc("null"),                             not_more_char,  !,  add_col(4).  
command('number')                           -->> lc("number"),                           not_more_char,  !,  add_col(6).      
command('numeric')                          -->> lc("numeric"),                          not_more_char,  !,  add_col(7).       
command('offset')                           -->> lc("offset"),                           not_more_char,  !,  add_col(6).       
command('only')                             -->> lc("only"),                             not_more_char,  !,  add_col(4).       
command('on')                               -->> lc("on"),                               not_more_char,  !,  add_col(2).       
command('order')                            -->> lc("order"),                            not_more_char,  !,  add_col(5).       
command('outer')                            -->> lc("outer"),                            not_more_char,  !,  add_col(5).                
command('primary')                          -->> lc("primary"),                          not_more_char,  !,  add_col(7).       
command('real')                             -->> lc("real"),                             not_more_char,  !,  add_col(4).       
command('recursive')                        -->> lc("recursive"),                        not_more_char,  !,  add_col(9).       
command('references')                       -->> lc("references"),                       not_more_char,  !,  add_col(10).    
command('rename')                           -->> lc("rename"),                           not_more_char,  !,  add_col(6).     
command('restrict')                         -->> lc("restrict"),                         not_more_char,  !,  add_col(8).           
command('rollback')                         -->> lc("rollback"),                         not_more_char,  !,  add_col(8).       
command('rows')                             -->> lc("rows"),                             not_more_char,  !,  add_col(4).       
command('savepoint')                        -->> lc("savepoint"),                        not_more_char,  !,  add_col(9).       
command('select')                           -->> lc("select"),                           not_more_char,  !,  add_col(6).          
command('set')                              -->> lc("set"),                              not_more_char,  !,  add_col(3).       
command('show')                             -->> lc("show"),                             not_more_char,  !,  add_col(4).        
command('smallint')                         -->> lc("smallint"),                         not_more_char,  !,  add_col(8).       
command('some')                             -->> lc("some"),                             not_more_char,  !,  add_col(4).
command('string')                           -->> lc("string"),                           not_more_char,  !,  add_col(6).
command('tables')                           -->> lc("tables"),                           not_more_char,  !,  add_col(6).              
command('table')                            -->> lc("table"),                            not_more_char,  !,  add_col(5).    
command('text')                             -->> lc("text"),                             not_more_char,  !,  add_col(4).          
command('then')                             -->> lc("then"),                             not_more_char,  !,  add_col(4).       
command('timestamp')                        -->> lc("timestamp"),                        not_more_char,  !,  add_col(9).       
command('time')                             -->> lc("time"),                             not_more_char,  !,  add_col(4).      
command('type')                             -->> lc("type"),                             not_more_char,  !,  add_col(4).        
command('to')                               -->> lc("to"),                               not_more_char,  !,  add_col(2).       
command('true')                             -->> lc("true"),                             not_more_char,  !,  add_col(4).       
command('union')                            -->> lc("union"),                            not_more_char,  !,  add_col(5).       
command('unique')                           -->> lc("unique"),                           not_more_char,  !,  add_col(6).       
command('update')                           -->> lc("update"),                           not_more_char,  !,  add_col(6).       
command('using')                            -->> lc("using"),                            not_more_char,  !,  add_col(5).       
command('values')                           -->> lc("values"),                           not_more_char,  !,  add_col(6).    
command('varchar2')                         -->> lc("varchar2"),                         not_more_char,  !,  add_col(8).          
command('varchar')                          -->> lc("varchar"),                          not_more_char,  !,  add_col(7).  
command('views')                            -->> lc("views"),                            not_more_char,  !,  add_col(5).                               
command('view')                             -->> lc("view"),                             not_more_char,  !,  add_col(4).                               
command('when')                             -->> lc("when"),                             not_more_char,  !,  add_col(4).       
command('where')                            -->> lc("where"),                            not_more_char,  !,  add_col(5).       
command('with')                             -->> lc("with"),                             not_more_char,  !,  add_col(4).   
command('work')                             -->> lc("work"),                             not_more_char,  !,  add_col(4).    
     
function('sqrt'/1)                  -->> lc("sqrt"),                     not_more_char, !, add_col(4).
function('ln'/1)                    -->> lc("ln"),                       not_more_char, !, add_col(2).
function('log'/1)                   -->> lc("log"),                      not_more_char, !, add_col(3).
function('log'/2)                   -->> lc("log"),                      not_more_char, !, add_col(3).
function('sin'/1)                   -->> lc("sin"),                      not_more_char, !, add_col(3).
function('cos'/1)                   -->> lc("cos"),                      not_more_char, !, add_col(3).
function('tan'/1)                   -->> lc("tan"),                      not_more_char, !, add_col(3).
function('cot'/1)                   -->> lc("cot"),                      not_more_char, !, add_col(3).
function('asin'/1)                  -->> lc("asin"),                     not_more_char, !, add_col(4).
function('acos'/1)                  -->> lc("acos"),                     not_more_char, !, add_col(4).
function('atan'/1)                  -->> lc("atan"),                     not_more_char, !, add_col(4).
function('acot'/1)                  -->> lc("acot"),                     not_more_char, !, add_col(4).
function('abs'/1)                   -->> lc("abs"),                      not_more_char, !, add_col(3).
function('power'/2)                 -->> lc("power"),                    not_more_char, !, add_col(5).
function('exp'/1)                   -->> lc("exp"),                      not_more_char, !, add_col(3).
function('float_integer_part'/1)    -->> lc("float_integer_part"),       not_more_char, !, add_col(18).
function('float_fractional_part'/1) -->> lc("float_fractional_part"),    not_more_char, !, add_col(21).
function('year'/1)                  -->> lc("year"),                     not_more_char, !, add_col(4).
function('month'/1)                 -->> lc("month"),                    not_more_char, !, add_col(5).
function('day'/1)                   -->> lc("day"),                      not_more_char, !, add_col(3).
function('hour'/1)                  -->> lc("hour"),                     not_more_char, !, add_col(4).
function('minute'/1)                -->> lc("minute"),                   not_more_char, !, add_col(6).
function('second'/1)                -->> lc("second"),                   not_more_char, !, add_col(6).
function('float'/1)                 -->> lc("float"),                    not_more_char, !, add_col(5).
function('integer'/1)               -->> lc("integer"),                  not_more_char, !, add_col(7).
function('sign'/1)                  -->> lc("sign"),                     not_more_char, !, add_col(4).
function('gcd'/2)                   -->> lc("gcd"),                      not_more_char, !, add_col(3).
function('min'/2)                   -->> lc("min"),                      not_more_char, !, add_col(3).
function('max'/2)                   -->> lc("max"),                      not_more_char, !, add_col(3).
function('mod'/2)                   -->> lc("mod"),                      not_more_char, !, add_col(3).
function('truncate'/1)              -->> lc("truncate"),                 not_more_char, !, add_col(8).
function('truncate'/2)              -->> lc("truncate"),                 not_more_char, !, add_col(8).
function('trunc'/1)                 -->> lc("trunc"),                    not_more_char, !, add_col(5).
function('trunc'/2)                 -->> lc("trunc"),                    not_more_char, !, add_col(5).
function('round'/1)                 -->> lc("round"),                    not_more_char, !, add_col(5).
function('round'/2)                 -->> lc("round"),                    not_more_char, !, add_col(5).
function('floor'/1)                 -->> lc("floor"),                    not_more_char, !, add_col(5).
function('ceiling'/1)               -->> lc("ceiling"),                  not_more_char, !, add_col(7).
function('rand'/1)                  -->> lc("rand"),                     not_more_char, !, add_col(4).
function('rand'/2)                  -->> lc("rand"),                     not_more_char, !, add_col(4).
function('concat'/2)                -->> lc("concat"),                   not_more_char, !, add_col(6).
function('length'/1)                -->> lc("length"),                   not_more_char, !, add_col(6).
function('lower'/1)                 -->> lc("lower"),                    not_more_char, !, add_col(5).
function('lpad'/2)                  -->> lc("lpad"),                     not_more_char, !, add_col(4).
function('lpad'/3)                  -->> lc("lpad"),                     not_more_char, !, add_col(4).
function('rpad'/2)                  -->> lc("rpad"),                     not_more_char, !, add_col(4).
function('rpad'/3)                  -->> lc("rpad"),                     not_more_char, !, add_col(4).
function('instr'/2)                 -->> lc("instr"),                    not_more_char, !, add_col(5).
function('replace'/3)               -->> lc("replace"),                  not_more_char, !, add_col(7).
function('reverse'/1)               -->> lc("reverse"),                  not_more_char, !, add_col(7).
function('substr'/3)                -->> lc("substr"),                   not_more_char, !, add_col(6).
function('upper'/1)                 -->> lc("upper"),                    not_more_char, !, add_col(5).
function('left'/2)                  -->> lc("left"),                     not_more_char, !, add_col(4).
function('ltrim'/1)                 -->> lc("ltrim"),                    not_more_char, !, add_col(5).
function('rtrim'/1)                 -->> lc("rtrim"),                    not_more_char, !, add_col(5).
function('trim'/1)                  -->> lc("trim"),                     not_more_char, !, add_col(4).
function('repeat'/2)                -->> lc("repeat"),                   not_more_char, !, add_col(6).
function('right'/2)                 -->> lc("right"),                    not_more_char, !, add_col(5).
function('space'/1)                 -->> lc("space"),                    not_more_char, !, add_col(5).
function('datetime_add'/2)          -->> lc("datetime_add"),             not_more_char, !, add_col(12).
function('datetime_sub'/2)          -->> lc("datetime_sub"),             not_more_char, !, add_col(12).
function('add_months'/2)            -->> lc("add_months"),               not_more_char, !, add_col(10).
function('current_time'/0)          -->> lc("current_time"),             not_more_char, !, add_col(12).
function('current_datetime'/0)      -->> lc("current_datetime"),         not_more_char, !, add_col(16).
function('current_date'/0)          -->> lc("current_date"),             not_more_char, !, add_col(12).
function('sysdate'/0)               -->> lc("sysdate"),                  not_more_char, !, add_col(7).
function('to_char'/1)               -->> lc("to_char"),                  not_more_char, !, add_col(7).
function('to_char'/2)               -->> lc("to_char"),                  not_more_char, !, add_col(7).
function('to_date'/1)               -->> lc("to_date"),                  not_more_char, !, add_col(7).
function('to_date'/2)               -->> lc("to_date"),                  not_more_char, !, add_col(7).
function('cast'/2)                  -->> lc("cast"),                     not_more_char, !, add_col(4).
function('coalesce'/n)              -->> lc("coalesce"),                 not_more_char, !, add_col(8).
function('greatest'/n)              -->> lc("greatest"),                 not_more_char, !, add_col(8).
function('iif'/3)                   -->> lc("iif"),                      not_more_char, !, add_col(3).
function('least'/n)                 -->> lc("least"),                    not_more_char, !, add_col(5).
function('nvl2'/3)                  -->> lc("nvl2"),                     not_more_char, !, add_col(4).
function('nvl'/2)                   -->> lc("nvl"),                      not_more_char, !, add_col(3).
function('nullif'/2)                -->> lc("nullif"),                   not_more_char, !, add_col(6).
function('avg'/1)                   -->> lc("avg"),                      not_more_char, !, add_col(3).
function('count'/1)                 -->> lc("count"),                    not_more_char, !, add_col(5).
function('count'/0)                 -->> lc("count"),                    not_more_char, !, add_col(5).
function('max'/1)                   -->> lc("max"),                      not_more_char, !, add_col(3).
function('min'/1)                   -->> lc("min"),                      not_more_char, !, add_col(3).
function('sum'/1)                   -->> lc("sum"),                      not_more_char, !, add_col(3).
function('times'/1)                 -->> lc("times"),                    not_more_char, !, add_col(5).
function('extract'/1)               -->> lc("extract"),                  not_more_char, !, add_col(7).    

function(Fn/1)        -->>
  udef_fn_name(Fn),
  !,
  {atom_length(Fn, L)},
   add_col(L).

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
      is_number_code(C) }.

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

quoted_identifier(Identifier) -->>
  """",
  rest_of_quotes_id(Identifier).
  
% rest_of_string(-String)//
rest_of_quotes_id(Identifier) -->>
  quotes_id_codes(IdentifierCodes),
  """",
  !,
  {atom_codes(Identifier, IdentifierCodes),
   length(IdentifierCodes, Length),
   Cols is Length+2},
  add_col(Cols).
rest_of_quotes_id(_IdentifierCodes) -->>
  set_error(quoted_id),
  {!, fail}.

% string_codes(-Codes)//
quotes_id_codes([Code|Codes]) -->>
  """""", % Escaped double quotes
  !,
  inc_col,
  {"""" = [Code]},
  quotes_id_codes(Codes).
quotes_id_codes([]) -->> % End of string
  {[C]=""""},
  dcg/[C|_], % Lookahead. right-hand contexts unsupported in -->>
  !.
quotes_id_codes([Code|Codes]) -->>
  [Code],
  quotes_id_codes(Codes).

% Rule to recognize an identifier
identifier(Identifier) -->>
  (letter(Code); dollar_sign(Code); underscore(Code)), % Allow letters or underscores as the first character
  alphanum_star(Codes),
  {atom_codes(Identifier, [Code|Codes])},
  {length([Code|Codes], Length)},
  add_col(Length).

identifier(_Identifier) -->>
  set_error(identifier),
  {!, fail}.

% alphanum_star(-Codes)//
% Zero or more alphanumeric codes
alphanum_star([Code|Codes]) -->>
  ( letter(Code)
  ; digit_code(Code)
  ; underscore(Code)),
  alphanum_star(Codes).
alphanum_star([]) -->>
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
  "$" = [DSCode],
  Code == DSCode.

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
  (is_number_code(Code); is_underscore_code(Code)),
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
non_visible_code(13).  % carriage return

eoc([], []).

udef_fn_name(Fn) -->>
  identifier(Fn),
  {udef_fn_name(Fn)}.

udef_fn_name(Fn) :-
  atom_concat(fn, T, Fn),
  T \== ''.
  
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
  test(lexer, lex, "1 '2' ""3"" ", [int(1):pos(1,1),str('2'):pos(1,3),quoted_id('3'):pos(1,7)]). 

test002 :-
  test(lexer, lex, "1 '2' \"3\" ", [int(1):pos(1,1),str('2'):pos(1,3),quoted_id('3'):pos(1,7)]). 

test003 :-
  test(lexer, lex, "10 1234.34 -1 -43.0", [int(10):pos(1,1),frac(1234,34):pos(1,4),op(-):pos(1,12),int(1):pos(1,13),op(-):pos(1,15),frac(43,0):pos(1,16)]). 

test004 :-
  test(lexer, lex, ".1 -.12", [frac(0,1):pos(1,1),op(-):pos(1,4),frac(0,12):pos(1,5)]). 

test005 :-
  test(lexer, lex, "1e1 1e+1 1e-1 1.1e1 1.1e+1 1.1e-1", [float(1,0,1):pos(1,1),float(1,0,1):pos(1,5),float(1,0,-1):pos(1,10),float(1,1,1):pos(1,15),float(1,1,1):pos(1,21),float(1,1,-1):pos(1,28)]). 

test006 :-
  test(lexer, lex, " """" ""ab"" ""a""""b"" ", [quoted_id(''):pos(1,2),quoted_id(ab):pos(1,5),quoted_id('a"b'):pos(1,10)]).

test007 :-
  test(lexer, lex, "'ab' 1.0", [str(ab):pos(1,1),frac(1, 0):pos(1,6)]).

test008 :-
  test(lexer, lex, 'test/test001.sql', [cmd(select):pos(1,1),id(id):pos(1,8),punct(','):pos(1,10),id(age):pos(1,12),punct(nl):pos(1,15),cmd(from):pos(2,1),id(user1):pos(2,6),punct(nl):pos(2,11),cmd(where):pos(3,1),id(age):pos(3,7),comparisonOp(>):pos(3,11),int(18):pos(3,13),punct(;):pos(3,15)]).

test009 :-
  test(lexer, lex, 'test/test002.sql', [cmd(select):pos(1,1),id(nombreproducto):pos(1,8),punct(','):pos(1,22),id(precio):pos(1,24),cmd(from):pos(1,31),quoted_id('Productos'):pos(1,36),punct(nl):pos(1,47),cmd(where):pos(2,1),id(precio):pos(2,7),op(-):pos(2,14),punct('('):pos(2,16),cmd(select):pos(2,17),fn(avg/1):pos(2,24),punct('('):pos(2,27),id(precio):pos(2,28),punct(')'):pos(2,34),cmd(from):pos(2,36),id(productos):pos(2,41),punct(')'):pos(2,50),punct(;):pos(2,51)]).

test010 :-
  test(lexer, lex, 'test/test003.sql', [quoted_id(nOmbre):pos(1,1),quoted_id('no"3mbre'):pos(1,10),quoted_id('NOMBRE'):pos(1,22),punct(nl):pos(1,30),str(nOmbre):pos(2,1),str(nombre):pos(2,10),str('NOMBRE'):pos(2,19),punct(nl):pos(2,28),id(nombre):pos(3,1),id(nombre):pos(3,8),id(nombre):pos(3,15)]).

test011 :-
  test(lexer, lex, 'test/test004.sql', [str('X=\'\'\'X'):pos(1,1),id(a):pos(1,13),punct(nl):pos(1,14),str(s):pos(2,1),id(b):pos(2,5),punct(nl):pos(2,6),str('"s"'):pos(3,1),id(c):pos(3,7),punct(nl):pos(3,8),str('"s"s""\''):pos(4,1),punct(nl):pos(4,11),str('It\'s raining outside'):pos(5,1),id(d):pos(5,25),punct(nl):pos(5,26),str('O\'Connell'):pos(6,1),punct(nl):pos(6,13),str(' d '):pos(7,1),punct(nl):pos(7,6),str('O\'Connell'):pos(8,1),id(e):pos(8,14),punct(nl):pos(8,15),str(' %e_ '):pos(9,1),punct(nl):pos(9,8),str(''):pos(10,1),punct(nl):pos(10,3),str('_12e'):pos(11,1)]).
      
test012 :-
  test(lexer, lex, 'test/test005.sql', [id('_algo'):pos(1,1),punct(nl):pos(1,6),cmd(varchar2):pos(2,1),id(a_2):pos(2,10),punct(nl):pos(2,13),id(algo_):pos(3,1),punct(nl):pos(3,6),id('__e'):pos(4,1),punct(nl):pos(4,4),id(e__):pos(5,1),punct(nl):pos(5,4),id('_e_'):pos(6,1),punct(nl):pos(6,4)]).

test013 :-
  test(lexer, lex, 'test/test006.sql', [cmd(select):pos(1,1),op(*):pos(1,8),comment('select -- Este * es  + un_ "comentario" \'de\' linea unica '):pos(1,10),punct(nl):pos(1,67),cmd(from):pos(2,1),id(tabla):pos(2,6)]).

test014 :-
  test(lexer, lex, 'test/test007.sql', [fn(times/1):pos(1,1),cmd(timestamp):pos(1,7),cmd(no):pos(1,17),op(not):pos(1,20),fn(sign/1):pos(1,24),id(timesa):pos(1,29),id(times1):pos(1,36),punct(nl):pos(1,42),fn(substr/3):pos(2,1),id(substring):pos(2,8),punct(nl):pos(2,17)]).

test015 :-
  test(lexer, lex, 'test/test008.sql', [comment('\nEste es un comentario\nh\nde varias lneas\n'):pos(1,1),punct(nl):pos(5,1),int(1):pos(6,1)]).
  
test016 :-
  test(lexer, lex, 'test/test009.sql', [cmd(alter):pos(1,1),cmd(table):pos(1,7),id(a):pos(1,13),cmd(add):pos(1,15),cmd(constraint):pos(1,20),cmd(primary):pos(1,31),id(key):pos(1,39),punct('('):pos(1,43),id(a):pos(1,44),punct(')'):pos(1,45),punct(;):pos(1,46),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(alter):pos(3,1),cmd(table):pos(3,7),id(b):pos(3,13),cmd(drop):pos(3,15),cmd(constraint):pos(3,20),op(not):pos(3,31),cmd(null):pos(3,35),id(b):pos(3,40),punct(;):pos(3,41),punct(nl):pos(3,42),punct(nl):pos(4,1),cmd(alter):pos(5,1),cmd(table):pos(5,7),id(d):pos(5,13),cmd(add):pos(5,15),cmd(constraint):pos(5,20),cmd(check):pos(5,31),punct('('):pos(5,37),id(a):pos(5,38),comparisonOp(>):pos(5,39),int(0):pos(5,40),punct(')'):pos(5,41),punct(;):pos(5,42),punct(nl):pos(5,43),punct(nl):pos(6,1)]).

test017 :-
  test(lexer, lex, 'test/test010.sql', [cmd(select):pos(1,1),op(*):pos(1,8),cmd(from):pos(1,10),id(tabla):pos(1,15),cmd(where):pos(1,21),id(nombre):pos(1,27),comparisonOp(=):pos(1,34),str('Juan Prez'):pos(1,36),punct(nl):pos(1,47),punct(nl):pos(2,1),cmd(select):pos(3,1),op(*):pos(3,8),cmd(from):pos(3,10),id(customers):pos(3,15),punct(nl):pos(3,24),cmd(where):pos(4,1),id(customername):pos(4,7),cmd(like):pos(4,20),str('a%'):pos(4,25),punct(;):pos(4,29),punct(nl):pos(4,30),punct(nl):pos(5,1),cmd(insert):pos(6,1),cmd(into):pos(6,8),id(a):pos(6,13),cmd(values):pos(6,15),punct('('):pos(6,22),str(a1):pos(6,23),punct(')'):pos(6,27),punct(;):pos(6,28)]).

test018 :-
  test(lexer, lex, 'test/test011.sql', [punct(nl):pos(1,1),cmd(select):pos(2,1),op(*):pos(2,8),cmd(from):pos(2,10),id(t):pos(2,15),punct(','):pos(2,16),id(s):pos(2,17),cmd(where):pos(2,19),id(t):pos(2,25),punct('.'):pos(2,26),id(a):pos(2,27),comparisonOp(=):pos(2,28),id(s):pos(2,29),punct('.'):pos(2,30),id(a):pos(2,31),op(and):pos(2,33),id(t):pos(2,37),punct('.'):pos(2,38),id(b):pos(2,39),comparisonOp(=):pos(2,40),id(s):pos(2,41),punct('.'):pos(2,42),id(b):pos(2,43),punct(;):pos(2,44),punct(nl):pos(2,45)]).  

test019 :-
  test(lexer, lex, 'test/test012.sql', [cmd(create):pos(1,1),op(or):pos(1,8),fn(replace/3):pos(1,11),cmd(view):pos(1,19),id(v1_1):pos(1,24),punct('('):pos(1,28),id(a):pos(1,29),punct(')'):pos(1,30),cmd(as):pos(1,32),cmd(select):pos(1,35),id(t1):pos(1,42),punct('.'):pos(1,44),id(a):pos(1,45),cmd(from):pos(1,47),id(v1_2):pos(1,52),id(t1):pos(1,57),punct(','):pos(1,59),id(v2_2):pos(1,60),id(t2):pos(1,65),cmd(where):pos(1,68),id(t1):pos(1,74),punct('.'):pos(1,76),id(a):pos(1,77),comparisonOp(=):pos(1,78),id(t2):pos(1,79),punct('.'):pos(1,81),id(a):pos(1,82),punct(nl):pos(1,83),punct(nl):pos(2,1),cmd(insert):pos(3,1),cmd(into):pos(3,8),id(t):pos(3,13),cmd(values):pos(3,15),punct('('):pos(3,22),int(1):pos(3,23),punct(','):pos(3,24),str('1'):pos(3,25),punct(')'):pos(3,28)]).  

test020 :-
  test(lexer, lex, "a1.2", [id(a1):pos(1,1),punct('.'):pos(1,3),int(2):pos(1,4)]).      

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
  test(lexer, lex, "as_e@", failure(error('Lexical', identifier, pos(1, 1)))).

test028 :-
  test(lexer, lex, 'test/test013.sql',  [id('$1'):pos(1, 1), id('$t1t'):pos(1, 4), str('$T1.t'):pos(1, 9), quoted_id('$T.1'):pos(1, 17)]).

punctuation('comilla') -->> "'",  !, inc_col.