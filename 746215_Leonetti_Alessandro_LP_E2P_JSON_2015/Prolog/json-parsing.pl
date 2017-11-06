% 728590 Trevi Alessandro
% 736424 Cassataro Riccardo
% 746215 Leonetti Alessandro
%
% parses any char out of string (if not empty)
%
char(C, [C | S], S).

% parses a string by cutting its chars from input stream one-by-one
%
string(S) -->
    foldl(char, S).

% parses a char satisfying a predicate
%
satisfy(Pred, C) -->
    char(C),
    { call(Pred, C) }.

between(L, H, C) :-
    L =< C,
    H >= C.

% parses a char in range (which is designated by one-char strings)
%
% usage: in_range("0", "9", C)
%
in_range([L], [H], C) -->
    satisfy(between(L, H), C).

% returns as list of zero or more sequental successes of parser
%
zero_or_more(Parser, List) -->
    one_or_more(Parser, List).

zero_or_more(_, []) -->
    {}.

% returns as list of one or more sequental successes of parser
%
one_or_more(Parser, [One | More]) -->
    call(Parser, One),
    zero_or_more(Parser, More).

% parses any count of sequental space, tab or newline characters
%
spaces -->
    char(Space),
    { member(Space, " \t\n") },
    spaces.

spaces -->
    {}.

% parses a token and all spaces after it
%
token(S) -->
    string(S),
    spaces.

% parses a number
%
json_number(N) -->
    one_or_more(in_range("0", "9"), Chars),
    { name(N, Chars) },
    spaces.

% parses any non double quote char or escaped double quote
%
json_char(C) -->
    string("\\\""), % escaped dquote

    { [C] = "\"" }.

json_char(C) -->
    char(C),

    { [C] \= "\"" }. % any non dquote

% parses json string
%
json_string(Name) -->
    { [DQuote] = "\"" },

    char(DQuote),
    zero_or_more(json_char, RawString),
    char(DQuote),
    spaces,

    { name(Name, RawString) }. % convert raw string back to atom

% parses any json-value
%
json(JSON) -->
    object(JSON);
    array(JSON);
    json_number(JSON);
    json_string(JSON).

% parses json object
%
object(json_object(Pairs)) -->
    token("{"),
    sepBy(token(","), pair, Pairs),
    token("}").

% parses json array
%
array(json_array(Array)) -->
    token("["),
    sepBy(token(","), json, Array),
    token("]").

% parses a (name : value) pair
%
pair((Key, Value)) -->
    json_string(Key),
    token(":"),
    json(Value).

% parses occurences of P separated by Sep
% only P's results are returned
%
sepBy(Sep, P, [X | Xs]) -->
    call(P, X),            % P
    sepByAux(Sep, P, Xs).  % many (Sep before P)

sepBy(_, _, []) -->
    [].

% helper, parses many occurences of P, each prefixed with Sep
%
sepByAux(Sep, P, [X | Xs]) -->
    call(Sep),
    call(P, X),
    sepByAux(Sep, P, Xs).

sepByAux(_, _, []) -->
    {}.

% parses a JSON value with any spaces before
%
json_parse_aux(JSON, Value) :-
    name(JSON, String),
    phrase((spaces, json(Value)), String, _).

% exported, wrapper; parses first possible interpretation of some
% stringified JSON
%
json_parse(JSON, Value) :-
    findall(Val, json_parse_aux(JSON, Val), [Value | _]).

% exported, wrapper; performs access to the parsed value by path
% stringified JSON
%
json_dot(JSON, Path, Value) :-
    findall(Val, json_dot_aux(JSON, Path, Val), [Value | _]).

% accesses structure of JSON value; guided by path in 2nd arg
%

% path ended?
%
json_dot_aux(It, [], It).

% object?
%
json_dot_aux(json_object(Fields), [Name | Rest], It) :-
    member((Name, Sub), Fields),
    json_dot_aux(Sub, Rest, It).

% array?
%
json_dot_aux(json_array(Array), [Point | Rest], It) :-
    nth(Point, Array, Sub),
    json_dot(Sub, Rest, It).

% helper, returns nth element of the list
%
nth(Point, Array, It) :-
    length(Before, Point),
    append(Before, [It | _], Array).

