json_parse(JSONString, Object) :-
    json_object(Object, JSONString, []), !.
json_parse(JSONString, Array) :-
    json_array(Array, JSONString, []), !.

% json_array/3
json_array(json_array([])) -->
    "[", ws, "]", ws.
json_array(jsonarray(Array)) -->
    "[", ws, json_array_members(Array), ws, "]", ws.

% json_object/3
json_object(jsonobject([])) -->
    "{", ws, "}", ws.
json_object(jsonobject(Object)) -->
    "{", ws, json_members(Object), ws, "}", ws.

% json_array_members/3
json_array_members([Member | Members]) -->
    json_value(Member), ws,
    ",", ws,
    json_array_members(Members).
json_array_members([Member]) -->
    json_value(Member).

% json_members/3
json_members([Member | Members]) -->
    json_pair(Member), ws,
    ",", ws, !,
    json_members(Members).
json_members([Object]) -->
    json_pair(Object).

% json_pair/3
% Pair ::= String ':' Value
json_pair((Key, Value)) -->
    json_string(Key), ws, ":", ws, json_value(Value).

json_value(Value) -->
    json_string(Value), !.
json_value(Value) -->
    json_object(Value), !.
json_value(Value) -->
    json_array(Value), !.
json_value(Value) -->
    json_value_number(Number), % analizzo la prima parte del numero,
    ".",
    json_value_number(Mantissa), % analizzo la mantissa
    { flatten([Number, "." | Mantissa], Float) }, % unisco le due liste e ne faccio una sola senza nested lists
    % converto la lista prima in stringa, e poi in un atomo prolog
    { string_to_list(Value1, Float), number_chars(Value, Value1) }, !.
json_value(Value) -->
    % un integer
    json_value_number(Codes),
    { number_chars(Value, Codes) }, !.

% json_string/3
% definizione di una stringa
% String ::= '"' AnyCharSansDQ* '"' | '’' AnyCharSansSQ* '’'
% AnyCharSansDQ ::= <qualunque carattere (ASCII) diverso da '"'>
% AnyCharSansSQ ::= <qualunque carattere (ASCII) diverso da '’'>

json_string(Value) -->
    (("\"", "\"")
    |
    ("`", "`")),
    { atom_chars(Value, "") }.
json_string(Value) -->
    ("\"", !, json_value_string_q(Codes), { atom_chars(Value, Codes) }, "\"")
    |
    ("`", !, json_value_string_a(Codes), { atom_chars(Value, Codes) }, "`"), !.

% json_value_number/3
json_value_number([H | T]) --> [H], { char_type(H, digit) }, json_value_number(T).
json_value_number([]) --> [].

% json_value_string_q/3
% una stringa json inclusa tra ", che non contiene "
json_value_string_q([H | T]) --> [H], { H \= '"', !, json_valid_char(H) }, json_value_string_q(T).
json_value_string_q([]) --> [].

% json_value_string_a/3
% una stringa json inclusa tra `, che non contiene `
json_value_string_a([H | T]) --> [H], { H \= '`', !, json_valid_char(H) }, json_value_string_a(T).
json_value_string_a([]) --> [].

% json_valid_char/1
% Controlla che il carattere sia valido per una json_string
json_valid_char(H) :-
    char_type(H, alnum), !.
json_valid_char(H) :-
    char_type(H, space), !.
json_valid_char(H) :-
    char_type(H, punct), !.

% ws/2
% consuma gli spazi bianchi
ws --> ws_, ws, ! | ws_, ! | [].
ws_ --> [W], { char_type(W, space) }.


