% json_parse/2
%% Con il primo gestisco il caso in cui il primo
%% argomento sia tra apici singoli, eg:
%% json_parse('{"a":"b"}', JSON).
json_parse(JSONAtom, Object) :-
    atomic(JSONAtom),
    atom_codes(JSONAtom, JSONString),
    json_parse(JSONString, Object),
    !.
%% questi altri due gestiscono le liste di codici
%% di caratteri.
json_parse(JSONList, Object) :-
    json_object(Object, JSONList, []),
    !.
json_parse(JSONList, Array) :-
    json_array(Array, JSONList, []).

% json_array/3
json_array(jsonarray([])) --> "[", ws, "]", ws, !.
json_array(jsonarray(Array)) -->
    "[", ws, json_array_members(Array), ws, "]", ws.

% json_object/3
json_object(jsonobject([])) --> "{", ws, "}", ws, !.
json_object(jsonobject(Object)) -->
    "{", ws, json_members(Object), ws, "}", ws.

% json_array_members/3
json_array_members([Member | Members]) -->
    json_value(Member), ws,
    ",", ws, !,
    json_array_members(Members).
json_array_members([Member]) -->
    json_value(Member).

% json_members/3
json_members([Member | Members]) -->
    json_pair(Member), ws,
    ",", ws, !,
    json_members(Members).
json_members([Object]) --> json_pair(Object).

% json_pair/3
% Pair ::= String ':' Value
json_pair((Key, Value)) -->
    json_string(Key), ws,
    ":", ws,
    json_value(Value).

% json_value/3
json_value(Value) --> json_string(Value), !.
json_value(Value) --> json_object(Value), !.
json_value(Value) --> json_array(Value), !.
json_value(Value) -->
    % analizzo la prima parte del numero,
    json_value_number(Number),
    ".", !,
    % analizzo la mantissa
    json_value_number(Mantissa),
    % unisco le due liste e ne faccio una sola senza nested lists
    { flatten([Number, 0'. | Mantissa], Float) },
    % converto la lista prima in stringa, e poi in un atomo prolog
    {
        string_to_list(Value1, Float),
        number_chars(Value, Value1)
    }, !.
json_value(Value) -->
    % un integer
    json_value_number(Codes),
    { length(Codes, L), L > 0, number_chars(Value, Codes) }, !.

% json_string/3
% definizione di una stringa
% String ::= '"' AnyCharSansDQ* '"' | '’' AnyCharSansSQ* '’'
% AnyCharSansDQ ::= <qualunque carattere (ASCII) diverso da '"'>
% AnyCharSansSQ ::= <qualunque carattere (ASCII) diverso da '’'>
json_string(Value) -->
    (
        ("\"", "\"") | ("'", "'")
    ),
    { string_codes(Value, "") }, !.
json_string(Value) -->
    (
        "\"", !,
        json_value_string_dq(Codes),
        { string_codes(Value, Codes) },
        "\""
    ) |
    (
        "'", !,
        json_value_string_sq(Codes),
        { string_codes(Value, Codes) },
        "'"
    ), !.

% json_value_number/3
% una stringa composta solo da interi
json_value_number([H | T]) -->
    [H],
    { char_type(H, digit) },
    json_value_number(T), !.
json_value_number([]) --> [].

% json_value_string_dq/3
% una stringa json inclusa tra ", che non contiene "
json_value_string_dq([H | T]) -->
    [H],
    { H \= 0'" },
    json_value_string_dq(T), !.
json_value_string_dq([]) --> [].

% json_value_string_sq/3
% una stringa json inclusa tra `, che non contiene `
json_value_string_sq([H | T]) -->
    [H],
    { H \= 0'' },
    json_value_string_sq(T), !.
json_value_string_sq([]) --> [].

% ws/2
% consuma gli spazi bianchi
ws --> ws_, ws, ! | ws_, ! | [].
ws_ --> [W], { char_type(W, space) }.
