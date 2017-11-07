%% debug only: sbatti ogni volta scrivere [p]. per ricaricare il file
r :- [p].
%% debug only: sbatti scrivere debug ogni volta per fare il trace
t :- trace.
%% debug only: sbatti scrivere nodebug ogni volta per uscire dal debug(mi esce sia dal trace che dal debug)
nd :- nodebug.
%% invece di dover usare ` ogni volta
:- set_prolog_flag(double_quotes, chars).

json_parse(JSONString, Object) :-
    json_object(Object, JSONString, []).
json_parse(JSONString, Array) :-
    json_array(Array, JSONString, []).

% json_array/3
json_array(json_array([])) -->
    "[", "]".
json_array(jsonarray(Array)) -->
    "[", json_array_members(Array), "]".

% json_object/3
json_object(jsonobject([])) -->
    "{", "}".
json_object(jsonobject(Object)) -->
    "{", json_members(Object), "}".

% json_array_members/3
json_array_members([Member | Members]) -->
    json_value(Member),
    ",",
    json_array_members(Members).
json_array_members([Member]) -->
    json_value(Member).

%json_members
json_members([Member | Members]) -->
    json_pair(Member),
    ",", !,
    json_members(Members).
json_members([Object]) -->
    json_pair(Object).

% json_pair/3
json_pair((Key, Value)) -->
    json_key(Key), ":", json_value(Value).

% json_key/3
json_key(Key) -->
    "\"", json_key_string(Chars), { atom_chars(Key, Chars) }, "\"".

% json_value/3
json_value(Value) -->
    json_values(Value).

% json_values/3
json_values(Value) -->
    "\"",
    json_value_string(Codes),
    { atom_chars(Value, Codes) },
    "\"".
json_values(Value) -->
    json_object(Value), !.
json_values(Value) -->
    json_array(Value), !.
json_values(Value) -->
    json_value_double(Codes),
    { number_chars(Value, Codes) }, !.
json_values(Value) -->
    json_value_integer(Codes),
    { number_chars(Value, Codes) }, !.

% restringo solamente ai caratteri alfanumerici
json_key_string([]) --> [].
json_key_string([H | T]) --> [H], { char_type(H, alnum) }, json_key_string(T).

json_value_double(_Double) --> [].
json_value_double(Double) --> [D], { char_type(D, digit) }, json_value_double(Double).
json_value_integer(_Integer) --> [].
json_value_integer(Integer) --> [I], { char_type(I, digit) }, json_value_integer(Integer).
json_value_string([]) --> [].
json_value_string([H | T]) --> [H], json_value_string(T).

% ws/2
% consuma gli spazi bianchi
ws --> [W], { char_type(W, space) }, ws.
ws --> [].
