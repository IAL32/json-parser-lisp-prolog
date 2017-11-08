%% debug only: sbatti ogni volta scrivere [p]. per ricaricare il file
r :- [p].
%% debug only: sbatti scrivere debug ogni volta per fare il trace
t :- trace.
%% debug only: sbatti scrivere nodebug ogni volta per uscire dal debug(mi esce sia dal trace che dal debug)
nd :- nodebug.
%% debug only: invece di dover usare ` ogni volta
%:- set_prolog_flag(double_quotes, chars).

% json_load/2
json_load(FileName, JSON) :-
    open(FileName, read, BufferIn),
    read_string(BufferIn, _, String),
    atom_chars(String, Chars),
    json_parse(Chars, JSON),
    close(BufferIn).

% json_write/2
% TODO
json_write(JSON, FileName) :-
    json_parse(JSON, String),
    open(FileName, write, BufferOut),
    write(BufferOut, String),
    close(BufferOut).

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
    "{", ws, "}".
json_object(jsonobject(Object)) -->
    "{", ws, json_members(Object), ws, "}", ws.

% json_array_members/3
json_array_members([Member | Members]) -->
    json_value(Member), ws,
    ",", ws,
    json_array_members(Members).
json_array_members([Member]) -->
    json_value(Member), !.

%json_members
json_members([Object]) -->
    json_pair(Object).
json_members([Member | Members]) -->
    json_pair(Member), ws,
    ",", ws,
    json_members(Members).

% json_pair/3
json_pair((Key, Value)) -->
    json_key(Key), ws, ":", ws, json_value(Value).

% json_key/3
json_key(Key) -->
    ("\"", json_key_string(Chars), { atom_chars(Key, Chars) }, "\"")
    |
    ("'", json_key_string(Chars), { atom_chars(Key, Chars) }, "'")
    |
    ("`", json_key_string(Chars), { atom_chars(Key, Chars) }, "`").

% json_values/3
json_value(Value) -->
    (("\"", "\"")
    |
    ("`", "`")),
    { atom_chars(Value, "") }.
json_value(Value) -->
    ("\"", !, json_value_string_q(Codes), { atom_chars(Value, Codes) }, "\"")
    |
    ("`", !, json_value_string_a(Codes), { atom_chars(Value, Codes) }, "`"), !.
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

% restringo solamente ai caratteri alfanumerici
json_key_string([H | T]) --> [H], { char_type(H, alnum) }, json_key_string(T).
json_key_string([]) --> [].

json_value_number([H | T]) --> [H], { char_type(H, digit) }, json_value_number(T).
json_value_number([]) --> [].

% json_value_string_q/2
% una stringa json inclusa tra ", che non contiene "
% TODO: effettivamente far sì che fallisca quando contiene un "
json_value_string_q([H | T]) --> [H], { char_type(H, alnum) }, json_value_string_q(T).
json_value_string_q([]) --> [].

% json_value_string_a/2
% una stringa json inclusa tra `, che non contiene `
% TODO: effettivamente far sì che fallisca quando contiene un `
json_value_string_a([H | T]) --> [H], { char_type(H, alnum) | char_type(H, space) }, json_value_string_a(T).
json_value_string_a([]) --> [].


% ws/2
% consuma gli spazi bianchi
ws --> [W], { char_type(W, space) }, ws, !.
ws --> [].



%% getter e setter
