json_parse(JSON) -->
    json_object(JSON).

json_parse(JSON) -->
    json_array(jarr(JSON)).

json_object([]) -->
    %whitespace,
    "{",
    %whitespace,
    "}".
json_object(JSON) -->
    [JSON],
    %whitespace,
    "{",
    %whitespace,
    json_pair(JSON),
    %whitespace,
    "}"
    %whitespace
    .

json_array(_JSON) -->
    %whitespace,
    "[",
    %whitespace,
    "]".
    %whitespace.
json_array(JSON) -->
    %whitespace,
    "[",
    %whitespace,
    json_members(JSON),
    %json_value(JSON),
    %whitespace,
    "]".
    %whitespace.

json_members(JSON) -->
    json_value(JSON),
    ",",
    json_members(JSON).
json_members(JSON) -->
    json_value(JSON).

json_pair(JSON) -->
    "\"",
    json_key_string, % la chiave può essere composta solamente da [alpha|numeri]
    "\"",
    %whitespace,
    ":",
    %whitespace,
    json_value(JSON).

json_pair --> [].


json_value(JSON) -->
    [n,u,l,l],
    "null".
json_value(JSON) -->
    json_array(JSON).
json_value(JSON) -->
    json_object(JSON).
json_value(JSON) -->
    json_value_number(JSON).

%json_value -->
%    json_key_string.

json_value_number(json_number(JSON)) -->
    [A],
    {
        char_type(A, digit)
    },
    json_value_number_r(B).
json_value_number_r(JSON) -->
    [A],
    {
        char_type(A, digit)
    },
    json_value_number_r(JSON).
json_value_number_r(_JSON) --> [].

json_key_string --> [A], { char_type(A, alpha) }, json_key_string_r(B). % la stringa deve iniziare almeno con una lettera
json_key_string_r([A|As]) --> [A], { char_type(A, alnum) }, json_key_string_r(As). % poi dopo ci possono essere anche dei numeri
json_key_string_r([])     --> []. % la stringa può anche essere vuota

whitespace --> [W], { char_type(W, space) }, whitespace.
whitespace --> [].
