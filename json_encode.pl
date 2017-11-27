% json_encode(+JsonObject, -String).
json_encode(jsonobject([]), '{}') :- !.
json_encode(jsonarray([]), '[]') :- !.

json_encode(jsonobject(Members), StringObject) :-
    json_encode_object_members(Members, StringMembers),
    atomic_list_concat(['{', StringMembers , '}'], StringObject), !.

json_encode(jsonarray(Members), StringArray) :-
    json_encode_array_members(Members, StringMembers),
    atomic_list_concat(['[', StringMembers, ']'], StringArray).

% json_encode_array_members(+JsonArrayMembers, -StringMerged).
json_encode_array_members([Member | Members], StringMerged) :-
    json_encode_value(Member, StringValue),
    json_encode_array_members(Members, StringMembers),
    string_concat(StringValue, ',', StringValueComma),
    string_concat(StringValueComma, StringMembers, StringMerged), !.

json_encode_array_members([Member], StringMerged) :-
    json_encode_value(Member, StringMerged), !.

% json_encode_members(+JsonObjectMembers, -StringMerged).
json_encode_object_members([Member | Members], StringMerged) :-
    json_encode_pair(Member, StringPair),
    json_encode_object_members(Members, StringMembers),
    string_concat(StringPair, ',', StringPairComma),
    string_concat(StringPairComma, StringMembers, StringMerged), !.

json_encode_object_members([Member], StringMerged) :-
    json_encode_pair(Member, StringMerged), !.

% json_encode_pair(+JsonPair, -StringPair).
json_encode_pair((Key, Value), StringPair) :-
    atomic_list_concat(['"', Key, '"'], StringKey),
    json_encode_value(Value, StringValue),
    string_concat(StringKey, ':', StringKeyColon),
    string_concat(StringKeyColon, StringValue, StringPair).

% json_encode_key(+JsonPairKey, -StringKey).
json_encode_key(Key, StringKey) :-
    json_encode_string(Key, StringKey).

% json_encode_value(+JsonValue, -StringValue).
json_encode_value(Object, StringValue) :-
    json_encode(Object, StringValue),
    !.
json_encode_value(Number, StringNumber) :-
    number(Number),
    atom_number(AtomNumber, Number),
    atom_chars(AtomNumber, NumberChars),
    string_chars(StringNumber, NumberChars),
    !.
json_encode_value(String, StringValue) :-
    json_encode_string(String, StringValue),
    !.

% json_encode_string(+JsonString, -StringValue).
json_encode_string(String, StringValue) :-
    atomic_list_concat(['"', String, '"'], StringValue).
