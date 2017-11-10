% json_get/3
json_get(jsonobject(Members), Key, Value) :-
    %atom_chars(KeyAtom, Key),
    member((Key, Value), (Members)), !.

json_get(Object, [Key], Value) :-
    json_get(Object, Key, Value), !.

json_get(Object, [Key, Nth], Value) :-
    json_get(Object, Key, jsonarray(Array)),
    nth0(Nth, Array, Value), !.
%json_get_member([(Key, Value) | _Members], Key, Value).

