json_encode(jsonobject([]), "{}").
json_encode(jsonarray([]), "[]").

json_encode(jsonobject(Members), [StringMembers | String]) :-
    json_encode_members(Members, StringMembers).

json_encode_members([Member | Members], StringMerged) :-
    json_encode_pair(Member, StringPair),
    json_encode_members(Members, StringMembers),
    string_concat(StringPair, ",", StringPairComma),
    string_concat(StringPairComma, StringMembers, StringMerged).

json_encode_pair((Key, Value), StringPair) :-
    atomic_list_concat(["\"", Key, "\""], StringKey),
    json_encode_value(Value, StringValue)
    string_concat(StringKey, ":", String).
