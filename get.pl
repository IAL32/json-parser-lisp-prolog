% json_get/3
json_get(JSON, Field, Result).
json_get(JSON, [First | Tail], Result).

json_get_members([Member | Members], Fields, Index) :-
    json_get_member(Member, Fields, Index),
    json_get_members(Members, Fields, Index).
json_get_members([Member], Fields, Index) :-
    json_get_member(Member, Fields, Index).
