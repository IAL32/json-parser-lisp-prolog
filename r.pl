json_parse(Json, Object) :-
    json_object(Object, Json, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% json_object/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
json_object(json_object(Object), ["{" | Tail], After) :-
    json_object_members(Object, Tail, Final),
    Final = ["}" | After].
% json_object = {}
json_object(json_object(Object), ["{", "}" | _Tail], _After).

json_object_fields(Object, String, Rest) :-
    json_object_members(Object, String, Rest).
json_object_fields([], String, Rest).
% json_object_members/3
% viene passato come primo argomento la lista degli elementi appartenenti alla lista
json_object_members([Member | Members], String, Final) :-
    json_pair(Member, String, [Last | Tail]),
    Last = ',',
    json_object_members(Member, Tail, Final).
% caso base, viene individuato l'ultimo(o unico)pair dell'oggetto
json_object_members([Member], String, Final) :-
    json_pair(Member, String, Final).

% json_pair/2
% individuo l'apertura e chiusura delle virgolette dei pairs
json_pair((Key, Value), String, Final) :-
    json_quote([Quote | Final1]),
    json_value_string(Key, Final1, Final2),
    json_quote([Quote, Final2]).


% json_quote/1
json_quote(["\"" | _String]).
json_quote(["'" | _String]).
json_quote(["`" | _String]).

% json_value_string/3
json_value_string(Value, [First | Tail], Final) :-
    atom_chars(Value, Chars).
