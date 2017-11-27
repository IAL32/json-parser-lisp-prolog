:- begin_tests(io).

test(json_load) :-
    json_load("ex_arthurdent.json", jsonobject([("nome", "Arthur"), ("cognome", "Dent")])).

test(json_load) :-
    json_load("ex_emptyobject.json", jsonobject([])).

test(json_load) :-
    json_load("ex_emptyarray.json", jsonarray([])).

test(json_load) :-
    json_load("ex_superbook.json", jsonobject([("modello", "SuperBook 1234"),  ("anno di produzione", 2014),  ("processore", jsonobject([("produttore", "EsseTi"),  ("velocità di funzionamento (GHz)", jsonarray([1, 2, 4, 8]))]))])).

test(json_load) :-
    json_load("ex_wikipedia.json", jsonobject([("type", "menu"),  ("value", "File"),  ("items", jsonarray([jsonobject([("value", "New"),  ("action", "CreateNewDoc")]), jsonobject([("value", "Open"),  ("action", "OpenDoc")]), jsonobject([("value", "Close"),  ("action", "CloseDoc")])]))])).

% write
test(json_write, [
    cleanup(delete_file("test1.json"))
]) :-
    json_write(jsonobject([]), "test1.json").
test(json_write, [
    cleanup(delete_file("test2.json"))
]) :-
    json_write(jsonarray([]), "test2.json").
test(json_write, [
    cleanup(delete_file("test3.json"))
]) :-
    Object = jsonobject([("modello", "SuperBook 1234"),  ("anno di produzione", 2014),  ("processore", jsonobject([("produttore", "EsseTi"),  ("velocità di funzionamento (GHz)", jsonarray([1, 2, 4, 8]))]))]),
    json_write(Object, "test3.json"),
    json_load("test3.json", Object).


:- end_tests(io).

:- begin_tests(jsonparse).

test(json_parse) :-
    json_parse("{}", jsonobject([])).
test(json_parse) :-
    json_parse('[]', jsonarray([])).
% da email di Lorenzo Aldeghi, sabato 25-11-2017, 13:20
test(json_parse) :-
    json_parse('{"st{rin}ga" : "va[lo]re"}', jsonobject([("st{rin}ga", "va[lo]re")])).
% ^
test(json_parse):-
    json_parse('{\n  "confondendo" : "il, riconoscimento, di una coppia, stringa-valore",\n   "da:::lle" : "al,,,,,,,,tre"}', jsonobject([("confondendo", "il, riconoscimento, di una coppia, stringa-valore"),("da:::lle", "al,,,,,,,,tre")])).
% ^
test(json_parse) :-
    json_parse('["1", "[2]", "{3{{}}}"]', jsonarray(["1", "[2]", "{3{{}}}"])).

% email Prof. Antoniotti sabato 25-11-2017, 14:31
test(json_parse) :-
    json_parse('{"fo,32": "43"}', jsonobject([("fo,32", "43")])).
test(json_parse) :-
    json_parse('{"fo{32}bar": "43"}', jsonobject([("fo{32}bar", "43")])).

:- end_tests(jsonparse).
