%% debug only: sbatti ogni volta scrivere [p]. per ricaricare il file
r :- make.
%% debug only: sbatti scrivere debug ogni volta per fare il trace
t :- trace.
%% debug only: sbatti scrivere nodebug ogni volta per uscire dal debug(mi esce sia dal trace che dal debug)
nd :- nodebug.
%% debug only: invece di dover usare ` ogni volta
%% %% Disabilitato! Non piu' necessario
:- set_prolog_flag(double_quotes, chars).

%% necessario per evitare che prolog mi metta i puntini
%% su cose troppo lunghe
:- set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).
