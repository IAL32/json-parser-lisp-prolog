% Qui andremo a definire le procedure di input-output

% json_load/2
json_load(FileName, JSON) :-
    open(FileName, read, BufferIn),
    read_string(BufferIn, _, String),
    json_parse(String, JSON),
    close(BufferIn).

% json_write/2
json_write(JSON, FileName) :-
    json_parse(JSON, String),
    open(FileName, write, BufferOut),
    write(BufferOut, String),
    close(BufferOut).
