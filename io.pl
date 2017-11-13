% json_load/2
json_load(FileName, JSON) :-
    open(FileName, read, BufferIn),
    read_string(BufferIn, _, String),
    normalize_space(atom(Normalized), String),
    atom_chars(Normalized, StringList),
    json_parse(StringList, JSON),
    close(BufferIn).

% json_write/2
% TODO
json_write(JSON, FileName) :-
    json_encode(JSON, String),
    open(FileName, write, BufferOut),
    write(BufferOut, String),
    close(BufferOut).
