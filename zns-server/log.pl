:- module('log', [
    log_error/1, log_warn/1, log_success/1, log_info/1, log_debug/1,
    log_error/2, log_warn/2, log_success/2, log_info/2, log_debug/2
]).

log_error(Atom) :- log_error(Atom, []).
log_error(Atom, Format) :- log_(Atom, Format, '#F00', '#FFF').

log_warn(Atom) :- log_warn(Atom, []).
log_warn(Atom, Format) :- log_(Atom, Format, '#FF7F00', '#FFF').

log_success(Atom) :- log_success(Atom, []).
log_success(Atom, Format) :- log_(Atom, Format, '#000', '#0F0').

log_info(Atom) :- log_info(Atom, []).
log_info(Atom, Format) :- log_(Atom, Format, '#000', '#00F').

log_debug(Atom) :- log_debug(Atom, []).
log_debug(Atom, Format) :- log_(Atom, Format, '#000', '#444').
    
log_(Atom, Format, Bg, Fg) :-
    get_time(Time),
    format_time(atom(TimeA), '[%d/%m/%y %T] ', Time),
    atom_concat(TimeA, Atom, A1),
    atom_concat(A1, '~n', A2),
    ansi_format([bold, bg(Bg), fg(Fg)], A2, Format).