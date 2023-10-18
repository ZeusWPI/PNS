:- module('cache', [lookup/2, update/2]).

:- use_module('log').

:- dynamic cached/2.

lookup(Domain, Answer) :-
    cached(Domain, Answer).

update(Domain, Answer) :-
    CacheLine = cached(Domain, Answer),
    retract(CacheLine),
    assert(CacheLine).
