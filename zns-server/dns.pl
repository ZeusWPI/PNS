:- module('dns', []).

:- use_module(library(record)).

:- record env(data:list(integer), offset:integer=0, domains:dict=_{}).

state(S), [S] --> [S].       % Access the state.
state(S, NS), [NS] --> [S].  % Access the state and modify.

byte(D) -->  % Take one byte from the input
    state(S, NS), 
    {
        env_data(S, [D|Ds]),
        env_offset(S, Offset),
        NOffset is Offset + 1,
        set_data_of_env(Ds, S, S1),
        set_offset_of_env(NOffset, S1, NS)
    }.

dns(Dns, Data) :-
    make_env([data(Data)], Env),
    phrase(dns(Dns), [Env], _).

dns(Dns) --> 
    { var(Dns) }, !,
    uint(2, Identification), 
    uint(2, Header),
    uint(2, NQuestions),
    uint(2, NAnswers),
    uint(2, NAuthority),
    uint(2, NAditional),
    questions(NQuestions, Qs), 
    resource_record(NAnswers, As), 
    resource_record(NAuthority, Auths), 
    resource_record(NAditional, Adds), 
    { Dns = dns(Identification, Header, Qs, As, Auths, Adds) }, !.

dns(Dns) --> 
    { ground(Dns) }, !,
    { Dns = dns(Identification, Header, Qs, As, Auths, Adds) },
    { 
        length(Qs, NQuestions),
        length(As, NAnswers),
        length(Auths, NAuthority),
        length(Adds, NAditional)
    },
    uint(2, Identification), 
    uint(2, Header),
    uint(2, NQuestions),
    uint(2, NAnswers),
    uint(2, NAuthority),
    uint(2, NAditional),
    questions(NQuestions, Qs),
    resource_record(NAnswers, As),
    resource_record(NAuthority, Auths),
    resource_record(NAditional, Adds), !.

%env_consume(Bytes, E, E.put([offset=NO])) :- NO is E.offset + Bytes.

uint(Bytes, Value) --> { var(Value)    }, !, uint_v(Bytes, Value).
uint(Bytes, Value) --> { ground(Value) }, !, uint_g(Bytes, Value).

uint_v(0, 0) --> !.
uint_v(Bytes, Value) --> { NBytes is Bytes - 1 }, byte(I), uint_v(NBytes, CValue), { Value is (I * (2 ^ (NBytes*8))) + CValue }.

uint_g(0, _) --> !.
uint_g(Bytes, Value) --> { NBytes is Bytes - 1 }, { I is (Value >> (NBytes * 8)) /\ 0xff }, byte(I), uint_g(NBytes, Value).

questions(0, []) --> !.
questions(N, [q(D,Type,Class) | Qs]) --> 
    { ground(N), ! },
    domain(D), 
    uint(2, Type), 
    uint(2, Class), 
    { NM is N - 1 }, 
    questions(NM, Qs).
questions(N, Qs) --> { var(N), !, length(Qs, N) }, questions(N, Qs).

resource_record(0, []) --> !.
resource_record(N, [rr(D, Type, Class, TTL, RDLength, RData)|RRs]) --> 
    { ground(N) },
    domain(D),
    uint(2, Type), 
    uint(2, Class),
    uint(4, TTL),
    uint(2, RDLength),
    type(Type, RDLength, RData),
    { NM is N - 1 }, 
    resource_record(NM, RRs).
resource_record(N, RRs) --> { var(N), length(RRs, N) }, resource_record(N, RRs).

% A Record
type(1, _, ip(N1, N2, N3, N4)) --> byte(N1), byte(N2), byte(N3), byte(N4).

% CNAME Record
type(5, _, D) --> domain(D).

% Any Other
type(_, Bytes, Data) --> data(Bytes, Data).

domain(D) --> { var(D)    }, !,  domain_v(D), !.
domain(D) --> { ground(D) }, !,  domain_g(D), !.

domain_v(D) --> 
    uint(2, N),
    state(S),
    {
        Mask = 0xc000,
        Flags is Mask /\ N,
        Flags == Mask,
        Offset is 0x3fff /\ N,
        env_domains(S, Domains),
        get_dict(Offset, Domains, D)
    }, !.

domain_v([]) --> uint(1, 0).
domain_v([SD | Ds]) -->
    state(InitialState),
    data(1, [N]),
    data(N, Data),
    domain_v(Ds),
    state(S, NS),
    { 
        string_codes(SD, Data),
        env_offset(InitialState, Offset),
        env_domains(InitialState, Domains),
        set_domains_of_env(Domains.put([Offset=[SD|Ds]]), S, NS)
    }.

domain_g(D) -->
    state(InitialState),
    {
        env_domains(InitialState, Domains),
        get_dict(Offset, Domains, D),
        N is 0xc000 /\ Offset
    },
    uint(2, N).

domain_g([]) --> uint(1, 0).
domain_g([SD | Ds]) -->
    { string_codes(SD, Codes), length(Codes, N) }, 
    state(InitialState),
    uint(1, N), 
    data(N, Codes), 
    domain_g(Ds),
    state(S, NS),
    {
        env_offset(InitialState, Offset),
        env_domains(InitialState, Domains),
        set_domains_of_env(Domains.put([Offset=[SD|Ds]]), S, NS)
    }.

data(0, []    ) --> !.
data(N, [C|Cs]) --> { ground(N) }, !, byte(C), { NM is N - 1 }, data(NM, Cs).
data(N, Data  ) --> { var(N), length(Data, N) }, !, data(N, Data).
