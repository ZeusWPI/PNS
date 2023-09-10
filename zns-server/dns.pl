:- module('dns', []).

dns(Dns) --> 
    { var(Dns) }, !,
    { E1 = env{offset: 0, domains: _{}} },
    uint(2, Identification, E1, E2), 
    uint(2, Header, E2, E3),
    uint(2, NQuestions, E3, E4),
    uint(2, NAnswers, E4, E5),
    uint(2, NAuthority, E5, E6),
    uint(2, NAditional, E6, E7),
    questions(NQuestions, Qs, E7, E8),
    resource_record(NAnswers, As, E8, E9),
    resource_record(NAuthority, Auths, E9, E10),
    resource_record(NAditional, Adds, E10, _),
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
    { E1 = env{offset: 0, domains: _{}} },
    uint(2, Identification, E1, E2), 
    uint(2, Header, E2, E3),
    uint(2, NQuestions, E3, E4),
    uint(2, NAnswers, E4, E5),
    uint(2, NAuthority, E5, E6),
    uint(2, NAditional, E6, E7),
    questions(NQuestions, Qs, E7, E8),
    resource_record(NAnswers, As, E8, E9),
    resource_record(NAuthority, Auths, E9, E10),
    resource_record(NAditional, Adds, E10, _), !.

env_consume(Bytes, E, E.put([offset=NO])) :- NO is E.offset + Bytes.

uint(Bytes, Value, E, NE) --> { var(Value)   , !, env_consume(Bytes, E, NE) }, uint_v(Bytes, Value).
uint(Bytes, Value, E, NE) --> { ground(Value), !, env_consume(Bytes, E, NE) }, uint_g(Bytes, Value).

uint_v(0, 0) --> !.
uint_v(Bytes, Value) --> { NBytes is Bytes - 1 }, [I], uint_v(NBytes, CValue), { Value is (I * (2 ^ (NBytes*8))) + CValue }.

uint_g(0, _) --> !.
uint_g(Bytes, Value) --> { NBytes is Bytes - 1 }, { I is (Value >> (NBytes * 8)) /\ 0xff }, [I], uint_g(NBytes, Value).

questions(0, [], E, E) --> !.
questions(N, [q(D,Type,Class) | Qs], E, NE) --> 
    { ground(N), ! },
    domain(D, E, E1), 
    uint(2, Type, E1, E2), 
    uint(2, Class, E2, E3), 
    { NM is N - 1 }, questions(NM, Qs, E3, NE).
questions(N, Qs, E, NE) --> { var(N), !, length(Qs, N) }, questions(N, Qs, E, NE).

resource_record(0, [], E, E) --> !.
resource_record(N, [rr(D, Type, Class, TTL, RDLength, RData)|RRs], E, NE) --> 
    { ground(N) },
    domain(D, E, E2),
    uint(2, Type, E2, E3), 
    uint(2, Class, E3, E4),
    uint(4, TTL, E4, E5),
    uint(2, RDLength, E5, E6),
    data(RDLength, RData, E6, E7),
    { NM is N - 1 }, resource_record(NM, RRs, E7, NE).
resource_record(N, RRs, E, NE) --> { var(N), length(RRs, N) }, resource_record(N, RRs, E, NE).

domain(D, E, NE) --> { var(D)   , ! }, domain_v(D, E, NE), !.
domain(D, E, NE) --> { ground(D), ! }, domain_g(D, E, NE), !.

domain_v(D, E, NE) --> domain_v(D, init, E, NE).

domain_v(D, init, E, NE) --> 
    uint(2, N, E, NE),
    {
        Mask = 0xc000,
        Flags is Mask /\ N,
        Flags == Mask,
        Offset is 0x3fff /\ N,
        get_dict(Offset, E.domains, D)
    }, !.


domain_v([], init, E, NE) --> uint(1, 0, E, NE).
domain_v([SD | Ds], init, E, NE) -->
    data(1, [N] , E , E2),
    {
        Mask = 0b11000000,
        Flags is Mask /\ N,
        Flags == 0b00000000
    },
    data(N, Data, E2, E3),
    domain_v(Ds, noinit, E3, E4),
    { 
        string_codes(SD, Data),
        E = Env{offset: Offset, domains: Domains},
        NDomains = Domains.put([Offset=[SD|Ds]]),
        NOffset = E4.offset,
        NE = Env{offset: NOffset, domains: NDomains}
    }.

domain_v([], noinit, E, NE) --> uint(1, 0, E, NE).
domain_v([SD | Ds], noinit, E, NE) -->
    data(1, [N] , E , E2),
    data(N, Data, E2, E3),
    domain_v(Ds, noinit, E3, NE),
    { string_codes(SD, Data) }.

domain_g(D, E, NE) --> domain_g(D, init, E, NE).

domain_g([], init, E, NE) --> uint(1, 0, E, NE).
domain_g(D, init, E, NE) -->
    {
        get_dict(Offset, E.domains, D),
        N is 0xc000 + Offset
    },
    uint(2, N, E, NE).

domain_g(D, init, E, NE) -->
    domain_g(D, noinit, E, E2),
    {
        E = Env{offset: Offset, domains: Domains},
        NDomains = Domains.put([Offset=D]),
        NOffset = E2.offset,
        NE = Env{offset: NOffset, domains: NDomains}
    }.

domain_g([], noinit, E, NE) --> uint(1, 0, E, NE).
domain_g([SD|Ds], noinit, E, NE) -->
    { string_codes(SD, Codes), length(Codes, N) }, 
    uint(1, N, E, E2), 
    data(N, Codes, E2, E3), 
    domain_g(Ds, init, E3, NE).

data(0, []    , E, E ) --> !.
data(N, [C|Cs], E, NE) --> { ground(N), env_consume(1, E, E2) }, !, [C], { NM is N - 1 }, data(NM, Cs, E2, NE).
data(N, Data  , E, NE) --> { var(N), length(Data, N) }, !, data(N, Data, E, NE).