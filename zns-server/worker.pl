:- module('worker', []).


:- use_module('dns').
:- use_module('notifier').

run(Socket, JobQueue, NotifyQueue) :-
    udp_socket(Client),
    repeat,
        thread_get_message(JobQueue, packet(From, Data)),
        dns:dns(Dns, Data, []),
        write(Dns), nl,
        udp_send(Client, Data, ip(1,1,1,1):53, [as(codes)]),
        udp_receive(Client, Response, ip(1,1,1,1):53, [as(codes)]),
        (dns:dns(Dns2, Response, []) ->
            write(Dns2), nl,
            udp_send(Socket, Response, From, [as(codes)]),
            From = Ip:_,
            remote(NotifyQueue, Ip),
            ansi_format([bold,fg('#00FF00')], 'ok~n', [])
            ;
            write(error), nl, write(Response), nl
        ),
        fail.