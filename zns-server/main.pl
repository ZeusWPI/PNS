:- use_module('dns').

:- initialization(main).

main() :-
    current_prolog_flag(argv, [start]),

    repeat,
        udp_socket(Socket),
        tcp_bind(Socket, localhost:2000),
        message_queue_create(Queue, [max_size(1024)]),

        thread_create(server(Socket, Queue), ServerId, []),
        spawn_workers(1, Socket, Queue, WorkerIds),

        thread_join(ServerId, _),
        foreach(member(WorkerId, WorkerIds), thread_join(WorkerId, _)),
        fail.

spawn_workers(0, _     , _    , []).
spawn_workers(N, Socket, Queue, [WorkerId | WorkerIds]) :-
    NM is N - 1,
    thread_create(worker(Socket, Queue), WorkerId, []),
    spawn_workers(NM, Socket, Queue, WorkerIds).


server(Socket, Queue) :-
    repeat,
        udp_receive(Socket, Data, From, [as(codes)]),
        thread_send_message(Queue, packet(From, Data)),
        fail.

worker(Socket, Queue) :-
    udp_socket(Client),
    repeat,
        thread_get_message(Queue, packet(From, Data)),
        dns:dns(Dns, Data, []),
        write(Dns), nl,
        udp_send(Client, Data, ip(1,1,1,1):53, [as(codes)]),
        udp_receive(Client, Response, ip(1,1,1,1):53, [as(codes)]),
        dns:dns(Dns2, Response, []),
        write(Dns2), nl,
        udp_send(Socket, Response, From, [as(codes)]),
        fail.