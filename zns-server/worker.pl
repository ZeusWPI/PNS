:- module('worker', []).


:- use_module('dns').
:- use_module('notifier').
:- use_module('log').

run(Socket, JobQueue, NotifyQueue) :-
    udp_socket(Client),
    log_debug('Worker ready to process requests'),
    repeat,
        % Catch any error a worker thread might encounter.
        % Print the error and resume with the next request.
        % TODO: Don't forget the request but proxy to upstream dns_server in hopes it can handle the request.
        catch(
            run_(Socket, JobQueue, NotifyQueue, Client),
            Error,
            (
                log_error('Worker encountered error: ~w', [Error]), 
                fail
            )
        ).


run_(Socket, JobQueue, NotifyQueue, Client) :-
    repeat,
        thread_get_message(JobQueue, packet(From, Data)),
        
        dns:dns(Dns, Data),
        log_debug('--> ~w', [Dns]),
        Dns = dns(_, _, [q(Domain, _, _)], _, _, _),
        log_debug('~w', [Domain]),

        udp_send(Client, Data, ip(1,1,1,1):53, [as(codes)]),
        udp_receive(Client, Response, ip(1,1,1,1):53, [as(codes)]),
        (dns:dns(Dns2, Response) ->
            log_debug('<-- ~w', [Dns2]),
            udp_send(Socket, Response, From, [as(codes)]),
            From = Ip:_,
            remote(NotifyQueue, Ip)
            ;
            write(error), nl, write(Response), nl
        ),
        fail.