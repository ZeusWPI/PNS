:- module('notifier', [run/1, remote/2, local/2, error/2]).

type(remote, 0).
type(local, 1).
type(error, 2).

run(Queue) :- 
    udp_socket(Client),
    repeat,
        thread_get_message(Queue, notify(Type, ip(N1, N2, N3, N4))),
        type(Type, Byte),
        udp_send(Client, [Byte, N1, N2, N3, N4], ip(127,0,0,1):12512, [as(codes)]),
        ansi_format([bold,fg('#FF7F00')], 'sent~n', []),
        fail.

remote(Queue, Ip) :- thread_send_message(Queue, notify(remote, Ip), [timeout(0.5)]).
local( Queue, Ip) :- thread_send_message(Queue, notify(local , Ip), [timeout(0.5)]).
error( Queue, Ip) :- thread_send_message(Queue, notify(error , Ip), [timeout(0.5)]).