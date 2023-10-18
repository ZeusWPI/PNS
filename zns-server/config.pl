:- module('config', [read_config/0]).

:- use_module('log').

:- dynamic config/2.

read_config :-
    log_info('Loading config.yml'),
    catch(
        yaml_read('config.yml', DOM),
        Error,
        (term_to_atom(Error, AError), log_error(AError), fail)
    ),
    read_notifier(DOM),
    read_upstream_dns(DOM),
    log_debug('Loaded config!'). 

read_notifier(DOM) :-
    (_{ip: SIp, port: Port} = DOM.notifier ->
        catch(
            (ip_name(Ip, SIp)),
            Error,
            (term_to_atom(Error, AError), log_error('Failed parsing notifier ip: ~s', [AError]), fail)
        ),
        assert(config('notifier', Ip:Port))
        ;
        log_error('notifier config wrong.'), fail
    ).

read_upstream_dns(DOM) :-
    (_{ip: SIp, port: Port} = DOM.server.upstream_dns ->
        catch(
            (ip_name(Ip, SIp)),
            Error,
            (term_to_atom(Error, AError), log_error('Failed parsing upstream_dns ip: ~s', [AError]), fail)
        ),
        assert(config('upstream_dns', Ip:Port))
        ;
        log_error('upstream_dns config wrong.'), fail
    ).

