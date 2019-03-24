:- module(connection, []).

:- use_module(library(http/websocket)).
:- use_module(library(debug)).

:- use_module(interpreter, []).


open_connection(IpAddress, Port) :-
    debug(info/connection, '[CONNECTION] Opening "ws://~w:~w/"...', [IpAddress, Port]),
    format(atom(URL), 'ws://~w:~w/', [IpAddress, Port]),
    http_open_websocket(URL, WS, []),
    debug(info/connection, '[CONNECTION] Entering message loop...', []),
    send_login(WS),
    loop(WS, waiting, state{ status:waiting }),
    debug(info/connection, '[CONNECTION] Message loop closed.', []).

%%
%% Send login information
%%
send_login(WS) :-
    % TODO: read login from commandline or settinsg file.
    ws_send(WS, json(login{ type:login, name:'janzi', code:'lRkCA2Gaq83mlGxgkpgy' })).


loop(_WS, closing, _State) :-
    debug(warn/connection, '[CONNECTION] connection closed by server!', []).
loop(WS, exit, _State) :-
    debug(warn/connection, '[CONNECTION] closing connection...', []),
    ws_close(WS),
    debug(warn/connection, '[CONNECTION] connection closed by client!', []).
loop(WS, _Status, State) :-
    catch(receive(WS, State, NewState), Error, loop_error(Error, State, NewState)),
    loop(WS, NewState.status, NewState).
loop(WS, Status, State) :-
    debug(warn/connection, '[CONNECTION] "receive" failed!', []),
    loop(WS, Status, State).

loop_error(Error, State, State) :- 
    debug(warn/connection, '[CONNECTION] Error occured evaluating "receive". ~w', [Error]).


receive(WS, State, NewState) :-
    debug(info/connection, '[CONNECTION] waiting for message...', []),
    ws_receive(WS, Message, [ format(json) ]),
    websocket{ opcode:OpCode, data: Payload } :< Message,
    receive_opcode(OpCode, in(Payload, State), out(Resp, NewState)),
    send_response(WS, Resp).
receive(_WS, State, State) :-
    debug(error/connection, '[CONNECTION] general catch-all activated, "receive" failed!', []).

receive_opcode(text, Message, Result) :- 
    debug(debug/connection, '[CONNECTION] received message with opcode "text"...', []),
    receive_text(Message, Result).
receive_opcode(close, in(_Payload, State), out(none, NewState)) :-
    debug(info/connection, '[CONNECTION] connection closed by server!', []),
    NewState = State.put([status=closed]).
receive_opcode(binary, in(_Payload, S), out(none, S)) :-
    debug(warn/connection, '[CONNECTION] received binary message, but no handlers defined for binary messages!', []).
receive_opcode(Other, in(_Payload, S), out(none, S)) :-
    debug(warn/connection, '[CONNECTION] received unknown opcode "~w", ignoring...', [Other]).

receive_text(in(Payload, State), Result) :- 
    _Key{ event: Event, parts: Parts, dataType: DataType, data: Data } :< Payload,
    debug(info/connection, '[CONNECTION] handling message "~w"...', [Event]),
    debug(debug/connection, '[CONNECTION] message contained event "~w"...', [Event]),
    debug(debug/connection, '[CONNECTION] receive_text parts was "~w".', [Parts]),
    debug(debug/connection, '[CONNECTION] receive_text data was "~w".', [Data]),
    debug(debug/connection, '[CONNECTION] receive_text datatype was "~w".', [DataType]),
    debug(debug/connection, '[CONNECTION] receive_text state was "~w".', [State]),
    debug(debug/connection, '[CONNECTION] receive_text result was "~w".', [Result]),
    receive_event(Parts, event(Event, Parts, Data, DataType, State), Result).
receive_text(in(Payload, S), out(none, S)) :- 
    debug(warn/connection, '[CONNECTION] Received message of unknown format "~w"!', [Payload]).


receive_event(["server", "requests", "login"], Event, Result) :-
    debug(debug/connection, '[CONNECTION] message contained login request...', []),
    debug(debug/connection, '[CONNECTION] receive_event event was "~w".', [Event]),
    debug(debug/connection, '[CONNECTION] receive_event result "~w".', [Result]),
    handle_login(Event, Result).
receive_event(["server", "request", "login"], Event, Result) :-
    debug(debug/connection, '[CONNECTION] message contained login request...', []),
    debug(debug/connection, '[CONNECTION] receive_event event was "~w".', [Event]),
    debug(debug/connection, '[CONNECTION] receive_event result "~w".', [Result]),
    handle_login(Event, Result).
receive_event(["server", "accepted", "client"], Event, Result) :-
    handle_accepted(Event, Result).
receive_event(["server", "rejected", "client"], Event, Result) :-
    handle_rejected(Event, Result).
receive_event([Source, "updated", "state"], Event, Result) :-
    handle_update(Event, Result).
receive_event([_Subject, _Predicate, _Object], Event, Result) :-
    handle_other(Event, Result).

send_response(_WS, none).
send_response(WS, login(Name, Code)) :- 
    debug(info/connection, '[CONNECTION] sending response login( ~w, ********)...', [Name]),
    ws_send(WS, json(login{ type:login, name:Name, code:Code })).
send_response(WS, publish(Event, Data, DataType)) :- 
    debug(info/connection, '[CONNECTION] sending response publish( ~w )...', [Event]),
    ws_send(WS, json(publish{ type:publish, event:Event, data:Data, dataType:DataType })).
send_response(WS, subscribe(Event)) :- 
    debug(info/connection, '[CONNECTION] sending response subscribe( ~w )...', [Event]),
    ws_send(WS, json(subscribe{ type:subscribe, event:Event })).
send_response(_WS, Resp) :- 
    debug(warn/connection, '[CONNECTION] send_response: Unknown response format "~w"!', [Resp]).


handle_login(event(_Event, _P, _D, _DT, State), Result) :-
    debug(info/connection, '[CONNECTION] handling login request...', []),
    debug(debug/connection, '[CONNECTION] state is "~w"', [State]),
    Result = out(login("janzi", "lRkCA2Gaq83mlGxgkpgy"), State).

handle_accepted(event(_Event, _P, Data, _DT, State), out(none, NewState)) :- 
    debug(info/connection, '[CONNECTION] login successful...', []),
    _{ identifier: Id } :< Data,
    NewState = State.put([identifier=Id, status=accepted]).

handle_rejected(event(_Event, _P, _D, _DT, State), out(none, NewState)) :- 
    debug(info/connection, '[CONNECTION] login failed, exiting...', []),
    NewState = State.put([status=exit]).

handle_update(event(_Event, [Source, "updated", "state"], Data, DateType, State), Result) :-
    debug(info/connection, '[CONNECTION] handling the updated state of ~w...', []),
    atom_string(Key, Source)
    NewState = State.put([Key=Data]),
    interpreter:solve_state(NewState, NewNewState),
    Result = out(publish("rule_engine.ran.rules", NewNewState.results, n_quads), NewNewState).

handle_other(_Event, _Result).

test_type(Term) :-
    atom(Term),
    debug(info/connection, '[CONNECTION] Term ~w is atom', [Term]).  
test_type(Term) :-
    string(Term),
    debug(info/connection, '[CONNECTION] Term ~w is string', [Term]). 
test_type(Term) :-
    compound(Term),
    debug(info/connection, '[CONNECTION] Term ~w is compound', [Term]). 
test_type(Term) :-
    debug(info/connection, '[CONNECTION] Term ~w is unknown', [Term]). 