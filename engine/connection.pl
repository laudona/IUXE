:- module(connection, []).

:- use_module(library(http/websocket)).
:- use_module(library(debug)).

:- use_module(solver, []).


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


%%
%% Enter message loop: wait for message, send response, wait for message again.
%%
loop(_WS, closing, _State) :-
    !, debug(warn/connection, '[CONNECTION] connection closed by server!', []).
loop(WS, exit, _State) :-
    !, debug(warn/connection, '[CONNECTION] closing connection...', []),
    ws_close(WS),
    debug(warn/connection, '[CONNECTION] connection closed by client!', []).
loop(WS, _Status, State) :-
    catch(receive(WS, State, NewState), Error, loop_error(Error, State, NewState)),
    !, loop(WS, NewState.status, NewState).
loop(WS, Status, State) :-
    debug(warn/connection, '[CONNECTION] "receive" failed!', []),
    !, loop(WS, Status, State).

%%
%% Error trap from the loop. Ensures the loop still active even if a message handling
%% predicate throws an execption.
%%
loop_error(Error, State, State) :- 
    debug(warn/connection, '[ERROR][CONNECTION] Error occured evaluating "receive". ~w', [Error]).


%%
%% Receive a message from the server.
%%
receive(WS, State, NewState) :-
    debug(debug/connection, '[CONNECTION] waiting for message...', []),
    ws_receive(WS, Message, [ format(json) ]),
    websocket{ opcode:OpCode, data: Payload } :< Message,
    receive_opcode(OpCode, in(Payload, State), out(Resp, NewState)),
    send_response(WS, Resp).
receive(_WS, State, State) :-
    debug(error/connection, '[CONNECTION] general catch-all activated, "receive" failed!', []).

%%
%% Handle message based on the opcode.
%%
receive_opcode(text, Message, Result) :- 
    debug(debug/connection, '[CONNECTION] received message with opcode "text"...', []),
    !, receive_text(Message, Result).
receive_opcode(close, in(_Payload, State), out(none, NewState)) :-
    debug(debug/connection, '[CONNECTION] connection closed by server!', []),
    !, NewState = State.put([status=closed]).
receive_opcode(binary, in(_Payload, S), out(none, S)) :-
    !, debug(warn/connection, '[CONNECTION] received binary message, but no handlers defined for binary messages!', []).
receive_opcode(Other, in(_Payload, S), out(none, S)) :-
    !, debug(warn/connection, '[CONNECTION] received unknown opcode "~w", ignoring...', [Other]).


%%
%% Handle a message with the text opcode. 
%%
receive_text(in(Payload, State), out(none, State)) :- 
    string(Payload),
    !, debug(warn/connection, '[CONNECTION][WARN] message received is string instead of dict "~w" ...', [Payload]).
receive_text(in(Payload, State), out(Response, State)) :- 
    is_dict(Payload),
    _Key{ event: Event, dataType: DataType, data: Data } :< Payload,
    debug(debug/connection, '[CONNECTION] handling event message "~w"...', [Event]),
    !, receive_event(Event, Data, DataType, Response).
receive_text(in(Payload, State), out(Response, State)) :- 
    _Key{ action: Action, dataType: DataType, data: Data } :< Payload,
    debug(debug/connection, '[CONNECTION] handling action message "~w"...', [Action]),
    !, receive_action(Action, Data, DataType, Response).
receive_text(in(Payload, S), out(none, S)) :- 
    !, debug(warn/connection, '[CONNECTION] Received message of unknown format "~w"!', [Payload]).


%%
%% Send a response back to the server.
%%
send_response(_WS, actions( [] )).
send_response(WS, actions( [ Action | Actions ] )) :- 
    debug(info/connection/send_response, '[CONNECTION] send_response: list of actions, now "~w"!', [Action]),
    send_response(WS, Action),
    send_response(WS, actions( Actions )).
send_response(_WS, none).
send_response(WS, event(Event, Data, DataType)) :- 
    debug(info/connection/send_response, '[CONNECTION] sending event ( ~w )...', [Event]),
    ws_send(WS, json(event{ type:event, event:Event, data:Data, dataType:DataType })).
send_response(WS, action(Action, Data, DataType)) :- 
    debug(info/connection/send_response, '[CONNECTION] sending action ( ~w )...', [Action]),
    ActionDict = action{ type:action, action:Action, data:Data, dataType:DataType },
    debug(info/connection/send_response, '[CONNECTION] sending action ( ~w )...', [ActionDict]),
    ws_send(WS, json(ActionDict)).
send_response(_WS, Resp) :- 
    debug(warn/connection/send_response, '[CONNECTION] send_response: Unknown response format "~w"!', [Resp]).


%%
%% Handle an event. 
%%
receive_event(Event, EventData, EventDataType, Response) :-
    debug(info/connection/receive_event, '[CONNECTION] Calling solver for event "~w" .', [Event]),
    solver:solve_event(Event, EventData, EventDataType, Actions),
    %% Actions >> [ action(Action, ActionData, ActionDataType) ]...
    debug(info/connection/receive_event, '[CONNECTION] Event "~w" resulted in following actions: ~w.', [Event, Actions]),
    Response = actions(Actions), !.
receive_event(_Event, _EventData, _EventDataType, nothing).


%%
%% Handle an event. 
%%
receive_action(Action, Data, DataType, nothing) :-
    debug(info/connection, '[CONNECTION] Action "~w"!', [Action]),
    debug(info/connection, '[CONNECTION] DataType "~w".', [DataType]),
    debug(info/connection, '[CONNECTION] Data "~w".', [Data]).


%%
%% Util predicate to test the type of a variable.
%%
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