/* eslint-disable no-console */
import EventEmitter2 from 'eventemitter2'

export default function connectServer (name, code, onMessage=function(){}) {
    const protocol = 'ws';
    const host = window.location.hostname;
    const port = '3001';
    const path = '';
    const url = protocol + '://' + host + ':' + port +'/' + path;
    
    console.log(`Opening connection with ${url}...`);
    const emitter = new EventEmitter2({wildcard: true});
    emitter.on('**', onMessage);
    const socket = new WebSocket(url);

    socket.onmessage = function (messageEvent) {
        console.log(`Received message: `, messageEvent.data);
        const msg = JSON.parse(messageEvent.data);
        console.log(`Emitting '${msg.action || msg.event}'...`);
        emitter.emit(msg.action || msg.event, msg)
    };

    socket.onopen = function () {
        console.log("Connection established.");
        socket.send(JSON.stringify({ type: 'login', name, code }));
    };

    socket.onclose = function () {
        console.log("Connection closed.");
    };

    socket.onerror = function (err) {
        console.log("Connection error: ", err);
    };

    return {
        sendEvent(event, data) { 
            socket.send(JSON.stringify({ type: 'event', event, data, dataType: 'text/turtle' }));
        },
        sendAction(action, data) { 
            socket.send(JSON.stringify({ type: 'action', action, data, dataType: 'text/turtle' }));
        },
        on(event, cb) { emitter.on(event, cb); }
    };
}
