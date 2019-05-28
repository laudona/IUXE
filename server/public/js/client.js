var Spotless = Spotless || {};

Spotless.actions = {};
Spotless.events = {};
Spotless.onshow = function(html) {};

Spotless.connect = function () {
    this.protocol = 'ws';
    this.host = window.location.hostname;
    this.port = '3001';
    this.path = '';
    this.url = this.protocol + '://' + this.host + ':' + this.port +'/' + this.path;

    const socket = new WebSocket(this.url);

    Spotless.send = function (data) {
        socket.send(JSON.stringify(data));
    };

    socket.onmessage = function (event) {
        console.log(event.data);
        var msg = JSON.parse(event.data);

        if (msg['action'] && Spotless.actions[msg['action']]) {
            Spotless.actions[msg['action']].apply(Spotless.actions, msg['args'] || []);
        } else if (msg['event'] && Spotless.events[msg['event']]) {
            Spotless.events[msg['event']].apply(Spotless.events, msg['args'] || []);
        } else {
            console.log(`unknown message ${event.data} ...`);
        }
    };

    socket.onopen = function (event) {
        console.log("Connection established.");
        socket.send(JSON.stringify({ type: 'login', name:'tro', code:'fhJV40NPPLBX4lPr2gRj' }));
        Spotless.send({ name: 'webpage', role:'server-commands'});
    };

    socket.onclose = function (event) {
        console.log("Connection closed.");
    };

    socket.onerror = function (event) {
        console.log("Connection error: ", event);
    };



    return socket;
};



