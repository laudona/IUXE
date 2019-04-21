var Spotless = Spotless || {};

Spotless.actions = {};
Spotless.events = {};

Spotless.connect = function () {
    this.protocol = 'ws';
    this.host = window.location.hostname;
    this.port = '3001';
    this.path = '';
    this.url = this.protocol + '://' + this.host + ':' + this.port +'/' + this.path;

    const socket = new WebSocket(this.url);

    Spotless.send_event = function (event, data, dataType) {
        var message = { type: 'event', event:event, data:data, dataType:dataType }
        Spotless.send(message);
    };

    Spotless.send = function (data) {
        socket.send(JSON.stringify(data));
    };

    var handle_actions = function(triples) {
        var i = 0, triple;
        for (i = 0; i < triples.length; i++) {
            triple = triples[i];
            subject = triple[0];
            predicate = triple[1];
            object = triple[2];
            if (Spotless.actions[subject] && Spotless.actions[subject][predicate]) {
                Spotless.actions[subject][predicate](object, triple);
            } else {
                console.log('No action defined for ' + subject + "." + predicate + "( " + object + " ).");
            }
        }
    };

    var handle_events = function(triples) {
        console.log('No action defined for event ', triples);
    };

    socket.onmessage = function (event) {
        console.log(event.data);
        var msg = JSON.parse(event.data);

        if (msg['action'] && msg['data']) {
            handle_actions(msg['data']);
        } else if (msg['event'] && msg['data']) {
            handle_events(msg['data']);
        } else {
            console.log(`unknown message ${event.data} ...`);
        }
    };

    socket.onopen = function (event) {
        console.log("Connection established.");
        Spotless.send({ type:'login', name: 'tro', code:'fhJV40NPPLBX4lPr2gRj'});
    };

    socket.onclose = function (event) {
        console.log("Connection closed.");
    };

    socket.onerror = function (event) {
        console.log("Connection error: ", event);
    };

    return socket;
};



