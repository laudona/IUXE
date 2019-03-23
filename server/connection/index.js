const _ = require('lodash');


const connect = function(ws, router) {

    let handler = router.guest();

    const login = function ({ name, code }) {
        console.log(`login attempt by ${name}.`);
        handler = router.login(ws, name, code);
        console.log(`Client is now identified as ${handler.name}.`);
    };

    const received = function (message) {
        console.log(`Message received from ${handler.name}`);
        if (message instanceof Buffer) {
            handler.binary(message);
        } else {
            const data = JSON.parse(message);
            if (data['type'] === 'login') {
                login(data);
            } else if (data['type'] === 'event') {
                handler.event(data);
            } else if (data['type'] === 'action') {
                handler.action(data);
            } else {
                console.error(`unknown message received from ${handler.name}`, data);    
            }
        }
    };

    ws.on('message', msg => received(msg));
    ws.on('error', err => handler.received_error(err));
    ws.on('close', () => handler.closed());
};


module.exports = connect;