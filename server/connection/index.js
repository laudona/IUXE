const _ = require('lodash');


const connect = function(ws, router) {

    let handler = dummyHandler;

    const login = function ({ name, code }) {
        this.handler = router.login(ws, name, code);
    };

    const received = function (message) {
        if (message instanceof Buffer) {
            this.handler.binary(message);
        } else {
            const data = JSON.parse(message);
            if (data['type'] === 'login') {
                login(data);
            } else if (data['type'] === 'event') {
                this.handler.event(data);
            } else if (data['type'] === 'action') {
                this.handler.action(data);
            }
        }
    };

    ws.on('message', msg => received(msg));
    ws.on('error', err => handler.received_error(err));
    ws.on('close', () => handler.closed());
};


module.exports = connect;