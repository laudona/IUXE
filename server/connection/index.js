const _ = require('lodash');


const dummyHandler = {
    event ({ event, data, dataType }) {
        console.log(`Unidentified client send '${event}' event. Was rejected because client was unidentified.`);
    },
    action ({ action, data, dataType }) {
        console.log(`Unidentified client send '${action}' action. Was rejected because client was unidentified.`);
    },
    binary (data) {
        console.log(`Unidentified client send binary data. Was rejected because client was unidentified.`);
    },
    closed () {
        console.log(`Unidentified client left.`);   
    },
    received_error = function  (err) {
        console.log(`Connection with unidentified client is has error: ${err.message}.`, err);
    }
};


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