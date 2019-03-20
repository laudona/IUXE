const _ = require('lodash');
const Handler = require('./hander');


const connect = function(ws, router) {

    const handler = new Handler(ws, router);
    ws.on('message', msg => handler.received(msg));
    ws.on('error', err => handler.received_error(err));
    ws.on('close', () => handler.closed());
};


module.exports = connect;