const WebSocket = require('ws');


module.exports = function ({port, users}, ip_address, router) {

    function noop() {}

    function heartbeat() {
        this.isAlive = true;
    }

    const wss = new WebSocket.Server({ port });

    wss.on('connection', function connection(ws) {
        ws.isAlive = true;
        ws.on('pong', heartbeat);
        connect(ws);
    });

    const interval = setInterval(function ping() {
        wss.clients.forEach(function each(ws) {
            if (ws.isAlive === false) return ws.terminate();

            ws.isAlive = false;
            ws.ping(noop);
        });
    }, 30000);

    console.log(`Spotless Sprout Websocket Server running on ws://${ip_address}:${port}/`);

    return wss;
};












