const WebSocket = require('ws');
const handle = require('./interface');

const type = 'login'
const name = 'biwa';
const code = 'Wm7Wi3MoSlwc9HpZOp5s';

const ws = new WebSocket('ws://localhost:3001/');

ws.on('open', function open() {
  ws.send(JSON.stringify({ type, name, code }));
  handle(ws);
});

ws.on('message', function incoming(data) {
  console.log(data);
});

