const path = require('path');
const Koa = require('koa');
const route = require('koa-route');
const serve = require('koa-static');
const mount = require('koa-mount');
const handle = require('./handle');
var zerorpc = require("zerorpc");
var client = new zerorpc.Client();
client.connect("tcp://127.0.0.1:4242");


const WebSocket = require('ws');
const type = 'login'
const name = 'lurSfMnwiiTjZ';
const code = 'Wm7Wi3MoSlwc9HpZOp5s';

const ws = new WebSocket('ws://localhost:3001/');

ws.on('open', function open() {
    ws.send(JSON.stringify({ type, name, code }));
    handle(ws);
});

ws.on('message', function incoming(data) {
    console.log(data);
});


module.exports = function ({port, webDirectory}, ip_address, router, spotify) {

    const interface = router.login(ws, name, code);
    const app = module.exports = new Koa();

    app.use(mount('/public', serve(path.join(__dirname, '/public'))));
    app.use(mount('/www', serve(path.join(__dirname, webDirectory))));
    app.use(mount('/spotify', spotify));


    app.use(route.post('/music',
        function (ctx, next) {
        console.log("test123test");
        client.invoke("start","2",function(error, res, more) {
            console.log("lets go");
        });
        ctx.redirect('public/js/tablet.html');
    }));

    app.use(route.post('/finish',
        function (ctx, next) {
            client.invoke("finish",function(error, res, more) {
                console.log("finish this song");
            });
            ctx.redirect('public/js/tablet.html');
        }));
    app.use(route.post('/stop',
        function (ctx, next) {
            client.invoke("stop",function(error, res, more) {
                console.log("stopping the player");
            });
            ctx.redirect('public/js/tablet.html');
        }));
    app.use(route.post('/rules',
        function (ctx, next) {
            console.log("test456test");
            interface.send_message_to_client({
                type: 'event',
                event: 'server.event.ready',
                dataType: 'text/turtle',
                data: `<${base}agent> <${base}is> <${base}ready>  .`
            });
            ctx.redirect('public/js/tablet.html');
        }));

    app.use(route.get('/',
        async function (ctx) {
            ctx.redirect('public/js/tablet.html');
        }));

    app.use(route.get('/public',
        async function (ctx) {
            ctx.redirect('public/index.html');
        }));

    app.listen(port);
    console.log(`Spotless Sprout Web Server running on http://${ip_address}:${port}/`);

    return app;
};