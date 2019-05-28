const path = require('path');
const Koa = require('koa');
const route = require('koa-route');
const serve = require('koa-static');
const mount = require('koa-mount');

var zerorpc = require("zerorpc");
var client = new zerorpc.Client();
client.connect("tcp://127.0.0.1:4242");

module.exports = function ({port, webDirectory}, ip_address, router, socket) {

    const interface = router.login(socket, "biwa" , "Wm7Wi3MoSlwc9HpZOp5s");
    const app = module.exports = new Koa();

    app.use(mount('/public', serve(path.join(__dirname, '/public'))));
    app.use(mount('/www', serve(path.join(__dirname, webDirectory))));
    //app.use(mount('/spotify', spotify));


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
            const base = 'http://www.tudelft.nl/ewi/iuxe#';
            interface.event({
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