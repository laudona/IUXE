const path = require('path');
const Koa = require('koa');
const route = require('koa-route');
const serve = require('koa-static');
const mount = require('koa-mount');

var zerorpc = require("zerorpc");
var client = new zerorpc.Client();
client.connect("tcp://127.0.0.1:4242");

module.exports = function ({port, webDirectory}, ip_address, router, spotify) {

    const app = module.exports = new Koa();

    app.use(mount('/public', serve(path.join(__dirname, '/public'))));
    app.use(mount('/www', serve(path.join(__dirname, webDirectory))));
    app.use(mount('/spotify', spotify));


    app.use(route.post('/t',
        function (ctx, next) {
        console.log("test123test");
        client.invoke("start","2",function(error, res, more) {
            console.log("lets go");
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