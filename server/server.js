const path = require('path');
const Koa = require('koa');
const bodyParser = require('koa-bodyparser');
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
    app.use(bodyParser());


    app.use(route.post('/music',
        function (ctx, next) {
        var amount = ctx.request.body.amount;
        client.invoke("start",amount,function(error, res, more) {
            console.log("lets go");
        });
        ctx.redirect('public/js/tablet.html');
    }));

    app.use(route.post('/finish',
        function (ctx, next) {
        client.invoke("finish", function(error, res, more) {
            console.log("lets finish the song");
        });
        ctx.redirect('public/js/tablet.html');
    }));

    app.use(route.post('/t',
        function (ctx, next) {
        alert("this button doesn't do shit");
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