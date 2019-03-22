const path = require('path');
const Koa = require('koa');
const route = require('koa-route');
const serve = require('koa-static');
const mount = require('koa-mount');

module.exports = function ({port}, ip_address, router, spotify) {

    const app = module.exports = new Koa();

    app.use(mount('/public', serve(path.join(__dirname, '/public'))));
    app.use(mount('/spotify', spotify));


    app.use(route.get('/',
        async function (ctx) {
            ctx.redirect('public/index.html');
        }));

    app.use(route.get('/public',
        async function (ctx) {
            ctx.redirect('public/index.html');
        }));

    app.listen(port);
    console.log(`Spotless Sprout Web Server running on http://${ip_address}:${port}/`);

    return app;
};