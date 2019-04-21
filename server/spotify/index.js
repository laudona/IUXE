const path = require('path');
const Koa = require('koa');
const route = require('koa-route');

const Spotify = require('./spotify-client');
const cookies = require('./cookies');
const spotifyEvents = require('./spotify-events');


module.exports = function ({ redirectUri, clientId, clientSecret }, ipAddress, router) {
    const app = new Koa();
    const spotify = new Spotify({ redirectUri, clientId, clientSecret });
    const bind = spotifyEvents(spotify, router);

    app.use(route.post('/start',
        async function (ctx) {
            const credentials = cookies.readCredentials(ctx);
            await spotify.start(credentials).then(data => ctx.body = data).catch(err => {
                console.log('[ERROR][SPOTIFY] POST:/start', err);
                ctx.type = 'json';
                ctx.status = err.statusCode || 500;
                ctx.body = err;
            });
        }));

    app.use(route.get('/login',
        async function (ctx) {
            const state = cookies.buildState(ctx);
            const url = spotify.loginUrl(state);
            ctx.redirect(url);
        }));

    app.use(route.get('/callback',
        async function (ctx) {
            console.log(`[AUTHORIZATION] GET '/callback' called.`);
            await cookies.validateState(ctx)
                .then(code => spotify.authorize(code))
                .then(credentials => cookies.writeCredentials(ctx, credentials))
                .then(credentials => ctx.redirect('/public/spotify/back.html'))
                .catch(err => ctx.redirect('/public/spotify/error.html?spotify_error=' + err.statusCode));
        }));

    app.use(route.get('/search',
        async function (ctx) {
            const q = ctx.query.query || '';
            const limit = ctx.query.limit || '20';
            const offset = ctx.query.offset || '0';

            console.log(`[SPOTIFY] search query='${q}', limit=${limit}, offset=${offset}`);
            await spotify.search(q, limit, offset).then(data => ctx.body = data).catch(err => {
                console.log('[ERROR][SPOTIFY] POST:/search', err);
                ctx.type = 'json';
                ctx.status = err.statusCode || 500;
                ctx.body = err;
            });
        }));
    
    return app;
};




