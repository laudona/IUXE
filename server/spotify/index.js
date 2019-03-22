const path = require('path');
const Koa = require('koa');
const route = require('koa-route');

const spotify = require('./spotify-client');
const spotifyEvents = require('./spotify-events');
const cookies = require('./cookies');


const app = module.exports = new Koa();


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
            .then(credentials => ctx.redirect('/site/public/test-spotify.html'))
            .catch(err => ctx.redirect('/site/public/test-spotify-error.html?spotify_error=' + err.statusCode));
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
