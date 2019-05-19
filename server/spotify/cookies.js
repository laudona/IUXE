
const stateKey = 'spotify_auth_state';
const credentialsKey = 'spotify_auth_credentials';



const generateRandomString = function(length) {
    let text = '';
    const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    for (let i = 0; i < length; i++) {
        text += possible.charAt(Math.floor(Math.random() * possible.length));
    }
    return text;
};

const mismatchString = function(str1, str2) {
    return str1 === null || str1 !== str2
};

const buildState = function(ctx) {
    const state = generateRandomString(16);
    ctx.cookies.set(stateKey, state);
    return state;
};

const validateState = function (ctx) {
    const remoteState = ctx.query.state || null;
    const localState = ctx.cookies.get(stateKey) || null;
    if (mismatchString(remoteState, localState)) {
        return Promise.reject({ statusCode: 422, message: 'UnprocessableEntity', reason: 'state mismatch'});
    } else if (!ctx.query.code) {
        return Promise.reject({ statusCode: 422, message: 'UnprocessableEntity', reason: 'missing code parameter'});
    } else {
        return Promise.resolve(ctx.query.code);
    }
};

const readCredentials = function(ctx) {
    const str = ctx.cookies.get(credentialsKey);
    if (str) {
        return JSON.parse(str);
    } else {
        return { access_token: '', refresh_token: '' };
    }
};

const writeCredentials = function(ctx, credentials) {
    ctx.cookies.set(credentialsKey, JSON.stringify(credentials));
    return credentials;
};

module.exports = {
    buildState,
    validateState,
    readCredentials,
    writeCredentials
}