const SpotifyWebApi = require('spotify-web-api-node');
const EventEmitter2 = require('eventemitter2').EventEmitter2;

const scopes = ['user-read-private', 'user-read-email', 'user-read-playback-state', 'user-read-currently-playing', 'user-modify-playback-state'];


class SpotifyClient extends EventEmitter2 {

    constructor({ redirectUri, clientId, clientSecret }) {
        super();
        this.refreshId = null;
        this.credentials = {};
        console.log('Spotify client redirectUri', redirectUri);
        console.log('Spotify client clientId', clientId);
        console.log('Spotify client clientSecret', clientSecret);
        this.spotifyApi = new SpotifyWebApi({ redirectUri, clientId, clientSecret });
    }

    stopRefreshing() {
        if (this.refreshId) {
            clearTimeout(this.refreshId);
        }
    }

    startRefreshing(expires_in) {
        if (!this.refreshId) {
            this.refreshId = setTimeout(() => {
                this.refresh();
            }, (expires_in * 1000) / 2);
        }
    }

    loginUrl(state) {
        return this.spotifyApi.createAuthorizeURL(scopes, state);
    }

    authorize(code) {
        this.stopRefreshing();
        return this.spotifyApi.authorizationCodeGrant(code).then(
            (data) => {
                console.log('Data authorize ', data.body);
                console.log('The token expires in ' + data.body['expires_in']);
                console.log('The access token is ' + data.body['access_token']);
                console.log('The refresh token is ' + data.body['refresh_token']);
                return this.grant(data.body);
            });
    }

    grant({expires_in, access_token, refresh_token}) {
        this.spotifyApi.setAccessToken(access_token);
        this.spotifyApi.setRefreshToken(refresh_token);
        const d = new Date();
        d.setSeconds(d.getSeconds() + expires_in);
        const expiresOn = d.getTime();
        this.credentials = { expiresOn, expires_in, access_token, refresh_token };
        this.startRefreshing(expires_in);
        this.emit('spotify.granted.access', this);
        return this.credentials;
    }

    refresh() {
        console.log('Refreshing spotify lease!');
        this.stopRefreshing();
        return this.spotifyApi.refreshAccessToken().then(
            (data) => {
                console.log('Data refresh ', data.body);
                console.log('The token expires in ' + data.body['expires_in']);
                console.log('The access token is ' + data.body['access_token']);
                console.log('The refresh token is ' + data.body['refresh_token']);
                return this.grant(data.body);
            });
    }

    start({ expiresOn, expires_in, access_token, refresh_token }) {
        if (access_token && refresh_token && Date.now() < expiresOn) {
            this.spotifyApi.setAccessToken(access_token);
            this.spotifyApi.setRefreshToken(refresh_token);
            return this.refresh().then((credentials) => this.me());
        } else {
            return Promise.reject({ message: 'Unauthorized', statusCode: 401 });
        }
    }

    me() {
        return this.spotifyApi.getMe().then((d) => d.body).catch(err => {
            console.log('Error spotify.me(): ', err);
            throw err;
        });
    }

    devices() {
        return this.spotifyApi.getMyDevices().then((d) => d.body);
    }

    device(id) {
        this.device_id = id;
        return this.spotifyApi.transferMyPlayback({ deviceIds: [id], play: true }).then((d) => d.body);
    }

    search(query, limit=20, offset=0) {
        return this.spotifyApi.searchTracks(query, { limit, offset }).then((d) => d.body);
    }

    current() {
        return this.spotifyApi.getMyCurrentPlaybackState({}).then((d) => d.body);
    }

    play(track) {
        return this.spotifyApi.play({ uris: [track], device_id: this.device_id }).then((d) => d.body);
    }

    pause() {
        return this.spotifyApi.pause({ device_id: this.device_id }).then((d) => d.body);
    }

    resume() {
        return this.spotifyApi.play({ device_id: this.device_id }).then((d) => d.body);
    }
}

module.exports = SpotifyClient;