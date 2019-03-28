const _ = require('lodash');
const jsonld = require('jsonld');
const { tri, res, int, str, dec } = require('../router/convert');
const { track, device, devices, me, playback } = require('./spotify-ttl');


const bind = function (client, router) {
    let handler = router.guest();

    const send_error = function (err, action, data, dataTyp) {
        // { name: 'WebapiError', message: 'Unauthorized', statusCode: 401 }
        console.error(`[ERROR][SPOTIFY-CLIENT] error handling action `, err);
        
        if (err && err.statusCode === 401) {
            console.error(`[ERROR][SPOTIFY-CLIENT] spotify rejected api call as '${err.message}'. This can indicate a login failure or an expired token. Please try to login again.`, err);
            return handler.event({ 
                type: 'event', 
                event: 'spotify.event.has_error',
                data: tri(res, 'spotify', res, 'has_error', res, err.message),
                dataType: 'text/turtle'
            });
        }

        return handler.event({
            type: 'event',
            event: 'spotify.event.has_error',
            data: tri(res, 'spotify', res, 'has_error', res, 'other'),
            dataType: 'text/turtle'
        });
    };

    const send_event = function (event, data, dataType) {
        const message = { type: 'event', event, data, dataType }
        handler.event(message);
        return message;
    };

    /**
     * action(spotify-play-"spotify:track:5cjXFtQAc2ZRyWuEFEG06v"). %% Start action(spotify-play-current). %% Resume a User's Playback
     * action(spotify-play-next). %% Skip User’s Playback To Next Track
     * action(spotify-play-previous). %%  Skip User’s Playback To Previous Track
     * actions(action(spotify-seek-5000). %%  Seek To Position in msec
     * actions(action(spotify-playback-pause). %% Pause a User's Playback
     * actions(action(spotify-playback_on-"device_uri")). %% Transfer Playback
     */
    const actions = {
        'spotify': {
            'play': function (subject, predicate, object, triples) {
                console.log(`[SPOTIFY-CLIENT] play ${object} `);
                if (object === 'current') {
                    return client.resume().then(() => send_event('playing',
                    tri(res, 'spotify', res, 'resuming', res, 'playback'), 'text/turtle'));
                } else if (object === 'next') {
                    return client.next().then(() => send_event('playing',
                    tri(res, 'spotify', res, 'playing', res, 'next'), 'text/turtle'));
                } if (object === 'previous') {
                    return client.previous().then(() => send_event('playing',
                        tri(res, 'spotify', res, 'playing', res, 'next'), 'text/turtle'));
                } else {
                    return client.play(object).then(() => send_event('playing',
                    tri(res, 'spotify', res, 'playing', str, object), 'text/turtle'));
                }
            },
            'seek': function (subject, predicate, object, triples) {
                console.log(`[SPOTIFY-CLIENT] seek ${object} `);
                return client.seek(object).then(() => send_event('sought',
                    tri(res, 'spotify', res, 'sought', int, object), 'text/turtle'));
            },
            'playback': function (subject, predicate, object, triples) {
                console.log(`[SPOTIFY-CLIENT] playback ${object} `);
                return client.pause().then(() => send_event('paused',
                    tri(res, 'spotify', res, 'paused', res, playback), 'text/turtle'));
            },
            'playback_on': function (subject, predicate, object, triples) {
                console.log(`[SPOTIFY-CLIENT] playback_on device ${object} `);
                return client.device(object).then(() => send_event('playing_back_on',
                    tri(res, 'spotify', res, 'playing_on', str, object), 'text/turtle'));
            },
            'get': function (subject, predicate, object, triples) {
                console.log(`[SPOTIFY-CLIENT] get ${object} `);
                if (object === 'devices') {
                    return client.devices()
                        .then(body => send_event('devices', devices(body['devices']), 'text/turtle'));
                } else if (object === 'me') {
                    return client.me()
                        .then(body => send_event('me', me(body), 'text/turtle'));
                } else if (object === 'current') {
                    return client.current()
                        .then(body => send_event('current', playback(body), 'text/turtle'));
                }
            }
        }
    };

    const action_defined = function (triple) {
        console.log(`[SPOTIFY-CLIENT] action defined ${triple}.`);
        const [subject, predicate, object] = triple;  
        
        if (!actions[subject]) {
            console.log(`[SPOTIFY-CLIENT] subject '${subject}' is not defined for actions.`);
            return false;
        }

        if (!actions[subject][predicate]) {
            console.log(`[SPOTIFY-CLIENT] predicate '${predicate}' is not defined for actions of subject '${subject}'.`);
            return false;
        }

        return true;
    }

    const handle_action = function (triple, triples) {
        console.log(`[SPOTIFY-CLIENT] handling triple ${triple}.`);
        if (action_defined(triple)) {
            const [subject, predicate, object] = triple;
            console.log(`[SPOTIFY-CLIENT] calling ${subject}.${predicate}(${object})...`);
            return actions[subject][predicate](subject, predicate, object, triples);
        } else {
            return Promise.reject({ message: `there is no action defined for ${triple}`, code: 404 });
        }
    };

    const handle_actions = function (triples) {
        console.log(`[SPOTIFY-CLIENT] handling actions for `, triples);
        return Promise.all(_.chain(triples)
            .filter(action_defined)
            .map(t => handle_action(t, triples))
            .value());
    };

    const received_action = function ({ action, data, dataType }) {
        console.log(`[SPOTIFY-CLIENT] received action ${action} with ${dataType} data.`);
        handle_actions(data).catch(err => send_error(err, action, data, dataType));
    };

    const received_event = function ({ event, data, dataType }) {
        // Nothing maybe? ...
    };

    handler = router.login(router.dummyWs('spotify', received_action, received_event), 'siku', 'omPfnB0MH3nhMrOEwLN7');
}

module.exports = bind;
