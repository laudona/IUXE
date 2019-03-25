const _ = require('lodash');


/*

Track object
------------
artists - an array of simple artist objects
duration_ms - integer
href - string
id - string
is_playable - boolean
name - string
preview_url - string
type - string
uri - string

Artist
------
href - string
id - string
name - string
type - string
uri - string

 */

const base_iri = 'http://www.tudelft.nl/ewi/iuxe#';

/**
 * Takes a basic name and turns it into an iri-resource
 * @param name
 * @returns {string}
 */
const resource = function (name) {
    return base_iri + name;
};

/**
 * Takes a basic name and turns it into an iri-resource
 * @param name
 * @returns {string}
 */
const type = function () {
    return 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
};

/**
 * Takes a number value and return a json-ld type
 * @param i
 * @returns {{"@value": string, "@type": string}}
 */
const integer = function (i) {
    i = i || 0;
    return {
        "@value": t.toString(),
        "@type": "http://www.w3.org/2001/XMLSchema#integer"
    };
};

/**
 * Takes a number value and return a json-ld type
 * @param d
 * @returns {{"@value": string, "@type": string}}
 */
const decimal = function (d) {
    d = d || 0;
    return {
        "@value": d.toString(),
        "@type": "http://www.w3.org/2001/XMLSchema#decimal"
    };
};

/**
 * Takes a track json object and transforms it into json-ld format.
 * @param json
 */
const track = function (json) {
    const obj = {
        "@id": resource(json['id'])
    };
    if (!_.isUndefined(json['type'])) obj[type] = obj[type] = resource(json['type']);

    obj[resource('album')] = resource(json['album']['name']);

    if (!_.isUndefined(json['duration_ms'])) obj[resource('duration_ms')] = integer(json['duration_ms']);
    if (!_.isUndefined(json['name'])) obj[resource('name')] = json['name'];
    if (!_.isUndefined(json['popularity'])) obj[resource('popularity')] = integer(json['popularity']);
    if (!_.isUndefined(json['track_number'])) obj[resource('track_number')] = integer(json['track_number']);
    if (!_.isUndefined(json['uri'])) obj[resource('uri')] = json['uri'];

    return obj; // track_number
};

class EventHandler {

    constructor () {
        spotify.on('spotify.granted.access', (client) => this.on_granted(client));
    }

    on_granted(client) {
        this.stop_polling();
        this.client = client;
        this.client.me().then(me => {
            this.me = me;
            console.log("Me updated:");
            console.log(me);
            Bus.publish("spotify.updated.me", me, 'json-ld', 'spotify', 'music_provider');
        });
        this.client.devices().then(devices => { this.devices = devices; });
        this.client.current().then(current => { this.current = current; });
        this.start_polling();
    }

    stop_polling() {
        if (!this.pollId) {
            clearInterval(this.pollId);
        }
    }

    start_polling() {
        this.pollId = setInterval(() => this.poll(), 5000)
    }

    poll() {
        this.client.devices().then(devices => {
            this.devices = devices;
            console.log("Devices updated:");
            console.log(devices);

            const ld = {
                "@context": {
                    "s": "http://www.tudelft.nl/2019/ewi/iuxe/spotify#",
                    "w": "http://www.tudelft.nl/2019/ewi/iuxe/web#",
                    "i": "http://www.tudelft.nl/2019/ewi/iuxe/initial#",
                    "p": "http://www.tudelft.nl/2019/ewi/iuxe/epal#",
                    "l": "http://www.tudelft.nl/2019/ewi/iuxe/local#",
                    "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
                    "xsd": "http://www.w3.org/2001/XMLSchema#"
                },
                "@id": "http://www.tudelft.nl/2019/ewi/iuxe/spotify",
                "@graph": {
                    "s:spotify": {
                        "s:device": { "@id": "s:${id}"}
                    }
                }
            };

            const id_list = [];
            const de_list = [];
            devices['devices'].forEach(device => {
                id_list.push({ "@id": "s:" + device.id });
                de_list.push({
                    "@id": "s:" + device.id,
                    "s:id": device.id,
                    "s:status": (device.is_active ? "active" : "non_active"),
                    "s:session": (device.is_private_session ? "private" : "public"),
                    "s:access": (device.is_restricted ? "restricted" : "not_restricted"),
                    "s:name": device.name,
                    "s:type": device.type,
                    "s:volume": {
                        "@type": "xsd:decimal",
                        "@value": (device.volume ? device.volume.toString() : '0')
                    }
                });
            });

            Bus.publish("spotify.updated.devices", devices, 'json-ld', 'spotify', 'music_provider');
        });
        this.client.current().then(current => {
            this.current = current;
            console.log("Current updated:");
            console.log(current);
            Bus.publish("spotify.updated.current", current, 'json-ld', 'spotify', 'music_provider');
        });
    }
}

let handler = null;

/**
 * action(spotify-play-"spotify:track:5cjXFtQAc2ZRyWuEFEG06v"). %% Start action(spotify-play-current). %% Resume a User's Playback
 * action(spotify-play-next). %% Skip User’s Playback To Next Track
 * action(spotify-play-previous). %%  Skip User’s Playback To Previous Track
 * actions(action(spotify-seek-5000). %%  Seek To Position in msec
 * actions(action(spotify-playback-pause). %% Pause a User's Playback
 * actions(action(spotify-playback_on-"device_uri")). %% Transfer Playback
 */
const actions = {
    'spotify.action.get': function({action, data, dataType}) {
        console.log('Spotify received action', action);
        const args = data[0]['http://www.tudelft.nl/ewi/iuxe#get'][0];
        if (args['@id']) {
            console.log('sotify get', args['@id']);   
        } else {
            console.log('sotify get unknown argument', args);    
        }
    },
    'spotify.action.play': function({action, data, dataType}) {
        console.log('Spotify received action', action);
        const args = data[0]['http://www.tudelft.nl/ewi/iuxe#play'][0];
        if (args['@id']) {
            console.log('sotify play', args['@id']);   
        } else {
            console.log('sotify play unknown argument', args);    
        }
    },
    'spotify.action.seek': function({action, data, dataType}) {
        console.log('Spotify received action', action);
        const args = data[0]['http://www.tudelft.nl/ewi/iuxe#seek'][0];
        if (args['@id']) {
            console.log('sotify seek', args['@id']);   
        } else {
            console.log('sotify seek unknown argument', args);    
        }
    },
    'spotify.action.playback': function({action, data, dataType}) {
        console.log('Spotify received action', action);
        const args = data[0]['http://www.tudelft.nl/ewi/iuxe#playback'][0];
        if (args['@id']) {
            console.log('sotify playback', args['@id']);   
        } else {
            console.log('sotify playback unknown argument', args);    
        }
    },
    'spotify.action.playback_on': function({action, data, dataType}) {
        console.log('Spotify received action', action);
        console.log('Argument: ', data['http://www.tudelft.nl/ewi/iuxe#playback_on']); 
    },
    'server.event.ready': function({event, data, dataType}) {
        console.log('Spotify is ready to receive actions from router.');
        console.log('server is', data[0]['http://www.tudelft.nl/ewi/iuxe#is'][0]['@id']);
    },
};

const ws = {
    readyState: 1,
    send(message) {
        const data = JSON.parse(message);
        if (data.action && actions[data.action]) {
            console.log(`spotify received action: `, data.action);
            actions[data.action](data);
        } else if (data.event && actions[data.event]) {
            console.log(`spotify received event: `, data.event);
            actions[data.event](data);
        }
    }
}

const bind = function(client, router) {
    handler = router.login(ws, 'siku', 'omPfnB0MH3nhMrOEwLN7');
}

module.exports = bind;
