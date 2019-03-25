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

const bind = function(client, router) {
    let handler = router.guest();

    const xsd_integer = 'http://www.w3.org/2001/XMLSchema#nteger';
    const xsd_decimal = 'http://www.w3.org/2001/XMLSchema#decimal';
    const base = 'http://www.tudelft.nl/ewi/iuxe#';

    const me = function (myself) {
        console.log('Spotify data from myself ', myself);
        const data = `
        `;
        return data;
    };

    const device = function (device) {
        const data = `
        <${base}spotify> <${base}has_device> <${base}${device.id}> .
        <${base}${device.id}> <${base}id> "${device.id}" .
        <${base}${device.id}> <${base}status> <${base}${device.is_active ? "active" : "non_active"}> .
        <${base}${device.id}> <${base}session> <${base}${device.is_private_session ? "private" : "public"}> .
        <${base}${device.id}> <${base}access> <${base}${device.is_restricted ? "restricted" : "not_restricted"}> .
        <${base}${device.id}> <${base}name> "${device.name}" .
        <${base}${device.id}> <${base}type> "${device.type}" .
        <${base}${device.id}> <${base}volume> "${device.name}"^^<${xsd_decimal}> .
        `
        return data;
    }

    const devices = function(devices) {
        let data = '';
        devices['devices'].forEach(dev => {
            data += device(dev);
        });
        return data;
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
        'spotify.action.get': function({action, data, dataType}) {
            console.log('Spotify received action', action);
            const args = data[0]['http://www.tudelft.nl/ewi/iuxe#get'][0];
            if (args['@id'] && args['@id'] === 'http://www.tudelft.nl/ewi/iuxe#devices') {
                console.log('spotify get', args['@id']); 
                client.devices().then(body => { 
                    event = 'spotify.event.devices';
                    data = devices(body['devices']);
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error receiving spotify devices:", err));
            } else if (args['@id'] &&  args['@id'] === 'http://www.tudelft.nl/ewi/iuxe#me') {
                console.log('spotify get', args['@id']); 
                client.me().then(body => { 
                    event = 'spotify.event.me';
                    data = me(body['devices']);
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error receiving spotify devices:", err));
            } else {
                console.log('sotify get unknown argument', args);    
            }
        },
        'spotify.action.play': function({action, data, dataType}) {
            console.log('Spotify received action', action);
            const args = data[0]['http://www.tudelft.nl/ewi/iuxe#play'][0];
            if (args['@id'] && args['@id'] === 'http://www.tudelft.nl/ewi/iuxe#next') {
                console.log('spotify play', args['@id']);
                client.resume().then(body => { 
                    event = 'spotify.event.playing';
                    data = `<${base}spotify> <${base}playing> <${base}next> .`;
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error spotify playing:", err));   
            } else if (args['@id'] && args['@id'] === 'http://www.tudelft.nl/ewi/iuxe#previous') {
                console.log('spotify play', args['@id']);   
            }else if (args['@value']) {
                console.log('spotify plays', args['@value']);
                client.play(args['@value']).then(body => { 
                    event = 'spotify.event.playing';
                    data = `<${base}spotify> <${base}playing> "${args['@value']}" .`;
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error spotify playing:", err));   
            } else { 
                console.log('spotify play has unknown argument type', args);    
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
            if (args['@id']  && args['@id'] === 'http://www.tudelft.nl/ewi/iuxe#pause') {
                console.log('sotify playback', args['@id']); 
                client.resume().then(body => { 
                    event = 'spotify.event.playback';
                    data = `<${base}spotify> <${base}playback> <${base}paused> .`;
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error spotify playing:", err));  
            } else {
                console.log('sotify playback unknown argument', args);    
            }
        },
        'spotify.action.playback_on': function({action, data, dataType}) {
            console.log('Spotify received action', action);
            console.log('Argument: ', data['http://www.tudelft.nl/ewi/iuxe#playback_on']); 
            const args = data[0]['http://www.tudelft.nl/ewi/iuxe#playback'][0];
            if (args['@value']) {
                console.log('spotify playback on device', args['@@value']); 
                client.device(args['@value']).then(body => { 
                    event = 'spotify.event.playback_on';
                    data = `<${base}spotify> <${base}playback_on> "${args['@value']}" .`;
                    dataType = 'text/turtle';
                    handler.event({ event, data, dataType });
                }).catch(err => console.log("Error spotify playing:", err));  
            } else {
                console.log('sotify playback unknown argument', args);    
            }
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


    handler = router.login(ws, 'siku', 'omPfnB0MH3nhMrOEwLN7');
}

module.exports = bind;
