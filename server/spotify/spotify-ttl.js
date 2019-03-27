const _ = require('lodash');

const xsd_integer = 'http://www.w3.org/2001/XMLSchema#nteger';
const xsd_decimal = 'http://www.w3.org/2001/XMLSchema#decimal';
const base = 'http://www.tudelft.nl/ewi/iuxe#';

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


const track = function (track) {

    const item = {
        'img' : track_item['album']['images'][1]['url'],
        'name': track_item['name'],
        'artist': track_item["artists"][0]["name"],
        'duration_ms': track_item['duration_ms'],
        'uri': track_item['uri'],
        'id': track_item['id'],
        'type': track_item['type'],
        'preview_url': track_item['preview_url']
    };

    const data = `
    <${base}${item.id}> <${base}id> "${item.id}" .
    <${base}${item.id}> <${base}uri> "${item.uri}" .
    <${base}${item.id}> <${base}type> <${base}${item.type}> .
    <${base}${item.id}> <${base}name> "${item.name}" .
    <${base}${item.id}> <${base}artist> "${item.artist}" . 
    <${base}${item.id}> <${base}img> "${item.img}" . 
    <${base}${item.id}> <${base}preview_url> "${item.preview_url}" . 
    <${base}${item.id}> <${base}duration_ms> "${item.duration_ms}"^^<${xsd_integer}> . 
    `
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

const me = function (myself) {
    console.log('Spotify data for myself ', myself);
    const data = ` . `;
    return data;
};

const playback = function (playback) {
    console.log('Spotify data for playback ', playback);
    const data = ` . `;
    return data;
};

module.exports = { track, device, devices, me, playback };