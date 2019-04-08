const _ = require('lodash');

const xsd_integer = 'http://www.w3.org/2001/XMLSchema#nteger';
const xsd_decimal = 'http://www.w3.org/2001/XMLSchema#decimal';
const xsd_boolean = 'http://www.w3.org/2001/XMLSchema#boolean';
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
    console.log(`Devices: `, devices);
    if (devices) {
        return _.chain(devices).map(device).join('\n').value();
    } 
    return `<${base}spotify> <${base}no_devices_active> "Activate a device on account to use spotify." .`
};

/**
 * 
 * @param {*} myself 
 *  { 
 *      country: 'NL',
 *       display_name: 'siku',
 *       email: 'siku@gmail.com',
 *       external_urls: { spotify: 'https://open.spotify.com/user/siku' },
 *       followers: { href: null, total: 0 },
 *       href: 'https://api.spotify.com/v1/users/siku',
 *       id: 'siku',
 *       images: [],
 *       product: 'premium',
 *       type: 'user',
 *       uri: 'spotify:user:siku' 
 *   }
 */
const me = function (myself) {
    console.log('Spotify data for myself ', myself);
    const data = `
    <${base}spotify> <${base}user> <${base}${myself.id}> .
    <${base}${myself.id}> <${base}id> "${myself.id}" .
    <${base}${myself.id}> <${base}country> "${myself.country}" .
    <${base}${myself.id}> <${base}display_name> "${myself.display_name}" .
    <${base}${myself.id}> <${base}email> "${myself.email}" .
    <${base}${myself.id}> <${base}href> "${myself.href}" .
    <${base}${myself.id}> <${base}uri> "${myself.uri}" .
    <${base}${myself.id}> <${base}product> <${base}${myself.product}> .
    <${base}${myself.id}> <${base}type> <${base}${myself.type}> .
    `
    return data;
};

/**
 * 
 * @param {*} playback 
 * { device:
   { id: '497da0d1f6c367d2a84448ff5e463d2d9e299a64',
     is_active: true,
     is_private_session: false,
     is_restricted: false,
     name: 'Colocolo',
     type: 'Computer',
     volume_percent: 63 },
  shuffle_state: false,
  repeat_state: 'off',
  timestamp: 1554660156328,
  context: null,
  progress_ms: 6677,
  item:
   { album:
      { album_type: 'compilation',
        artists: [Array],
        available_markets: [Array],
        external_urls: [Object],
        href: 'https://api.spotify.com/v1/albums/6Bnuhn1Qo1JGQqmPOyEyON',
        id: '6Bnuhn1Qo1JGQqmPOyEyON',
        images: [Array],
        name: '100 Essential Hits - 60s',
        release_date: '2010-01-01',
        release_date_precision: 'day',
        total_tracks: 100,
        type: 'album',
        uri: 'spotify:album:6Bnuhn1Qo1JGQqmPOyEyON' },
     artists: [ [Object] ],
     available_markets: [ ... ],
     disc_number: 2,
     duration_ms: 145946,
     explicit: false,
     external_ids: { isrc: 'GBF086800754' },
     external_urls:
      { spotify: 'https://open.spotify.com/track/1xEV45D5xMSXSHxs7X27Zj' },
     href: 'https://api.spotify.com/v1/tracks/1xEV45D5xMSXSHxs7X27Zj',
     id: '1xEV45D5xMSXSHxs7X27Zj',
     is_local: false,
     name: 'Son Of A Preacher Man',
     popularity: 38,
     preview_url:
      'https://p.scdn.co/mp3-preview/2dc638e550b4586363d52afbf73adca43091faca?cid=3848d012f506457997ebde1cb526ebcf',
     track_number: 7,
     type: 'track',
     uri: 'spotify:track:1xEV45D5xMSXSHxs7X27Zj' },
  currently_playing_type: 'track',
  actions: { disallows: { pausing: true, skipping_prev: true } },
  is_playing: false }

 */
const playback = function (playback) {
    console.log('Spotify data for playback ', playback);
    const item = playback.item || {};
    const data = `
    <${base}${item.id}> <${base}id> "${item.id}" .
    <${base}${item.id}> <${base}uri> "${item.uri}" .
    <${base}${item.id}> <${base}type> <${base}${item.type}> .
    <${base}${item.id}> <${base}name> "${item.name}" .
    <${base}${item.id}> <${base}artist> "${item.artist}" . 
    <${base}${item.id}> <${base}img> "${item.img}" . 
    <${base}${item.id}> <${base}preview_url> "${item.preview_url}" . 
    <${base}${item.id}> <${base}duration_ms> "${item.duration_ms}"^^<${xsd_integer}> . 
    <${base}${item.id}> <${base}progress_ms> "${playback.progress_ms}"^^<${xsd_integer}> . 
    <${base}${item.id}> <${base}is_playing> "${playback.is_playing}"^^<${xsd_boolean}> . 
    `
    return data;
};

const extractProgress = function (playback) {
    const item = playback.item || {};
    const data = {}
    const uri = item.uri;
    const progress_ms = playback.progress_ms;
    const duration_ms = item.duration_ms;
    return { uri, progress_ms, duration_ms };
};

module.exports = { track, device, devices, me, playback, extractProgress };