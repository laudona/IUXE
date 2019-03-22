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


const track = function (json) {
    const root = `<http://www.tudelft.nl/ewi/iuxe#${json['id']}>`;
    let str = '';
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'duration_ms'}> "${json['duration_ms']}"^^<http://www.w3.org/2001/XMLSchema#integer> . \n`;
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'href'}> "${json['href']}" . \n`;
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'id'}> "${json['id']}" . \n`;
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'name'}> "${json['name']}" . \n`;
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'preview_url'}> "${json['preview_url']}" . \n`;
    str += `${root} <http://www.tudelft.nl/ewi/iuxe#${'uri'}> "${json['uri']}" . \n`;

    return str;
};