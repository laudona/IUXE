const Bus = require('../../bus');
const spotify = require('./spotify-client');


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

const handler = new EventHandler();

module.exports = handler;
