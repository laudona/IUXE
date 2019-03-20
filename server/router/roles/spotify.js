const jsonld = require('jsonld');
const BaseRole = require('./role');

class Role extends BaseRole{

    constructor (router, ws, user) {
        super(router, ws, user);

        router.on('spotify.action', act => this.receive_action(act));
    }

    receive_action ({ action, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' reacts to action ${action}.`);
        if (dataType === 'application/ld+json') {
            this.send({ action, data, dataType });
        } else if (dataType === 'application/n-quads' ||
            dataType === 'text/turtle' || dataType === 'application/n-triples') {
            this.send({ event, data, dataType });
            jsonld.fromRDF(data, {format: 'application/n-quads'})
                .then(doc => this.send({ event, data: doc, dataType: 'application/ld+json' }))
                .catch(err =>
                    console.log(`Unable to convert ${dataType} '${action}' data into 'application/ld+json' format.`));
        } else if (dataType === 'application/json') {
            this.send({ action, data, dataType });
        }
    }

    event ({ event, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' emitting event ${event}.`);
        this.router.emit(event, { event, data, dataType });
    }
}

module.exports = Role;