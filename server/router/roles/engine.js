const jsonld = require('jsonld');
const BaseRole = require('./role');

class Role extends BaseRole{

    constructor (router, ws, user) {
        super(router, ws, user);

        router.on('*.event', evt => this.receive_event(evt));
    }

    receive_event ({ event, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' reacts to event ${event}.`);
        if (dataType === 'application/ld+json') {
            jsonld.toRDF(data, {format: 'application/n-quads'})
                .then(doc => this.send({ event, data: doc, dataType: 'application/n-quads' }))
                .catch(err =>
                    console.log(`Unable to convert json-ld '${event}' data into 'application/n-triples' format.`));
        } else if (dataType === 'application/n-quads' ||
            dataType === 'text/turtle' || dataType === 'application/n-triples') {
            this.send({ event, data, dataType });
        } else if (dataType === 'application/json') {
            this.send({ event, data, dataType });
        }
    }

    action ({ action, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' emitting action ${action}.`);
        this.router.emit(action, { action, data, dataType });
    }
}

module.exports = Role;