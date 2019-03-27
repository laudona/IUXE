const _ = require('lodash');
const EventEmitter = require('eventemitter2').EventEmitter2;
const convert = require('./convert');
const Interface = require('./interface');


const dummyInterface = {
    name: 'unknown',
    event ({ event, data, dataType }) {
        console.log(`Unidentified client send '${event}' event. Was rejected because client was unidentified.`);
    },
    action ({ action, data, dataType }) {
        console.log(`Unidentified client send '${action}' action. Was rejected because client was unidentified.`);
    },
    binary (data) {
        console.log(`Unidentified client send binary data. Was rejected because client was unidentified.`);
    },
    closed () {
        console.log(`Unidentified client left.`);   
    },
    received_error (err) {
        console.log(`Connection with unidentified client is has error: ${err.message}.`, err);
    }
};


class Router extends EventEmitter {

    constructor (configuration) {
        super({ wildcard: true });
        this.configuration = configuration;
        this.users = this.configuration['users'];
        convert.configure(configuration['convert']);
    }

    login (ws, name, code) {
        const user = _.find(this.users, user => user['name'] === name && user['code'] === code);
        if (user) {
            console.log(`User ${user.name} logged in.`);
            return new Interface(this, ws, user);
        } else {
            console.log(`User ${name} failed to log in.`);
            return dummyInterface;   
        }
    }

    guest () {
        return dummyInterface;      
    }

    dummyWs (name, received_action, received_event) {
        return {
            readyState: 1,
            send(message) {
                // console.log(`[ROUTER/${name}] received message: `, message);
                try {
                    const data = JSON.parse(message);
                    if (data.action) {
                        console.log(`[ROUTER/${name}] sending action ${data.action} to ${name}.`);
                        received_action(data);
                    } else if (data.event) {
                        console.log(`[ROUTER/${name}] sending event ${data.event} to ${name}.`);
                        received_event(data);
                    }
                } catch (e) {
                    console.error(`[ROUTER/${name}] an error handling message.`, e);
                }
            }
        }
    }
}

module.exports = Router;