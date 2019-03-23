const _ = require('lodash');
const EventEmitter = require('eventemitter2').EventEmitter2;
const Interface = require('./interface');


const dummyInterface = {
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
    received_error = function  (err) {
        console.log(`Connection with unidentified client is has error: ${err.message}.`, err);
    }
};


class Router extends EventEmitter {

    constructor (configuration) {
        super({ wildcard: true });
        this.configuration = configuration;
        this.users = this.configuration['users'];
    }

    login (ws, name, code) {
        const user = _.find(user => user['name'] === name && user['code'] === code);
        if (user) {
            return new Interface(this, ws, user);
        } else {
            return dummyInterface;   
        }
    }

    guest () {
        return dummyInterface;      
    }
}

module.exports = Router;