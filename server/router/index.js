const _ = require('lodash');
const EventEmitter = require('eventemitter2').EventEmitter2;
const uuid = require('uuid');


class Router extends EventEmitter {

    constructor (configuration) {
        super({ wildcard: true });
        this.configuration = configuration;
        this.users = this.configuration['users'];
    }

    login (ws, name, code) {
        const user = _.find(user => user['name'] === name && user['code'] === code);
    }
}

module.exports = Router;