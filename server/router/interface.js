const convert = require('./convert');
const _ = require('lodash');


class Interface {

    constructor (router, ws, user) {
        this.ws = ws;
        this.router = router;
        this.name = user.name;
        this.role = user.role;
        this.preferedDataType = user.preferedDataType;

        console.log(`Client '${this.name}/${this.role}' joined.}`);

        _.forEach(this.user.subscribes, subscribe => {
            console.log(`Client '${this.name}/${this.role}' subscribed to '${subscribe}'.}`);
            this.router.on(`subscribe`, msg => this.send_message_to_client(msg));
        });
    }

    event ({ event, data, dataType }) {
        const eventName = `${this.role}.event.${event}`;
        console.log(`Client '${this.name}' emits ${eventName} event.`);
        this.router.emit(eventName, { event, data, dataType });
    }

    action ({ action, data, dataType }) {
        const actionName = `${this.role}.action.${action}`;
        console.log(`Client '${this.name}' emits ${actionName} action.`);
        this.router.emit(actionName, { action, data, dataType });
        console.log(`Clients of type '${this.type}' are not allowed to send actions.`);
    }

    binary (data) {
        console.log(`Clients of type '${this.type}' are not allowed to send binary data.`);
    }

    closed () {
        console.log(`Client '${this.name}/${this.role}' left.}`);
    }

    received_error = function  (err) {
        console.error(`Connection to '${this.name}/${this.role}' had error because ${err.message}`, err);
    }

    send_message_to_client(msg) {
        if (msg['action']) {
            this.send_action_to_client(msg);
        } else if (msg['event']) {
            this.send_event_to_client(msg);
        } else {
            console.error(`Unable to send message to '${this.name}/${this.role}' because it was neither an action nor an event.`, msg);
        }
    }

    send_action_to_client({ action, data, dataType }) {
        convert(this.preferedDataType, data, dataType).then((convertedData) => {
            this.send_to_client({ action, data: convertedData, dataType: this.preferedDataType });
        }).catch(err => console.error(`Unable to send action '${action}' to '${this.name}/${this.role}' because ${err.message}`, err));
    }

    send_event_to_client({ event, data, dataType }) {
        convert(this.preferedDataType, data, dataType).then((convertedData) => {
            this.send_to_client({ event, data: convertedData, dataType: this.preferedDataType });
        }).catch(err => console.error(`Unable to send event '${action}' to '${this.name}/${this.role}' because ${err.message}`, err));
    }

    send_to_client (data) {
        if (this.ws && this.ws.readyState === 1) {
            const message = JSON.stringify(data);
            console.log(`Sending data '${message}' to ${this.name} as ${this.role}.`);
            this.ws.send(message);
        }
    }
}

module.exports = Interface;