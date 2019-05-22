const convert = require('./convert');
const _ = require('lodash');
var zerorpc = require("zerorpc");
var client = new zerorpc.Client();
client.connect("tcp://127.0.0.1:4242");

class Interface {

    constructor (router, ws, user) {
        this.ws = ws;
        this.router = router;
        this.name = user.name;
        this.role = user.role;
        this.preferedDataType = user.preferedDataType;

        console.log(`Client '${this.name}/${this.role}' joined.`);
    
        const base = 'http://www.tudelft.nl/ewi/iuxe#';
        this.send_message_to_client({
            type: 'event',
            event: 'server.event.ready',
            dataType: 'text/turtle',
            data: `<${base}agent> <${base}is> <${base}ready>  .`
        });


        _.forEach(user.subscribes, subscribe => {
            console.log(`Client '${this.name}/${this.role}' subscribed to '${subscribe}'.`);
            this.router.on(subscribe, msg => this.send_message_to_client(msg));
        });
    }

    event ({ event, data, dataType }) {
        const eventName = (event.indexOf('.') < 0 ? `${this.role}.event.${event}` : event);
        console.log(`Client '${this.name}' emits ${eventName} event.`);
        // if(eventName == 'pepper.event.start') {
        //     client.invoke("start", 2,function(error, res, more) {
        //         console.log("testin python node connection");
        //         // console.log(res.toString());
        //         //for some reason this doesn't work: this.send_message_to_client({action: 'say',data: 'hello',dataType: 'text/turtle'});
        //     });
        // }
        this.router.emit(eventName, { event, data, dataType });
    }

    action ({ action, data, dataType }) {
        let actionName = '';
        if (action && action.split('.').length > 0){
            actionName = action      
        } else {
            actionName = `${this.role}.action.${action}`;
        }
        if(actionName == 'spotify.action.start') {
            client.invoke("start", 2,function(error, res, more) {
                console.log("testin python node connection");
            });
        }
        else {
            console.log(`Client '${this.name}' emits ${actionName} action.`);
            this.router.emit(actionName, {action, data, dataType});
            // console.log(`Clients of type '${this.type}' are not allowed to send actions.`);
        }
    }

    binary (data) {
        console.log(`Clients of type '${this.type}' are not allowed to send binary data.`);
    }

    closed () {
        console.log(`Client '${this.name}/${this.role}' left.}`);
    }

    received_error (err) {
        console.error(`Connection to '${this.name}/${this.role}' had error because ${err.message}`, err);
    }

    send_message_to_client(msg) {
        console.log(`Client '${this.name}/${this.role}' send_message_to_client (${msg}).`);
        if (msg['action']) {
            this.send_action_to_client(msg);
        } else if (msg['event']) {
            this.send_event_to_client(msg);
        } else {
            console.error(`Unable to send message to '${this.name}/${this.role}' because it was neither an action nor an event.`, msg);
        }
    }

    send_action_to_client({ action, data, dataType }) {
        console.log(`Sending action to client ${this.name}/${this.role}...`);
        convert(this.preferedDataType, data, dataType).then((convertedData) => {
            console.log(`Converted action data into client ${this.name}/${this.role} prefered format...`);
            this.send_to_client({ action, data: convertedData, dataType: this.preferedDataType });
        }).catch(err => console.error(`Unable to send action '${action}' to '${this.name}/${this.role}' because ${err.message}`, err));
    }

    send_event_to_client({ event, data, dataType }) {
        console.log(`Sending event to client ${this.name}/${this.role}...`);
        convert(this.preferedDataType, data, dataType).then((convertedData) => {
            console.log(`Converted event data into client ${this.name}/${this.role} prefered format...`);
            return this.send_to_client({ event, data: convertedData, dataType: this.preferedDataType });
        }).catch(err => console.error(`Unable to send event '${action}' to '${this.name}/${this.role}' because ${err.message}`, err));
    }

    send_to_client (data) {
        console.log(`Sending data to ${this.name} as ${this.role}...`);
        if (this.ws && this.ws.readyState === 1) {
            const message = _.isString(data) ? data : JSON.stringify(data);
            console.log(`Sending data '${message}' to ${this.name} as ${this.role}.`);
            this.ws.send(message);
        }
    }
}

module.exports = Interface;