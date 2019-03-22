class Handler {

    constructor (ws, router) {
        this.handler = {
            ws,
            router,
            name: 'unknown',
            role: 'guest',

            event ({ event, data, dataType }) {
                console.log(`User '${this.name}' in role '${this.role}' is has no permission to send events.`);
            },

            action ({ action, data, dataType }) {
                console.log(`User '${this.name}' in role '${this.role}' is has no permission to send action.`);
            },

            binary (data) {
                console.log(`User '${this.name}' in role '${this.role}' is has no permission to send binary.`);
            }
        };
    }

    login ({ name, code }) {
        this.handler = this.handler.router.login(this.handler.ws, name, code);
    }

    received_json (data) {
        if (data['type'] === 'login') {
            this.handler.login(data);
        } else if (data['type'] === 'event') {
            this.handler.event(data);
        } else if (data['type'] === 'action') {
            this.handler.action(data);
        }


    }

    received_binary (data) {
        this.handler.binary(data);
    }

    received (message) {
        if (message instanceof Buffer) {
            this.received_binary(message);
        } else {
            this.received_json(JSON.parse(message));
        }
    }

    received_error (err) {
        console.log(`Connection with user '${this.handler.name}' in role '${this.handler.role}' is has error: ${err.message}.`, err);
    }

    closed () {
        console.log(`User '${this.handler.name}' in role '${this.handler.role}' left.`);
    }
}

module.exports = Handler;