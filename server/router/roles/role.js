class Role {

    constructor (router, ws, user) {
        this.ws = ws;
        this.router = router;
        this.name = user.name;
        this.role = user.role;
    }

    event ({ event, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' is has no permission to send events.`);
    }

    action ({ action, data, dataType }) {
        console.log(`User '${this.name}' in role '${this.role}' is has no permission to send action.`);
    }

    binary (data) {
        console.log(`User '${this.name}' in role '${this.role}' is has no permission to send binary.`);
    }

    send (data) {
        if (this.ws && this.ws.readyState === 1) {
            const message = JSON.stringify(data);
            console.log(`Sending data '${message}' to ${this.name} as ${this.role}.`);
            this.ws.send(message);
        }
    }
}

module.exports = Role;