from ws4py.client.threadedclient import WebSocketClient
import json


class PepperClient(WebSocketClient):

    def opened(self):
        print("Connection to established.")
        self.send_json({"type":"login", "name": "yangqin",  "code": "arOVtlLyNywncMEPdRBE"})

    def link_to_qi(self, application):
        print("Starting qi application")
        application.set_send_callback(self.send_json)
        self.application = application

    def closed(self, code, reason=None):
        print("Closed down", code, reason)

    def received_message(self, message):
        data = json.loads(message.data)

        if 'action' in data:
            print("Received action '{0}'.".format(data['action']))
            self.received_action(data)
        elif 'event' in data:
            print("Received event '{0}'.".format(data['event']))
            self.received_event(data)
        else:
            print("Received unknown data '{0}'.".format(data))
            self.received_unknown(data)

    def received_action(self, message):
        action = message.get('action', 'unknown')
        data = message.get('data', {})

        mapping = {
            'pepper.action.say': 'say',
            'pepper.action.listen': 'listen',
            'pepper.action.play': 'play',
            'pepper.action.navigate_to': 'navigate_to',
            'pepper.action.move_to': 'move_to',
            'pepper.action.run': 'run',
            'pepper.action.start': 'start',
            'pepper.action.show': 'show',
            'pepper.action.song' : 'song'
        }

        action_method = mapping[action]

        def create_fallback(action):
            def fallback(data):
                print("Action '{0}' was not defined in QiApplication. '{1}' is not a valid action".format(action, data))
            return fallback

        func = getattr(self.application, action_method, create_fallback(action))
        func(data)

    def received_event(self, data):
        pass

    def received_unknown(self, data):
        pass

    def send_binary(self, data_buffer):
        self.send(data_buffer, binary=True)

    def send_json(self, data):
        self.send(json.dumps(data), binary=False)
