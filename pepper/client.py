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
        print("Received message '{0}'".format(message))
        print("Type of message is '{0}'".format(type(message.data)))
        data = json.loads(message.data)

        if 'action' in data:
            self.received_action(data)
        elif 'event' in data:
            self.received_event(data)
        else:
            self.received_unknown(data)

    def received_action(self, data):
        action = data.get('action', 'unknown')
        args = data.get('args', [])

        func = getattr(self.application, action, self.action_unknown)
        func(*args)
        self.send_json({'action': 'action_completed', 'args': [action]})

    def received_event(self, data):
        pass

    def received_unknown(self, data):
        pass

    def action_unknown(self, *args):
        print('Action was unknown.')
        self.send_json({'action': 'action_unknown', 'args': []})

    def send_audio_buffer(self, audio_buffer):
        self.send(audio_buffer, binary=True)

    def send_json(self, data):
        self.send(json.dumps(data), binary=False)
