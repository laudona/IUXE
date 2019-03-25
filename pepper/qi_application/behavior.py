BASE = 'http://www.tudelft.nl/ewi/iuxe#'
DECIMAL = 'http://www.w3.org/2001/XMLSchema#decimal'
INTEGER = 'http://www.w3.org/2001/XMLSchema#integer'


class Behavior:

    def __init__(self, session):
        self.behavior_service = session.service("ALBehaviorManager") 

        self.module_name = "PURRING_BUTTER_behavior"
        self.send_callback = lambda x: None

        print("Behavior Service initialized.")

    def send(self, event, data, data_type):
        self.send_callback({ 'type':'event', 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def run(self, name):
        self.behavior_service.runBehavior(name)
        self.send('ran', "<{0}pepper> <{0}ran> \"{1}\" .".format(BASE, name), 'text/turtle')

    def start(self, name):
        self.behavior_service.startBehavior(name)
        self.send('running', "<{0}pepper> <{0}running> \"{1}\" .".format(BASE, name), 'text/turtle')
