import qi
import audio
import vision
import touch


class QiApplication:

    def __init__(self, ip="127.0.0.1", port=9559):
        connection_url = "tcp://" + ip + ":" + str(port)
        print("Connecting to nao-qi at {0} ...".format(connection_url))
        self.app = qi.Application(["--qi-url=" + connection_url])
        self.app.start()
        self.session = self.app.session

        self.audio = audio.Audio(self.session)
        self.vision = vision.Vision(self.session)
        self.touch = touch.Touch(self.session)

        self.audio.set_callback(self.send)
        self.vision.set_callback(self.send)
        self.touch.set_callback(self.send)

        # self.session.registerService(self.audio.module_name, self.audio)
        # self.session.registerService(self.vision.module_name, self.vision)

    def send(self, message):
        print("Sending event '{0}' as {1}".format(message['event'], message['dataType']))
        print("=" * 20)
        print(message['data'])
        print("=" * 20)
        # self.send_callback({ 'event': event, 'data':data, 'dataType': data_type})

    def set_send_callback(self, callback):
        self.audio.set_callback(callback)
        self.vision.set_callback(callback)
        self.touch.set_callback(callback)

    def run_forever(self):
        self.app.run()

    def say(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#say'][0]['@value']
        self.audio.say(text)

    def listen(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#listen'][0]['@value']
        self.audio.listen(text)

    def play(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#play'][0]['@value']
        self.audio.play(text)

