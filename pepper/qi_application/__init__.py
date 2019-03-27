import qi
import audio
import vision
import touch
import motion
import behavior
import tablet


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
        self.motion = motion.Motion(self.session)
        self.behavior = behavior.Behavior(self.session)
        self.tablet = tablet.Tablet(self.session)

        self.audio.set_callback(self.send)  # Set send fallback
        self.vision.set_callback(self.send)  # Set send fallback
        self.touch.set_callback(self.send)  # Set send fallback
        self.motion.set_callback(self.send)  # Set send fallback
        self.behavior.set_callback(self.send)  # Set send fallback

    def send(self, message):
        """
        I am a fallback method. I do not send anything to the server, but make sure 
        callbacks arrive at a method even if no other fallback is set.
        """
        print("Sending event '{0}' as {1}".format(message['event'], message['dataType']))
        print("=" * 20)
        print(message['data'])
        print("=" * 20)

    def set_send_callback(self, callback):
        self.audio.set_callback(callback)
        self.vision.set_callback(callback)
        self.touch.set_callback(callback)
        self.motion.set_callback(callback)
        self.behavior.set_callback(callback)

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

    def navigate_to(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#navigate_to'][0]['@value']
        self.motion.navigate_to(text)

    def move_to(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#move_to'][0]['@value']
        self.motion.move_to(text)

    def show(self, data):
        text = data[0]['http://www.tudelft.nl/ewi/iuxe#show'][0]['@value']
        self.tablet.show(text)
