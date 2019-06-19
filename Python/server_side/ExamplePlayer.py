import threading

global end
end = threading.Event()

class Eplayer(threading.Thread):

    def __init__(self, username, spotify, device, example):
        threading.Thread.__init__(self)
        self.username = username
        self.spotify = spotify
        self.device = device
        self.example = example
        self.offset = ''
        self.server_url = "ws://localhost:3001/"
        self.ws = client.Client(self.server_url)
        self.ws.connect()

    def run(self):
        self.spotify.start_playback(self.device, uris=[self.example[2]])
        self.offset = self.example[3] / 3
        self.spotify.seek_track(self.offset, self.device)
        time.sleep(30)
        self.spotify.pause_playback(self.device)
        info = (self.example[0] + " : " + self.example[1]).replace(" ", "_")
        data = """
                                @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                                @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                                iuxe:spotify iuxe:info iuxe:{0} .
                                """.format(info)
        self.ws.send_json({"type": "event", "event": "info", "data": data, "dataType": "text/turtle"})
        end.wait()
        #have the robot finish the example run
        info = (self.example[0] + " : " + self.example[1]).replace(" ", "_")
        data = """
                                        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                                        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                                        iuxe:spotify iuxe:info iuxe:{0} .
                                        """.format(info)
        self.ws.send_json({"type": "event", "event": "info", "data": data, "dataType": "text/turtle"})
        self.ws.close()
        end.clear


    def finishexample(self):
        end.set()
