import random
import time
import threading
import json
import client
import sys

global interupt
interupt = threading.Event()
global pause
pause = threading.Event()
global stop
stop = threading.Event()
global skip
skip = threading.Event()
global next
next = threading.Event()
global confirmation
confirmation = threading.Event()


class Player(threading.Thread):

    def __init__(self, username, spotify, device, playlist):
        threading.Thread.__init__(self)
        self.username = username
        self.spotify = spotify
        self.device = device
        self.playlist = playlist
        self.offset = ''
        self.server_url = "ws://localhost:3001/"
        self.ws = client.Client(self.server_url)
        self.ws.connect()
        self.stopp = False
        self.paused = False
        self.unpaused = True
        self.is_playing = False
        pause.set()


    def run(self):
        random.shuffle(self.playlist)
        # for t in self.playlist[:self.tracks]:
        #     print t[0]
        info = ""
        data = """
                @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                iuxe:spotify iuxe:info iuxe:{0} .
                """.format(info)
        self.ws.send_json({"type": "event", "event": "info", "data": data, "dataType": "text/turtle"})

        for track in self.playlist:
            next.clear()
            if stop.is_set():
                return ""
            else:
                pause.wait()
                #send warning that music starts
                info = ""
                data = """
                        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                        iuxe:spotify iuxe:info iuxe:{0} .
                        """.format(info)
                self.ws.send_json({"type": "event", "event": "info", "data": data, "dataType": "text/turtle"})
                confirmation.wait()
                self.spotify.start_playback(self.device, uris=[track[2]])
                self.offset = track[3]/3
                self.spotify.seek_track(self.offset, self.device)
                self.is_playing = True
                confirmation.clear()
                interupt.wait(30)
                if interupt.is_set() and self.paused:
                    pause.wait()
                    playback = self.spotify.current_playback()
                    progress = (playback['progress_ms'] - self.offset)/1000
                    remainder = 30 - progress
                    print remainder
                    interupt.clear()
                    interupt.wait(remainder)
                    if interupt.is_set():
                        playback = self.spotify.current_playback()
                        remainder = (track[3] - playback['progress_ms'])/1000
                        interupt.clear()
                        skip.wait(remainder)
                    self.paused = False

                elif interupt.is_set():
                    if self.stopp:
                        stop.set()
                    else:
                        playback = self.spotify.current_playback()
                        remainder = (track[3] - playback['progress_ms'])/1000
                        interupt.clear()
                        skip.wait(remainder)
                skip.clear()
                self.spotify.pause_playback(self.device)
                self.is_playing = False
                info = (track[0] + " : " + track[1]).replace(" ", "_")
                print info
                data = """
                        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
    
                        iuxe:spotify iuxe:info iuxe:{0} .
                        """.format(info)
                self.ws.send_json({"type":"event", "event": "info", "data": data, "dataType": "text/turtle"})
                if not stop.is_set():
                    next.wait()
                info = ""
                data = """
                        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                        iuxe:spotify iuxe:info iuxe:{0} .
                        """.format(info)
                self.ws.send_json({"type": "event", "event": "info", "data": data, "dataType": "text/turtle"})

        data = """
                @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                iuxe:spotify iuxe:end_game iuxe:end .
                """
        self.ws.send_json({"type":"event", "event": "end_game", "data": data, "dataType": "text/turtle"})
        self.ws.close()
        stop.clear()


    def finish(self):
        print "finishing song"
        interupt.set()

    def stopplayer(self):
        print "stopping the player"
        self.spotify.pause_playback(self.device)
        self.stopp = True
        self.playlist = self.playlist[:1]
        interupt.set()

# works partially
    def pauseplayer(self):
        if self.unpaused:
            self.spotify.pause_playback(self.device)
            self.paused = True
            self.unpaused = False
            pause.clear()
            interupt.set()
        else:
            self.spotify.start_playback(self.device)
            self.paused = False
            self.unpaused = True
            pause.set()

    def skipsong(self):
        skip.set()

    def nextsong(self):
        next.set()

    def confirm(self):
        confirmation.set()