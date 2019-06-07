import random
import time
import threading
import json
import client

global event
event = threading.Event()
global pauze
pauze = threading.Event()

class Player(threading.Thread):

    def __init__(self, username, spotify, device, playlist, tracks):
        threading.Thread.__init__(self)
        self.username = username
        self.spotify = spotify
        self.device = device
        self.playlist = playlist
        self.tracks = tracks
        self.server_url = "ws://localhost:3001/"
        self.ws = client.Client(self.server_url)
        self.ws.connect()
        self.stop = False
        self.pauzed = False
        self.unpauzed = True
        self.is_playing = False


    def run(self):
        random.shuffle(self.playlist)
        for t in self.playlist[:self.tracks]:
            print t[0]

        for track in self.playlist[:self.tracks]:
            if self.stop:
                break
            pauze.wait()
            self.spotify.start_playback(self.device, uris=[track[2]])
            offset = track[3]/3
            self.spotify.seek_track(offset, self.device)
            self.is_playing = True
            if self.stop:
                break
            event.wait(10)
            if event.is_set() and self.pauzed:
                pauze.wait()
                playback = self.spotify.current_playback()
                remainder = (playback['progress_ms'] - self.offset)/1000
                event.clear()
                event.wait(remainder)
                if event.is_set():
                    playback = self.spotify.current_playback()
                    remainder = (track[3] - playback['progress_ms'])/1000
                    event.clear()
                    time.sleep(remainder)
                self.pauzed = False

            elif event.is_set():
                playback = self.spotify.current_playback()
                remainder = (track[3] - playback['progress_ms'])/1000
                event.clear()
                time.sleep(remainder)
            self.spotify.pause_playback(self.device)
            self.is_playing = False
            if self.stop:
                break
            info = (track[0] + ":" + track[1]).replace(" ", "_")
            print info
            data = """
                    @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                    @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                    iuxe:spotify iuxe:info iuxe:{0} .
                    """.format(info)
            self.ws.send_json({"type":"event", "event": "info", "data": data, "dataType": "text/turtle"})
            time.sleep(5)

        data = """
                @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
                @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

                iuxe:spotify iuxe:end_game iuxe:end .
                """
        self.ws.send_json({"type":"event", "event": "end_game", "data": data, "dataType": "text/turtle"})
        self.ws.close()

    def finish(self):
        print "finishing song"
        event.set()

    def stop(self):
        self.spotify.pause_playback(self.device)
        self.stop = True

    def pauze(self):
        if self.unpauzed:
            self.pauzed = True
            self.unpauzed = False
            pauze.clear()
        else:
            self.pauzed = False
            self.unpauzed = True
            pauze.set()
