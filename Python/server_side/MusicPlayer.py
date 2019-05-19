import random
import time
import threading
import json

global event
event = threading.Event()

class Player(threading.Thread):

    def __init__(self, username, spotify, device, playlist, tracks):
        threading.Thread.__init__(self)
        self.username = username
        self.spotify = spotify
        self.device = device
        self.playlist = playlist
        self.tracks = tracks


    def run(self):
        random.shuffle(self.playlist)
        for t in self.playlist[:self.tracks]:
            print t[0]
        for track in self.playlist[:self.tracks]:
            self.spotify.start_playback(self.device, uris=[track[2]])
            offset = track[3]/3
            self.spotify.seek_track(offset, self.device)
            event.wait(10)
            if event.is_set():
                playback = self.spotify.current_playback()
                remainder = (track[3] - playback['progress_ms'])/1000
                event.clear()
                time.sleep(remainder)
            self.spotify.pause_playback(self.device)
            time.sleep(5)

    def finish(self):
        print "finishing song"
        event.set()
