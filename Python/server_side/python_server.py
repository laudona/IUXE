import zerorpc
import MusicPlayer as Player
import ExamplePlayer as Eplayer

class Server:

    def __init__(self, username, spotify, device, playlist, example):
        self.username = username
        self.spotify = spotify
        self.device = device
        self.playlist = playlist
        self.example = example
        self.thread = ""

    def start(self):
        if self.thread == "" or not self.thread.is_alive():
            print "initing player"
            player = Player.Player(self.username, self.spotify, self.device, self.playlist)
            self.thread = player
            self.thread.start()
            return "started player"
        else:
            print "player is already running"
            return "player is already running"

    def finish(self):
        print "trying to finish song"
        if self.thread.is_alive():
            self.thread.finish()
            return "finish song"
        else:
            return "no active player"

    def playerstop(self):
        print "trying to stop player"
        if self.thread.is_alive():
            self.thread.stopplayer()
            return "stop player"
        else:
            return "no active player"

    def pause(self):
        print "trying to pause player"
        if self.thread.is_alive():
            self.thread.pauseplayer()
            return "stop player"
        else:
            return "no active player"

    def skip(self):
        print "trying to finish song"
        if self.thread.is_alive():
            self.thread.skipsong()
            return "skipping song"
        else:
            return "no active player"


    def nexttrack(self):
        print "trying to finish song"
        if self.thread.is_alive():
            self.thread.nextsong()
            return "starting next song"
        else:
            return "no active player"


    def examplestart(self):
        if self.thread == "" or not self.thread.is_alive():
            print "initing player"
            eplayer = Eplayer.Eplayer(self.username, self.spotify, self.device, self.example)
            self.thread = eplayer
            self.thread.start()
            return "started example player"
        else:
            print "player is already running"
            return "player is already running"

    def examplefinish(self):
        print "trying to finish song"
        if self.thread.is_alive():
            self.thread.finishexample()
            return "finish song"
        else:
            return "no active player"