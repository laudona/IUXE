import zerorpc
import MusicPlayer as Player

class Server:

    def __init__(self, username, spotify, device, playlist):
        self.username = username
        self.spotify = spotify
        self.device = device
        self.playlist = playlist
        self.thread = ""

    def start(self, amount):
        print amount
        if self.thread == "" or not self.thread.is_alive():
            print "initing player"
            player = Player.Player(self.username, self.spotify, self.device, self.playlist, int(amount))
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
            self.thread.skip()
            return "skipping song"
        else:
            return "no active player"
# s = zerorpc.Server(HelloRPC())
# s.bind("tcp://0.0.0.0:4242")
# s.run()