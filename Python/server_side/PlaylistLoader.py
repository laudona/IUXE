import sys
import json



class Loader:

    def __init__(self, username, spotify):
        self.username = username
        self.spotify = spotify

    def load(self, playlistid):

        # results4 = self.spotify.user_playlist(self.username, '2VfLR7RLFR0xyP0WbXpLur')
        # print json.dumps(results4, indent=4)

        # results2 = self.spotify.user_playlist(self.username, '2VfLR7RLFR0xyP0WbXpLur')
        results2 = self.spotify.user_playlist(self.username, playlistid)
        list = results2['tracks']['items']
        # print list
        playlist = []
        for item2 in list:
            track = item2['track']
            # print track['name'] + ' - ' + track['artists'][0]['name']
            playlist.append((track['name'],track['artists'][0]['name'],track['uri'],track['duration_ms']))
        # print playlist
        return playlist

        # spotify.pause_playback('3c09f2803d994caea6e66ed276e55669a41963d8')
        device1 = '3c09f2803d994caea6e66ed276e55669a41963d8'
        device2 = '14e30e031d0af575f97fbcb6f1732c9e2ed0a019'
        # txt = raw_input("stop music? y/n")
        # if txt == 'y':
        #     stop = True
        # else:
        #     stop = False
        # if stop:
        #     spotify.pause_playback(device2)
        # else:
        #     spotify.start_playback(device2)
        # print results