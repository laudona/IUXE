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
            name = self.namefilter(track['name'])
            playlist.append((name,track['artists'][0]['name'],track['uri'],track['duration_ms']))
        #print playlist
        return playlist


    def namefilter(self, name):
        if "(" in name:
            temp = name.replace("(", " ")
            temp2 = temp.replace(")", " ")
            name = temp2.replace(".", " ")
        if "-" in name:
            name = name.replace(" -", "")
        if "'" in name:
            name = name.replace("'","")
        if "/" in name:
            name = name.replace("/","")
        return name