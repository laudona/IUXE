import random
import math


class Checker:

    def __init__(self):
        print "lets check"

    def check(self, playlist, players):
        cards = []
        min1 = 0
        sec1 = 0
        min2 = 0
        sec2 = 0
        for i in range(players):
            tracks = playlist[:9]
            card = []
            for track in tracks:
                slot = {"track": track, "marked": False}
                card.append(slot)
            cards.append(card)
            random.shuffle(playlist)
        for song in playlist:
            sec1 += 30
            sec2 += 150
            if sec1 > 60:
                min1 += 1
                sec1 -= 60
            if sec2 > 60:
                min2 += int(math.floor((sec2/60)))
                sec2 = sec2 % 60
            for card in cards:
                self.marksong(song, card)
                bingo = self.checkbingo(card)
                if bingo:
                    result = "min time: %s:%s , max time: %s:%s" % (min1,sec1,min2,sec2)
                    return result
            sec1 += 30
            sec2 += 30
            if sec1 > 60:
                min1 += 1
                sec1 -= 60
            if sec2 > 60:
                min2 += int(math.floor((sec2 / 60)))
                sec2 = sec2 % 60
        return "no bingo found"

    def marksong(self, song, card):
        for slot in card:
            if slot["track"] == song:
                slot["marked"] = True

    def checkbingo(self, card):
        bingoc = self.checkcollums(card)
        bingor = self.checkrows(card)
        bingod = self.checkdiagonal(card)
        if bingoc or bingor or bingod:
            return True
        else:
            return False

    def checkcollums(self, card):
        for i in range(3):
            slot1 = card[i]["marked"]
            slot2 = card[i+3]["marked"]
            slot3 = card[i+6]["marked"]
            if slot1 and slot2 and slot3:
                return True
        return False

    def checkrows(self, card):
        for i in range(9):
            j = int(math.floor(i/3) * 3)
            slot1 = card[j]["marked"]
            slot2 = card[j+1]["marked"]
            slot3 = card[j+2]["marked"]
            if slot1 and slot2 and slot3:
                return True
        return False

    def checkdiagonal(self, card):
        slot1 = card[0]["marked"]
        slot2 = card[4]["marked"]
        slot3 = card[8]["marked"]
        if slot1 and slot2 and slot3:
            return True
        slot1 = card[2]["marked"]
        slot2 = card[4]["marked"]
        slot3 = card[6]["marked"]
        if slot1 and slot2 and slot3:
            return True
        return False