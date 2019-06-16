import random, sys
import pdfkit
# check for the right # of args
#if len(sys.argv) != 4:
 #   print "USAGE: " + sys.argv[0], " [file of terms] [output file] [# of cards]"
  # sys.exit(1)

class Generator:

    def __init__(self, terms, amount):
        self.terms = terms
        self.amount = amount

    # read in the bingo terms
    def readTerms(self):
        in_file = open(self.terms, 'r')
        termsfile = [line.strip() for line in in_file.readlines()]
        termsfile = filter(lambda x: x != "", termsfile)
        self.termsfile = termsfile
        in_file.close()

        # XHTML4 Strict, y'all!
        self.headS = ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
            "<html lang=\"en\">\n<head>\n"
            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n"
            "<title>Bingo Cards</title>\n"
            "<style type=\"text/css\">\n"
            "\tbody { font-size: 26px; }\n"
            "\ttable { margin: 40px auto; border-spacing: 2px; }\n"
            "\t.newpage { page-break-after:always; }\n"
            "\ttr { height: 100px; }\n"
            "\ttd { text-align: center; border: thin black solid; padding: 10px; width: 100px; }\n"
            "</style>\n</head>\n<body>\n")

        self.headM = ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
            "<html lang=\"en\">\n<head>\n"
            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n"
            "<title>Bingo Cards</title>\n"
            "<style type=\"text/css\">\n"
            "\tbody { font-size: 26px; }\n"
            "\ttable { margin: 40px auto; border-spacing: 2px; }\n"
            "\t.newpage { page-break-after:always; }\n"
            "\ttr { height: 80px; }\n"
            "\ttd { text-align: center; border: thin black solid; padding: 10px; width: 80px; }\n"
            "</style>\n</head>\n<body>\n")

        self.headL = ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
            "<html lang=\"en\">\n<head>\n"
            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n"
            "<title>Bingo Cards</title>\n"
            "<style type=\"text/css\">\n"
            "\tbody { font-size: 26px; }\n"
            "\ttable { margin: 40px auto; border-spacing: 2px; }\n"
            "\t.newpage { page-break-after:always; }\n"
            "\ttr { height: 60px; }\n"
            "\ttd { text-align: center; border: thin black solid; padding: 10px; width: 60px; }\n"
            "</style>\n</head>\n<body>\n")

    # Generates an HTML table representation of the bingo card for terms
    def generateTable(self, terms, size, pagebreak = True):
        if size == 3:
            ts = terms[:9]
        elif size == 4:
            ts = terms[:16]
        elif size == 5:
            ts = terms[:12] + ["Muziek Bingo"] + terms[12:24]
        if pagebreak:
            res = "<table class=\"newpage\">\n"
        else:
            res = "<table>\n"
        for i, term in enumerate(ts):
            if i % size == 0: #<- for 3x3, 5==0 for 5x5
                res += "\t<tr>\n"
            res += "\t\t<td>" + term + "</td>\n"
            if i % size == (size-1): #<- for 3x3, 5==4 for 5x5
                res += "\t</tr>\n"
        res += "</table>\n"
        return res

    def start(self, size):
        self.readTerms()
        if size == 3:
            out_file = open("bingoS.html", 'w')
            out_file.write(self.headS)
        elif size == 4:
            out_file = open("bingoM.html", 'w')
            out_file.write(self.headM)
        elif size == 5:
            out_file = open("bingoL.html", 'w')
            out_file.write(self.headL)

        cards = int(self.amount)
        for i in range(cards):
            random.shuffle(self.termsfile)
            if i != cards - 1:
                out_file.write(self.generateTable(self.termsfile, size))
            else:
                out_file.write(self.generateTable(self.termsfile, size, False))
            out_file.write("</body></html>")

        out_file.close()
        options = {
            'orientation': "Landscape"
        }
        if size == 3:
            pdfkit.from_file('bingoS.html', 'outS.pdf', options=options)
        elif size == 4:
            pdfkit.from_file('bingoM.html', 'outM.pdf', options=options)
        elif size == 5:
            pdfkit.from_file('bingoL.html', 'outL.pdf', options=options)
