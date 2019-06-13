import random, sys
#import pdfkit
# check for the right # of args
#if len(sys.argv) != 4:
 #   print "USAGE: " + sys.argv[0], " [file of terms] [output file] [# of cards]"
  # sys.exit(1)

class Generator:

    def __init__(self, terms, output, amount):
        self.terms = terms
        self.output = output
        self.amount = amount

    # read in the bingo terms
    def readTerms(self):
        in_file = open(self.terms, 'r')
        termsfile = [line.strip() for line in in_file.readlines()]
        termsfile = filter(lambda x: x != "", termsfile)
        self.termsfile = termsfile
        in_file.close()

        # XHTML4 Strict, y'all!
        self.head = ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
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

    # Generates an HTML table representation of the bingo card for terms
    def generateTable(self, terms, pagebreak = True):
        ts = terms[:9]#<-for 3x3, terms[:12] + ["Muziek Bingo"] + terms[12:24] for 5x5,
        if pagebreak:
            res = "<table class=\"newpage\">\n"
        else:
            res = "<table>\n"
        for i, term in enumerate(ts):
            if i % 3 == 0: #<- for 3x3, 5==0 for 5x5
                res += "\t<tr>\n"
            res += "\t\t<td>" + term + "</td>\n"
            if i % 3 == 2: #<- for 3x3, 5==4 for 5x5
                res += "\t</tr>\n"
        res += "</table>\n"
        return res

    def start(self):
        self.readTerms()
        out_file = open(self.output, 'w')
        out_file.write(self.head)
        cards = int(self.amount)
        for i in range(cards):
            random.shuffle(self.termsfile)
            if i != cards - 1:
                out_file.write(self.generateTable(self.termsfile))
            else:
                out_file.write(self.generateTable(self.termsfile, False))
            out_file.write("</body></html>")

        out_file.close()
        options = {
            'orientation': "Landscape"
        }
        pdfkit.from_file('bingo.html', 'out.pdf', options=options)
