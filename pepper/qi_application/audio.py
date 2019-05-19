import time

"""
Detected 'Ja' with a confidence of 0.611400008202.
Sending event 'heard' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

        iuxe:pepper iuxe:heard iuxe:word_273 .
        iuxe:word_273 iuxe:word "Ja" .
        iuxe:word_273 iuxe:confidence "0.611400008202"^^xsd:decimal .
        
====================
Detected 'Nee' with a confidence of 0.602199971676.
Sending event 'heard' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

        iuxe:pepper iuxe:heard iuxe:word_274 .
        iuxe:word_274 iuxe:word "Nee" .
        iuxe:word_274 iuxe:confidence "0.602199971676"^^xsd:decimal .
        
====================
Detected 'Misschien' with a confidence of 0.586399972439.
Sending event 'heard' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

        iuxe:pepper iuxe:heard iuxe:word_275 .
        iuxe:word_275 iuxe:word "Misschien" .
        iuxe:word_275 iuxe:confidence "0.586399972439"^^xsd:decimal .
        
====================
"""

BASE = 'http://www.tudelft.nl/ewi/iuxe#'
DECIMAL = 'http://www.w3.org/2001/XMLSchema#decimal'
INTEGER = 'http://www.w3.org/2001/XMLSchema#integer'

class Audio:

    def __init__( self, session):
        self.memory_service = session.service("ALMemory")
	self.word_subscription = self.memory_service.subscriber("WordRecognized")
        self.word_subscription.signal.connect(self.on_word_recognized)
        self.speech_recognition_service = session.service("ALSpeechRecognition")
        self.speech_service = session.service("ALAnimatedSpeech")
        self.text_to_speech_service = session.service("ALTextToSpeech")
        self.player_service = session.service("ALAudioPlayer")
        
        self.counter = 273
        self.module_name = "PURRING_BUTTER_Vision"
        self.send_callback = lambda x: None

        self.language = self.speech_recognition_service.getLanguage()
        self.languages = self.speech_recognition_service.getAvailableLanguages()
        print('Speech Recognition Language {0}.'.format(self.language))
        print('Speech Recognition Available Languages [ {0} ].'.format(self.languages))

        self.understand('Dutch')
        self.listen_to('Ja, Nee, Misschien')

        self.speech_recognition_service.subscribe("PURRING_BUTTER")
        
        self.speech_subscription = self.memory_service.subscriber("SpeechDetected")
        self.speech_subscription.signal.connect(self.on_speech_detected)

        print("Audio Service initialized.")

    def say(self, text):
        self.speech_service.say(text)
        self.send('said', "<{0}pepper> <{0}said> \"{1}\" .".format(BASE, text), 'text/turtle')

    def listen(self, text):
        self.listen_to(text)
        self.send('listening', "<{0}pepper> <{0}listening> \"{1}\" .".format(BASE, text), 'text/turtle')

    def play(self, file):
        """
        file: *absolute* path to audio file. wav and ogg prefered
        see: http://doc.aldebaran.com/2-5/naoqi/audio/alaudioplayer.html
        """
        self.player_service.playFile(file)
        self.send('playing', "<{0}pepper> <{0}playing> \"{1}\" .".format(BASE, file), 'text/turtle')

    def send(self, event, data, data_type):
        self.send_callback({ 'type':'event', 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def understand(self, language):
        try:
            self.speech_recognition_service.pause(True)
            self.speech_recognition_service.setLanguage(language)
            self.text_to_speech_service.setLanguage(language)
            print("Pepper now speaks and recognizes {0}.".format(language))
        except Exception as e:
            print("Pepper failed to recognizes {0} because {1}.".format(language, e))
        finally:
            self.speech_recognition_service.pause(False)

    def listen_to(self, words):
        try:
            if isinstance(words, basestring):
                word_list = [x.strip() for x in words.split(',')]
            else:
                word_list = words

            self.speech_recognition_service.pause(True)
            self.speech_recognition_service.setVocabulary(word_list, False)
            print("Pepper now listens to {0}.".format(word_list))
        except Exception as e:
            print("Pepper failed to listen to words {0} because {1}.".format(words, e))
        finally:
            self.speech_recognition_service.pause(False)

    def on_word_recognized(self, args):
        [word, confidence] = args
        print("Detected '{0}' with a confidence of {1}.".format(word, confidence))
        
        data = """
        <{0}pepper> <{0}heard> <{0}word_{1}> .
        <{0}word_{1}> <{0}word> "{2}" .
        <{0}word_{1}> <{0}confidence> "{3}"^^xsd:decimal .
        """.format(BASE, self.counter, word, confidence)

        self.counter += 1

        self.send("heard", data, "text/turtle")

    def on_speech_detected(self, args):
        #if args == 1:
         #   self.send("speech_detected", "<{0}pepper> <{0}detected> <{0}speech>".format(BASE), "text/turtle")
        pass
