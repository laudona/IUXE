BASE = 'http://www.tudelft.nl/ewi/iuxe#'
DECIMAL = 'http://www.w3.org/2001/XMLSchema#decimal'
INTEGER = 'http://www.w3.org/2001/XMLSchema#integer'


class Tablet:

    def __init__(self, session):
        self.tablet_service = session.service("ALTabletService") 

        self.module_name = "PURRING_BUTTER_tablet"
        self.send_callback = lambda x: None

        print("Behavior Service initialized.")

    def configureWifi(self, security, ssid, key):
        self.tablet_service.enableWifi()
        self.tablet_service.configureWifi(security, ssid, key)

    def loadUrl(self, url):
        self.tablet_service.showWebview(url)

    def send(self, event, data, data_type):
        self.send_callback({ 'type':'event', 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def show(self, page):
        self.tablet_service.showWebview(page)
        self.send('showed', "<{0}pepper> <{0}showed> \"{1}\" .".format(BASE, page), 'text/turtle')