BASE = 'http://www.tudelft.nl/ewi/iuxe#'
DECIMAL = 'http://www.w3.org/2001/XMLSchema#decimal'
INTEGER = 'http://www.w3.org/2001/XMLSchema#integer'


class Motion:

    def __init__(self, session):
        self.navigation_service = session.service("ALNavigation")  # ALMotion
        self.motion_service = session.service("ALMotion") 

        self.module_name = "PURRING_BUTTER_motiion"
        self.send_callback = lambda x: None

        print("Motion Service initialized.")

    def send(self, event, data, data_type):
        self.send_callback({ 'type':'event', 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def navigate_to(self, coordinate_string):
        if isinstance(coordinate_string, basestring):
            coords_list = [x.strip() for x in coordinate_string.split(',')]
        else:
            coords_list = coordinate_string
        if self.navigation_service.navigateTo(*coords_list):
            self.send('reached', "<{0}pepper> <{0}reached> \"{1}\" .".format(BASE, coordinate_string), 'text/turtle')
        else:
            self.send('failed_to_reach', "<{0}pepper> <{0}failed_to_reach> \"{1}\" .".format(BASE, coordinate_string), 'text/turtle')


    def move_to(self, coordinate_string):
        if isinstance(coordinate_string, basestring):
            coords_list = [x.strip() for x in coordinate_string.split(',')]
        else:
            coords_list = coordinate_string
        if self.motion_service.moveTo(*coords_list):
            self.send('moved', "<{0}pepper> <{0}moved> \"{1}\" .".format(BASE, coordinate_string), 'text/turtle')
        else:
            self.send('movement_interrupted', "<{0}pepper> <{0}movement_interrupted> \"{1}\" .".format(BASE, coordinate_string), 'text/turtle')



          
            
