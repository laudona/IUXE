class Touch:

    def __init__( self, session):
        self.memory_service = session.service("ALMemory")
        
        self.counter = 273
        self.module_name = "PURRING_BUTTER_Touch"
        self.send_callback = lambda x: None

        self.right_bumper_pressed_subscription = self.memory_service.subscriber("RightBumperPressed")
        self.right_bumper_pressed_subscription.signal.connect(self.on_right_bumper_pressed)

        self.left_bumper_pressed_subscription = self.memory_service.subscriber("LeftBumperPressed")
        self.left_bumper_pressed_subscription.signal.connect(self.on_left_bumper_pressed)

        self.back_bumper_pressed_subscription = self.memory_service.subscriber("BackBumperPressed")
        self.back_bumper_pressed_subscription.signal.connect(self.on_back_bumper_pressed)

        self.front_head_touched_subscription = self.memory_service.subscriber("FrontTactilTouched")
        self.front_head_touched_subscription.signal.connect(self.on_front_head_touched)

        self.middle_head_touched_subscription = self.memory_service.subscriber("MiddleTactilTouched")
        self.middle_head_touched_subscription.signal.connect(self.on_back_head_touched)

        self.back_head_touched_subscription = self.memory_service.subscriber("RearTactilTouched")
        self.back_head_touched_subscription.signal.connect(self.on_back_head_touched)

        self.right_hand_back_touched_subscription = self.memory_service.subscriber("HandRightBackTouched")
        self.right_hand_back_touched_subscription.signal.connect(self.on_right_hand_back_touched)

        self.right_hand_left_touched_subscription = self.memory_service.subscriber("HandRightLeftTouched")
        self.right_hand_left_touched_subscription.signal.connect(self.on_right_hand_left_touched)

        self.right_hand_right_touched_subscription = self.memory_service.subscriber("HandRightRightTouched")
        self.right_hand_right_touched_subscription.signal.connect(self.on_right_hand_right_touched)

        self.left_hand_back_touched_subscription = self.memory_service.subscriber("HandLeftBackTouched")
        self.left_hand_back_touched_subscription.signal.connect(self.on_left_hand_back_touched)

        self.left_hand_left_touched_subscription = self.memory_service.subscriber("HandLeftLeftTouched")
        self.left_hand_left_touched_subscription.signal.connect(self.on_left_hand_left_touched)

        self.left_hand_right_touched_subscription = self.memory_service.subscriber("HandLeftRightTouched")
        self.left_hand_right_touched_subscription.signal.connect(self.on_left_hand_right_touched)
        
        print("Touch Service initialized.")

    def send(self, event, data, data_type):
        self.send_callback({ 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def on_right_bumper_pressed(self, *args):
        self.on_touched("right_bumper", value, *args)

    def on_left_bumper_pressed(self, *args):
        self.on_touched("left_bumper", value, *args)

    def on_back_bumper_pressed(self, *args):
        self.on_touched("back_bumper", value, *args)

    def on_front_head_touched(self, *args):
        self.on_touched("front_head", value, *args)

    def on_middle_head_touched(self, *args):
        self.on_touched("middle_head", value, *args)

    def on_back_head_touched(self, *args):
        self.on_touched("back_head", value, *args)

    def on_right_hand_back_touched(self, *args):
        self.on_touched("right_hand_back", value, *args)

    def on_right_hand_left_touched(self, *args):
        self.on_touched("right_hand_left", value, *args)

    def on_right_hand_right_touched(self, *args):
        self.on_touched("right_hand_right", value, *args)

    def on_left_hand_back_touched(self, *args):
        self.on_touched("left_hand_back", value, *args)

    def on_left_hand_left_touched(self, *args):
        self.on_touched("left_hand_left", value, *args)

    def on_left_hand_right_touched(self, *args):
        self.on_touched("left_hand_right", value, *args)

    def on_touched(self, area, value, *args):
        event = "was_touched" if False else "was_released"
        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .

        iuxe:pepper iuxe:{0} iuxe:{1} .
        """.format(event, area)
        self.send(event, data, "text/turtle")

          
            
