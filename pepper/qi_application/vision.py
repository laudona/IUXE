"""
Colocolo:pepper ralfwolter$ python main.py --pepper_ip 192.168.178.50
Connecting to nao-qi at tcp://192.168.178.50:9559/ ...
Connecting to nao-qi at tcp://192.168.178.50:9559 ...
Speech Recognition Language Dutch.
Speech Recognition Available Languages [ ['Dutch', 'English'] ].
Pepper now speaks and recognizes Dutch.
Pepper now listens to ['Ja', 'Nee', 'Misschien'].
Audio Service initialized.
Vision Service initialized.
Connection to nao-qi at tcp://192.168.178.50:9559/ established!
Creating to server client fot ws://localhost:3001/ ...
linking nao-qi client and server client...
nao-qi client and server client linked!
Connecting to server at ws://localhost:3001/ ...
Connection to server at ws://localhost:3001/ established!
Client is up and running! Ready to process messages...
Event 'on_just_arrived': person '90562' just arrived .
Event 'name': person '90562' entered zone 1 .
position_in_robot_frame [1.4063955545425415, -0.07068486511707306, 1.5507882833480835]
 position_in_robot_frame [1.4063955545425415, -0.07068486511707306, 1.5507882833480835]
position_in_torso_frame [1.4152692556381226, -0.06934718787670135, 0.7330993413925171]
position_in_torso_frame [1.4152692556381226, -0.06934718787670135, 0.7330993413925171]
position_inworld_frame [-0.9129376411437988, -1.7115716934204102, 1.5507882833480835]
position_inworld_frame [-0.9129376411437988, -1.7115716934204102, 1.5507882833480835]
shirt_color_HSV [324.0, 0.43529412150382996, 0.11764705926179886]
Sending event 'entered_zone1' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:sees iuxe:person_90562 .
        iuxe:person_90562 iuxe:entered iuxe:zone1
        iuxe:person_90562 iuxe:distance "1.36600005627"^^xsd:decimal .
iuxe:person_90562 iuxe:face_detected "0"^^xsd:boolean .
iuxe:person_90562 iuxe:is_visible "1"^^xsd:decimal .
iuxe:person_90562 iuxe:not_seen_since "0"^^xsd:integer .
iuxe:person_90562 iuxe:present_since "4"^^xsd:integer .
iuxe:person_90562 iuxe:height "1.6529815197"^^xsd:decimal .
iuxe:person_90562 iuxe:shirt_color "Black" .

        
====================
shirt_color_HSV [324.0, 0.43529412150382996, 0.11764705926179886]
Sending event 'just_arrived' as text/turtle
====================

        

        
====================
Event 'name': person '90562' entered zone 2 .
position_in_robot_frame [1.7904431819915771, 0.035842783749103546, 1.5364292860031128]
position_in_torso_frame [1.9290146827697754, 0.07521915435791016, 0.7087972164154053]
position_inworld_frame [-0.7501330375671387, -2.219804525375366, 1.5259199142456055]
shirt_color_HSV [324.0, 0.43529412150382996, 0.11764705926179886]
Sending event 'entered_zone2' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:sees iuxe:person_90562 .
        iuxe:person_90562 iuxe:entered iuxe:zone2
        iuxe:person_90562 iuxe:distance "1.73300004005"^^xsd:decimal .
iuxe:person_90562 iuxe:face_detected "0"^^xsd:boolean .
iuxe:person_90562 iuxe:is_visible "1"^^xsd:decimal .
iuxe:person_90562 iuxe:not_seen_since "0"^^xsd:integer .
iuxe:person_90562 iuxe:present_since "5"^^xsd:integer .
iuxe:person_90562 iuxe:height "1.6529815197"^^xsd:decimal .
iuxe:person_90562 iuxe:shirt_color "Black" .

        
====================
Event 'on_just_left': person '90562' just left .
Error reading distance
Error reading face_detected
Error reading is_visible
Error reading not_seen_since
Error reading position_in_robot_frame
Error reading position_in_torso_frame
Error reading position_inworld_frame
Error reading present_since
Error reading height
Error reading shirt_color
Error reading shirt_color_HSV
Sending event 'just_left' as text/turtle
====================

        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:just_left iuxe:person_90562 .
        
        
====================
"""


class Vision(object):

    def __init__( self, session):
        super(Vision, self).__init__()
        self.memory_service = session.service("ALMemory")
        self.perception_service = session.service("ALPeoplePerception")
        self.zones_service = session.service("ALEngagementZones")
        
        self.counter = 546
        self.module_name = "PURRING_BUTTER_Vision"
        self.send_callback = lambda x: None

        self.perception_service.setFaceDetectionEnabled(True)
        self.perception_service.setMovementDetectionEnabled(True)
        # self.perception_service.setFastModeEnabled(True)

        self.just_arrived_subscription = self.memory_service.subscriber("PeoplePerception/JustArrived")
        self.just_arrived_subscription.signal.connect(self.on_just_arrived)
        self.just_left_subscription = self.memory_service.subscriber("PeoplePerception/JustLeft")
        self.just_left_subscription.signal.connect(self.on_just_left)

        self.entered_zone1_subscription = self.memory_service.subscriber("EngagementZones/PersonEnteredZone1")
        self.entered_zone1_subscription.signal.connect(self.on_entered_zone1)

        self.entered_zone2_subscription = self.memory_service.subscriber("EngagementZones/PersonEnteredZone2")
        self.entered_zone2_subscription.signal.connect(self.on_entered_zone2)

        self.entered_zone3_subscription = self.memory_service.subscriber("EngagementZones/PersonEnteredZone3")
        self.entered_zone3_subscription.signal.connect(self.on_entered_zone3)

        print("Vision Service initialized.")

    def send(self, event, data, data_type):
        self.send_callback({'type':'event', 'event': event, 'data':data, 'dataType': data_type})

    def set_callback(self, callback):
        self.send_callback = callback

    def get_person(self, id):
        data = ''

        # angles_yaw_pitch = self.memory_service.getData("PeoplePerception/Person/{0}/AnglesYawPitch".format(id))
        
        try: 
            distance = self.memory_service.getData("PeoplePerception/Person/{0}/Distance".format(id))
            data += 'iuxe:person_{0} iuxe:distance "{1}"^^xsd:decimal .\n'.format(id, distance)
        except:
            print("Error reading distance")
        
        try:
            face_detected = self.memory_service.getData("PeoplePerception/Person/{0}/IsFaceDetected".format(id))
            data += 'iuxe:person_{0} iuxe:face_detected "{1}"^^xsd:boolean .\n'.format(id, face_detected)
        except:
            print("Error reading face_detected")

        try:
            is_visible = self.memory_service.getData("PeoplePerception/Person/{0}/IsVisible".format(id))
            data += 'iuxe:person_{0} iuxe:is_visible "{1}"^^xsd:decimal .\n'.format(id, is_visible)
        except:
            print("Error reading is_visible")

        try:
            not_seen_since = self.memory_service.getData("PeoplePerception/Person/{0}/NotSeenSince".format(id))
            data += 'iuxe:person_{0} iuxe:not_seen_since "{1}"^^xsd:integer .\n'.format(id, not_seen_since)
        except:
            print("Error reading not_seen_since")

        try:
            position_in_robot_frame = self.memory_service.getData("PeoplePerception/Person/{0}/PositionInRobotFrame".format(id))
            print("position_in_robot_frame {0}".format(position_in_robot_frame))
        except:
            print("Error reading position_in_robot_frame")

        try:
            position_in_torso_frame = self.memory_service.getData("PeoplePerception/Person/{0}/PositionInTorsoFrame".format(id))
            print("position_in_torso_frame {0}".format(position_in_torso_frame))
        except:
            print("Error reading position_in_torso_frame")
        
        try:
            position_inworld_frame = self.memory_service.getData("PeoplePerception/Person/{0}/PositionInWorldFrame".format(id))
            print("position_inworld_frame {0}".format(position_inworld_frame))
        except:
            print("Error reading position_inworld_frame")

        try:
            present_since = self.memory_service.getData("PeoplePerception/Person/{0}/PresentSince".format(id))
            data += 'iuxe:person_{0} iuxe:present_since "{1}"^^xsd:integer .\n'.format(id, present_since)
        except:
            print("Error reading present_since")

        try:
            height = self.memory_service.getData("PeoplePerception/Person/{0}/RealHeight".format(id))
            data += 'iuxe:person_{0} iuxe:height "{1}"^^xsd:decimal .\n'.format(id, height)
        except:
            print("Error reading height")

        try:
            shirt_color = self.memory_service.getData("PeoplePerception/Person/{0}/ShirtColor".format(id))
            data += 'iuxe:person_{0} iuxe:shirt_color "{1}" .\n'.format(id, shirt_color)
        except:
            print("Error reading shirt_color")

        try:
            shirt_color_HSV =self.memory_service.getData("PeoplePerception/Person/{0}/ShirtColorHSV".format(id))
            print("shirt_color_HSV {0}".format(shirt_color_HSV))
        except:
            print("Error reading shirt_color_HSV")

        return data

    def on_just_arrived(self, id):
        print("Event '{0}': person '{1}' just arrived .".format("on_just_arrived", id))
        person_str = self.get_person(id)

        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:just_arrived iuxe:person_{0} .
        {1}
        """.format(id, person_str)
        self.send("just_arrived", data, "text/turtle")

    def on_just_left(self, id):
        print("Event '{0}': person '{1}' just left .".format("on_just_left", id))

        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:just_left iuxe:person_{0} .
        """.format(id)
        self.send("just_left", data, "text/turtle")

    def on_entered_zone1(self, id):
        print("Event '{0}': person '{1}' entered zone 1 .".format("name", id))
        person_str = self.get_person(id)

        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:sees iuxe:person_{0} .
        iuxe:person_{0} iuxe:entered iuxe:zone1
        {1}
        """.format(id, person_str)
        self.send("entered_zone1", data, "text/turtle")

    def on_entered_zone2(self, id):
        print("Event '{0}': person '{1}' entered zone 2 .".format("name", id))
        person_str = self.get_person(id)

        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:sees iuxe:person_{0} .
        iuxe:person_{0} iuxe:entered iuxe:zone2
        {1}
        """.format(id, person_str)
        self.send("entered_zone2", data, "text/turtle")

    def on_entered_zone3(self, id):
        print("Event '{0}': person '{1}' entered zone 3 .".format("name", id))
        person_str = self.get_person(id)

        data = """
        @prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> .
        @prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
        
        iuxe:pepper iuxe:sees iuxe:person_{0} .
        iuxe:person_{0} iuxe:entered iuxe:zone3
        {1}
        """.format(id, person_str)
        self.send("entered_zone3", data, "text/turtle")