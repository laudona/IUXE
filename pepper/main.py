import client
import qi_application
import argparse


if __name__ == '__main__':

    parser = argparse.ArgumentParser()

    parser.add_argument("--pepper_ip", type=str, default="127.0.0.1",
                        help="Robot IP address. On robot or Local Naoqi: use '127.0.0.1'.")
    parser.add_argument("--server_ip", type=str, default="localhost",
                        help="Server IP address. On same machine as server: use 'localhost'.")
    parser.add_argument("--pepper_port", type=str, default='9559',
                        help="Naoqi port number")
    parser.add_argument("--server_port", type=str, default='3001',
                        help="Server port number")

    args = parser.parse_args()

    server_url = "ws://{0}:{1}/".format(args.server_ip, args.server_port)
    print server_url

    try:
        print("Connecting to nao-qi at tcp://{0}:{1}/ ...".format(args.pepper_ip, args.pepper_port))
        app = qi_application.QiApplication(ip=args.pepper_ip, port=args.pepper_port)
        print("Connection to nao-qi at tcp://{0}:{1}/ established!".format(args.pepper_ip, args.pepper_port))

        print("Creating to server client fot {0} ...".format(server_url))
        ws = client.PepperClient(server_url)
        print("linking nao-qi client and server client...")
        ws.link_to_qi(app)
        print("nao-qi client and server client linked!")

        print("Connecting to server at {0} ...".format(server_url))
        ws.connect()
        print("Connection to server at {0} established!".format(server_url))

        print('Client is up and running! Ready to process messages...')
        ws.run_forever()
        # app.run_forever()
    except KeyboardInterrupt:
        ws.close()
        print("Client closed!")
