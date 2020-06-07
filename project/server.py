import asyncio
import aiohttp
import argparse
from collections import namedtuple
from datetime import datetime
import logging
import json
from time import gmtime, strftime, time

API_KEY = 'API_KEY'
Servers = namedtuple('Server', ['port', 'connections'])
def get_server_info(server_name):
    if server_name == 'Hill':
        return Servers(12590, ['Jaquez', 'Smith'])
    elif server_name == 'Jaquez':
        return Servers(12591, ['Singleton', 'Hill'])
    elif server_name == 'Smith':
        return Servers(12592, ['Hill', 'Singleton', 'Campbell'])
    elif server_name == 'Campbell':
        return Servers(12593, ['Singleton', 'Smith'])
    elif server_name == 'Singleton':
        return Servers(12594, ['Jaquez', 'Smith', 'Campbell'])
History = namedtuple('History', ['server', 'time_dif', 'lat_long', 'time'])

def get_time():
    return strftime("%H:%M:%S", gmtime())

def parse_latlong(string):
    splitat = 0
    plusminus = 0
    for index, char in enumerate(string):
        if char == '+' or char == '-':
            if plusminus == 0:
                plusminus += 1
            else:
                splitat = index
                break
    return (float(string[:splitat]), float(string[splitat:]))

async def fetch(session, url):
    async with session.get(url) as r:
        return await r.json()


######################
###  CLIENT CLASS  ###
######################

class Client:
    def __init__(self, ip='127.0.0.1', port=8888, name='client', message_max_length=1e6):
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message, server):
        reader, writer = await asyncio.open_connection(self.ip, self.port)

        print(f'{self.name} send: {message!r}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} received: {data.decode()!r}')

        print('close the socket')
        writer.close()


######################
###  SERVER CLASS  ###
######################

class Server:
    def __init__(self, name, ip='127.0.0.1', port=8888, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = get_server_info(name).port
        self.message_max_length = int(message_max_length)
        self.clients = {}

    async def log_error(self, error, msg):
        logging.error('{} [{}] at server {}:{} at {}'.format(error, msg.strip(), self.name, self.port, get_time()))

    async def flood(self, at_message):
        connections = get_server_info(self.name).connections    #array of server names
        logging.info('Server {}:{} flooding servers {} at {}'.format(self.name, self.port, connections, get_time()))
        for server in connections:
            port_num = get_server_info(server).port
            try:
                logging.info('Attempt from {}:{} to flood server {}:{} at {}'.format(self.name, self.port, server, port_num, get_time()))
                await Client(port=port_num).tcp_echo_client(at_message, server)
            except:
                await self.log_error('Server {}:{} failed to propagate AT message to server {}:{}'.format(self.name, self.port, server, port_num), at_message)

    async def handle_at(self, at_message):
        logging.info('Flooding at {}:{} with message \'{}\' at {}'.format(self.name, self.port, at_message, get_time()))
        at_array = at_message.split()
        client_name = at_array[3]
        client_history = self.clients.get(client_name)
        if client_history is None or float(at_array[5]) != float(client_history.time):
            self.clients[client_name] = History(at_array[1], at_array[2], at_array[4], at_array[5])
            logging.info('Updated client history for server {}:{} at {}'.format(self.name, self.port, get_time()))
            await self.flood(at_message)
        else:
            logging.info('Did not update client history for server {}:{} at {}'.format(self.name, self.port, get_time()))
        return at_message


    async def handle_iamat(self, message, receive_time):
        msg_array = message.split()
        client_name = msg_array[1]
        lat_long = msg_array[2]
        posix_time = msg_array[3]
        interval = time() - float(receive_time)
        interval_string = "{:.9f}".format(interval)
        if interval > 0:
            interval_string = '+' + str(interval_string)
        self.clients[client_name] = History(self.name, interval_string, lat_long, posix_time)
        reply = "AT {} {} {} {} {}\n".format(self.name, interval_string, client_name, lat_long, posix_time)
        await self.flood(reply)
        logging.info('Sent message \'{}\' at server {}:{} at {}'.format(reply, self.name, self.port, get_time()))
        return reply

    async def handle_whatsat(self, message):
        msg_array = message.split()
        client_name = msg_array[1]
        radius = float(msg_array[2])*1000
        max_items = int(msg_array[3])

        client_info = self.clients.get(client_name)
        if client_info is None:
            await self.log_error('Client {} does not exist in history'.format(client_name), ' '.join(msg_array))
            return '? ' + message
        reply = "AT {} {} {} {} {}\n".format(client_info.server, client_info.time_dif, client_name, client_info.lat_long, client_info.time)

        lat, lon = parse_latlong(client_info.lat_long)
        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={},{}&radius={}&key={}'.format(lat, lon, radius, API_KEY)
        async with aiohttp.ClientSession() as session:
            response = await fetch(session, url)
            response['results'] = response['results'][:max_items]
            reply += json.dumps(response, indent=4) + '\n\n'

        logging.info('Sent message \'{}\' at server {}:{} at {}'.format(reply, self.name, self.port, get_time()))
        return reply

    async def handle_echo(self, reader, writer):
        logging.info('New connection at server {}:{} at {}'.format(self.name, self.port, get_time()))
        data = await reader.read(self.message_max_length)
        message = data.decode()
        msg_array = message.split()
        addr = writer.get_extra_info('peername')
        receive_time = time()
        print("{} received {} from {}".format(self.name, message, addr))
        logging.info('Received message \'{}\' at server {}:{} at {}'.format(' '.join(msg_array), self.name, self.port, get_time()))

        sendback_message = '? ' + message
        
        if msg_array[0] == 'IAMAT':
            sendback_message = await self.handle_iamat(message, receive_time)
        elif msg_array[0] == 'WHATSAT':
            sendback_message = await self.handle_whatsat(message)
        elif msg_array[0] == 'AT':
            sendback_message = await self.handle_at(message)
        else:
            await self.log_error('Invalid first field', ' '.join(msg_array))

        print("{} send: {}".format(self.name, sendback_message))
        writer.write(sendback_message.encode())
        await writer.drain()

        print("close the client socket")
        logging.info('Closed connection at server {}:{} at {}'.format(self.name, self.port, get_time()))
        writer.close()

    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)
        logging.basicConfig(filename='server_{}.log'.format(self.name), filemode='w',level=logging.INFO)
        logging.info('Start server {}:{} at {}'.format(self.name, self.port, get_time()))

        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()

        logging.info('Shut down server {}:{} at {}'.format(self.name, self.port, get_time()))
        server.close()


def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    print("Hello, welcome to server {}".format(args.server_name))

    server = Server(args.server_name)
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass


if __name__ == '__main__':
<<<<<<< HEAD
    main()
=======
    main()
>>>>>>> 7b25df7f5a4d66148ed4913d1199129a2f969d73
