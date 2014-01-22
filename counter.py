import pykka


class Counter(pykka.ThreadingActor):
    counter = 0

    def on_receive(self, message):
        if message['method'] == 'get_counter':
            return self.counter
        elif message['method'] == 'increase':
            self.counter += message['amount']
        elif message['method'] == 'decrease':
            self.counter -= message['amount']
