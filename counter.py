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
        elif message['method'] == 'set_counter':
            self.counter = message['amount']


class Counter2(pykka.ThreadingActor):
    counter = 0

    def increase(self, amount):
        self.counter += amount

    def decrease(self, amount):
        self.counter -= amount

    def get_counter(self):
        return self.counter

    def set_counter(self, amount):
        self.counter=amount
