import pykka


class Counter(pykka.ThreadingActor):
    amount = 0

    def on_receive(self, message):
        if message['method'] == 'get_counter':
            return self.amount
        elif message['method'] == 'inc':
            self.amount += message['n']
        elif message['method'] == 'dec':
            self.amount -= message['n']
        elif message['method'] == 'set_counter':
            self.amount = message['n']


class Counter2(pykka.ThreadingActor):
    amount = 0

    def inc(self, n):
        self.amount += n

    def dec(self, n):
        self.amount -= n

    def get_counter(self):
        return self.amount

    def set_counter(self, n):
        self.amount=n
