import pytest
from counter import Counter, Counter2
from threading import Thread


@pytest.fixture(scope="module")
def counter_ref(request):
    counter_ref = Counter.start()
    def fin():
        counter_ref.stop()
    request.addfinalizer(fin)
    return counter_ref


@pytest.fixture(scope="module")
def proxy(request):
    counter_ref = Counter2.start()
    proxy = counter_ref.proxy()
    def fin():
        counter_ref.stop()
    request.addfinalizer(fin)
    return proxy


def test_simple_counter(counter_ref):
    counter_ref.tell({'method': 'set_counter',
                      'amount': 0})
    counter_ref.tell({'method': 'increase',
                      'amount': 10})
    counter_ref.tell({'method': 'increase',
                      'amount': 5})
    counter_ref.tell({'method': 'decrease',
                      'amount': 7})
    value = counter_ref.ask({'method': 'get_counter'})
    assert value == 8


def test_multiprocess(counter_ref):
    counter_ref.tell({'method': 'set_counter',
                      'amount': 0})
    def increment():
        counter_ref.tell({'method': 'increase',
                          'amount': 10})
    threads = []
    for i in range(0,10):
        thread = Thread(target=increment)
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()
    value = counter_ref.ask({'method': 'get_counter'})
    assert value == 100


def test_simple_counter_with_proxy(proxy):
    proxy.set_counter(0)
    proxy.increase(10)
    proxy.decrease(8)
    proxy.increase(5)
    value = proxy.get_counter().get()
    assert value == 7


def test_multiprocess_with_proxy(proxy):
    proxy.set_counter(0)

    def increment():
        proxy.increase(1)

    threads = []
    for i in range(0,10):
        thread = Thread(target=increment)
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()
    value = proxy.get_counter().get()
    assert value == 10
