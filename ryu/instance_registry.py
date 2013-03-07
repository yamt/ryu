

import weakref


class InstanceRegistry(type):
    def __new__(cls, name, bases, dict):
        def unregister(self):
            for i, r in enumerate(self._instances):
                v = r()
                if v is None or v is self:
                    self._instances.pop(i)
                if v is self:
                    break
        dict['_instances'] = []
        dict['instance_registry_unregister'] = unregister
        return type.__new__(cls, name, bases, dict)

    def __call__(self, *args):
        i = type.__call__(self, *args)
        self._instances.append(weakref.ref(i))
        return i

    def __iter__(self):
        class Iter(object):
            def __init__(self, iter):
                self.iter = iter

            def __iter__(self):
                return self

            def next(self):
                while True:
                    r = self.iter.next()
                    v = r()
                    if not v is None:
                        break
                return v

        return Iter(iter(self._instances))


if __name__ == '__main__':
    class A(object):
        __metaclass__ = InstanceRegistry

        def __init__(self, name):
            self.name = name

        def __repr__(self):
            return self.name

    class B(A):
        pass

    a = A("a1")
    b = B("b1")

    print a,b

    print "iterate A"
    for i in A:
        print i
    print "iterate B"
    for i in B:
        print i

#    del a
#    del b
#    del i
#    import gc
#    gc.collect()

    a.instance_registry_unregister()
    print "iterate A"
    for i in A:
        print i
    print "iterate B"
    for i in B:
        print i
