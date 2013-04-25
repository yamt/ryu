# Copyright (C) 2013 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2013 YAMAMOTO Takashi <yamamoto at valinux co jp>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
# implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import pickle
from ryu.lib import hub
from ryu.base import named_queue


# the use of pickle here is not essential.  it's just to ensure
# the passed objects are "simple" enough.

class _Reply(object):
    def __init__(self, type, obj):
        self._type = type
        self._obj = obj

    def __call__(self):
        if self._type == "exception":
            raise self._obj
        return self._obj


class ObjectFrontend(object):
    def __init__(self, backend_queue_id):
        self._backend_queue_id = backend_queue_id
        self._replies = named_queue.NamedQueue()
        self._calling = None  # debug

    def get_backend_queue(self):
        return self._backend_queue_id

    def __getattr__(self, name):
        def wrapper2(*args, **kwargs):
            req = pickle.dumps((name, args, kwargs, self._replies.get_name()))
            q = named_queue.NamedQueueRegistry.lookup(self._backend_queue_id)
            assert self._replies.empty()
            q.put(req)
            return pickle.loads(self._replies.get())()

        def wrapper(*args, **kwargs):
            if self._calling:
                raise Exception("_calling=%s, current=%s" % \
                    (self._calling, hub.getcurrent()))

            assert not self._calling
            self._calling = hub.getcurrent()
            try:
                return wrapper2(*args, **kwargs)
            finally:
                self._calling = None

        return wrapper


class ObjectBackend(object):
    def __init__(self, baseobj):
        self._base = baseobj
        self._incoming = named_queue.NamedQueue()
        self._thread = hub.spawn(self._loop)

    def get_queue(self):
        return self._incoming.get_name()

    def _loop(self):
        while True:
            req = self._incoming.get()
            pickle.loads(req)
            (name, args, kwargs, queue) = pickle.loads(req)
            try:
                basemethod = getattr(self._base, name)
                assert callable(basemethod)
                rep = _Reply('value', basemethod(*args, **kwargs))
            except Exception, e:
                rep = _Reply('exception', e)
            named_queue.NamedQueueRegistry.lookup(queue).put(pickle.dumps(rep))
