#!/usr/bin/env python
#
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

import os


# we don't bother to use cfg.py because monkey patch needs to be
# called very early.  instead, we use an environment variable to
# select the type of hub.
HUB_TYPE = os.getenv('RYU_HUB_TYPE', 'eventlet')

if HUB_TYPE == 'gevent':
    import gevent
    import gevent.monkey
    import gevent.pywsgi
    import gevent.queue

    spawn = gevent.spawn
    getcurrent = gevent.getcurrent
    patch = gevent.monkey.patch_all
    sleep = gevent.sleep
    kill = gevent.kill
    joinall = gevent.joinall
    Queue = gevent.queue.Queue
    StreamServer = gevent.server.StreamServer
    WSGIServer = gevent.pywsgi.WSGIServer
    Timeout = gevent.Timeout

    class Event(object):
        def __init__(self):
            self._ev = gevent.event.Event()

        def set(self):
            self._ev.set()

        def wait(self, timeout=None):
            self._ev.wait(timeout)
elif HUB_TYPE == 'eventlet':
    import eventlet
    import eventlet.event
    import eventlet.queue
    import eventlet.timeout
    import eventlet.wsgi
    import ssl

    spawn = eventlet.spawn
    getcurrent = eventlet.getcurrent
    patch = eventlet.monkey_patch
    sleep = eventlet.sleep

    def kill(thread):
        thread.kill()

    def joinall(threads):
        for t in threads:
            try:
                t.wait()
            except:
                pass

    Queue = eventlet.queue.Queue

    class StreamServer(object):
        def __init__(self, listen_info, handle=None, backlog=None,
                     spawn='default', **ssl_args):
            assert backlog is None
            assert spawn == 'default'
            self.server = eventlet.listen(listen_info)
            if ssl_args:
                def wrap_and_handle(sock, addr):
                    ssl_args.setdefault('server_side', True)
                    handle(ssl.wrap_socket(sock, **ssl_args), addr)

                self.handle = wrap_and_handle
            else:
                self.handle = handle

        def serve_forever(self):
            while True:
                sock, addr = self.server.accept()
                spawn(self.handle, sock, addr)

    class WSGIServer(StreamServer):
        def serve_forever(self):
            eventlet.wsgi.server(self.server, self.handle)

    Timeout = eventlet.timeout.Timeout

    class Event(object):
        def __init__(self):
            self._ev = eventlet.event.Event()

        def set(self):
            self._ev.send()

        def wait(self, timeout=None):
            if timeout is None:
                self._ev.wait()
            with Timeout(timeout):
                self._ev.wait()
