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

# use gevent if available.  otherwise use eventlet.

try:
    import gevent
    import gevent.coros
    import gevent.monkey
    import gevent.pywsgi
    import gevent.queue
    GEVENT=True
except:
    import eventlet
    import eventlet.wsgi
    GEVENT=False

if GEVENT:
    coros = gevent.coros
    spawn = gevent.spawn
    patch = gevent.monkey.patch_all
    sleep = gevent.sleep
    kill = gevent.kill
    joinall = gevent.joinall
    queue = gevent.queue
    StreamServer = gevent.server.StreamServer
    WSGIServer = gevent.pywsgi.WSGIServer
else:
    coros = eventlet.semaphore
    spawn = eventlet.spawn
    patch = eventlet.monkey_patch
    sleep = eventlet.sleep
    def kill(thread):
        thread.kill()
    def joinall(threads):
        map(lambda x:x.wait(), threads)
    queue = eventlet.queue
    class StreamServer(object):
        # XXXssl
        def __init__(self, listen_info, application, **config):
            self.server = eventlet.listen(listen_info)
            self.application = application
        def serve_forever(self):
            while True:
                sock, addr = self.server.accept()
                spawn(self.application, sock, addr)
    class WSGIServer(StreamServer):
        def serve_forever(self):
            eventlet.wsgi.server(self.server, self.application)
