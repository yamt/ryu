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

# IPC mechanism using pipe

import os
import pickle
import struct


HEADER_FORMAT = 'I'
HEADER_SIZE = struct.calcsize(HEADER_FORMAT)


def send_msg(fd, msg):
    msg_str = pickle.dumps(msg)
    blen = len(msg_str)
    hdr = struct.pack(HEADER_FORMAT, blen)
    os.write(fd, hdr)
    os.write(fd, msg_str)


def recv_msg(fd):
    hdr = os.read(fd, HEADER_SIZE)
    blen, = struct.unpack(HEADER_FORMAT, hdr)
    msg_str = os.read(fd, blen)
    return pickle.loads(msg_str)


class ByNameCall(object):
    def __init__(self, obj, method, *args, **kwargs):
        self.obj = obj
        self.method = method
        self.args = args
        self.kwargs = kwargs

    def __call__(self, dicts):
        for dict in dicts:
            try:
                obj = dict[self.obj]
                break
            except:
                pass
        method = getattr(obj, self.method)
        return method(*self.args, **self.kwargs)

    def __str__(self):
        return "CallByName %s %s %s %s" % (self.obj, self.method, self.args,
                                           self.kwargs)


class CallViaPipeReply(object):
    def __init__(self, type, value):
        self.type = type
        self.value = value

    def __call__(self):
        if self.type == "exception":
            raise(self.value)
        return self.value


class CallViaPipe(object):
    def __init__(self, rpipe, wpipe, basecls):
        self.rpipe = rpipe
        self.wpipe = wpipe
        self.basecls = basecls

    def __getattr__(self, name):
        def method(*args, **kwargs):
            send_msg(self.wpipe,
                     ByNameCall(self.basecls, name, *args, **kwargs))
            return recv_msg(self.rpipe)()
        return method


def serve(rpipe, wpipe, dicts):
    """
    serve a single call
    """
    f = recv_msg(rpipe)
    try:
        ret = f(dicts)
        result = CallViaPipeReply("return", ret)
    except Exception, e:
        result = CallViaPipeReply("exception", e)
    send_msg(wpipe, result)
