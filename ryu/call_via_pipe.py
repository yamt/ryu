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
# assumption: packets are small enough to send via pipe atomically

import os
import pickle


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
            f = ByNameCall(self.basecls, name, *args, **kwargs)
            os.write(self.wpipe, pickle.dumps(f))
            replys = os.read(self.rpipe, 1024)
            reply = pickle.loads(replys)
            return reply()
        return method


def serve(rpipe, wpipe, dicts):
    """
    serve a single call
    """
    reqs = os.read(rpipe, 1024)
    f = pickle.loads(reqs)
    try:
        ret = f(dicts)
        result = CallViaPipeReply("return", ret)
    except Exception, e:
        result = CallViaPipeReply("exception", e)
    os.write(wpipe, pickle.dumps(result))
