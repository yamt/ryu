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


class NamedQueueRegistry(object):
    _registry = {}
    _next = 0

    @classmethod
    def register(cls, obj):
        name = cls._next
        cls._next += 1
        cls._registry[name] = obj
        return name

    @classmethod
    def lookup(cls, name):
        return cls._registry[name]


class NamedQueue(hub.Queue):
    def __init__(self, *args, **kwargs):
        super(NamedQueue, self).__init__(*args, **kwargs)
        self._name = NamedQueueRegistry.register(self)

    def get_name(self):
        return self._name
