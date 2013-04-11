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

import time
import unittest
from nose.tools import raises

from ryu.lib import hub
hub.patch()


class MyException(BaseException):
    pass


class Test_hub(unittest.TestCase):
    """ Test case for ryu.lib.hub
    """

    def setUp(self):
        pass

    def tearDown(self):
        pass

    # we want to test timeout first because the rest of tests rely on it.
    # thus test_0_ prefix.

    @raises(hub.Timeout)
    def test_0_timeout1(self):
        with hub.Timeout(0.1):
            hub.sleep(1)

    @raises(MyException)
    def test_0_timeout2(self):
        with hub.Timeout(0.1, MyException):
            hub.sleep(1)

    def test_0_timeout3(self):
        with hub.Timeout(1):
            hub.sleep(0.1)
        # sleep some more to ensure timer cancelation
        hub.sleep(2)

    def test_spawn_event1(self):
        def _child(ev, result):
            hub.sleep(1)
            result.append(1)
            ev.set()

        ev = hub.Event()
        result = []
        with hub.Timeout(2):
            hub.spawn(_child, ev, result)
            ev.wait()
        assert len(result) == 1

    def test_spawn_event2(self):
        def _child(ev, result):
            hub.sleep(1)
            result.append(1)
            ev.set()

        ev = hub.Event()
        result = []
        with hub.Timeout(2):
            hub.spawn(_child, ev, result)
            try:
                ev.wait(timeout=0.5)
                raise BaseException("should timed out")
            except hub.Timeout:
                pass
        assert len(result) == 0

    def test_spawn_event3(self):
        def _child(ev, ev2, result):
            ev2.wait()
            hub.sleep(0.5)
            result.append(1)
            ev.set()

        ev = hub.Event()
        ev2 = hub.Event()
        result = []
        with hub.Timeout(2):
            hub.spawn(_child, ev, ev2, result)
            hub.spawn(_child, ev, ev2, result)
            hub.sleep(0.5)
            ev2.set()  # this should wake up the above created two threads
            ev.wait(timeout=1)
        assert len(result) == 2

    def test_event1(self):
        ev = hub.Event()
        ev.set()
        with hub.Timeout(1):
            ev.wait()  # should return immediately

    def test_event2(self):
        ev = hub.Event()
        # allow multiple sets unlike eventlet Event
        ev.set()
        ev.set()
