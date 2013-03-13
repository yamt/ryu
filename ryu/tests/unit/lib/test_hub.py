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

    @raises(hub.Timeout)
    def test_timeout1(self):
        with hub.Timeout(0.1):
            hub.sleep(1)

    @raises(MyException)
    def test_timeout2(self):
        with hub.Timeout(0.1, MyException):
            hub.sleep(1)

    def test_timeout3(self):
        with hub.Timeout(1):
            hub.sleep(0.1)
        # sleep some more to ensure timer cancelation
        hub.sleep(2)
