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

import unittest
from nose.tools import ok_
from nose.tools import eq_

from ryu.utils import import_module
import ryu.tests.unit.lib.test_mod.fuga.mod
import ryu.tests.unit.lib.test_mod.no_double_import.mod


class Test_import_module(unittest.TestCase):
    """ Test case for ryu.utils.import_module
    """

    def setUp(self):
        pass

    def tearDown(self):
        pass

    @staticmethod
    def _my_import(name):
        mod = __import__(name)
        components = name.split('.')
        for c in components[1:]:
            mod = getattr(mod, c)
        return mod

    def test_import_module_with_same_basename(self):
        fuga = import_module('ryu.tests.unit.lib.test_mod.fuga.mod')
        eq_("this is fuga", fuga.name)
        hoge = import_module('ryu.tests.unit.lib.test_mod.hoge.mod')
        eq_("this is hoge", hoge.name)
        fuga2 = import_module('ryu.tests.unit.lib.test_mod.fuga.mod')
        eq_("this is fuga", fuga2.name)

    def test_import_module_by_filename(self):
        fuga = import_module('./lib/test_mod/fuga/mod.py')
        eq_("this is fuga", fuga.name)
        hoge = import_module('./lib/test_mod/hoge/mod.py')
        eq_("this is hoge", hoge.name)
        fuga2 = import_module('./lib/test_mod/fuga/mod.py')
        eq_("this is fuga", fuga2.name)

    def test_import_same_module1(self):
        fuga1 = import_module('./lib/test_mod/fuga/mod.py')
        eq_("this is fuga", fuga1.name)
        # the above ends up with creating another instance of the module.
        # if it can be a problem, it's the module's responsibility to deal
        # with it.
        ok_(ryu.tests.unit.lib.test_mod.fuga.mod != fuga1)

    def test_import_same_module2(self):
        fuga1 = import_module('./lib/test_mod/fuga/mod.py')
        eq_("this is fuga", fuga1.name)
        fuga2 = import_module('ryu.tests.unit.lib.test_mod.fuga.mod')
        eq_("this is fuga", fuga2.name)
        # the above ends up with creating another instance of the module.
        # if it can be a problem, it's the module's responsibility to deal
        # with it.
        ok_(fuga1 != fuga2)

    def test_import_same_module3(self):
        fuga1 = import_module('./lib/test_mod/fuga/mod.py')
        eq_("this is fuga", fuga1.name)
        fuga3 = self._my_import('ryu.tests.unit.lib.test_mod.fuga.mod')
        eq_("this is fuga", fuga3.name)
        # the above ends up with creating another instance of the module.
        # if it can be a problem, it's the module's responsibility to deal
        # with it.
        ok_(fuga1 != fuga3)

    def test_import_same_module4(self):
        ndbl1 = import_module('./lib/test_mod/no_double_import/mod.py')
        eq_("this is no_double_import", ndbl1.name)
        eq_(ryu.tests.unit.lib.test_mod.no_double_import.mod, ndbl1)
        hoge1 = import_module('./lib/test_mod/hoge/mod.py')
        eq_("this is hoge", hoge1.name)
        ndbl2 = import_module('./lib/test_mod/no_double_import/mod.py')
        eq_("this is no_double_import", ndbl2.name)
        eq_(ryu.tests.unit.lib.test_mod.no_double_import.mod, ndbl2)
