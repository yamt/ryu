# Copyright (C) 2011-2015 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2011 Isaku Yamahata <yamahata at valinux co jp>
# Copyright (C) 2013-2015 YAMAMOTO Takashi <yamamoto at valinux co jp>
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

# a bug workaround.  http://bugs.python.org/issue15881
try:
    import multiprocessing
except ImportError:
    pass

import itertools
import sys

import setuptools

import ryu
import ryu.hooks


def subpackages(dirs):
    dirs = itertools.chain.from_iterable([(x, x + '.*') for x in dirs])
    return setuptools.find_packages(include=dirs)


packages = {
    'ryu-base': {
        'packages': [
            'ryu',
            'ryu.base',
            # REVISIT(yamamoto): controller package has a lot of
            # openflow specific stuff
            'ryu.controller',
            'ryu.lib',
            'ryu.contrib',
            'ryu.contrib._eventlet',
            # REVISIT(yamamoto): probably ovsdb stuff ought to be separated
            'ryu.contrib.ovs',
        ] + subpackages([
            'ryu.contrib.tinyrpc',  # used by wsgi
        ]),
    },
    'ryu-lib-packet': {
        'packages': [
            'ryu.lib.packet',
        ],
        'install_requires': [
            'ryu-base==%s' % ryu.version,
        ],
    },
    'ryu-lib-netconf': {
        'packages': [
            'ryu.contrib.ncclient',
            'ryu.lib.netconf',
            'ryu.lib.of_config',
        ],
        'install_requires': [
            'ryu-base==%s' % ryu.version,
            'lxml',
            'paramiko',
        ],
    },
    # REVISIT(yamamoto): ryu.app package is a mess.
    # note that ryu.app.wsgi is used by app_manager.
    'ryu-app': {
        'packages': [
            'ryu.app',
            'ryu.app.gui_topology',
        ],
        'install_requires': [
            'ryu-base==%s' % ryu.version,
        ],
    },
    'ryu-app-topology': {
        'packages': [
            'ryu.topology',
        ],
        'install_requires': [
            'ryu-ofproto==%s' % ryu.version,
        ],
    },
    'ryu-app-ofctl': {
        'packages': [
            'ryu.app.ofctl',
        ],
        'install_requires': [
            'ryu-ofproto==%s' % ryu.version,
        ],
    },
    'ryu-bin': {
        'packages': [
          'ryu.cmd',
        ],
        'entry_points': {
            'console_scripts': [
                'ryu-manager = ryu.cmd.manager:main',
                'ryu = ryu.cmd.ryu_base:main',
            ]
        },
        'files': {
            'data_files': [
                'etc/ryu = etc/ryu/ryu.conf',
            ]
        },
        'install_requires': [
          'ryu-base==%s' % ryu.version,
        ],
    },
    'ryu-ofproto': {
        'packages': [
            'ryu.ofproto',
            'ryu.topology',
        ],
        'install_requires': [
            'ryu-base==%s' % ryu.version,
        ],
    },
    'ryu-services': {
        'packages': subpackages(['ryu.services']),
        'install_requires': [
            'ryu-base==%s' % ryu.version,
            'paramiko',
        ],
    },
}

packages['ryu'] = {
    'install_requires': ['%s==%s' % (p, ryu.version) for p in packages.keys()]
}


def dosetup(target):
    kwargs = packages[target]
    ryu.hooks.save_orig()
    py_modules = kwargs.pop('py_modules', [])
    py_modules += [
        'dosetup',
        'ryu/__init__',
        'ryu/hooks',
    ]
    setuptools.setup(name=target,
                     py_modules=py_modules,
                     version=ryu.version,
                     setup_requires=['d2to1'], d2to1=True,
                     **kwargs)
    sys.exit(0)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        sys.exit(1)
    target = sys.argv.pop(1)
    dosetup(target)
    sys.exit(0)
