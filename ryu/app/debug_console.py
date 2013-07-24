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

# a RyuApp to provide a python interactive console for debugging

import code
import os
import sys
import signal
import eventlet.patcher
threading = eventlet.patcher.original('threading')

from ryu.base import app_manager


class DebugConsole(app_manager.RyuApp):
    def __init__(self, *args, **kwargs):
        super(DebugConsole, self).__init__(*args, **kwargs)
        t = threading.Thread(target=self.__thread)
        t.setDaemon(True)
        t.start()

    def __thread(self):
        code.InteractiveConsole().interact("Ryu Debug Console")

        # XXX should be a graceful shutdown
        os.kill(os.getpid(), signal.SIGTERM)
