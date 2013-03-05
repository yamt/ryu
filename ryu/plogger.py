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

class PrefixedLogger(object):
    def __init__(self, logger, prefix):
        self.logger = logger
        self.prefix = prefix

    def __getattr__(self, name):
        basemethod = getattr(self.logger, name)
        if not name in ['debug', 'info', 'warn', 'error', 'critical',
                        'exception']:
            raise AttributeError

        def method(msg, *args, **kwargs):
            return basemethod("%s %s" % (self.prefix, msg), *args, **kwargs)
        return method
