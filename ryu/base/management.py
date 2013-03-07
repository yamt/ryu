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

# internal management api
#
# this module is intended to contain thin api wrappers.
# ideally each of them would be a call to an appropriate RyuApp.

import logging

import ryu.logger


def set_log_level(name, newlvl):
    """
    Set the log level of the specified logger
    """
    if not name in ryu.logger.RyuLogger.loggers:
        raise LookupError
    logger = logging.getLogger(name)
    oldlvl = logger.getEffectiveLevel()
    logger.setLevel(newlvl)


def get_log_level(name):
    """
    Return the log level of the specified logger
    """
    if not name in ryu.logger.RyuLogger.loggers:
        raise LookupError
    logger = logging.getLogger(name)
    return logger.getEffectiveLevel()


def list_loggers():
    """
    Return a list of logger names
    """
    return ryu.logger.RyuLogger.loggers


def list_bricks():
    """
    Return a list of configured bricks
    """
    from ryu.base.app_manager import SERVICE_BRICKS
    return SERVICE_BRICKS.keys()


def list_datapaths():
    """
    Return a list of connected datapaths
    """
    from ryu.controller.controller import Datapath
    return [d.id for d in Datapath if not d.id is None]
