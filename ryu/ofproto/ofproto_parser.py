# Copyright (C) 2011, 2012 Nippon Telegraph and Telephone Corporation.
# Copyright (C) 2011 Isaku Yamahata <yamahata at valinux co jp>
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

import base64
import collections
import logging
import struct
import sys
import functools
import inspect

from ryu import exception

from . import ofproto_common

LOG = logging.getLogger('ryu.ofproto.ofproto_parser')


def header(buf):
    assert len(buf) >= ofproto_common.OFP_HEADER_SIZE
    #LOG.debug('len %d bufsize %d', len(buf), ofproto.OFP_HEADER_SIZE)
    return struct.unpack_from(ofproto_common.OFP_HEADER_PACK_STR, buffer(buf))


_MSG_PARSERS = {}


def register_msg_parser(version):
    def register(msg_parser):
        _MSG_PARSERS[version] = msg_parser
        return msg_parser
    return register


def msg(datapath, version, msg_type, msg_len, xid, buf):
    assert len(buf) >= msg_len

    msg_parser = _MSG_PARSERS.get(version)
    if msg_parser is None:
        raise exception.OFPUnknownVersion(version=version)

    return msg_parser(datapath, version, msg_type, msg_len, xid, buf)


def create_list_of_base_attributes(f):
    @functools.wraps(f)
    def wrapper(self, *args, **kwargs):
        ret = f(self, *args, **kwargs)
        self._base_attributes = set(dir(self))
        return ret
    return wrapper


# Some arguments to __init__ is mungled in order to avoid name conflicts
# with builtin names.
# The standard mangling is to append '_' in order to avoid name clashes
# with reserved keywords.
#
# PEP8:
# Function and method arguments
#   If a function argument's name clashes with a reserved keyword,
#   it is generally better to append a single trailing underscore
#   rather than use an abbreviation or spelling corruption. Thus
#   class_ is better than clss. (Perhaps better is to avoid such
#   clashes by using a synonym.)
#
# grep __init__ *.py | grep '[^_]_\>' showed that
# 'len', 'property', 'set', 'type'
# A bit more generic way is adopted
import __builtin__
__RESERVED_KEYWORD = dir(__builtin__)


_mapdict = lambda f, d: dict([(k, f(v)) for k, v in d.items()])


class StringifyMixin(object):
    def __str__(self):
        buf = ''
        sep = ''
        for k, v in ofp_python_attrs(self):
            buf += sep
            buf += "%s=%s" % (k, repr(v))  # repr() to escape binaries
            sep = ','
        return self.__class__.__name__ + '(' + buf + ')'
    __repr__ = __str__  # note: str(list) uses __repr__ for elements

    @staticmethod
    def _encode_value(v):
        assert not isinstance(v, dict)
        if isinstance(v, (bytes, unicode)):
            json_value = base64.b64encode(v)
        elif isinstance(v, list):
            json_value = map(StringifyMixin._encode_value, v)
        else:
            try:
                json_value = v.to_jsondict()
            except:
                json_value = v
        return json_value

    def to_jsondict(self):
        """returns an object to feed json.dumps()
        """
        dict_ = {}
        for k, v in ofp_attrs(self):
            dict_[k] = self._encode_value(v)
        return {self.__class__.__name__: dict_}

    @classmethod
    def _decode_value(cls, json_value):
        if isinstance(json_value, (bytes, unicode)):
            v = base64.b64decode(json_value)
        elif isinstance(json_value, list):
            decode = lambda x: cls._decode_value(x)
            v = map(decode, json_value)
        elif isinstance(json_value, dict):
            import sys
            parser = sys.modules[cls.__module__]
            v = ofp_from_jsondict(parser, json_value)
        else:
            v = json_value
        return v

    @classmethod
    def from_jsondict(cls, dict_):
        """create an instance from a result of json.loads()
        """
        kwargs = _mapdict(cls._decode_value, dict_)
        try:
            return cls(**kwargs)
        except TypeError:
	    # XXXhack for MsgBase derived classes
	    try:
		import ryu.ofproto.ofproto_v1_2
		class D(object):
		    def __init__(self, ofp):
			self.ofproto = ofp
		dp = D(ryu.ofproto.ofproto_v1_2)
		return cls(datapath=dp, **kwargs)
	    except TypeError:
		#debug
		print "CLS", cls
		print "ARG", dict_
		print "KWARG", kwargs
		raise


def ofp_from_jsondict(parser, jsondict):
    assert len(jsondict) == 1
    for k, v in jsondict.iteritems():
        return getattr(parser, k).from_jsondict(v)


class MsgBase(StringifyMixin):
    @create_list_of_base_attributes
    def __init__(self, datapath):
        super(MsgBase, self).__init__()
        self.datapath = datapath
        self.version = None
        self.msg_type = None
        self.msg_len = None
        self.xid = None
        self.buf = None

    def set_headers(self, version, msg_type, msg_len, xid):
        assert msg_type == self.cls_msg_type

        self.version = version
        self.msg_type = msg_type
        self.msg_len = msg_len
        self.xid = xid

    def set_xid(self, xid):
        assert self.xid is None
        self.xid = xid

    def set_buf(self, buf):
        self.buf = buffer(buf)

    def __str__(self):
        buf = 'version: 0x%x msg_type 0x%x xid 0x%x ' % (self.version,
                                                         self.msg_type,
                                                         self.xid)
        return buf + StringifyMixin.__str__(self)

    @classmethod
    def parser(cls, datapath, version, msg_type, msg_len, xid, buf):
        msg_ = cls(datapath)
        msg_.set_headers(version, msg_type, msg_len, xid)
        msg_.set_buf(buf)
        return msg_

    def _serialize_pre(self):
        assert self.version is None
        assert self.msg_type is None
        assert self.buf is None

        self.version = self.datapath.ofproto.OFP_VERSION
        self.msg_type = self.cls_msg_type
        self.buf = bytearray(self.datapath.ofproto.OFP_HEADER_SIZE)

    def _serialize_header(self):
        # buffer length is determined after trailing data is formated.
        assert self.version is not None
        assert self.msg_type is not None
        assert self.msg_len is None
        assert self.buf is not None
        assert len(self.buf) >= self.datapath.ofproto.OFP_HEADER_SIZE

        self.msg_len = len(self.buf)
        if self.xid is None:
            self.xid = 0

        struct.pack_into(self.datapath.ofproto.OFP_HEADER_PACK_STR,
                         self.buf, 0,
                         self.version, self.msg_type, self.msg_len, self.xid)

    def _serialize_body(self):
        pass

    def serialize(self):
        self._serialize_pre()
        self._serialize_body()
        self._serialize_header()


def msg_pack_into(fmt, buf, offset, *args):
    if len(buf) < offset:
        buf += bytearray(offset - len(buf))

    if len(buf) == offset:
        buf += struct.pack(fmt, *args)
        return

    needed_len = offset + struct.calcsize(fmt)
    if len(buf) < needed_len:
        buf += bytearray(needed_len - len(buf))

    struct.pack_into(fmt, buf, offset, *args)


def ofp_python_attrs(msg_):
    import collections
    # a special case for namedtuple which seems widely used in
    # ofp parser implementations.
    if hasattr(msg_, '_fields'):
        for k in msg_._fields:
            yield(k, getattr(msg_, k))
        return
    base = getattr(msg_, '_base_attributes', [])
    for k, v in inspect.getmembers(msg_):
        if k.startswith('_'):
            continue
        if callable(v):
            continue
        if k in base:
            continue
        if hasattr(msg_.__class__, k):
            continue
        yield (k, v)


def ofp_attrs(msg_):
    for k, v in ofp_python_attrs(msg_):
	if k.endswith('_') and k[:-1] in __RESERVED_KEYWORD:
	    k = k[:-1]
	yield (k, v)


def namedtuple(typename, fields, **kwargs):
    class _namedtuple(StringifyMixin,
                      collections.namedtuple(typename, fields, **kwargs)):
        pass
    return _namedtuple


def msg_str_attr(msg_, buf, attr_list=None):
    if attr_list is None:
        attr_list = ofp_attrs(msg_)
    for attr in attr_list:
        val = getattr(msg_, attr, None)
        if val is not None:
            buf += ' %s %s' % (attr, val)

    return buf
