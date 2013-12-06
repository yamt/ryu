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

# convenient classes to manipulate OF-Config XML
# in a little more pythonic way.
# currently assuming OF-Config 1.1.1.

from ryu.lib import stringify

from lxml import objectify
import lxml.etree as ET


_ns_of111 = 'urn:onf:of111:config:yang'
_ns_netconf = 'urn:ietf:params:xml:ns:netconf:base:1.0'
_nsmap = {
    'of111': _ns_of111,
    'nc': _ns_netconf,
}


def _pythonify(name):
    return name.replace('-', '_')


class _e(object):
    def __init__(self, name):
        self.name = name
        self.cls = None


# complexType
class _ct(_e):
    def __init__(self, name, cls=None):
        super(_ct, self).__init__(name)
        self.cls = cls


class _Base(stringify.StringifyMixin):
    _M = objectify.ElementMaker(annotate=False,
                                namespace=_ns_of111,
                                nsmap=_nsmap)

    def __init__(self, **kwargs):
        for e in self._ELEMENTS:
            k = _pythonify(e.name)
            try:
                v = kwargs.pop(k)
            except KeyError:
                v = None
            setattr(self, k, v)
        if kwargs:
            raise TypeError('unknonw kwargs %s' % kwargs)

    def to_et(self, tag):
        def convert(v):
            if isinstance(v, _Base):
                return v.to_et(e.name)
            elif isinstance(v, objectify.ObjectifiedElement):
                assert ET.QName(v.tag).localname == itag
                return v
            return self._M(itag, v)

        args = []
        for e in self._ELEMENTS:
            itag = e.name
            k = _pythonify(itag)
            v = getattr(self, k)
            if v is None:
                continue
            if isinstance(v, list):
                ele = map(convert, v)
            else:
                ele = [convert(v)]
            args.extend(ele)
        return self._M(tag, *args)

    def to_xml(self, tag):
        e = self.to_et(tag)
        return ET.tostring(e, pretty_print=True)

    @classmethod
    def from_xml(cls, xmlstring):
        et = objectify.fromstring(xmlstring)
        return cls.from_et(et)

    @classmethod
    def from_et(cls, et):
        def convert(v):
            if not e.cls is None:
                return e.cls.from_et(v)
            return v

        kwargs = {}
        for e in cls._ELEMENTS:
            try:
                v = et[e.name]
            except AttributeError:
                continue
            assert isinstance(v, objectify.ObjectifiedElement)
            if len(v) == 1:
                v = convert(v)
            else:
                v = map(convert, v)
            k = _pythonify(e.name)
            assert not k in kwargs
            kwargs[k] = v
        return cls(**kwargs)


class _Unimpl(_Base):
    _ELEMENTS = [
        _e('raw_et'),
    ]

    def to_et(self, tag):
        assert self.raw_et.tag == tag
        return self.raw_et

    @classmethod
    def from_et(cls, et):
        return cls(raw_et=et)


# python classes for OF-Config elements
# class names are taken from OF-Config 1.1.1 (and partly 1.1) schma.
# ideally these classes should be generated from the schema.


class OFPortConfigurationType(_Base):
    _ELEMENTS = [
        _e('admin-state'),
        _e('no-receive'),
        _e('no-forward'),
        _e('no-packet-in'),
    ]


class OFPortType(_Base):
    _ELEMENTS = [
        _e('resource-id'),
        _e('number'),
        _e('name'),
        _e('current-rate'),
        _e('max-rate'),
        _ct('configuration', OFPortConfigurationType),
        _ct('state'),
        _ct('features'),
    ]


class OFQueuePropertiesType(_Base):
    _ELEMENTS = [
        _e('min-rate'),
        _e('max-rate'),
        _e('experimenter'),
    ]


class OFQueueType(_Base):
    _ELEMENTS = [
        _e('resource-id'),
        _e('id'),
        _e('port'),
        _ct('properties', OFQueuePropertiesType),
    ]


class OFCapableSwitchResourceListType(_Base):
    _ELEMENTS = [
        _ct('port', OFPortType),
        _ct('queue', OFQueueType),
        _ct('owned-certificate'),
        _ct('external-ceritifcate'),
        _ct('flow-table'),
    ]


class OFLogicalSwitchResourceListType(_Base):
    _ELEMENTS = [
        _e('port'),
        _e('queue'),
        _e('certificate'),
        _e('flow-table'),
    ]


class OFLogicalSwitchType(_Base):
    _ELEMENTS = [
        _e('id'),
        _e('capabilities'),
        _e('datapath-id'),
        _e('enabled'),
        _e('check-controller-certificate'),
        _e('lost-connection-behaviour'),
        _ct('controllers'),
        _ct('resources', OFLogicalSwitchResourceListType),
    ]


class OFLogicalSwitchListType(_Base):
    _ELEMENTS = [
        _ct('switch', OFLogicalSwitchType),
    ]


class OFCapableSwitchType(_Base):
    _ELEMENTS = [
        _e('id'),
        _ct('configuration-points'),
        _ct('resources', OFCapableSwitchResourceListType),
        _ct('logical-switches', OFLogicalSwitchListType),
    ]


# probably should not be here but for convenience
class NETCONF_Config(_Base):
    _ELEMENTS = [
        _ct('capable-switch', OFCapableSwitchType),
    ]

    def to_xml(self):
        return super(NETCONF_Config, self).to_xml('{%s}config' % _ns_netconf)
