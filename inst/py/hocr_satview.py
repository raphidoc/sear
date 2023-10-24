# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class HocrSatview(KaitaiStruct):
    """HOCR binary files contains all the numerical counts for each radiometric
    channel. The calibration file (.cal) for each instruments preciesely define
    the structure of a binary packet send by that instrument.
    
    SatView append 7 bytes to HOCR frame, b24 for date 'YYYYDDD' and u4 for time 'HHMMSSmmm'.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = self._io.read_bytes(3200)
        self.packets = []
        i = 0
        while not self._io.is_eof():
            self.packets.append(HocrSatview.HocrPacket(self._io, self, self._root))
            i += 1


    class HocrPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.instrument = (self._io.read_bytes(6)).decode(u"ASCII")
            self.sn = (self._io.read_bytes(4)).decode(u"ASCII")
            self.inttime = self._io.read_u2be()
            self.sampledelay = self._io.read_s2be()
            self.channel = []
            for i in range(180):
                self.channel.append(self._io.read_u2be())

            self.darksample = self._io.read_u1()
            self.darkaverage = self._io.read_u2be()
            self.spectemp = self._io.read_bytes(6)
            self.frame = self._io.read_u1()
            self.timer = self._io.read_bytes(10)
            self.checksum = self._io.read_u1()
            self.crlf = self._io.read_u2be()
            self.date = self._io.read_bits_int_be(24)
            self._io.align_to_byte()
            self.time = self._io.read_u4be()



