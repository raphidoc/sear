# This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

import kaitaistruct
from kaitaistruct import KaitaiStruct, KaitaiStream, BytesIO


if getattr(kaitaistruct, 'API_VERSION', (0, 9)) < (0, 9):
    raise Exception("Incompatible Kaitai Struct Python API: 0.9 or later is required, but you have %s" % (kaitaistruct.__version__))

class HocrMte(KaitaiStruct):
    """HOCR binary files contains all the numerical counts for each radiometric
    channel. The calibration file (.cal) for each instruments preciesely define
    the structure of a binary packet send by that instrument and allow the creation
    of this parser. All three HOCR on seadoo a and b have the same binary packet
    structure but posses different central wavelength and corresponding calibration
    coefficient.
    Could improve the structure of this file by separating DataLogger (MTE), HOCR,
    SatView sequence.
    
    DataLogger (MTE) prefix HOCR frame with: u4 for time in millisecond,
    two empty bytes, two or three bytes for date stamp in unkown format,
    two empty bytes and u1 for datalogger port number.
    
    SatView appended 7 bytes to HOCR frame, b24 for date 'YYYYDDD' and u4 for time 'HHMMSSmmm'.
    """
    def __init__(self, _io, _parent=None, _root=None):
        self._io = _io
        self._parent = _parent
        self._root = _root if _root else self
        self._read()

    def _read(self):
        self.header = (self._io.read_bytes(512)).decode(u"UTF-8")
        self.packets = []
        i = 0
        while not self._io.is_eof():
            self.packets.append(HocrMte.HocrPacket(self._io, self, self._root))
            i += 1


    class HocrPacket(KaitaiStruct):
        def __init__(self, _io, _parent=None, _root=None):
            self._io = _io
            self._parent = _parent
            self._root = _root if _root else self
            self._read()

        def _read(self):
            self.time = self._io.read_u4be()
            self.void1 = self._io.read_bytes(2)
            self.mysterydate = self._io.read_bits_int_be(24)
            self._io.align_to_byte()
            self.void2 = self._io.read_bytes(2)
            self.loggerport = self._io.read_u1()
            self.instrument = self._io.read_bytes(6)
            self.sn = self._io.read_bytes(4)
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
            self.fixed_size_void = self._io.read_bytes(103)



