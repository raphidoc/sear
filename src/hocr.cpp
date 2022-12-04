#include "hocr.h"
#include <Rcpp.h>
using namespace Rcpp;


hocr_t::hocr_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent, hocr_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = this;
    m_packets = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void hocr_t::_read() {
    m_header = kaitai::kstream::bytes_to_str(m__io->read_bytes(512), std::string("UTF-8"));
    m_packets = new std::vector<hocr_packet_t*>();
    {
        int i = 0;
        while (!m__io->is_eof()) {
            m_packets->push_back(new hocr_packet_t(m__io, this, m__root));
            i++;
        }
    }
}

hocr_t::~hocr_t() {
    _clean_up();
}

void hocr_t::_clean_up() {
    if (m_packets) {
        for (std::vector<hocr_packet_t*>::iterator it = m_packets->begin(); it != m_packets->end(); ++it) {
            delete *it;
        }
        delete m_packets; m_packets = 0;
    }
}

hocr_t::hocr_packet_t::hocr_packet_t(kaitai::kstream* p__io, hocr_t* p__parent, hocr_t* p__root) : kaitai::kstruct(p__io) {
    m__parent = p__parent;
    m__root = p__root;
    m_channel = 0;

    try {
        _read();
    } catch(...) {
        _clean_up();
        throw;
    }
}

void hocr_t::hocr_packet_t::_read() {
    m_gpstime = m__io->read_u4be();
    m_void1 = m__io->read_bytes(2);
    m_mysterydate = m__io->read_bits_int_be(24);
    m__io->align_to_byte();
    m_void2 = m__io->read_bytes(2);
    m_loggerport = m__io->read_u1();
    m_instrument = m__io->read_bytes(6);
    m_sn = m__io->read_bytes(4);
    m_inttime = m__io->read_u2be();
    m_sampledelay = m__io->read_s2be();
    m_channel = new std::vector<uint16_t>();
    const int l_channel = 180;
    for (int i = 0; i < l_channel; i++) {
        m_channel->push_back(m__io->read_u2be());
    }
    m_darksample = m__io->read_u1();
    m_darkaverage = m__io->read_u2be();
    m_spectemp = m__io->read_bytes(6);
    m_frame = m__io->read_u1();
    m_timer = m__io->read_bytes(10);
    m_checksum = m__io->read_u1();
    m_crlf = m__io->read_u2be();
    m_fixed_size_void = m__io->read_bytes(103);
}

hocr_t::hocr_packet_t::~hocr_packet_t() {
    _clean_up();
}

void hocr_t::hocr_packet_t::_clean_up() {
    if (m_channel) {
        delete m_channel; m_channel = 0;
    }
}
