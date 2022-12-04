#ifndef HOCR_H_
#define HOCR_H_

// This is a generated file! Please edit source .ksy file and use kaitai-struct-compiler to rebuild

#include "kaitai/kaitaistruct.h"
#include <stdint.h>
#include <vector>

#if KAITAI_STRUCT_VERSION < 9000L
#error "Incompatible Kaitai Struct C++/STL API: version 0.9 or later is required"
#endif

/**
 * HOCR binary files contains all the numerical counts for each radiometric
 * channel. The calibration file (.cal) for each instruments preciesely define
 * the structure of a binary packet send by that instrument and allow the creation
 * of this parser. All three HOCR on seadoo A and B have the same binary packet
 * structure but posses different central wavelength and corresponding calibration
 * coefficient.
 * Could improve the structure of this file by separating DataLogger (MTE), HOCR,
 * SatView sequence.
 * 
 * DataLogger (MTE) prefix HOCR frame with: u4 for time in millisecond,
 * two empty bytes, two or three bytes for date stamp in unkown format,
 * two empty bytes and u1 for datalogger port number.
 * 
 * SatView appended 7 bytes to HOCR frame, b24 for date 'YYYYDDD' and u4 for time 'HHMMSSmmm'.
 */

class hocr_t : public kaitai::kstruct {

public:
    class hocr_packet_t;

    hocr_t(kaitai::kstream* p__io, kaitai::kstruct* p__parent = 0, hocr_t* p__root = 0);

private:
    void _read();
    void _clean_up();

public:
    ~hocr_t();

    class hocr_packet_t : public kaitai::kstruct {

    public:

        hocr_packet_t(kaitai::kstream* p__io, hocr_t* p__parent = 0, hocr_t* p__root = 0);

    private:
        void _read();
        void _clean_up();

    public:
        ~hocr_packet_t();

    private:
        uint32_t m_gpstime;
        std::string m_void1;
        uint64_t m_mysterydate;
        std::string m_void2;
        uint8_t m_loggerport;
        std::string m_instrument;
        std::string m_sn;
        uint16_t m_inttime;
        int16_t m_sampledelay;
        std::vector<uint16_t>* m_channel;
        uint8_t m_darksample;
        uint16_t m_darkaverage;
        std::string m_spectemp;
        uint8_t m_frame;
        std::string m_timer;
        uint8_t m_checksum;
        uint16_t m_crlf;
        std::string m_fixed_size_void;
        hocr_t* m__root;
        hocr_t* m__parent;

    public:

        /**
         * timestamp in millisecond, likely synchro with the applanix
         */
        uint32_t gpstime() const { return m_gpstime; }
        std::string void1() const { return m_void1; }

        /**
         * probably a date
         */
        uint64_t mysterydate() const { return m_mysterydate; }
        std::string void2() const { return m_void2; }

        /**
         * datalogger hocr port number
         */
        uint8_t loggerport() const { return m_loggerport; }
        std::string instrument() const { return m_instrument; }
        std::string sn() const { return m_sn; }
        uint16_t inttime() const { return m_inttime; }
        int16_t sampledelay() const { return m_sampledelay; }
        std::vector<uint16_t>* channel() const { return m_channel; }
        uint8_t darksample() const { return m_darksample; }
        uint16_t darkaverage() const { return m_darkaverage; }
        std::string spectemp() const { return m_spectemp; }
        uint8_t frame() const { return m_frame; }
        std::string timer() const { return m_timer; }
        uint8_t checksum() const { return m_checksum; }
        uint16_t crlf() const { return m_crlf; }
        std::string fixed_size_void() const { return m_fixed_size_void; }
        hocr_t* _root() const { return m__root; }
        hocr_t* _parent() const { return m__parent; }
    };

private:
    std::string m_header;
    std::vector<hocr_packet_t*>* m_packets;
    hocr_t* m__root;
    kaitai::kstruct* m__parent;

public:
    std::string header() const { return m_header; }
    std::vector<hocr_packet_t*>* packets() const { return m_packets; }
    hocr_t* _root() const { return m__root; }
    kaitai::kstruct* _parent() const { return m__parent; }
};

#endif  // HOCR_H_
