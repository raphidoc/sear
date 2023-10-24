meta:
  id: hocr_satview
  title: SatView HOCR binary parser
  application: parse .raw file produced by SatView
  file-extension: raw
  encoding: ASCII
  endian: be
  bit-endian: be

doc:  |
  HOCR binary files contains all the numerical counts for each radiometric
  channel. The calibration file (.cal) for each instruments preciesely define
  the structure of a binary packet send by that instrument.

  SatView append 7 bytes to HOCR frame, b24 for date 'YYYYDDD' and u4 for time 'HHMMSSmmm'.

seq:
  - id: header
    size: 3200
    #type: str
    #encoding: UTF-8

  - id: packets
    type: hocr_packet
    repeat: eos

types:
  hocr_packet:
    seq:

      - id: instrument
        size: 6
        type: str

      - id: sn
        size: 4
        type: str

      - id: inttime
        type: u2

      - id: sampledelay
        type: s2

      - id: channel
        type: u2
        repeat: expr
        repeat-expr: 180

      - id: darksample
        type: u1

      - id: darkaverage
        type: u2

      - id: spectemp
        size: 6
        #type: str

      - id: frame
        type: u1

      - id: timer
        size: 10
        #type: str

      - id: checksum
        type: u1

      - id: crlf
        type: u2

      # SatView format
      - id: date
        type: b24be
        # 3 byte unsigned integer big endian

      - id: time
        type: u4
