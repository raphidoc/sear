meta:
  id: hocr_mte
  title: HOCR datalogger binary parser
  application: parse .bin file produced by the datalogger containing binary data from the seadoo HOCR
  file-extension: bin
  encoding: ASCII
  endian: be
  bit-endian: be

doc:  |
  HOCR binary files contains all the numerical counts for each radiometric
  channel. The calibration file (.cal) for each instruments preciesely define
  the structure of a binary packet send by that instrument and allow the creation
  of this parser. All three HOCR on seadoo A and B have the same binary packet
  structure but posses different central wavelength and corresponding calibration
  coefficient.
  Could improve the structure of this file by separating DataLogger (MTE), HOCR,
  SatView sequence.

  DataLogger (MTE) prefix HOCR frame with: u4 for time in millisecond,
  two empty bytes, two or three bytes for date stamp in unkown format,
  two empty bytes and u1 for datalogger port number.

  SatView appended 7 bytes to HOCR frame, b24 for date 'YYYYDDD' and u4 for time 'HHMMSSmmm'.

seq:
  - id: header
    size: 512
    type: str
    encoding: UTF-8

  - id: packets
    type: hocr_packet
    repeat: eos

types:
  hocr_packet:
    seq:
      - id: time
        type: u4
        doc: |
          timestamp in millisecond, synchro with the applanix

      - id: void1
        size: 2

      - id: mysterydate
        type: b24
        doc: |
          probably a date

      - id: void2
        size: 2

      - id: loggerport
        type: u1
        doc: |
          datalogger hocr port number

      - id: instrument
        size: 6
        #type: str

      - id: sn
        size: 4
        #type: str

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

      - id: fixed_size_void # Datalogger store HOCR frame in a fixed size memory
        size: 103


      # for SatView format
      #- id: date
      #  type: b24be
        # 3 byte unsigned integer big endian

      #- id: time
      #  type: u4
