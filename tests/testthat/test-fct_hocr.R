test_that("read hocr binary from MTE", {
  skip_if_no_kaitaistruct()

  expect_type(read_hocr(app_sys("extdata", "DATA_20211111_171729.bin")), "list")
})
