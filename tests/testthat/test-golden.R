# The generated artefacts committed at the repo root ARE the golden fixtures.
#
# The plan called for pinning copies of them under tests/, but that would commit a
# second ~10 MB of the same bytes and leave two things to keep in step. Instead
# the build writes to a temp directory and we compare against the committed
# copies, which git already versions and `make release` already archives.
#
# So: change anything in R/, and if these pass the refactor was
# behaviour-preserving. To change the output deliberately, rebuild at the root,
# review `git diff`, and commit the artefacts on their own -- the discipline used
# for the min/max renormalisation in acf2c55.

test_that("the build reproduces every committed artefact byte for byte", {
  built <- apd_test_build()

  for (file in APD_OUTPUTS) {
    committed <- file.path(APD_ROOT, file)
    skip_if_not(file.exists(committed),
                paste(file, "is not committed; nothing to compare against"))

    expect_identical(apd_bytes(file.path(built, file)), apd_bytes(committed),
                     info = paste(file, "differs from the committed copy"))
  }
})

test_that("the build is deterministic", {
  # Two builds of the same inputs must agree, so that nothing order-dependent can
  # creep in unnoticed -- an unsorted join, a hashed set, an embedded timestamp.
  first <- apd_test_build()
  second <- withr::local_tempdir()
  suppressMessages(apd_build_data(data_dir = APD_DATA, out_dir = second))

  for (file in APD_OUTPUTS) {
    expect_identical(apd_bytes(file.path(second, file)),
                     apd_bytes(file.path(first, file)),
                     info = paste(file, "differs between two builds"))
  }
})

test_that("the build does not write to data/", {
  # The build used to reach the trait table only through
  # convert_APD_traits_input_yml_to_csv(), which wrote data/APD_traits_input.csv
  # as a side effect, so every build dirtied its own tracked input.
  fingerprint <- function() {
    files <- sort(list.files(APD_DATA, full.names = TRUE))
    vapply(files, function(f) paste(file.size(f), file.mtime(f)), character(1))
  }

  before <- fingerprint()
  suppressMessages(apd_build_data(data_dir = APD_DATA,
                                  out_dir = withr::local_tempdir()))

  expect_identical(fingerprint(), before)
})
