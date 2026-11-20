scratch_items <- function() {
  data.frame(
    id_item = 1:3,
    type_item = c("coding", "coding", "content"),
    stimulus_text = c(
      "```\nload(url(\"https://pandar.netlify.app/daten/df_tiger.rda\"))\nx <- carData::Guyer\n```",
      "```\nlibrary(car)\nload(url(\"https://pandar.netlify.app/daten/alc.rda\"))\nstats::sd(1:3)\n```",
      # content items are ignored
      "load(url(\"https://pandar.netlify.app/daten/other.rda\")) foo::bar"
    ),
    answeroption_01 = c(NA, "load(url(\"https://pandar.netlify.app/daten/df_tiger.rda\"))", NA),
    stringsAsFactors = FALSE
  )
}

test_that("pandar_data_urls() finds each dataset of the coding items once", {
  expect_equal(
    pandar_data_urls(scratch_items()),
    c(
      "https://pandar.netlify.app/daten/alc.rda",
      "https://pandar.netlify.app/daten/df_tiger.rda"
    )
  )
  expect_equal(pandar_data_urls(scratch_items()[3, ]), character())
})

test_that("scratchpad_packages() lists pkg:: packages, not base ones", {
  expect_equal(scratchpad_packages(scratch_items()), "carData")
})

test_that("scratchpad_fetch_data() encodes cached files and reports failures", {
  dir <- withr::local_tempdir()
  writeBin(as.raw(1:5), file.path(dir, "cached.rda"))
  res <- scratchpad_fetch_data(
    c("https://pandar.netlify.app/daten/cached.rda", "http://127.0.0.1:9/missing.rda"),
    cache_dir = dir
  )
  expect_equal(length(res$files), 1L)
  expect_equal(res$files[[1]]$name, "cached.rda")
  expect_equal(jsonlite::base64_dec(res$files[[1]]$b64), as.raw(1:5))
  expect_equal(res$failed, "missing.rda")
  expect_false(file.exists(file.path(dir, "missing.rda")))
})
