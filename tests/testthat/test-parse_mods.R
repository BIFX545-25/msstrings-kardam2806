test_that("parse_mods works", {
  data(MSfrags)

# Check
  test1 <- parse_mods(MSfrags)

  expect_s3_class(test1, "protein_mods")
  expect_equal(test1$mods[[2]],"Oxidation")
})
