test_that("Observations results after Cleaning Process", {

  df_clean <- eq_clean_data(earthquakes_raw)

  expect_that(ncol(df_clean),             equals(45))
  expect_that(class(df_clean$EQ_PRIMARY), equals("numeric"))
  expect_that(class(df_clean$DEATHS),     equals("numeric"))
  expect_that(class(df_clean$LONGITUDE),  equals("numeric"))
  expect_that(class(df_clean$LATITUDE),   equals("numeric"))

})
