test_that("Creates the LOCATION column", {

       data_location <- earthquakes_raw %>%
                                eq_clean_data() %>%
                                eq_location_clean()

       expect_that(class(data_location$LOCATION_NAME), equals("character") )
})
