test_that("Geom timeline creates a ggplot object", {

       df <- earthquakes_raw %>%
               eq_clean_data() %>%
               eq_location_clean() %>%
               filter(lubridate::years(DATE) > 2010,
                      COUNTRY %in% c('USA','JAPAN'))

       # Checking the type of object. Should be ggplot.
       expect_is(df %>%
                   ggplot() + geom_timeline(aes(x = DATE, y = COUNTRY,
                                                size  = EQ_PRIMARY,
                                                color = DEATHS)), "ggplot")

})
