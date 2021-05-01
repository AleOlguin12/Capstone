test_that("Geom timeline label creates a ggplot object", {

       df <- earthquakes_raw %>%
               eq_clean_data() %>%
               eq_location_clean() %>%
               filter(lubridate::years(DATE) > 2010,
                      COUNTRY %in% c('USA','JAPAN'))

       # Checking the type of object. Should be ggplot.
       expect_is(df %>%
                   ggplot() +
                         geom_timeline(ggplot2::aes(x     = DATE,
                                                    y     = COUNTRY,
                                                    size  = EQ_PRIMARY,
                                                    color = DEATHS)) +
                         geom_timeline_label(ggplot2::aes(x     = DATE,
                                                          label = LOCATION_NAME,
                                                          y     = COUNTRY,
                                                          mag   = EQ_PRIMARY,
                                                          n_max = 5)), "ggplot")
})
