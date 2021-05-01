test_that("Mapping works", {

       df_clean <- earthquakes_raw %>%
                        eq_clean_data() %>%
                        eq_location_clean() %>%
                        drop_na(LATITUDE, LONGITUDE) %>%
                        mutate(popup_text = eq_create_label(.)) %>%
                        eq_map(annot_col = 'popup_text')


       expect_that(class(df_clean)[1], equals("leaflet"))
})
