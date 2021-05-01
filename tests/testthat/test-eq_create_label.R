test_that("Creating the Label to Popup", {

       labels_popup <- earthquakes_raw %>% eq_clean_data() %>% eq_create_label()

       expect_that(class(labels_popup), equals("character") )
})
