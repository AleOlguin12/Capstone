test_that("Checking the GeomTimeline", {

       expect_is(GeomTimeline, "Geom")
       expect_is(GeomTimeline, "ggproto")
       expect_is(GeomTimeline, "gg")
})
