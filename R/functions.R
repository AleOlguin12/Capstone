#' Clean Data
#'
#' This function cleans the dataframe, creates a DATE column out of the year,
#' month and day transform the columns LATITUDE and LONGITUD to numeric class
#'
#' @param df A dataframe with earthquake data to be cleaned
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @importFrom lubridate years
#' @importFrom magrittr "%>%"
#'
#' @return The resulting cleaned dataframe
#'
#' @examples
#' \dontrun{eq_clean_data(earthquakes_raw)}
#'
#' @export

eq_clean_data <- function(df){
  df %>%
    dplyr::mutate(YEAR_AUX = .data$YEAR,
           YEAR  = dplyr::if_else(.data$YEAR < 0, 0, .data$YEAR),
           MONTH = tidyr::replace_na(.data$MONTH, 1),
           DAY   = tidyr::replace_na(.data$DAY, 1)) %>%
    tidyr::unite("DATE", .data$YEAR, .data$MONTH, .data$DAY, sep = "-") %>%
    dplyr::mutate(DATE = as.Date(.data$DATE)) %>%
    dplyr::mutate(DATE = dplyr::if_else(.data$DATE == as.Date("0000-01-01"),
                                        as.Date(.data$DATE) + lubridate::years(.data$YEAR_AUX), .data$DATE),
                  LATITUDE  = as.numeric(.data$LATITUDE),
                  LONGITUDE = as.numeric(.data$LONGITUDE)) %>%
    dplyr::select(-.data$YEAR_AUX)
}

#' Clean Location
#'
#' This function cleans the LOCATION column
#'
#' @inheritParams eq_clean_data
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import stringr
#' @importFrom magrittr "%>%"
#'
#' @return The resulting cleaned dataframe
#'
#' @examples
#' \dontrun{eq_clean_data(earthquakes_raw) %>% eq_location_clean()}
#'
#' @export

eq_location_clean <- function(df){
  suppressWarnings(
    df %>%
      tidyr::separate(.data$LOCATION_NAME,
                      into = c("COUNTRY_NAME", "CITY"),
                      sep = "(?<=: )", extra = "merge") %>%
      dplyr::mutate(LOCATION_NAME = stringr::str_to_title(.data$CITY),
                    LOCATION_NAME = stringr::str_squish(.data$LOCATION_NAME)) %>%
      dplyr::select(-.data$COUNTRY_NAME)
  )
}

#' Geom Timeline Proto
#'
#' Hidden function used to build the Geom Timeline
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @import grid
#' @importFrom magrittr "%>%"
#' @importFrom plyr defaults
#'
#' @return Visual Structure of the Geom Timeline
#'
#' @keywords internal

GeomTimeline <- ggplot2::ggproto(`_class` = "GeomTimeline",
                                 `_inherit`      = ggplot2::GeomPoint,
                                 required_aes    = "x",
                                 default_aes     = plyr::defaults(ggplot2::aes(y     = 0.5,
                                                                               size  = 2,
                                                                               alpha = 0.7),
                                                                  ggplot2::GeomPoint$default_aes),
                                 draw_panel = function(data, panel_params, coord){

                                   dates_grob <- ggplot2::GeomPoint$draw_panel(data, panel_params, coord)
                                   coords     <- coord$transform(data, panel_params)
                                   timeline_grob <- grid::polylineGrob(coords$x, coords$y,
                                                                       id = coords$group,
                                                                       gp = grid::gpar(col = gray(0.5)))


                                          grid::grobTree(dates_grob, timeline_grob)

                                 }
)


#' Geom Timeline
#'
#' This geom produce a timeline with the corresponding earthquake for a given country
#'
#' @inheritParams ggplot2::geom_point
#'
#' @import tibble
#' @import ggplot2
#' @importFrom magrittr "%>%"
#'
#' @return Timeline with the earthquakes sorted by country
#'
#' @examples
#'
#' \dontrun{earthquake_raw %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' ggplot() %>%
#' geom_timeline(aes(x = DATE, y = COUNTRY, size  = EQ_PRIMARY, color = DEATHS))}
#'
#' @export

geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          na.rm = TRUE,
                          position = "identity",
                          stat = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}

#' Geom Timeline Label Proto
#'
#' Hidden function used to build the Geom Timeline Label
#'
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @import ggplot2
#' @import grid
#' @importFrom magrittr "%>%"
#'
#' @return Visual Structure of the Geom Timeline Label
#'
#' @keywords internal

GeomTimelineLabel <- ggplot2::ggproto(`_class`     = "GeomTimelineLabel",
                                      `_inherit`   = ggplot2::Geom,
                                      required_aes = "x",
                                      optional_aes =  c('label', 'y', 'mag',
                                                        'color', 'alpha', 'n_max'),
                                      default_aes  = ggplot2::aes(shape = 21, colour = "black",
                                                                  size = 7.0, n_max = 5,
                                                                  fill = 'black',alpha = 0.4,
                                                                  stroke = 2),
                                      draw_key     = ggplot2::draw_key_point,
                                      draw_panel   = function(data, panel_params, coord) {

                                        data <- data %>% dplyr::arrange(dplyr::desc(.data$mag))

                                        data <- utils::head(data, unique(data$n_max))

                                        coords <- coord$transform(data, panel_params)

                                        lines <- grid::segmentsGrob(x0 = coords$x,
                                                                    y0 = coords$y,
                                                                    x1 = coords$x,
                                                                    y1 = coords$y + 0.05,
                                                                    default.units = "npc",
                                                                    gp = grid::gpar(col =      coords$colour,
                                                                                    alpha =    coords$alpha,
                                                                                    fontsize = coords$size,
                                                                                    lwd =      coords$stroke))


                                        texts <- grid::textGrob(label = coords$label,
                                                                x = coords$x,
                                                                y = coords$y + 0.06,
                                                                just = "left",
                                                                rot = 45,
                                                                check.overlap = TRUE,
                                                                default.units = "npc",
                                                                gp = grid::gpar(col =      coords$colour,
                                                                                fontsize = coords$size,
                                                                                lwd =      coords$stroke))

                                        grid::gTree(children = grid::gList(lines, texts))
                                      })


#' Geom Timeline Label
#'
#' This geom adds the location label to an existing timeline
#'
#' @inheritParams ggplot2::geom_point
#'
#' @import tibble
#' @import ggplot2
#'
#' @return Labels for the earthquakes' timelines
#'
#' @examples
#' \dontrun{earthquakes_raw %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' ggplot() %>%
#' geom_timeline(aes(x = DATE, y = COUNTRY, size  = EQ_PRIMARY, color = DEATHS)) +
#' geom_timeline_label(aes(x = DATE, label = LOCATION_NAME, y = COUNTRY, mag = EQ_PRIMARY, n_max = 5))}
#'
#' @export

geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                inherit.aes = TRUE, ...){

  ggplot2::layer(geom = GeomTimelineLabel,
                 mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm,...)
  )
}

#' Map with Earthquakes
#'
#' This function plots a World Map indicating each earthquake with a geom point
#'
#' @param df_map A dataframe with earthquake data to be plotted
#' @param annot_col The label of the corresponding information for each earthquake
#'
#' @import dplyr
#' @import tibble
#' @import leaflet
#' @importFrom magrittr "%>%"
#'
#' @return The resulting map to be plotted
#'
#' @examples
#' \dontrun{earthquakes_raw %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map()}
#'
#' @export

eq_map <- function(df_map = rlang::.data, annot_col = NULL) {

  leaflet::leaflet(df_map) %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(lng = ~LONGITUDE,
                        lat = ~LATITUDE,
                        weight = 1,
                        radius = ~EQ_PRIMARY * 10000,
                        popup = ~eval(parse(text = annot_col)) )

}

#' Create Label for Map
#'
#' This function creates the label to be added to the map plot
#'
#' @param df_create_label A dataframe with earthquake data to create the label
#'
#' @return A column with the structure to be added as a label
#'
#' @examples
#' \dontrun{earthquakes_raw %>%
#' eq_clean_data() %>%
#' eq_location_clean() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = 'popup_text')}
#'
#' @export

eq_create_label <- function(df_create_label = rlang::.data){
  paste("<b>Location: </b>",     df_create_label$LOCATION_NAME, "<br>",
        "<b>Magnitude: </b>",    df_create_label$EQ_PRIMARY,    "<br>",
        "<b>Total deaths: </b>", df_create_label$DEATHS,        sep = "")
}
