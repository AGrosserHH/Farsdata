#' Read file with FARS data
#'
#' This is a function that reads the data from a given file abd returns the data as a tibble. If the file does not exist it stops
#'
#' @param filename A character string giving the text the function will print
#'
#' @return This function returns a data frame as a tibble
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' data<-fars_read("data.csv")
#' data<-fars_read(filename="data.csv")
#'}
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Make data file name
#'
#' Make .csv data file name related to the given year. There is no check if the file is available.
#'
#' @param year A string or an integer with the input
#'
#' @return This function returns a string with the data file name for a given
#'   year, and the file path within the package.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read Fars years
#'
#' Help function used fars_summarize_years
#'
#' @param years A vector with a list of years
#'
#' @return The function returns a data frame including entries in data by month or NULL if the year is not valid
#'
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarise FARS years
#'
#' This function summarizes yearly accidents data, by month
#'
#' @param years A vector with a list of years to summarize by.
#'
#' @return It returns a data.frame with number of accidents by years summarized by month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr summarize
#' @importFrom dplyr group_by
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013,2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Display accidents map by state and year
#'
#' It displays a plot with a state map which includes the accidents location by year
#' If the state number is incorrect then it shows an error
#'
#' @param state.num An Integer with the State Code
#' @param year A string or an integer with the input year
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#'
#' @return None
#'
#' @examples
#' \dontrun{ fars_map_state(45, 2014)}
#'
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
