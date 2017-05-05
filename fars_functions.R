#' Read in Fatality Analysis Reporting System data file
#' 
#' Reads in data from the US National Highway Traffic 
#' Safety Administration's Fatality Analysis Reporting System
#' 
#' @param filename takes a csv file containing the data
#' 
#' @return  Searching for the file. If the file exists, it will 
#' be imported and returned as a data frame tbl.  If it does not exist
#' an error message will be returned.
#' 
#' @examples 
#' fars_read("accident_2015.csv.bz2")
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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


#' Make File Name
#' 
#' Creates a specific filename based on the year provided.
#'  
#' @param year the year to concatenate to the file name
#' 
#' @return return a file name based on the year given as input
#' As an example. If 2016 is provided, the filename "accident_2016.csv.bz2"
#' will be created 
#'    
#' @examples 
#' make_filename(2016)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in Fatality Analysis Reporting System data files
#' 
#' Read in multiple reporting data#' data files based on 
#' the a range of years provided. It creates the filename,
#' read in the information and process each years
#' 
#' @param years The years relating to the file names to be ingested
#' 
#' @return For each existing year, months and year data will be returned
#' 
#' @seealso 
#' \code{\link{make_filename}} filename creation based on year
#' \code{\link{fars_read}} read file function
#' 
#' @examples 
#' fars_read_years(2013:2015)
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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


#' Summarize Observations by Year
#' 
#' Consolidate and summarize multiple data files based on the years 
#' provided.
#' 
#' @param years The years relating to the file names to be ingested
#' 
#' @return return a spreaded data frame including number in accident
#' per month for each year provided as input. 
#' 
#' @seealso 
#' \code{\link{fars_read_years}} for file creation/ingestion
#' 
#' @examples 
#' fars_summarize_years(2013:2015)
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' 
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plotting of State Accidents on a map
#' 
#' Produce a graphical plot of the accidents on a map for a given state
#' and year.
#' 
#' @param state.num the intended State number
#' @param years the intended year
#' 
#' @return return a plot of accidents for the given
#'   state and year. If no accidents or the state number doesn't exist
#'   an error message is procuded
#' 
#' @seealso 
#' \code{\link{make_filename}} filename creation based on year
#' \code{\link{fars_read}} read file function
#' 
#' @examples 
#' fars_map_state(5, 2015)
#' 
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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