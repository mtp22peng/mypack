#' This is a function that reads in a data file (using the \code{filename} #'argument).
#'
#' The function "read_csv" is imported from external package "readr"
#' The function "tbl_df" is imported from external package "dplyr"
#'
#' @param filename A character string giving the name of a csv file. When #'the function can not find the file, it stops and prints an error message #'"file \code{filename} does not exist".
#'
#'
#' @return This function returns a data frame
#'
#' @examples
#' fars_read("data.csv")
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




#' This is a function that prints a file name containing a specific year
#' number.(using the \code{year} argument).
#'
#' @param year A number giving the year
#'
#' @return This function prints and returns a string file name
#'
#' @examples
#' make_filename(2017)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}




#' This is a function that selects files containing a specified list of #'years, and in each data frame representing data of each selected file #'creats a new year columne, and finally select this new conlumn and the #'MONTH column.(using the \code{years} argument).
#' The function "mutate" and "select" is imported from external package #'"dplyr"
#'
#' @param years A list containing years to select. This function calls #'"fars_read" function to read files corresponding to the given years. For #'a given year, the file may not exist, and then will prints an error #'message "invalid year: ", \code{year} and return a NULL object for this #'year.
#'
#' @return This function returns a data frame containing the year column #'and the month column. As a side effect, this function also prints out
#'    these two columns.
#'
#' @examples
#'fars_read_years(list(2014, 2013, 2011))
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



#' This is a function that selects files containing a specified list of #'years, and then select specific columns containing year and month, and #'row-combines these columns from each file before summarizing the result #'by counting the same year and month pair and spread the year and count #'into multiple columns(using the \code{years} argument).
#'
#' The function "bind_rows", "group_by" and "summarizeis" are imported from #'external package "dplyr".
#' The function "spread" is imported from external package "tidyr".
#' the piping operator "%>%" requires external package "magrittr".
#'
#' @param years A list containing years to select
#'
#' @return This function returns a summary with year and counts of each #'month appearances
#'
#' @examples
#' fars_summarize_years(list(2014, 2013, 2011))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' This is a function that plots the LONGITUD and LATITUDE of specified #'state on a map, whcih has to appear in the data frame specified by a #'ceetain year.(using the \code{state.num,year} argument).
#'
#'
#' The function "filter" is imported from external package "dplyr".
#' The function "map" is imported from external package "maps".
#'
#'
#' @param state.num A number giving the id of a state. This function
#' reads in \code{state.num} and \code{year}. In the file read accordin
#' to the \code{year}, there may not be matched \code{state.num}, and
#' this case the function stops and prints "invalid STATE number: ",
#' \code{state.num}). The filtered data according to \code{state.num}
#' and \code{year} may not exist, and in this case it prints a warning
#' message "no accidents to plot" and return a NULL object.
#'
#' @param year A number giving the year
#'
#' @return This function plots the LONGITUD and LATITUDE of the selected
#' states on a map.
#'
#' @examples
#' fars_map_state(25, 2015)
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
