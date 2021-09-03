

#' Remove slash junk
#'
#' Remove slash junk
#'
#' Remove slash junk
#'
#' @param x the character vector
#' @return clean character vector
#' @examples
#' sophisthse:::remove_slash_junk('xxx \n yyy')
remove_slash_junk <- function(x) {
  x <- gsub("\n", " ", x)
  x <- gsub("\r", " ", x)
  x <- gsub("  ", " ", x)
  return(x)
}

#' Get metadata from downloaded time series
#'
#' Get metadata from downloaded time series
#'
#' Get metadata from downloaded time series
#'
#' @param df downloaded multivariate time series or data.frame
#' @return data.frame with information
#' @export
#' @examples
#' df <- sophisthse0('WAG_Y')
#' sophisthse_metadata(df)
sophisthse_metadata <- function(df) {
  info <- attr(df, "metadata")
  return(info)
}


#' Replace cyrillic letters by corresponding latin letters
#'
#' Replace cyrillic letters by corresponding latin letters
#'
#' Replace cyrillic letters by corresponding latin letters
#'
#' @param x the character vector
#' @return clean character vector
#' @examples
#' sophisthse:::decyrillic(intToUtf8(1057))
decyrillic <- function(x) {
  for (i in 1:length(x)) {
    codes <- utf8ToInt(x[i])
    codes[codes == 1052] <- 77 # M
    codes[codes == 1045] <- 69 # E
    codes[codes == 1040] <- 65 # A
    codes[codes == 1057] <- 67 # C
    codes[codes == 1061] <- 88 # X
    codes[codes == 1054] <- 79 # O
    x[i] <- intToUtf8(codes)
  }
  return(x)
}



#' Convert string with a number in Russian tradition in numeric
#'
#' This function is useful for automatic conversion of strings to numeric
#'
#' Russian standards prescribes to use comma as a decimal separator.
#' This function removes spaces and converts string to number.
#'
#' @param x the string with the number
#' @return numeric the number converted from the string
#' @export
#' @examples
#' rus2num('34 345,34')
rus2num <- function(x) {
  x <- gsub(",", ".", x)

  # remove all but 0-9, . and minus:
  x <- gsub("[^0-9.-]+", "", x)

  # remove lonely minus or lonely dot
  x <- gsub("^-$", "", x)
  x <- gsub("^\\.$", "", x)

  return(as.numeric(x))
}

#' Get table name from time series name
#'
#' Get table name from time series name
#'
#' On sophist.hse.ru time series are stored in tables. The package download whole tables
#' and not individual time series. When user requests time series we need to know
#' corresponding table name.
#'
#' @param ts_names character vector of time series or table names
#' @return character vector of corresponding table names
#' @export
#' @examples
#' series2tables("M2_Y")
series2tables <- function(ts_names) {
  table_names <- ts_names[ts_names %in% sophisthse::series_info$table]
  ts_table_correspondance <- dplyr::tibble(tsname = setdiff(ts_names, table_names))
  ts_table_correspondance <- dplyr::left_join(ts_table_correspondance,
                                              sophisthse::series_info[, c("table", "tsname")],
                                              by = "tsname")
  table_names <- c(table_names, ts_table_correspondance$table)

  return(table_names)
}


#' Obtain additional information for specific time series
#'
#' Obtain additional information for specific time series from sophist.hse.ru
#'
#' Internal function. Obtain additional information for specific time series
#' from sophist.hse.ru. Either 'methodology', 'source' or 'comment'.
#'
#' @param series.name the names of the time series
#' @param n.vars number of variables
#' @param info type of information (methodology/source/comment)
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return character vector with info for each variable
#' @examples
#' info <- sophisthse:::get_stat_hse_info_vector('IP_EA_Q', 1, 'methodology')
get_stat_hse_info_vector <- function(series.name = "IP_EA_Q",
                                     n.vars = 1, info = c("methodology", "source", "comment"),
                                     ...) {

  info <- match.arg(info)

  if (info == "methodology") {
    url <- paste("http://sophist.hse.ru/hse/1/met/", series.name,
                 ".html", sep = "")
  }
  if (info == "source") {
    url <- paste("http://sophist.hse.ru/hse/1/sor/", series.name,
                 ".html", sep = "")
  }
  if (info == "comment") {
    url <- paste("http://sophist.hse.ru/hse/1/com/", series.name,
                 ".html", sep = "")
  }

  url.html <- RCurl::getURL(url, .encoding = "UTF-8", ...)
  url.parsed <- XML::htmlTreeParse(url.html)
  url.root <- XML::xmlRoot(url.parsed)


  # there maybe 2 situations:
  # one entry for each variable
  # one entry for all variables
  # or more than 2 situations ;)

  if (length(XML::xmlChildren(url.root)) > 2) {
    # series CNSTR_Y, CNSTR_Q, CNSTR_M, GOV_M
    # have no methodology

    n.on.site <- length(XML::xmlChildren(url.root[[3]][[3]])) %/% 2  # only approximate
    text <- rep("", n.vars)

    for (i in 1:min(n.on.site, n.vars)) {
      temp.value <- XML::xmlValue(url.root[[3]][[3]][[2 * i]])
      if (length(temp.value) > 0) {
        text[i] <- temp.value  # avoid empty blocks
      }
    }

    text <- remove_slash_junk(text)
    text <- as.character(text)
    Encoding(text) <- "UTF-8"
  } else {
    text <- rep(paste0("Failed to parse ", url), n.vars)
  }

  return(text)
}



#' Obtain time series from sophist.hse.ru
#'
#' This function obtains table of time series from sophist.hse.ru
#'
#' The output is tibble.
#'
#' @param series.name the names of the time series, i.e. 'WAG_Y'
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return data.frame with the corresponding time series
#' @export
#' @examples
#' df <- sophisthse0('IP_EA_Q')
#' df <- sophisthse0('WAG_Y')
sophisthse0 <- function(series.name = "IP_EA_Q", ...) {

  # download main data
  url <- paste("http://sophist.hse.ru/hse/1/tables/", series.name,
               ".htm", sep = "")
  url.html <- RCurl::getURL(url, .encoding = "UTF-8", ...)

  # get main table
  tables <- XML::readHTMLTable(url.html)
  df <- tibble::as_tibble(tables[[1]])



  # replace cyrillic letters by corresponding latin
  colnames(df) <- decyrillic(colnames(df))


  # html parse
  url.parsed <- XML::htmlTreeParse(url.html)
  url.root <- XML::xmlRoot(url.parsed)


  # all to character
  df <- dplyr::mutate_all(df, as.character)

  # save units of measure
  # first column is Time
  metadata <- tibble::tibble(tsname = colnames(df)[-1])
  metadata$unit <- gsub("&nbsp", "", df[1, 2:ncol(df)])
  Encoding(metadata$unit) <- "UTF-8"

  n.vars <- ncol(df) - 1  # remove 'T', the name of index


  # get full variable names
  full.names <- rep("", n.vars)
  for (i in 1:n.vars) {
    full.names[i] <- XML::xmlValue(url.root[[3]][[1]][[1]][[i]])
  }
  metadata$fullname <- remove_slash_junk(full.names)
  Encoding(metadata$fullname) <- "UTF-8"


  # remove unused lines (units, info about series)
  df <- df[2:(nrow(df) - 4), ]

  # remove spaces, replace ',' by '.', convert to numeric
  df = dplyr::mutate_at(df, -T, rus2num)

  # pretty time index

  # determine the type of data: yearly/quarterly/mothly
  t.type <- 1  # by default we assume early data
  if (length(grep("[IV]", df$T)) > 1) {
    t.type <- 4  # quarterly data
  }
  if (length(grep("^[23456789]$", df$T)) > 1) {
    t.type <- 12  # monthly data
  }

  # convert data to correct format
  if (t.type == 1) {
    df$T <- as.numeric(df$T)
    start.date <- df$T[1]
  }
  if (t.type == 4) {
    # we assume that the first observation has the year
    df$T <- gsub(" IV$", "-4", df$T)
    df$T <- gsub(" III$", "-3", df$T)
    df$T <- gsub(" II$", "-2", df$T)
    df$T <- gsub(" I$", "-1", df$T)

    start.date <- zoo::as.yearqtr(df$T[1])
    df$T <- start.date + seq(from = 0, by = 1/4, length = nrow(df))
  }
  if (t.type == 12) {
    # we assume that the first observation has the year
    start.date <- zoo::as.yearmon(df$T[1], format = "%Y %m")
    df$T <- start.date + seq(from = 0, by = 1/12, length = nrow(df))
  }



  # get methodology, comment and source

  metadata$methodology <- get_stat_hse_info_vector(series.name,
                                                         n.vars, "methodology", ...)
  metadata$source <- get_stat_hse_info_vector(series.name,
                                                    n.vars, "source", ...)
  metadata$comment <- get_stat_hse_info_vector(series.name,
                                                     n.vars, "comment", ...)

  metadata$freq <- t.type


  attr(df, "metadata") <- metadata

  return(df)
}

#' Obtain multivariate time series from sophist.hse.ru
#'
#' This function obtains multivariate time series from sophist.hse.ru
#'
#' The output may be choosen to be 'ts', 'zoo' or 'data.frame'. Metadata is saved
#' into the attribute 'metadata'.
#'
#' @param series.name the names of the time series, i.e. 'WAG_Y'
#' @param output the desired output format, either 'ts', 'zoo', 'data.frame' or 'tsibble'.
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return data.frame with the corresponding time series
#' @export
#' @examples
#' df <- sophisthse('IP_EA_Q')
#' df <- sophisthse('WAG_Y')
sophisthse <- function(series.name = "IP_EA_Q",
                       output = c("ts", "zoo", "data.frame", "tsibble"), ...) {

  # transform series name to table names (and leave table names)
  series.name <- stats::na.omit(unique(series2tables(series.name)))

  output <- match.arg(output)

  all_data <- NULL
  all_meta <- NULL

  for (sname in series.name) {
    one_data <- sophisthse0(sname, ...)

    if (length(unique(colnames(one_data))) < ncol(one_data)) {
      message("Non unique colnames: ", paste0(colnames(one_data), " "))
      message("Adding numbers to them :)")
      colnames(one_data)[2:ncol(one_data)] <-
        paste0(colnames(one_data)[2:ncol(one_data)], "_", 1:(ncol(one_data) - 1))
    }

    one_meta <- attr(one_data, "metadata")
    all_meta <- dplyr::bind_rows(all_meta, one_meta)

    if (is.null(all_data)) {
      all_data <- one_data
    } else {
      all_data <- merge(all_data, one_data, by = "T", all = TRUE)
    }
  }

  actual_frequency <- unique(all_meta$freq)

  if (length(actual_frequency) > 1) {
    warning("Probably requested series have different frequency: ",
            paste0(actual_frequency, " "))
  }

  all_data <- set_variable_labels(all_data,
                labels = c("Date", all_meta$fullname))

  if (output == "zoo") {
    all_data <- zoo::zoo(dplyr::select(all_data, -T),
                         order.by = all_data$T,
                    frequency = actual_frequency)
  }
  if (output == "ts") {

    start_numeric <- as.numeric(all_data$T[1])
    start_year <- floor(start_numeric)
    start_small_unit <- 1 + round(actual_frequency * (start_numeric - start_year))
    start_ts <- c(start_year, start_small_unit)

    all_data <- stats::ts(dplyr::select(all_data, -T), start = start_ts,
                   frequency = actual_frequency,
                   names = all_meta$tsname)
  }

  if (output == "tsibble") {
    if (actual_frequency == 4) {
      all_data$T = tsibble::yearquarter(all_data$T)
    }
    all_data <- tsibble::as_tsibble(all_data, regular = TRUE, index = T)
  }



  attr(all_data, "metadata") <- all_meta
  return(all_data)
}


#' Construct a vector of all the available tables
#'
#' Construct a vector of all the available tables
#'
#' Construct a vector of all the available tables. For the moment contains an error.
#' BRDATA is a table of tables :) And also some regional data cannot be parsed.
#'
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return vector of all the available tables
#' @export
#' @examples
#' sophisthse_tables()
sophisthse_tables <- function(...) {
  message("Some table names may not work.")
  message("On June 2016: MPP2 and TOPL_C do not work.")

  url <- "http://sophist.hse.ru/hse/nindex.shtml"
  url_chr <- RCurl::getURL(url, ...)
  x <- stringr::str_match_all(url_chr,
              "/tables/([A-Z0-9\\-\\_]+)\\.htm")[[1]][, 2]
  x <- x[!x == "BRDATA"]

  # no need to parse BRDATA page, /exes/tables/BRDATA.htm
  # all time series there have direct links from /hse/nindex.shtm

  # remove regional data
  x <- x[!stringr::str_detect(x, "^R200")]


  return(x)
}

#' Set variable labels of a data.frame
#'
#' Set variable labels of a data.frame
#'
#' Set variable labels of a data.frame. They will be nicely visible
#' with `View()` in Rstudio.
#'
#' @param df data.frame
#' @param labels character vector of variable names
#' @return data.frame with labelled variables
#' @export
#' @examples
#' cars2 <- set_variable_labels(cars,
#'   labels = c("Speed (mph)", "Stopping distance (ft)"))
set_variable_labels <- function(df, labels) {
  if (!length(colnames(df) == length(labels))) {
    warning("The number of columns in df,", length(colnames(df)),
      ", is not equal to the number of labels, ", length(labels), ".")
  }

  for (col_no in 1:length(colnames(df))) {
    attr(df[[col_no]], "label") <- labels[col_no]
  }

  return(df)
}

