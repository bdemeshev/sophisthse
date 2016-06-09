#' sophisthse
#'
#' @name sophisthse
#' @docType package
#' @author Boris Demeshev
NULL

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
  x <- gsub(" ", "", x)
  return(as.numeric(x))
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
  # or more than 2 ;)

  n.on.site <- length(XML::xmlChildren(url.root[[3]][[3]]))%/%2  # only approximate
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

  return(text)
}



#' Guess series frequency from it's name
#'
#' Guess series frequency from it's name for sophist.hse.ru
#'
#' Guess series frequency from it's name
#' by checking presence of letters Y, M, Q
#'
#' @param series.name the name of a time series
#' @return guessed frequency (1/4/12)
#' @examples
#' sophisthse:::requested_freq('WAG_Y')
requested_freq <- function(series.name) {
  req_type <- rep(1, length(series.name))  # we assume yearly ts by default
  req_type[grepl("_Y$|_Y_", series.name)] <- 1
  req_type[grepl("_M$|_M_", series.name)] <- 12
  req_type[grepl("_Q$|_Q_", series.name)] <- 4
  return(req_type)
}





#' Obtain time series from sophist.hse.ru
#'
#' This function obtains univariate time series from sophist.hse.ru
#'
#' The output may be choosen to be 'zoo' or 'data.frame'. Metadata is saved
#' into the attribute 'metadata'.
#'
#' @param series.name the names of the time series, i.e. 'WAG_Y'
#' @param output the desired output format, either 'zoo' or 'data.frame'
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return data.frame with the corresponding time series
#' @export
#' @examples
#' df <- sophisthse0('IP_EA_Q')
#' df <- sophisthse0('WAG_Y')
sophisthse0 <- function(series.name = "IP_EA_Q", output = c("zoo",
                                                            "data.frame"), ...) {

  output <- match.arg(output)

  # download main data
  url <- paste("http://sophist.hse.ru/exes/tables/", series.name,
               ".htm", sep = "")
  url.html <- RCurl::getURL(url, .encoding = "UTF-8", ...)

  # get main table
  tables <- XML::readHTMLTable(url.html)
  df <- tables[[1]]

  # html parse
  url.parsed <- XML::htmlTreeParse(url.html)
  url.root <- XML::xmlRoot(url.parsed)


  # all to character
  for (i in 1:ncol(df)) {
    df[, i] <- as.character(df[, i])
  }

  # save units of measure
  metadata <- dplyr::data_frame(tsname = colnames(df))
  metadata$unit <- gsub("&nbsp", "", df[1, ])
  Encoding(metadata$unit) <- "UTF-8"

  # get full variable names
  full.names <- rep("", ncol(df))
  for (i in 2:ncol(df)) {
    full.names[i] <- XML::xmlValue(url.root[[3]][[1]][[1]][[i - 1]])
  }
  metadata$fullname <- remove_slash_junk(full.names)
  Encoding(metadata$fullname) <- "UTF-8"


  # remove unused lines (units, info about series)
  df <- df[2:(nrow(df) - 4), ]

  # remove spaces, replace ',' by '.', convert to numeric
  for (i in 2:ncol(df)) {
    df[, i] <- rus2num(df[, i])
  }


  # pretty time index

  # determine the type of data: yearly/quarterly/mothly
  t.type <- 1  # by default we assume early data
  if (length(grep("[IV]", df$T)) > 1) {
    t.type <- 4  # quarterly data
  }
  if (length(grep("^[23456789]$", df$T)) > 1) {
    t.type <- 12  # monthly data
  }

  req.type <- requested_freq(series.name)
  if (!req.type == t.type) {
    warning("The guessed requested frequency (", req.type,
            ") does not match detected frequency (", t.type,
            ")")
  }

  # convert data to correct format
  if (t.type == 1)
    df$T <- as.numeric(df$T)
  if (t.type == 12) {
    # we assume that the first observation has the year
    start.date <- zoo::as.yearmon(df$T[1], format = "%Y %m")
    df$T <- start.date + seq(from = 0, by = 1/12, length = nrow(df))
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





  # get methodology, comment and source

  n.vars <- ncol(df) - 1  # remove 'T', the name of index

  metadata$methodology <- c("", get_stat_hse_info_vector(series.name,
                                                         n.vars, "methodology", ...))
  metadata$source <- c("", get_stat_hse_info_vector(series.name,
                                                    n.vars, "source"), ...)
  metadata$comment <- c("", get_stat_hse_info_vector(series.name,
                                                     n.vars, "comment"), ...)

  metadata$freq <- t.type

  if (output == "zoo") {
    df <- zoo::zoo(dplyr::select(df, -T), order.by = df$T, frequency = t.type)
  }

  metadata <- metadata[!metadata$tsname == "T", ]
  attr(df, "metadata") <- metadata

  return(df)
}

#' Obtain multivariate time series from sophist.hse.ru
#'
#' This function obtains multivariate time series from sophist.hse.ru
#'
#' The output may be choosen to be 'zoo' or 'data.frame'. Metadata is saved
#' into the attribute 'metadata'.
#'
#' @param series.name the names of the time series, i.e. 'WAG_Y'
#' @param output the desired output format, either 'zoo' or 'data.frame'
#' @param ... further arguments passed into getURL. One may use them to work with proxy.
#' @return data.frame with the corresponding time series
#' @export
#' @examples
#' df <- sophisthse('IP_EA_Q')
#' df <- sophisthse('WAG_Y')
sophisthse <- function(series.name = "IP_EA_Q",
                       output = c("zoo", "data.frame"), ...) {

  output <- match.arg(output)

  req_type <- requested_freq(series.name)
  if (length(unique(req_type)) > 1)
    warning("Probably requested series have different frequency.")

  all_data <- sophisthse0(series.name[1], output = "data.frame", ...)
  all_meta <- attr(all_data, "metadata")
  series.name <- series.name[-1]

  for (sname in series.name) {
    one_data <- sophisthse0(sname, output = "data.frame", ...)
    one_meta <- attr(one_data, "metadata")
    all_meta <- dplyr::rbind_list(all_meta, one_meta)
    all_data <- merge(all_data, one_data, by = "T", all = TRUE)
  }

  if (output == "zoo")
    all_data <- zoo::zoo(dplyr::select(all_data, -T), order.by = all_data$T,
                    frequency = unique(all_meta$freq))

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
  message("The output is not complete. BRDATA is a table of tables.")
  message("Some regional data cannot be parsed.")
  url <- "http://sophist.hse.ru/hse/nindex.shtml"
  url_chr <- RCurl::getURL(url, ...)
  x <- stringr::str_match_all(url_chr,
              "/tables/([A-Z0-9\\-\\_]+)\\.html")[[1]][, 2]
  return(x)
}


