#' sophisthse
#'
#' @name sophisthse
#' @docType package
#' @author Boris Demeshev 
#' @import XML dplyr RCurl zoo
NULL


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
#' rus2num("34 345,34")
rus2num <- function(x) {
  x <- gsub(",",".",x)
  x <- gsub(" ","",x)
  return(as.numeric(x))
}





#' Obtain additional information for specific time series
#'
#' Obtain additional information for specific time series from sophist.hse.ru
#'
#' Internal function. Obtain additional information for specific time series
#' from sophist.hse.ru. Either "methodology", "source" or "comment".
#'
#' @param series.name the names of the time series
#' @param n.vars number of variables
#' @param info type of information (methodology/source/comment)
#' @return character vector with info for each variable
#' @examples
#' info <- get_stat_hse_info_vector("IP_EA_Q", 1,"methodology")
get_stat_hse_info_vector <- function(series.name = "IP_EA_Q",
                                     n.vars = 1,
                                     info = c("methodology","source","comment")) {

  
  if (info[1]=="methodology") 
    url <- paste("http://sophist.hse.ru/hse/1/met/",series.name,".html",sep="")
  if (info[1]=="source") 
    url <- paste("http://sophist.hse.ru/hse/1/sor/",series.name,".html",sep="")
  if (info[1]=="comment") 
    url <- paste("http://sophist.hse.ru/hse/1/com/",series.name,".html",sep="")
  
  url.html <- getURL(url,.encoding="UTF-8")
  url.parsed <- htmlTreeParse(url.html)
  url.root <- xmlRoot(url.parsed)

  
  # there maybe 2 situations:
  # one entry for each variable
  # one entry for all variables
  # or more than 2 ;)
  
  n.on.site <- length(xmlChildren(url.root[[3]][[3]])) %/% 2 # only approximate
  text <- rep("",n.vars)
  
  for (i in 1:min(n.on.site,n.vars)) {
    temp.value <- xmlValue(url.root[[3]][[3]][[2*i]])
    if (length(temp.value)>0) text[i] <- temp.value # avoid empty blocks 
  }
    
  return(text)
}  
  
#' Obtain time series from sophist.hse.ru
#'
#' This function obtains multiple time series from sophist.hse.ru
#'
#' If many time series are requested at once they should be of the same frequency.
#'
#' @param series.name the names of the time series
#' @param output the desired output format, either 'zoo' or 'data.frame'
#' @return data.frame with the corresponding time series
#' @export
#' @examples
#' df <- sophisthse("IP_EA_Q")
#' df <- sophisthse("WAG_Y")
sophisthse <- function(series.name = "IP_EA_Q", output = c("zoo", "data.frame")) {  
  
  # download main data
  url <- paste("http://sophist.hse.ru/exes/tables/",series.name,".htm",sep="")
  url.html <- getURL(url,.encoding="UTF-8")
  
  # get main table
  tables <- readHTMLTable(url.html)
  df <- tables[[1]]  
  
  # html parse
  url.parsed <- htmlTreeParse(url.html)
  url.root <- xmlRoot(url.parsed)
  
  
  # all to character
  for (i in 1:ncol(df)) df[,i] <- as.character(df[,i])
  
  # save units of measure 
  attr(df,"units") <- gsub("&nbsp","",df[1,])
  
  # get full variable names
  full.names <- rep("",ncol(df))
  for (i in 2:ncol(df)) full.names[i] <- xmlValue(url.root[[3]][[1]][[1]][[i-1]])
  attr(df,"full.names") <- full.names
  
  # remove unused lines (units, info about series)
  df <- df[2:(nrow(df)-4),]
  
  # remove spaces, replace "," by ".", convert to numeric
  for (i in 2:ncol(df)) 
    df[,i] <- rus2num(df[,i])
  
  
  # pretty time index
  
  # determine the type of data: yearly/quarterly/mothly
  t.type <- "Y" # by default we assume early data
  if (length(grep("[IV]",df$T))>1) t.type <- "Q" # quarterly data
  if (length(grep("^[23456789]$",df$T))>1) t.type <- "M" # monthly data
  
  # convert data to correct format
  if (t.type=="Y") df$T <- as.numeric(df$T)
  if (t.type=="M") {
    # we assume that the first observation has the year
    start.date <- as.yearmon(df$T[1],format="%Y %m")
    df$T <- start.date + seq(from=0,by=1/12,length=nrow(df))
  }
  if (t.type=="Q") {
    # we assume that the first observation has the year
    df$T <- gsub(" IV$","-4",df$T)
    df$T <- gsub(" III$","-3",df$T)
    df$T <- gsub(" II$","-2",df$T)
    df$T <- gsub(" I$","-1",df$T)
    
    start.date <- as.yearqtr(df$T[1])
    df$T <- start.date + seq(from=0,by=1/4,length=nrow(df))
  }
    
  # get methodology, comment and source

  n.vars <- ncol(df)-1 # remove "T", the name of index
  
  attr(df,"methodology") <- c("",
    get_stat_hse_info_vector(series.name,n.vars,"methodology"))

  attr(df,"source") <- c("",
    get_stat_hse_info_vector(series.name,n.vars,"source"))
  
  attr(df,"comment") <- c("",
    get_stat_hse_info_vector(series.name,n.vars,"comment"))
						
  if (output[1] == "zoo") df <- as.zoo(df)
  return(df)  
}




