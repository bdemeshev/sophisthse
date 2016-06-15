#' Quarterly Russian real money income
#'
#' A dataset containing real money income index and per capita real income.
#' Source: \url{http://sophist.hse.ru/hse/nindex.shtml}.
#' Downloaded on 18.02.2016.
#'
#' @format A zooreg object with 93 rows and 3 variables:
#' \describe{
#'   \item{HHI_Q_DIRI}{Index of real money income. 1992 IV = 100}
#'   \item{HHI_Q_DIRI_SA}{Seasonally adjusted index of real money income. 1992 IV = 100}
#'   \item{HHI_Q}{Real money income per capita}
#' }
#' @source downloaded with sophisthse("HHI_Q_I") command
"hhi_q_i"

#' Description of time series available at sophist.hse.ru
#'
#' A dataset containing description of time series available at \url{http://sophist.hse.ru/hse/nindex.shtml}.
#' Downloaded on 15.06.2016.
#'
#' @format data.frame with 384 rows and 8 columns:
#' \describe{
#'   \item{table}{name of table that contains time series}
#'   \item{tsname}{short time series name}
#'   \item{freq}{frequency, 1 for yearly, 4 for quarterly, 12 for monthly data}
#'   \item{unit}{measurement unit}
#'   \item{fullname}{full time series name}
#'   \item{methodology}{methodology of calculation}
#'   \item{source}{source of time series}
#'   \item{comments}{further comments for time series}
#' }
#' @source downloaded from \url{http://sophist.hse.ru/hse/nindex.shtml} on 15.06.2016
"series_info"
