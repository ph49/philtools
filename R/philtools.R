
.pretty.digital <- function(x) {
    ifelse(is.na(x), NA,
           ifelse(abs(x) >= 1024^5, paste(round(x/1024^5,2), "P"),
                  ifelse(abs(x) >= 1024^4, paste(round(x/1024^4,2), "T"),
                         ifelse(abs(x) >= 1024^3, paste(round(x/1024^3,2), "G"),
                                ifelse(abs(x) >= 1024^2, paste(round(x/1024^2,2), "M"),
                                       ifelse(abs(x) >= 1024^1, paste(round(x/1024^1,2), "K"),
                                              x))))))
}


.digital.breaks <- function(x, nbreaks=4) {
    r=range(x)
    spread = r[2] - r[1]
    s4 = spread / nbreaks
    lo = floor(log(s4)/log(2))
    p2 = 2 ^ as.integer(lo)
    s1 = p2 * floor(r[1] / p2)
    s2 = p2 * ceiling(r[2] / p2)
    return(seq(s1, s2, by=p2))
}

#' Position scales for digital information (x & y)
#'
#' Continuous scales with breaks set to round numbers of powers of two,
#' and labels using K, M, G, T, P etc.
#'
#' @param nbreaks Integer count of breaks required
#' @param ... Other arguments passed on to scale_(x|y)_continuous
#'
#' @export
#'
#' @examples
#' diamonds %>% qplot(data=., x=carat, y=price) + scale_y_digital()
#'
scale_y_digital = function(nbreaks=4, ...) { ggplot2::scale_y_continuous(breaks=function(x){.digital.breaks(x, nbreaks)}, labels=.pretty.digital, ...) }

#' @rdname scale_y_digital
#' @export
scale_x_digital = function(nbreaks=4,...) { ggplot2::scale_x_continuous(breaks=function(x){.digital.breaks(x, nbreaks)}, labels=.pretty.digital, ...) }

#----------------

#' Comparing POSIXct objects
#'
#' Use %before% and %after% as infix operators between POSIXct objects and strings.
#'
#' @param ct Value to be compared
#' @param st Value to be compared
#'
#' @return TRUE or FALSE, the result of the comparison
#' @export
#'
#' @examples
#'  as.POSIXct("2015-01-01") %before% "2015-01-02"
#'
"%after%" <- function(ct, st) {
    ct = as.POSIXct(ct);
    st = as.POSIXct(st);
    return(ct > st);
}

#' @rdname grapes-after-grapes
#' @export
"%before%" <- function(ct, st) {
    ct = as.POSIXct(ct);
    st = as.POSIXct(st);
    return(ct < st);
}

#' Regular Expression Matching
#'
#' Use %~% to run a regex against a string, like perl.
#'
#' @param string The string to be searched
#' @param regex The regex to be matched
#'
#' @return TRUE or FALSE, the result of the regex search
#' @export
#'
#' @examples
#' "banana" %~% "(an){2}"
#'
"%~%" <- stringr::str_detect
