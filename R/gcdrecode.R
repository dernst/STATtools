##' Recode Austrian "Gemeindecodes" due to changes
##' over the years (e.g. "Gemeindestrukturreform 2015")
##'
##' Different states for \code{gcd} are available for 2011, 2012,
##' 2013, 2015, 2016, 2017 and 2018 (first of January). All of them,
##' except the first one, are therefore meaningful values for
##' \code{targetyear}, which has to be higher than \code{year}.
##'
##' \code{gcdrc} is the vectorized version to be used for
##' different values of \code{year}. If \code{year} is constant
##' in your data, \code{gcdrecode} should be used to avoid
##' needless overhead.
##'
##' There are three possibilities to handle wrong
##' gcd-year-combinations, which can be caused by wrong gcd e.g.
##' if the last two digits are zero (60100):
##' \enumerate{
##'   \item \code{invalid = "NA"} (default): result is set to
##'         \code{\link{NA}}
##'   \item \code{invalid = "orig"}: original values of
##'         \code{gcd} are returned instead of recoding them
##'   \item \code{invalid = <something else>}: something else
##'         (character or numeric) is returned
##' }
##'
##' For some \code{gcd} there is no unique match possible due to
##' splitting of municipalities in 2015. For these cases the
##' \code{gcd} of the part with the higher population (2015) is
##' used and the other is skipped (\code{\link{warning}}). The
##' numbers in brackets are the \code{gcd} belonging to the names
##' of municipalities in 2013 (before splitting) respectively
##' 2015 (after splitting).
##'
##' \itemize{
##'   \item Kohlberg (62336)
##'     \itemize{
##'       \item used Gnas (62380)
##'       \item skipped Paldau (62384)
##'     }
##'   \item Oberstorcha (62349)
##'     \itemize{
##'       \item used Paldau (62384)
##'       \item skipped Kirchberg an der Raab (62382)
##'     }
##'   \item Limbach bei Neudau (62277)
##'     \itemize{
##'       \item used Neudau (62274)
##'       \item skipped Bad Waltersdorf (62264)
##'     }
##'   \item Schlag bei Thalberg (62248)
##'     \itemize{
##'       \item used Rohrbach an der Lafnitz (62277)
##'       \item skipped Dechantskirchen (62265)
##'     }
##'   \item Stocking (61040)
##'     \itemize{
##'       \item used Wildon (61059)
##'       \item skipped Sankt Georgen an der Stiefing (61055)
##'     }
##' }
##'
##' @title Recode Austrian "Gemeindecodes"
##' @aliases gcdrecode gcdrc
##' @param year a character or an integer containing the year
##' @param gcd a vector containing Austrian "Gemeindekennziffern"
##'            corresponding to the \code{year}
##' @param targetyear a character or an integer containing the
##'                   year for which the \code{gcd} should be
##'                   returned
##' @param invalid a character or an integer specifying what
##'                should be returned for invalid
##'                gcd-year-combinations (see details)
##' @param \dots further arguments in \code{gcdrc} passed on
##'              \code{gcdrecode}
##' @return a vector containing Austrian "Gemeindecodes"
##'         corresponding to the \code{targetyear}
##' @author Gerhard Nachtmann \email{kpm.nachtmann@@gmail.com}
##' @keywords gcdrecode gcdrc Gemeindecodes recode
##' @importFrom methods as
##' @export
##' @examples
##'
##' ### 62380 is existing since 2015
##' gcdrecode(2011, 62380, 2015) # warning
##' gcdrecode(2011, 62380, 2015, invalid = "orig") # warning
##' gcdrecode(2011, 62380, 2015, invalid = "wrong") # warning
##' gcdrecode(2015, 62380, 2017) # didn't change
##' gcdrc(2011:2012, c(60201, 60201), 2016)
##' ### some gcd of the sequence are not existing --> warning
##' gcdrc(2016, 32401:32424, 2017) # warning; WU ex 2017


gcdrecode <- function(year, gcd, targetyear = 2018,
                      invalid = "NA"){
  ## invalid can be "NA", "orig" or sth else
  utils::data("gcdnum", package="STATtools")
  if(length(invalid) != 1){
    stop("'invalid' has to be of length 1")
  }
  if(is.na(invalid)){
    invalid <- "NA"
  }
  if(is.factor(gcd)){
    gcd <- as.character(gcd)
  }
  splitgem <- c(62380, 62384, 62274, 62277, 61059) # 2015
  gcdc <- class(gcd)
  gcdf <- as.factor(gcd)
  gcdl <- levels(gcdf)
  if(targetyear < year){
    stop("targetyear must be higher than year!")
  }
  if(year <= 2011) year <- 2011
  if(year == 2014) year <- 2013
  if(targetyear == 2014) targetyear <- 2013
  validty <- c(2011, 2012, 2013, 2015, 2016, 2017, 2018)
  y <- sub(20, "GKZ", year)
  reflist <- get("gcdnum")
  if(!targetyear %in% validty){
    targetyear <- 2018
    warning(paste("Unvalid targetyear. I set it to 2018.\n",
                  "Possible values are",
                  paste(validty, collapse = ", ")))
  }
  ty <- sub(20, "GKZ", targetyear)
  if(year <= 2015){
    reflist <- unique(reflist[, c(y, ty)])
  }
  refygcd <- as.character(reflist[, y])
  gcdaux <- gcdl %in% refygcd # F are not in refygcd
  if(!all(gcdaux)){
    warning("At least one of your 'gcd' is not valid in
            combination with 'year'.")
  }
  ### init gcdlnew
  if(invalid == "orig" | year == targetyear){
    gcdlnew <- gcdl
  } else {
    gcdlnew <- rep(NA, length(gcdl))
  }
  gcdlv <- gcdl[gcdaux]
  rclaux <- reflist[refygcd %in% gcdlv, ]
  gcdlnewv <- rclaux[order(rclaux[, y]), ty]
  if(any(gcdlnewv %in% splitgem) & year < 2015){
    warning(paste("No unique match possible for some gcd.\n",
                  "Reason: splitting of municipalities in 2015.",
                  "I took the part with the higher population",
                  "and skipped the other.\n",
                  "See help('gcdrecode') for details.")
    )
  }
  gcdlnew[gcdaux] <- gcdlnewv
  if(invalid == "NA"){
    gcdlnew[!gcdaux] <- as.character(NA)
  } else if(invalid != "orig"){
    gcdlnew[!gcdaux] <- invalid
  }
  levels(gcdf) <- gcdlnew
  gcdf <- as.character(gcdf)
  if(invalid %in% c("NA", "orig")){
    gcdf <- as(gcdf, gcdc) # set class to *input*
    # class of gcd
  }
  gcdf
}


#' @export
#' @rdname gcdrecode

gcdrc <- function(year, gcd, targetyear = 2018, ...){
  if(is.factor(gcd)){
    gcd <- as.character(gcd)
  }
  ly <- length(year)
  lgcd <- length(gcd)
  if(!(ly %in% c(lgcd, 1))){
    stop("Length of 'year' != length of 'gcd' or 1")
  }
  if(ly == 1){
    gcdn <- gcdrecode(year, gcd, targetyear = targetyear,
                      ...)
  } else {
    gcdn <- vector(length = lgcd, mode = mode(gcd))
    i2011 <- year <= 2011
    i2012 <- year == 2012
    i2013 <- year %in% 2013:2014
    i2015 <- year == 2015
    i2016 <- year == 2016
    i2017 <- year == 2017
    i2018 <- year >= 2018
    gcdn[i2011] <- gcdrecode(2011, gcd[i2011],
                             targetyear = targetyear, ...)
    if(targetyear >= 2012){
      gcdn[i2012] <- gcdrecode(2012, gcd[i2012],
                               targetyear = targetyear, ...)
    }
    if(targetyear >= 2013){
      gcdn[i2013] <- gcdrecode(2013, gcd[i2013],
                               targetyear = targetyear, ...)
    }
    if(targetyear >= 2015){
      gcdn[i2015] <- gcdrecode(2015, gcd[i2015],
                               targetyear = targetyear, ...)
    }
    if(targetyear >= 2016){
      gcdn[i2016] <- gcdrecode(2016, gcd[i2016],
                               targetyear = targetyear, ...)
    }
    if(targetyear >= 2017){
      gcdn[i2017] <- gcdrecode(2017, gcd[i2017],
                               targetyear = targetyear, ...)
    }
    if(targetyear >= 2018){
      gcdn[i2018] <- gcdrecode(2018, gcd[i2018],
                               targetyear = targetyear, ...)
    }
  }
  gcdn
}
