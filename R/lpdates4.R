#' @title Pulling Dates 2
#'
#' @description This function pulls the the first and last day of every year of the date vector.
#'
#' @usage lpdates2(x, year1=1981, date1="01-01", date2="12-31")
#'
#' @param x          Date vector
#' @param year1      The year the Date vector starts on
#' @param date1      January 1st
#' @param date2      December 31st

#'
#' @return A vector of indexes that correspond to the two dates every year, with respect to the start date.
#' @return NOTE: It would be significantly easier to save the output as a variable called 'dated', especially when using rasters so that you do not have to create an intermediate function.
#'
#' @examples
#' dates<-seq(from = as.Date("1981-01-01"), to = as.Date("2020-12-31"), by = "day") 
#' lpdates4(dates, year1=1981, date1="01-01", date2="12-31")
#'
#' @export
#' 
lpdates4 <- function(x, year1=1981, date1="01-01", date2="12-31"){
  i<-year1
  l<-round(length(x)/365)
  j<-i+l-1
  k<-c(0)
  for(year in i:j)
  {
    k[((year-i)*2)+1]<-which(grepl(paste(as.character(year),date1,sep="-"),x))
    k[((year-i)*2)+2]<-which(grepl(paste(as.character(year),date2,sep="-"),x))
  }
  return(k)
}