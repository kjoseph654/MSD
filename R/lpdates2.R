#' @title Pulling Dates
#'
#' @description This function pulls the dates that correspond with the Mid Summer Drought, and is used in conjuction with the MSD function.

#'
#' @usage lpdates2(x, year1=1981, date1="05-01", date2="06-01", date3="08-31", date4="10-31")
#'
#' @param x          Date vector
#' @param year1      The year the Date vector starts on
#' @param date1      Start index to find first onset date
#' @param date2      End index to find first onset date
#' @param date3      Start index to find second onset date
#' @param date4      End index to find second onset date
#'
#' @return A vector of indexes that correspond to the four dates every year, with respect to the start date.
#' @return NOTE: It would be significantly easier to save the output as a variable called 'dated', especially when using rasters so that you do not have to create an intermediate function.
#'
#' @examples
#' dates<-seq(from = as.Date("1981-01-01"), to = as.Date("2020-12-31"), by = "day") 
#' lpdates2(dates, year1=1981, date1="05-01", date2="06-01", date3="08-31", date4="10-31")
#'
#' @export
#' 
lpdates2 <- function(x, year1=1981, date1="05-01", date2="06-01", date3="08-31", date4="10-31"){
  i<-year1
  l<-round(length(x)/365)
  j<-i+l-1
  k<-c(0)
  for(year in i:j)
  {
    k[((year-i)*4)+1]<-which(grepl(paste(as.character(year),date1,sep="-"),x))
    k[((year-i)*4)+2]<-which(grepl(paste(as.character(year),date2,sep="-"),x))
    k[((year-i)*4)+3]<-which(grepl(paste(as.character(year),date3,sep="-"),x))
    k[((year-i)*4)+4]<-which(grepl(paste(as.character(year),date4,sep="-"),x))
  }
  return(k)
}