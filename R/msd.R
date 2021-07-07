#' @title Mid Summer Drought
#'
#' @description Calculate different statistics of the mid summer drought from a RasterBrick or a Time Series.
#'
#' @description The input must be in the form of daily data, with the first data point being January 1st of a respective year.
#' @description If x is a RasterBrick, then the output is a RasterBrick with a data point for each year. If you would like to find a statistical value of the new rasterbrick, it is recommended to use
#'
#' @description r <- raster::calc(x, mean)
#' 
#' @description If the index vector from lpdates2/lpdates4 is not saved as 'dated'/'dated6', a function will have to be created to use it in raster::calc.
#'
#' @usage MSD(x, dated1=dated, dated4=dated6, fun="duration")
#'
#' @param x          RasterBrick or TimeSeries
#' @param dated      Index of Dates from the lpdates2 function (should be saved as 'dated')
#' @param dated6     Index of Dates from the lpdates4 function (should be saved as 'dated')
#' @param fun        Specify what values to be pulled from the function. Options include 'duration', 'intensity', 'max1', 'max2', 'min', and 'mindex'.
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{raster}}
#'
#' @examples
#' # using RasterBricks
#' r<-raster::calc(x, MSD)
#'
#' # using TimeSeries
#' r<-MSD(x)
#' @export

MSD<-function(x, dated1=dated, dated4=dated6, fun="duration"){
  data<-c(as.numeric(x)) #making sure the data is numeric
  peaks<-findPeaks(data)-1 #finding all of the peaks of the data
  valleys<-findValleys(data)-1 #finding all of the valleys of the data
  export<-c(0) #creating a new variable
  for (years in 1:(round(length(data)/365))){ #running for every year
    date1<-dated1[4*years-2] #the next six lines just pull the proper indeces
    date2<-dated1[4*years-1]
    date3<-dated1[4*years-3]
    date4<-dated1[4*years]
    date5<-dated4[2*years-1]
    date6<-dated4[2*years]
    min<-min(data[valleys[valleys>=date1 & valleys<=date2]],na.rm=TRUE) #checking for min valley between the inner dates
    min2<-min(data[valleys[valleys>=date3 & valleys<=date4]],na.rm=TRUE) #checking for min valley between the outer dates
    mindate<-match(min, data) #finding the index of min
    mindate2<-match(min2, data) #finding the index of min2
    check1<-mindate==mindate2 #making sure that the index does overlap
    if (is.na(mindate)==TRUE){ #making sure we have a minimum, otherwise an NA is output
      export[years]<-NA
    }else{
      dates<-c(peaks[peaks>=date3 & peaks<=date4], mindate) #finding all the peaks between the outer dates
      dates<-sort(dates) #sorting them in order with the mindate
      mindex<-match(mindate,dates) #finding the index of the mindate
      maxdex1<-dates[1:(mindex-1)] #the next few lines find the max before the minimum and after
      maxdex2<-dates[(mindex+1):length(dates)]
      maxpos1<-data[maxdex1]
      maxpos2<-data[maxdex2]
      max1<-max(maxpos1,na.rm=TRUE)
      max2<-max(maxpos2,na.rm=TRUE)
      pos1<-match(max1,maxpos1)
      pos2<-match(max2,maxpos2)
      index1<-maxdex1[pos1]
      index2<-maxdex2[pos2]
      maxcheck1<-max(data[date5:mindate],na.rm=TRUE) #making sure that the max is the real between january and mindex
      maxcheck2<-max(data[mindate:date6],na.rm=TRUE) #making sure that the max is the real between mindex and december
      maxval1<-max1==maxcheck1
      maxval2<-max2==maxcheck2
      max1<-max1*maxval1
      max2<-max2*maxval2
      if (is.na(check1)==TRUE){ #making sure that the minimum is the minimum
        export[years]<-NA
      }else if (length(max1)==0){#the next couple ensure that we have values to pull from
        export[years]<-NA
      }else if (length(max2)==0){
        export[years]<-NA
      }else if (is.na(max1)==TRUE){
        export[years]<-NA
      }else if (is.na(max2)==TRUE){
        export[years]<-NA
      }else if (max1==0){
        export[years]<-NA
      }else if (max2==0){
        export[years]<-NA
      }
      else if (fun=="duration"){ #the different cases to choose from for 'fun'
        export[years]<-index2-index1
      }else if (fun=="intensity"){
        export[years]<-((max1+max2)/2)-min
      }else if (fun=="max1"){
        export[years]<-max1
      }else if (fun=="max2"){
        export[years]<-max2
      }else if (fun=="min"){
        export[years]<-min
      }else if (fun=="mindex"){
        export[years]<-index1
      }else
        export[years]<-NA
    }
  }
  return(export)
}