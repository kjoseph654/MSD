#' @title Bartlett Noise Filter
#'
#' @description A triangular Filter for smoothing data
#'
#' @description The filter will take a weighted average of a specified number of points around the point of interest to create a smoother time series. For example, if the size of the filter is set to 31, the filter will take the 15 points before and after the point of interest (for a total of 31 points), and calculate a weighted average based on how far away the points are from the point of interest.
#'
#' @usage bartlett_noise_filter(x,y)
#'
#' @param x          RasterBrick or TimeSeries
#' @param y          Size of Filter
#'
#' @return RasterBrick or TimeSeries of Yearly data
#'
#' @seealso \code{\link{raster}}
#'
#' @examples
#' # using RasterBricks or a Time Series
#' r<-bartlett_noise_filter(x, 31)
#'
#' @export
bartlett_noise_filter <- function(x,y) {
  # x = data set; y = size of bartlett filter
  library(signal) #installs signal package
  bartlett_window <- c(bartlett(y)) #contructs a Bartlett vector with the size of the bartlett filter
  bartlett_sum <- sum(bartlett_window) #creates a sum of the bartlett window to construct an average
  #apply filter weighted on the average and divded by the sum of the bartlett window to smooth out the data
  filtered_data <- stats::filter(x,bartlett_window/bartlett_sum,method="convolution")
  return(filtered_data)
}

#required packages are signal