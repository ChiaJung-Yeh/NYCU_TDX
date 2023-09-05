\name{Bike_Station}
\alias{Bike_Station}
\docType{data}
\title{
Retrieve the station of bike sharing system
}
\description{
The function is used to download the stations of bike sharing system for given county.
}
\usage{
Bike_Station(access_token, county, dates=F, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. Note that some of cities have no bike sharing system, please check it out first.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{dates}{If historical data would be retrieved, this argument should be set; otherwise the default value is FALSE. The date format should be \code{YYYY-MM-DD}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\author{
Maintainer: Chia Jung, Yeh  \email{chia-jung.yeh@sydney.edu.au}
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
Bike_Station(access_token, "Taipei")
Bike_Station(access_token, "Hsinchu", dtype="sf", out="./hsinchu_youbike.shp")

# historical data
Bike_Station(access_token, "Hsinchu", "2022-01-01")
}
\keyword{datasets}

