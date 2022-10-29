\name{Bike_Remain_His}
\alias{Bike_Remain_His}
\docType{data}
\title{
Retrieve the historical availability data of bike sharing system
}
\description{
The function is used to download the historical availability data of bike sharing system for given county and given dates.
}
\usage{
Bike_Remain_His(access_token, county, dates, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. Note that some of cities have no bike sharing system, please check it out first.}
  \item{dates}{Please fill the specific dates. It allows to retrieve multiple dates, but note that the limit is 7 days. The format of the date should be \code{YYYY-MM-DD} if it is single day. For the multiple dates, each of date should be separated by \code{;}, but if the date is in a specific range, using \code{~} to connect dates.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\author{
Maintainer: Chia Jung, Yeh  \email{robert1328.mg10@nycu.edu.tw}
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
Bike_Remain_His(access_token, "Taipei")
Bike_Remain_His(access_token, "Hsinchu", out="./hsinchu_youbike_availability.shp")
}
\keyword{datasets}

