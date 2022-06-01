\name{Bus_Shape}
\alias{Bus_Shape}
\docType{data}
\title{
Retrieve the geometry of bus routes
}
\description{
The function is used to download the geometry of bus routes. And it supports to download data of city buses and intercity buses.
}
\usage{
Bus_Shape(access_token, county, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
 \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. Note that some of cities have no cycling path, please check it out first.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
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
Bus_Shape(access_token, "Hsinchu")
Bus_StopOfRoute(access_token, "Taipei", dtype="sf", out="./Taipei_Stop.shp")
}
\keyword{datasets}

