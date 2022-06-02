\name{Bus_StopOfRoute}
\alias{Bus_StopOfRoute}
\docType{data}
\title{
Retrieve the stops of each bus routes
}
\description{
The function is used to download the bus stop of route. And it supports to download data of city buses and intercity buses.
}
\usage{
Bus_StopOfRoute(access_token, county, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{RouteUID}   \tab It contains every sub-routes and directions in a same RouteUID.\cr
      \tab \strong{SubRouteUID} \tab If the direction or sub-route is not the same, a different SubRouteUID must be recorded.\cr
      \tab \strong{StopSequence} \tab The sequence of the bus stops for each subroute.\cr
    }

Codes of Direction are listed below.
  \tabular{rlll}{
    \strong{0}   \tab outbound\cr
    \strong{1}   \tab inbound\cr
    \strong{2}   \tab loop\cr
    \strong{255}   \tab unknown\cr
  }
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
Bus_StopOfRoute(access_token, "Hsinchu")
Bus_StopOfRoute(access_token, "Taipei", dtype="text", out="./Taipei_StopOfRoute.csv")
}
\keyword{datasets}

