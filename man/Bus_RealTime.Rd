\name{Bus_RealTime}
\alias{Bus_RealTime}
\docType{data}
\title{
Retrieve the detailed information of each bus route
}
\description{
The function is used to download the bus route information, including the route name, subroute name, bus route type, direction, departure and destination stop name. Note that it supports to download data of city buses and intercity buses.
}
\usage{
Bus_RealTime(access_token, county, format, dates=F, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{format}{This argument shuld be \code{frequency} or \code{stop}. The former is the real time data of each vehicle, while the latter is the real time data based on depature and arrial of the bus for each station.}
  \item{dates}{If historical data would be retrieved, this argument should be set; otherwise the default value is FALSE. The date format should be \code{YYYY-MM-DD}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{RouteUID}   \tab It contains every subroutes and directions in a same RouteUID.\cr
      \tab \strong{SubRouteUID} \tab If the direction or subroute is not the same, a different SubRouteUID must be recorded.\cr
      \tab \strong{PlateNumb}   \tab The plate number of vehicle.\cr
    }

Codes of \strong{Direction} are listed below.
  \tabular{rlll}{
    \strong{0}   \tab outbound\cr
    \strong{1}   \tab inbound\cr
    \strong{2}   \tab loop\cr
    \strong{255}   \tab unknown\cr
  }

Codes of \strong{BusStatus} are listed below.
  \tabular{rlll}{
    \strong{0}   \tab normal\cr
    \strong{1}   \tab crash\cr
    \strong{2}   \tab breakdown\cr
    \strong{3}   \tab congestion\cr
    \strong{4}   \tab emergency\cr
    \strong{5}   \tab gas refill\cr
    \strong{90}   \tab unknown\cr
    \strong{91}   \tab unknown direction\cr
    \strong{98}   \tab route offset \cr
    \strong{99}   \tab non-operation status\cr
    \strong{100}   \tab full\cr
    \strong{101}   \tab chartered\cr
    \strong{255}   \tab unknown (due to data loss)\cr
  }
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
Bus_RealTime(access_token, "Taipei")
Bus_RealTime(access_token, "Hsinchu", out="./Hsinchu_Bus_Route.csv")
}
\keyword{datasets}

