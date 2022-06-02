\name{Bus_Route}
\alias{Bus_Route}
\docType{data}
\title{
Retrieve the detailed information of each bus route
}
\description{
The function is used to download the bus route information, including the route name, subroute name, bus route type, direction, departure and destination stop name. Note that it supports to download data of city buses and intercity buses.

}
\usage{
Bus_Route(access_token, county, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{RouteUID}   \tab It contains every sub-routes and directions in a same RouteUID.\cr
      \tab \strong{SubRouteUID} \tab If the direction or sub-route is not the same, a different SubRouteUID must be recorded.\cr
    }

Codes of RoadClass are listed below.
  \tabular{rlll}{
    \strong{0}   \tab outbound\cr
    \strong{1}   \tab inbound\cr
    \strong{2}   \tab loop\cr
    \strong{255}   \tab unknown\cr
  }

Codes of BusRouteType are listed below.
  \tabular{rlll}{
    \strong{11}   \tab City Bus\cr
    \strong{12}   \tab Intercity Bus (Normal)\cr
    \strong{13}   \tab Intercity Bus (Freeway)\cr
    \strong{14}   \tab Feeder bus\cr
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
Bus_Route(access_token, "Taipei")
Bus_Route(access_token, "Hsinchu", out="./Hsinchu_Bus_Route.csv")
}
\keyword{datasets}

