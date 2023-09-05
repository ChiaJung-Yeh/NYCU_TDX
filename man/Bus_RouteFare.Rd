\name{Bus_RouteFare}
\alias{Bus_RouteFare}
\docType{data}
\title{
Retrieve the route fare of each bus route
}
\description{
The function is used to download the bus route fare.
}
\usage{
Bus_RouteFare(access_token, county, out=F)
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
      \tab \strong{RouteUID}   \tab It contains every subroutes and directions in a same RouteUID.\cr
      \tab \strong{BZOStopID}   \tab The origin of StopID of buffer zone.\cr
      \tab \strong{BZDStopID}   \tab The destination of StopID of buffer zone.\cr
    }

Codes of \strong{FarePricingType} are listed below.
\tabular{rlll}{
  \strong{0}   \tab price by times\cr
  \strong{1}   \tab price by OD\cr
  \strong{2}   \tab price by buffer zone\cr
}

Codes of \strong{TicketType} are listed below.
  \tabular{rll}{
    \strong{1}   \tab One-way ticket\cr
    \strong{2}   \tab Round-trip ticket\cr
    \strong{3}   \tab Smart card\cr
    \strong{4}   \tab Multi-ride ticket\cr
    \strong{5}   \tab 30 days periodic ticket\cr
    \strong{6}   \tab 60 days periodic ticket\cr
  }

Codes of \strong{FareClass} are listed below.
  \tabular{rll}{
    \strong{1}   \tab Adult\cr
    \strong{2}   \tab Student\cr
    \strong{3}   \tab Child\cr
    \strong{4}   \tab Senior\cr
    \strong{5}   \tab Disabled\cr
    \strong{6}   \tab Disabled Child\cr
    \strong{7}   \tab Disabled accompany\cr
    \strong{8}   \tab Group\cr
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
bus_fare=Bus_RouteFare(access_token, "Taipei")

# Buffer zone of the bus route
bus_fare$BufferZone
# Fare in the zones
bus_fare$ZoneFare

Bus_RouteFare(access_token, "Hsinchu", out="./Hsinchu_Bus_Route.csv")
}
\keyword{datasets}

