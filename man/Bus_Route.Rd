\name{Bus_Route}
\alias{Bus_Route}
\docType{data}
\title{
Retrieve the detailed information of each bus route
}
\description{
The function is used to download the bus route information, including the route name, subroute name, bus route type, direction, departure and destination stop name.

The output code of direction is listed below.

  \tabular{rlll}{
    0   \tab outbound\cr
    1   \tab inbound\cr
    2   \tab loop\cr
    255   \tab unknown\cr
  }

The output code of bus route type is listed below.

  \tabular{rlll}{
    11   \tab City Bus\cr
    12   \tab Intercity Bus (Normal)\cr
    13   \tab Intercity Bus (Freeway)\cr
    14   \tab Feeder bus\cr
  }

Note that it supports to download data of city buses and intercity buses.

}
\usage{
Bus_Schedule(access_token, county, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
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
Bus_Schedule(access_token, "Taipei")
Bus_Schedule(access_token, "Hsinchu", out="./Hsinchu_Bus_Route.csv")
}
\keyword{datasets}

