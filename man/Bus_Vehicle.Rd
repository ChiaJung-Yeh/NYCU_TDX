\name{Bus_Vehicle}
\alias{Bus_Vehicle}
\docType{data}
\title{
Retrieve the detailed information of each bus route
}
\description{
The function is used to download the vehicle of each bus operator. Note that it supports to download data of city buses and intercity buses.
}
\usage{
Bus_Vehicle(access_token, county, out=F)
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
      \tab \strong{PlateNumb}   \tab The plate number of vehicle.\cr
      \tab \strong{SubRouteUID} \tab If the direction or subroute is not the same, a different SubRouteUID must be recorded.\cr
    }

Codes of \strong{VehicleClass} are listed below.
  \tabular{rlll}{
    \strong{1}   \tab bus\cr
    \strong{2}   \tab midibus\cr
    \strong{3}   \tab minibus\cr
    \strong{4}   \tab double-decker bus\cr
    \strong{5}   \tab articulated bus\cr
    \strong{6}   \tab taxi\cr
  }

Codes of \strong{VehicleType} are listed below.
  \tabular{rlll}{
    \strong{0}   \tab general\cr
    \strong{1}   \tab barrier-free bus\cr
    \strong{2}   \tab rehabus\cr
    \strong{3}   \tab minibus\cr
    \strong{4}   \tab charter bus\cr
    \strong{5}   \tab others\cr
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
Bus_Vehicle(access_token, "Taipei")
Bus_Vehicle(access_token, "Hsinchu", out="./Hsinchu_Bus_Vehicle.csv")
}
\keyword{datasets}

