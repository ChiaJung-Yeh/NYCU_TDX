\name{Bus_TravelTime}
\alias{Bus_TravelTime}
\docType{data}
\title{
Retrieve the travel time of each bus routes
}
\description{
The function is used to download the travel time of any two adjacent stops for a specific route. It supports to download data of city buses and intercity buses. Note that the function can retrieve the travel time data of single or mutiple bus routes.
}
\usage{
Bus_TravelTime(access_token, county, routeid, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{routeid}{The specific RouteID that should be retrive for its travel time. This can be a character or a vector. Please ensure that the RouteID must contain in the county; otherwise, it would retrieve nothing.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{RouteID}   \tab This is the main key of this table.\cr
      \tab \strong{SubRouteID}   \tab There might exists two or more SubRouteID due to the different direction of sub-route.\cr
      \tab \strong{Weekday} \tab Marked 0 to 6. "0" represents "Sunday".\cr
      \tab \strong{StartHour} \tab The start hour of the travel time data.\cr
      \tab \strong{EndHour} \tab The end hour of the travel time data. Note that the data is recorded meticulously with one hour interval in some counties (e.g.,Taipei), while most of data is recoreded in a longer interval, that is, the difference between StartHour and EndHour is usually larger than 1.\cr
      \tab \strong{RunTime} \tab The operation time under a specific time interval (StartHour~EndHour) of a specific sub-route."\cr
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
# routeid can be a character
Bus_TravelTime(access_token, "Taipei", "10132")

# routeid can also be a vector
Bus_TravelTime(access_token, "Taipei", c("10132", "10142"))

# routeid can be retrieve from the function Bus_Route()
temp=Bus_Route(access_token, "Hsinchu")
Bus_TravelTime(access_token, "Hsinchu", unique(temp$RouteID), out="./Hsinchu_TravelTime.csv")
}
\keyword{datasets}

