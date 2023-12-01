\name{Bus_ScheduleEst}
\alias{Bus_ScheduleEst}
\docType{data}
\title{
Retrieve the estimated schedule time of each bus stops
}
\description{
The function is used to download the estimated schedule time of any stop for a specific route. It supports to download data of city buses and intercity buses. Note that the function can retrieve the data of single or mutiple bus routes.
}
\usage{
Bus_ScheduleEst(access_token, county, routename, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. If \code{Intercity} is used, the function retrieves the intercity bus data.}
  \item{routename}{The specific RouteName that should be retrive for its estimated schedule time. This can be a character or a vector. Please ensure that the RouteName must contain in the county; otherwise, it would retrieve nothing.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{TripID}   \tab It records the ID for each trip.\cr
      \tab \strong{SubRouteID}   \tab There might exists two or more SubRouteID due to the different direction of sub-route.\cr
      \tab \strong{HistoricalEstArrivalTime} \tab The estimated travel time of specific bus stop.\cr
      \tab \strong{HistoricalEstArrivalTimeSD} \tab The standard deviation of the estimated travel time.\cr
      \tab \strong{ServiceTag} \tab It records the time of operation (weekday or weekend)."\cr
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
# routename can be a character
Bus_ScheduleEst(access_token, "Intercity", "0701")

# routename can also be a vector
Bus_ScheduleEst(access_token, "Intercity", c("0701", "0968"))

# routename can be retrieve from the function Bus_Route()
temp=Bus_Route(access_token, "Taipei")
Bus_TravelTime(access_token, "Taipei", unique(temp$RouteName), out="./Taipei_StopTime.csv")
}
\keyword{datasets}

