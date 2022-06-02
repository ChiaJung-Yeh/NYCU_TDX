\name{Bus_Schedule}
\alias{Bus_Schedule}
\docType{data}
\title{
Retrieve the schedule of each bus routes
}
\description{
The function is used to download the bus schedule. And it supports to download data of city buses and intercity buses. Note that the schedule is recorded in two forms, including timetable and frequency. Some cities (e.g., Taipei) record in both two methods, and they would be merged together in this function. Most of the cities only record in the schedule form.
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
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{RouteUID}   \tab It contains every sub-routes and directions in a same RouteUID.\cr
      \tab \strong{SubRouteUID} \tab If the direction or sub-route is not the same, a different SubRouteUID must be recorded.\cr
      \tab \strong{TripID} \tab It is used to record the sequence of Trips. It only occurs when the schedule is recorded in timetable format.\cr
      \tab \strong{Sunday} \tab If the trip is operated in that day, it is recorded in 1; ortherwise, 0. \emph{Other days are the same.}\cr
      \tab \strong{Date} \tab Use to record some special hoildays.\cr
      \tab \strong{Service Status} \tab Use to record the service status in special hoildays, including "Operating As Usual" and "Cancelling Operation."\cr
      \tab \strong{ArrivalTime} \tab The arrival time of the bus. This is only recorded in timetable format.\cr
      \tab \strong{DepartureTime} \tab The departure time of the bus. This is only recorded in timetable format.\cr
      \tab \strong{StartTime} \tab The start time of the specific frequency data. This is only recorded in frequency format.\cr
      \tab \strong{EndTime} \tab The end time of the specific frequency data. This is only recorded in frequency format.\cr
      \tab \strong{MinHeadwayMins} \tab The minimum headway of the specific frequency data. This is only recorded in frequency format.\cr
      \tab \strong{MaxHeadwayMins} \tab The maximum headway of the specific frequency data. This is only recorded in frequency format.\cr
    }

Codes of Direction are listed below.
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
Bus_Schedule(access_token, "Taipei")
Bus_Schedule(access_token, "Hsinchu", out="./Hsinchu_Schedule.csv")
}
\keyword{datasets}

