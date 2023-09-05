\name{gtfs}
\alias{gtfs}
\docType{data}
\title{
Retrieve the General Transit Feed Specification (GTFS) of the specific mode.
}
\description{
General Transit Feed Specification (GTFS) is a standard used information about public transport. It allows public transport agencies to publish data in a specific format that can be consumed by a wide variety of software applications. GTFS consists of two main parts: GTFS Schedule and GTFS Realtime. GTFS Schedule contains information about routes, schedules, fares, and geographic details. GTFS Realtime contains trip updates, vehicle positions, and service alerts. The function is used to download the GTFS data of a specific mode.
}
\usage{
gtfs(access_token, mode, county=F, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{mode}{Please fill in a specific mode, \code{Bus}, \code{TRA}, or \code{THSR}. If it is \code{Bus}, the argumnent \code{county} should be set up to retrieve GTFS data of specific county. If the mode is railway system, the argumnent \code{county} is muted.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. Note that some of cities have no cycling path, please check it out first.}
  \item{out}{If the result should be exported to the local, please fill in the directory \strong{\emph{(no need file extension)}}. The default value is FALSE, which does not export the data. All data would be stored as 'txt' file in a directory if they are exported.}
}
\value{
The result will be present in a list of dataframe, including agency, calendar, calendar_dates, frequencies, routes, shapes, stop_times, stops, trips.
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
gtfs_tra=gtfs(access_token, "TRA")
gtfs_tra$agency
gtfs_tra$frequencies
}
\keyword{datasets}

