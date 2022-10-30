\name{Car_Park}
\alias{Car_Park}
\docType{data}
\title{
Retrieve the parking lot information and location
}
\description{
The function is used to download the datail description and location of parking lot (off street) or side parking (on street) given by a specific county.
}
\usage{
Car_Park(access_token, county, street, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}.}
  \item{street}{Please fill in \code{on} or \code{off}. 'On street' means the parking area is along the road, while 'off street' represents the parking lot. Note that there are few counties provide on street parking data.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\value{
Some columns definition in off street data are listed below.
  \tabular{rlll}{
      \tab \strong{Description}   \tab Detailed information regarding number of parking grid for each type of vehicles (in words).\cr
      \tab \strong{FareDescription} \tab Detailed information regarding the fare (in words).\cr
      \tab \strong{IsPublic} \tab Whether it is public-owned, if yes, recording "1".\cr
      \tab \strong{OperationType} \tab The operation type of the parking lot, including public-to-private (1), public-operated (2), private-operated (3).\cr
      \tab \strong{LiveOccuppancyAvailable} \tab Whether it provides the in-time occupancy information, if yes, recording "1".\cr
      \tab \strong{EVRechargingAvailable} \tab Whether it owns eletric vehicle charging facilities, if yes, recording "1".\cr
      \tab \strong{MonthlyTicketAvailable} \tab Whether it provides monthly ticket service, if yes, recording "1".\cr
      \tab \strong{SeasonTicketAvailable} \tab Whether it provides season ticket service, if yes, recording "1".\cr
      \tab \strong{ReservationAvailable} \tab Whether it provides reservation service, if yes, recording "1".\cr
      \tab \strong{WheelchairAccessible} \tab Whether it provides accessible facilities, if yes, recording "1".\cr
      \tab \strong{OvernightPermitted} \tab Whether it allows parking overnight, if yes, recording "1".\cr
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
Car_Park(access_token, "Taipei", street="off")
Car_Park(access_token, "Tainan", street="on", dtype="sf", out="./TainanParking.shp")
}
\keyword{datasets}

