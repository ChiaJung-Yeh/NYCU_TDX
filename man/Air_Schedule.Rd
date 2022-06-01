\name{Air_Schedule}
\alias{Air_Schedule}
\docType{data}
\title{
Retrieve the schedule of domestic or international airline flights
}
\description{
The function is used to download the schedule of airline. And it supports to download data of domestic and international flights.
}
\usage{
Air_Schedule(access_token, domestic=T, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{domestic}{TRUE for the domestic airline, while FALSE for the international airline.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".csv" or ".txt" when exporting text file.}
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
# Retrieve the schedule of domestic flight
Air_Schedule(access_token, domestic=T)

# Retrieve the schedule of international flight
Air_Schedule(access_token, domestic=F, "./international_flight.csv")
}
\keyword{datasets}

