\name{VehicleOwn}
\alias{VehicleOwn}
\docType{data}
\title{
Retrieve the number of registrations for each vehicle type
}
\description{
The function is used to retrieve the number of registrations for each vehicle type by town.
}
\usage{
VehicleOwn(ym, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{ym}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. The data is only available for year after 2016.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{VehicleType} \tab Types of vehicle.\cr
      \tab \strong{VehicleCount} \tab Numbers of vehicle for each type.\cr
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
# Retrieve the vehicle registrations for a specific month
VehicleOwn(ym="2024-01")
}
\keyword{datasets}

