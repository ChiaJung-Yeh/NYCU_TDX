\name{Travel_Survey}
\alias{Travel_Survey}
\docType{data}
\title{
Retrieve the travel survey data
}
\description{
The function is used to retrieve the household travel survey data conducted by MOTC each year starting from 2011. This data includes the mode share (\code{ModeShare}), trip purpose (\code{TripPurpose}), and the proportion of trips versus staying at home (\code{OutTrip}).
}
\usage{
Travel_Survey(dtname=c("ModeShare","TripPurpose","OutTrip"), out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{dtname}{The name of the data. This includes \code{ModeShare}, \code{TripPurpose}, and \code{OutTrip}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{Proportion} \tab Proportion of trips, such as mode share, proportion of trip purpose, and proportion of trips versus staying at home.\cr
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
# Retrieve all available travel survey data
Travel_Survey(out="TravelSurvey.csv")
}
\keyword{datasets}

