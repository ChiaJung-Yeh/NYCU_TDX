\name{Air_Patronage}
\alias{Air_Patronage}
\docType{data}
\title{
Retrieve the patronage of international flight
}
\description{
The function is used to retrieve the patronage of international (including cross-strait) flight for each airline in Taiwan.
}
\usage{
Air_Patronage(ym, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{ym}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. The data is only available for year after 2009.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Meanings of prefix of the column name are summarised below.
  \tabular{rlll}{
      \tab \strong{Flight} \tab Number of flights for each airline.\cr
      \tab \strong{Seat} \tab Total seats available for each airline.\cr
      \tab \strong{Patronage} \tab Patroange for each airline.\cr
      \tab \strong{LoadFactor} \tab The ratio of patroange to total seats provided.\cr
    }
Meanings of suffix of the column name are summarised below.
  \tabular{rlll}{
      \tab \strong{Total} \tab The summation of departure and arrival.\cr
      \tab \strong{Departure} \tab The outbound trips.\cr
      \tab \strong{Arrival} \tab The inbound trips.\cr
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
# Retrieve the patronage for each airline in a specific month
Air_Patronage(ym="2024-12")
}
\keyword{datasets}

