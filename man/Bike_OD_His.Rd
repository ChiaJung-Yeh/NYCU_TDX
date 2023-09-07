\name{Bike_OD_His}
\alias{Bike_OD_His}
\docType{data}
\title{
Retrieve the historical OD data of bike sharing system in Taipei
}
\description{
The function is used to download the historical OD data of bike sharing system in Taipei City in given month. Note that data of YouBike 1.0 (bikesys=1) starts from 2016-01 to 2022-09, while data of YouBike 2.0 (bikesys=2) starts from 2020-04 to date.
}
\usage{
Bike_OD_His(bikesys, time, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{bikesys}{Please fill the valid YouBike system. It should me either 1 or 2.}
  \item{time}{Please fill the specific year and month. The format of the date should be \code{YYYY-MM}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".csv" or ".txt" when exporting text file.}
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
Bike_OD_His(bikesys=1, time="2022-10")
Bike_OD_His(bikesys=2, time="2023-05", out="./taipei_youbike_OD.csv")
}
\keyword{datasets}

