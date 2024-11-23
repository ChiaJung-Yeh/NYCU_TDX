\name{CellularPopulation}
\alias{CellularPopulation}
\docType{data}
\title{
Retrieve the population of activity and stay with given month
}
\description{
The function is used to download the population of activity and stay for the specific time and district from the cellular data. Level of districts include county and town. The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Social Economic Geographic Information System (SEGIS)} by Ministry of the Interior.
}
\usage{
CellularPopulation(district, time, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County} and \code{Town}.}
  \item{time}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. If the argument is blank (\code{NULL}), the latest data will be retrieved.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
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
CellularPopulation(district="County", time="2020-11")
CellularPopulation(district="Town", time="2023-11", age=T, out="./town_population.csv")
}
\keyword{datasets}

