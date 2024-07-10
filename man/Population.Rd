\name{Population}
\alias{Population}
\docType{data}
\title{
Retrieve the demographic data with given month
}
\description{
The function is used to download the demographic data of districts in Taiwan for the specific time. Level of districts include county, town, village, minimum statistical area (SA0), statistical area 1 (SA1), and statistical area 2 (SA2). The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Social Economic Geographic Information System (SEGIS)} by Ministry of the Interior.
}
\usage{
Population(district, time, age=F, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County}, \code{Town}, \code{Village}, \code{SA0} (minimum statistical area), \code{SA1} (statistical area 1), \code{SA2} (statistical area 2).}
  \item{time}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. Note that demographic data for `County`, `Town`, and `Village` is updated every three months, only March, June, September, and December are provided. Data for `SA0`, `SA1`, and `SA2` is updated half a year, only June and December are provided. If the argument is blank (\code{NULL}), the latest data will be retrieved.}
  \item{age}{If the population of each age (5-year-old interval) is required, please set \code{TRUE}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{H_CNT} \tab Number of household.\cr
      \tab \strong{P_CNT} \tab Total population.\cr
      \tab \strong{M_CNT} \tab Total male population.\cr
      \tab \strong{F_CNT} \tab Total female population.\cr
    }

If \code{age} is set \code{TRUE}, then the column name would contain each age interval. For instance, column "A20A24_M_CNT" means "the population of male in age 20 to 24." Column "A60A64_F_CNT" represents "the population of female in age 60 to 64."
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
Population(district="County", time="2022-06")
Population(district="SA0", time="2022-06", age=T, dtype="sf", out="./taiwan_village_population.shp")
}
\keyword{datasets}

