\name{Population}
\alias{Population}
\docType{data}
\title{
Retrieve the demographic data with given month
}
\description{
The function is used to download the demographic data of districts in Taiwan, including County, town, and village.
}
\usage{
Population(district, time, age=F, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County}, \code{Town}, and \code{Village}.}
  \item{time}{The time (year-month) of the data. Note that the demographic data is updated every three months, thus, only March, June, September, and December are provided. The format of time should be \code{YYYY-MM}.}
  \item{age}{If the population of each age is required, please set \code{TRUE}.}
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

If \code{age} is set \code{TRUE}, then the column name would contain each age. For instance, column "A20A24_M_CNT" means "the population of male in age 20 to 24."
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
Population(district="County", time="2022-06")
Population(district="Village", time="2022-06", out="./taiwan_village_population.shp")
}
\keyword{datasets}

