\name{Landuse}
\alias{Landuse}
\docType{data}
\title{
Retrieve the landuse data of given month
}
\description{
The function is used to download the landuse data of districts in Taiwan for the specific time, including minimum statistical area, statistical area 1 (SA1), and statistical area 2 (SA2). The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Social Economic Geographic Information System (SEGIS)} by Ministry of the Interior. The data can be pure text or shapefile with the polygon of specific district.
}
\usage{
Population(district, year, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County}, \code{Town}, \code{Village}, \code{SA0} (minimum statistical area), \code{SA1} (statistical area 1), \code{SA2} (statistical area 2).}
  \item{year}{The year of the data. The format of time should be \code{YYYY}. Note that data before year 2014 is not available.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
All the definition of column name is listed [here].
Also, please note the description of each version (year), some of the category was slightly revised.
  \tabular{rlll}{
      \tab \strong{~2015} \tab Number of household.\cr
      \tab \strong{P_CNT} \tab Total population.\cr
      \tab \strong{M_CNT} \tab Total male population.\cr
      \tab \strong{F_CNT} \tab Total female population.\cr
    }

If \code{age} is set \code{TRUE}, then the column name would contain each age. For instance, column "A20A24_M_CNT" means "the population of male in age 20 to 24." Column "A60A64_F_CNT" represents "the population of female in age 60 to 64."
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
Population(district="Village", time="2022-06", age=T, dtype="sf", out="./taiwan_village_population.shp")
}
\keyword{datasets}

