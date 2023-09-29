\name{Landuse}
\alias{Landuse}
\docType{data}
\title{
Retrieve the landuse data of given month
}
\description{
The function is used to download the landuse data of districts in Taiwan for the specific time, including minimum statistical area (SA0), statistical area 1 (SA1), and statistical area 2 (SA2). The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Social Economic Geographic Information System (SEGIS)} by Ministry of the Interior. The data can be pure text or shapefile with the polygon of specific district.
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
All the definition of column name is listed \href{https://chiajung-yeh.github.io/TDX_Guide/landuse_name.pdf}{here}.

Also, please note the description of each version (year) in corresponding documents listed below. Some of the category were slightly revised.
  \tabular{rlll}{
  \tab \strong{Year} \tab \strong{Document}\cr
      \tab \strong{~2015} \tab \href{https://ws.moi.gov.tw/Download.ashx?u=LzAwMS9VcGxvYWQvNDA3L3JlbGZpbGUvOTI3My8zMTg4LzAxOTg5ZDY1LTQ2NDEtNGJiMi1hZjQzLWM2YmZlYWM4YTE0OS5wZGY\%3d&n=Q2xhc3NMb29rVXBfOTUucGRm}{Land Use Classification 95-104}\cr
      \tab \strong{2016~2019} \tab \href{https://ws.moi.gov.tw/Download.ashx?u=LzAwMS9VcGxvYWQvNDA3L3JlbGZpbGUvOTI3My8zMTkyL2I2N2ZhZWIxLTY5ODctNGJkNS1iZGIwLWEzNThlMzhhOTY4OC5wZGY\%3d&n=MTA05Zyf5Zyw5Yip55So5YiG6aGe57O757Wx6KGoLnBkZg\%3d\%3d}{Land Use Classification 105-108}\cr
      \tab \strong{2020~} \tab \href{https://ws.moi.gov.tw/Download.ashx?u=LzAwMS9VcGxvYWQvNDA3L3JlbGZpbGUvOTI3My8zMTkzLzIxOGM3MWI3LTU2NTctNDg4NC1hOWEyLTU1Zjk1MjhmOWFkNi5wZGY\%3d&n=5Zyf5Zyw5Yip55So5YiG57Sa5YiG6aGe57O757Wx6KGo6Zm45Z\%2bf6YOo5Lu9LnBkZg\%3d\%3d}{Land Use Classification 109-}\cr
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
Landuse(district="SA0", year=2022)
Landuse(district="SA1", year=2021, dtype="sf", out="./taiwan_landuse.shp")
}
\keyword{datasets}

