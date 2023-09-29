\name{Hospital}
\alias{Hospital}
\docType{data}
\title{
Retrieve the hospital data with given month
}
\description{
The function is used to download the hospital data of districts in Taiwan for the specific time, including county, town, minimum statistical area (SA0), statistical area 1 (SA1), and statistical area 2 (SA2). The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Social Economic Geographic Information System (SEGIS)} by Ministry of the Interior. The data can be pure text or shapefile with the polygon of specific district.
}
\usage{
Hospital(district, time, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County}, \code{Town}, \code{SA0} (minimum statistical area), \code{SA1} (statistical area 1), \code{SA2} (statistical area 2).}
  \item{time}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. Note that hospital data is updated half a year, only June and December are provided.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{H_CNT} \tab Number of hospitals.\cr
      \tab \strong{H_BED} \tab Number of beds.\cr
      \tab \strong{H_SRVP} \tab Number of people served per hospital.\cr
      \tab \strong{H_SRVB} \tab Number of beds per 1000 people.\cr
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
Hospital(district="County", time="2010-06")
Hospital(district="SA2", time="2022-06", age=T, dtype="sf", out="./taiwan_SA2_hospital.shp")
}
\keyword{datasets}

