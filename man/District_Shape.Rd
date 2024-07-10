\name{District_Shape}
\alias{District_Shape}
\docType{data}
\title{
Retrieve the geometry of district
}
\description{
The function is used to download the geometry of districts in Taiwan, including County, town, village, and all the statistical level (SA2, SA1, and SA0).
}
\usage{
District_Shape(district, time=NULL, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{district}{Please fill in the district type. The valid input includes \code{County}, \code{Town}, \code{Village}, \code{SA2}, \code{SA1}, and \code{SA0}.}
    \item{time}{Please fill in the time of the data retrieved for statistical area (SA0, SA1, SA2). All statistical area will be downloaded based on the specified time. If the time does not match any in the dataset, the closest time of data will be downloaded. See \href{https://raw.githubusercontent.com/ChiaJung-Yeh/NYCU_TDX/main/others/statistical_area.csv}{here} for all available time of dataset. Note that all county, town and village data will be the latest version, leave this argument blank (\code{NULL}); otherwise, this argument will be deprecated. The format of time should be \code{YYYY-MM}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature.}
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
District_Shape("County")
District_Shape("SA0", out="./taiwan_SA0.shp")
}
\keyword{datasets}

