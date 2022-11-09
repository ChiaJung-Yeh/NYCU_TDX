\name{District}
\alias{District}
\docType{data}
\title{
Retrieve the geometry of district
}
\description{
The function is used to download the geometry of districts in Taiwan, including city, town, and village.
}
\usage{
District(access_token, district, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{district}{Please fill in the district type. The valid input includes \code{City}, \code{Town}, and \code{Village}.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
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
District(access_token, "City")
District(access_token, "Village", dtype="sf", out="./taiwan_village.shp")
}
\keyword{datasets}

