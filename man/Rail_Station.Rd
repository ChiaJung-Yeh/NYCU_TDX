\name{Rail_Station}
\alias{Rail_Station}
\docType{data}
\title{
Retrieve the station and geometry of each railway line
}
\description{
The function is used to download the stations of each railway route with geometry.
}
\usage{
Rail_Station(access_token, operator, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}.}
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
Rail_Station(access_token, "THSR")
Rail_Station(access_token, "TRTC", dtype="sf", out="./taipei_mrt_stop.shp")
}
\keyword{datasets}

