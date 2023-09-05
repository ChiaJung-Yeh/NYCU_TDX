\name{Rail_Shape}
\alias{Rail_Shape}
\docType{data}
\title{
Retrieve the shape of railway
}
\description{
The function is used to download the shape of railway. Please fill in the \code{operator} to retrieve its geometry.
}
\usage{
Rail_Shape(access_token, operator, dates=F, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}.}
  \item{dates}{If historical data would be retrieved, this argument should be set; otherwise the default value is FALSE. The date format should be \code{YYYY-MM-DD}.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
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
Rail_Shape(access_token, "TRA")
Rail_Shape(access_token, "TRA", dtype="sf", out="./TRA.shp")

# historical data
Rail_Shape(access_token, "TRTC", "2022-01-01", dtype="sf")
}
\keyword{datasets}

