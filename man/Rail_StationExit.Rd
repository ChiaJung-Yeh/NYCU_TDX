\name{Rail_StationExit}
\alias{Rail_StationExit}
\docType{data}
\title{
Retrieve the station exits of each railway stations
}
\description{
The function is used to download the station exits of each railway stations. The detailed information will be retrieved such as whehter there are stairs, esclators, or elevators in the exit.
}
\usage{
Rail_StationExit(access_token, operator, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}. And note that "THSR" is not allowed.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. The directory must contain ".csv" or ".txt" when exporting text file.}
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
Rail_StationExit(access_token, "TRA")
Rail_StationExit(access_token, "TRTC", out="./taipei_mrt_stationofline.csv")
}
\keyword{datasets}

