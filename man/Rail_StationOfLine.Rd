\name{Rail_StationOfLine}
\alias{Rail_StationOfLine}
\docType{data}
\title{
Retrieve the station and sequence of each railway line
}
\description{
The function is used to download the stations of each railway route with sequence. Please note that this function can retrieve the sequence of each railway route, but has no any geometry information. If you want to obtain the geometry information, please use function `Rail_Station()`.
}
\usage{
Rail_StationOfLine(app_id, app_key, operator, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{app_id}{Your APP ID applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{app_key}{Your APP Key applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe `TDX_Railway`. And note that "THSR" is not allowed.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. The directory must contain ".csv" or ".txt" when exporting text file.}
}
\author{
Maintainer: Chia Jung, Yeh  \email{1328robert@gmail.com}
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
Rail_StationOfLine(app_id , app_key, "TRA")
Rail_StationOfLine(app_id , app_key, "TRTC", out="./taipei_mrt_stop.csv")
}
\keyword{datasets}

