\name{Bus_Shape}
\alias{Bus_Shape}
\docType{data}
\title{
Retrieve the geometry of bus routes
}
\description{
The function is used to download the geometry of bus routes. And it supports to download data of city buses and intercity buses.
}
\usage{
Bus_Shape(app_id, app_key, county)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{app_id}{Your APP ID applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{app_key}{Your APP Key applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe `TDX_County`. Note that if 'Intercity' is filled in, it would retrieve the shape of intercity buses in Taiwan.}
  \item{dtype}{The class of data, including "text" (data frame) and "sf" (simple feature). The default value is "text".}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.} which does not export the data.}
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
Bus_Shape(app_id , app_key, "Hsinchu")
Bus_StopOfRoute(app_id , app_key, "Taipei", dtype="sf", out="./Taipei_Stop.shp")
}
\keyword{datasets}

