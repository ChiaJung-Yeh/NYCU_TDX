\name{ScenicSpot}
\alias{ScenicSpot}
\docType{data}
\title{
Retrieve the geometry of tourism scenic spot
}
\description{
The function is used to download the data of scenic spot, including the spot name, address and geometry.
}
\usage{
Bike_Station(app_id, app_key, county, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{app_id}{Your APP ID applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{app_key}{Your APP Key applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe `TDX_County`. Note that some of cities have no bike sharing system, please check it out first.}
  \item{dtype}{The class of data, including "text" (data frame) and "sf" (simple feature). The default value is "text".}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
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
Bike_Station(app_id , app_key, "Taipei")
Bike_Station(app_id , app_key, "Hsinchu", dtype="sf", out="./hsinchu_youbike.shp")
}
\keyword{datasets}

