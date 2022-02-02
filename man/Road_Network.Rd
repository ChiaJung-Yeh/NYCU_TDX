\name{Road_Network}
\alias{Road_Network}
\docType{data}
\title{
Retrieve the geometry of road network
}
\description{
The function is used to download the road network in Taiwan. This API can only retrieve the National Highway (RoadClass:0), Provincial Expressway (RoadClass:1), and Provincial Highway (RoadClass:3).
}
\usage{
Road_Network(county, roadclass, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe `TDX_County`. Note that if 'ALL' is filled in, it would retrieve the geometry of all provided road network in Taiwan.}
  \item{roadclass}{Please fill in the road class code of the network. Checking out the appropriate road class code in dataframe `TDX_RoadClass`. Note that if 'ALL' is used, it would retrieve all the road class.}
    \item{dtype}{The class of data, including "text" (data frame) and "sf" (simple feature). The default value is "text".}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}}
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
# retrieve the National Highway in Taipei
Road_Network("Taipei", 0)

# retrieve all road network in Hsinchu
Road_Network("Hsinchu", "ALL", dtype="sf", out="./HsinchuRoadNetwork.shp")

# retrieve all Provincial Highway in Taiwan
Road_Network("ALL", 3)

# retrieve all road network in Taiwan
Road_Network("ALL", "ALL", dtype="sf", out="./TaiwanRoadNetwork.shp")
}
\keyword{Road_Network}

