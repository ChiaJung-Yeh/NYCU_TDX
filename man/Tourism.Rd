\name{Tourism}
\alias{Tourism}
\docType{data}
\title{
Retrieve the geometry of tourism
}
\description{
The function is used to download the tourism data, including scenic spot, restaurant, and hotel. But note that the data is not collected completely, please take it as reference. Point of interests can be downloaded from other platform or map for relatively complete infromation.
}
\usage{
Tourism(access_token, county, poi, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}.}
  \item{poi}{Type of the point of interest, including \code{ScenicSpot}, \code{Restaurant}, and \code{Hotel}.}
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
Tourism(access_token, "Taipei", poi="Hotel")
Tourism(access_token, "Hsinchu", poi="Restaurant", dtype="sf", out="./hsinchu_restaurant.shp")
}
\keyword{datasets}

