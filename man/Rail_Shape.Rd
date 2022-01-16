\name{Rail_Shape}
\alias{Rail_Shape}
\docType{data}
\title{
Retrieve the shape of railway
}
\description{
The function is used to download the shape of railway. Please fill in the operator to retrieve its geometry.
}
\usage{
Rail_Shape(app_id, app_key, operator, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{app_id}{Your APP ID applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{app_key}{Your APP Key applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe `TDX_Railway`.}
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
Rail_Shape(app_id , app_key, "TRA")
Rail_Shape(app_id , app_key, "TRA", dtype="sf", out="./TRA.shp")
}
\keyword{datasets}

