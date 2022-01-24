\name{Geocoding}
\alias{Geocoding}
\docType{data}
\title{
Geocoding from address to geometry
}
\description{
The function is used to conduct geocoding via the MOTC API.
}
\usage{
Geocoding(address, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{address}{Place the address in vector form.}
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
Geocoding("ADDRESS")
}
\keyword{datasets}

