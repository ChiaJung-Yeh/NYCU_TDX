\name{Freeway_Shape}
\alias{Freeway_Shape}
\docType{data}
\title{
Retrieve the shape of freeway
}
\description{
The function is used to download the shape of freeway (also includes expressway). There are four types of data, including \code{section}, \code{link}, \code{gantry}, and \code{gantryod}. The detailed settings of these arguments, please find the content below. Note that this data is not retrieved from TDX, but from \href{https://tisvcloud.freeway.gov.tw/}{Freeway Bureau, MOTC}, and there is no need to use access token. In addition, please note that the etag gantries are only set up in freeway 1, 3, 5, and thus, there is no data on other freeway and expresseay for \code{gantry} and \code{gantryod}.
}
\usage{
Freeway_Shape(geotype, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{geotype}{Please fill in the geometry type of freeway. The argument includes \code{section}, \code{link}, \code{gantry}, and \code{gantryod}. \code{section} means the section between each adjacent interchanges. \code{link} means the detailed geometry in each section. \code{gantry} is "POINT" geometry type, which shows the location of etag gantry. \code{gantryod} repesents the "LINE" shape of each adjacent etag gantries.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}. Note that all of the geometry type is "LINESTRING", expect for the \code{gantry} is "POINT".}
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
Freeway_Shape(geotype="link")
Freeway_Shape(geotype="gantryod", dtype="sf", out="./gantryod.shp")
}
\keyword{datasets}

