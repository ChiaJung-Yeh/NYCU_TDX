\name{School}
\alias{School}
\docType{data}
\title{
Retrieve different level of school in Taiwan
}
\description{
The function is used to download all the level of schools in Taiwan.
}
\usage{
School(type, year, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{type}{Level of school. It should be \code{elementary}, \code{junior}, \code{senior}, or \code{university}.}
  \item{year}{Please fill in the year of required data. It shold be the year after 2010.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\value{
All the definition of column name is listed \href{https://chiajung-yeh.github.io/TDX_Guide/school_name.pdf}{here}.
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
School("university", 2022)
School("junior", 2020, dtype="sf", out="./taiwan_school.shp")
\keyword{datasets}

