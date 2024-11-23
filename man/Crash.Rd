\name{Crash}
\alias{Crash}
\docType{data}
\title{
Retrieve the crash data with given month or year
}
\description{
The function is used to download the crash data in Taiwan for the specific month or year.
}
\usage{
Crash(access_token, crash, county, time, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{crash}{Please fill in the crash type, including \code{A1} (crash with death within 24 hours), \code{A2} (crash with injury-only or death over 24 hours), and \code{A30} (crash with death over 30 days).}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}.}
  \item{time}{The time (year-month) of the data. The format of time should be \code{YYYY} or \code{YYYY-MM}.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}. Note that only crash data (T1) can be exported to shapefile, while person data (T2) remains text file.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory does not need to contain ".shp", ".csv", or ".txt". Note that if \code{dtype} is \code{sf}, then name of crash data (T2) would be ended with ".shp".}
}
\value{
The result would be a list containing crash data (T1) and person data (T2). Please use \code{$T1} to retrieve crash data, and use \code{$T2} to retrieve person data. Note that crash with death over 24 days (A30) only retrieves T2 data.

Columns and coding definition can be found by using \code{$DEF}.
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
#---download death-involved crash data of Taipei---#
crash_taipei=Crash(access_token, crash="A1", county="Taipei", time="2020-05", dtype="sf")
# retrieve crash data
crash_taipei$T1
# retrieve person data
crash_taipei$T2
# retrieve coding definition
crash_taipei$DEF

#---download injury-only crash data of Yilan---#
crash_yilan=Crash(access_token, crash="A2", county="YilanCounty", time="2021-06", dtype="sf", out="./crash_yilan")
}
\keyword{datasets}

