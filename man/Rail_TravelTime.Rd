\name{Rail_TravelTime}
\alias{Rail_TravelTime}
\docType{data}
\title{
Retrieve the travel time of each OD of railway system
}
\description{
The function is used to download the travel time of each OD for a given railway system. There are two types of format of time table, general and station. \code{general} means to record the time table by each train No., while \code{station} represents record the time table based on each station. Please look up the "record" arguments below to find out details of settings.
}
\usage{
Rail_TravelTime(access_token, operator, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".csv" or ".txt" when exporting text file.}
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
Rail_TravelTime(access_token, operator="TRTC")
}
\keyword{Road_Network}

