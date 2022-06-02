\name{Ship_Route}
\alias{Ship_Route}
\docType{data}
\title{
Retrieve the detailed information of each ship route
}
\description{
The function is used to download the ship route information by given county, including the route name, port name of OD pair, and the description of the schedule.
}
\usage{
Ship_Route(access_token, county, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe \code{TDX_County}. Note that the county is valid when there are ports in the county.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
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
Ship_Route(access_token, "NewTaipei")
Ship_Route(access_token, "PenghuCounty", out="./Penghu_Ship_Route.csv")
}
\keyword{datasets}

