\name{House_Price}
\alias{House_Price}
\docType{data}
\title{
Retrieve the house price with given year and season
}
\description{
The function is used to download the house price data for given . The data is downloaded from \href{https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx}{Real Estate Deal Price Website} by Ministry of the Interior. The data can be pure text or shapefile with the polygon of specific district.
}
\usage{
House_Price(year, season, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{year}{Select the year (after 2012).}
  \item{season}{Select the season (must be 1, 2, 3, or 4).}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition are summarized \href{https://chiajung-yeh.github.io/TDX_Guide/houseprice_name.pdf}{here}.
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
House_Price(2023, 2)
}
\keyword{datasets}

