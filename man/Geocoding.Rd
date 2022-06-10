\name{Geocoding}
\alias{Geocoding}
\docType{data}
\title{
Geocoding from address to geometry
}
\description{
The function is used to conduct geocoding via TDX API. The result summary would be shown after geocoding, including number of success, fail of geocoding, as well as number of the duplicated address. Note that if the warning \emph{"Reconnect!"} is shown after the progress bar, it does not imply that the address cannot be geocoded, but due to the "Request Timeout" error. The function would then automatically reconnect to TDX API.
}
\usage{
Geocoding(access_token, address, dtype="text", out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{address}{Place the address in vector form.}
  \item{dtype}{The class of data, including \code{text} (data frame) and \code{sf} (simple feature). The default value is \code{text}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".shp" when exporting simple feature. And the directory must contain ".csv" or ".txt" when exporting text file.}
}
\value{
The output would store the data frame of success geocoding result (\code{DATA$SUCCESS}), and a character with fail geocoding address (\code{DATA$FAIL}).

Some columns definition of success result are listed below.
  \tabular{rlll}{
      \tab \strong{AdressOriginal}   \tab Address filled in.\cr
      \tab \strong{AddressNew} \tab Address retrieved by the API.\cr
    }
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
# The ADDRESS should be typed in Chinese or a vector of Chinese address
address_geocoding=Geocoding(access_token, "ADDRESS")

# show the success geocoding result
address_geocoding$SUCCESS
}
\keyword{datasets}

