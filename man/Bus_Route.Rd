\name{Bus_Route}
\alias{Bus_Route}
\docType{data}
\title{
Retrieve the detailed information of each bus route
}
\description{
The function is used to download the bus route information, including the route name, subroute name, bus route type, direction, departure and destination stop name.

The output code of direction is listed below.

  \tabular{rlll}{
    0   \tab outbound\cr
    1   \tab inbound\cr
    2   \tab loop\cr
    255   \tab unknown\cr
  }

The output code of bus route type is listed below.

  \tabular{rlll}{
    11   \tab City Bus\cr
    12   \tab Intercity Bus (Normal)\cr
    13   \tab Intercity Bus (Freeway)\cr
    14   \tab Feeder bus\cr
  }

Note that it supports to download data of city buses and intercity buses.

}
\usage{
Bus_Schedule(app_id, app_key, county, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{app_id}{Your APP ID applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{app_key}{Your APP Key applied from \href{https://ptx.transportdata.tw/PTX/}{PTX platform}.}
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe `TDX_County`. Note that if 'Intercity' is filled in, it would retrieve the bis route of intercity buses in Taiwan.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
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
Bus_Schedule(app_id , app_key, "Taipei")
Bus_Schedule(app_id , app_key, "Hsinchu", out="./Hsinchu_Bus_Route.csv")
}
\keyword{datasets}

