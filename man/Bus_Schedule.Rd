\name{Bus_Schedule}
\alias{Bus_Schedule}
\docType{data}
\title{
Retrieve the schedule of each bus routes
}
\description{
The function is used to download the bus schedule. And it supports to download data of city buses and intercity buses. Note that the schedule is recorded in two forms, including timetable and frequency. Some cities (e.g., Taipei) record in both two methods, and they wold be merged together in this function. Most of the citiess only record in the schedule form.
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
  \item{county}{Please fill in the English county name. Checking out the appropriate county name in dataframe `TDX_County`. Note that if 'Intercity' is filled in, it would retrieve the shape of intercity buses in Taiwan.}
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
Bus_Schedule(app_id , app_key, "Hsinchu", out="./Hsinchu_Schedule.csv")
}
\keyword{datasets}

