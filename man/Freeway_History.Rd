\name{Freeway_History}
\alias{Freeway_History}
\docType{data}
\title{
Retrieve the historical data of freeway
}
\description{
The function is used to download the historical data of freeway. There are 6 types of data, including M03A, M04A, M05A, M06A, M07A, and M08A. The features of each data is summarized below.
  \tabular{rll}{
    \strong{\code{M03A}}   \tab \strong{\emph{traffic flow}} of each gantry for each vehicle type (every 5 minutes)\cr
    \strong{\code{M04A}}   \tab \strong{\emph{average travel time}} of each gantry pair for each vehicle type (every 5 minutes)\cr
    \strong{\code{M05A}}   \tab \strong{\emph{space mean speed}} of each gantry pair for each vehicle type (every 5 minutes)\cr
    \strong{\code{M06A}}   \tab \strong{\emph{path of gantry}} for each vehicle pass through (every 1 hour)\cr
    \strong{\code{M07A}}   \tab \strong{\emph{average travel distance}} of each vehicle type for each entering gantry (every 1 hour)\cr
    \strong{\code{M08A}}   \tab \strong{\emph{numbers of vehicles}} of each gantry pair for each vehicle type (every 5 minutes)\cr
  }
The output of the data (for all columns in M03A~M08A) are listed below.
  \tabular{rll}{
    \strong{\code{TimeInterval}}   \tab time interval of the data (5 minutes or 1 hour)\cr
    \strong{\code{DetectionTime_O}}   \tab the time passing through the first gantry for a trip (used in M06A)\cr
    \strong{\code{DetectionTime_D}}   \tab the time passing through the last gantry for a trip (used in M06A)\cr
    \strong{\code{GantryID}}   \tab the gantry ID, please refer to the gantry data from \code{Freeway_Shape{geotype="gantry"}}\cr
    \strong{\code{GantryID_O}}   \tab the gantry ID passing through the first gantry for a trip (used in M06A)\cr
    \strong{\code{GantryID_D}}   \tab the gantry ID passing through the last gantry for a trip (used in M06A)\cr
    \strong{\code{GantryFrom}}   \tab the gantry ID of upstream\cr
    \strong{\code{GantryTo}}   \tab the gantry ID of downstream \cr
    \strong{\code{Direction}}   \tab \code{N} stands for North; \code{S} stands for South\cr
    \strong{\code{VehicleType}}   \tab  \code{31} stands for passenger car; \code{32} stands for light truck; \code{41} stands for bus; \code{42} stands for truck; \code{5} stands for tractor-trailer\cr
    \strong{\code{Flow}}   \tab the traffic flow the gantry or the gantry pair\cr
    \strong{\code{TravelTime}}   \tab the average travel time of the gantry pair (used in M04A)\cr
    \strong{\code{TripLength}}   \tab the travel distance of the trip (used in M06A)\cr
    \strong{\code{TripDistance}}   \tab the average travel distance of the gantry pair (used in M07A)\cr
    \strong{\code{TripEnd}}   \tab whether the trip is ended (used in M06A)\cr
    \strong{\code{TripInformation}}   \tab all gantry IDs that a trip passing through (used in M06A)\cr
  }

Please check out \href{https://tisvcloud.freeway.gov.tw/documents/TDCS\%E4\%BD\%BF\%E7\%94\%A8\%E6\%89\%8B\%E5\%86\%8Av33.pdf}{here} for the detailed description of each data.
}
\usage{
Freeway_History(file, date, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{file}{\code{M03A}, \code{M04A}, \code{M05A}, \code{M06A}, \code{M07A}, and \code{M08A}.}
  \item{date}{The date to retrieve. The format of date should be \code{YYYY/MM/DD}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. The directory must contain ".csv" or ".txt" when exporting the file.}
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
Freeway_History(file="M03A", date="2022-01-01")
Freeway_History(file="M05A", date="2018-01-01", out="./M05A_2018-01-01.csv")
}
\keyword{datasets}

