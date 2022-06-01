\name{Rail_TimeTable}
\alias{Rail_TimeTable}
\docType{data}
\title{
Retrieve the timetable of railway system
}
\description{
The function is used to download the timetable of railway system. There are two types of format of time table, general and station. \code{general} means to record the time table by each train No., while \code{station} represents record the time table based on each station. Please look up the "record" arguments below to find out details of settings.
}
\usage{
Rail_TimeTable(access_token, operator, record, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}.}
  \item{record}{The types of format of time table, including \code{general} and \code{station}. \code{general} means to record the time table by each train No., while \code{station} represents record the time table based on each station. TRA provides both format of time table. THSR only provides "general" format. All MRT system only provides \code{station} based time table, but note that \code{TMRT} (Taichung MRT) does not provide any time table. Also please note that Wenhu Line does not provide station time table in \code{TRTC} (Taipei Metro).}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".csv" or ".txt" when exporting text file.}
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
# retrieve the general time table of TRA
Rail_TimeTable(access_token, operator="TRA", record="general")

# retrieve the station time table of TRTC (Taipei Metro)
Rail_TimeTable(access_token, operator="TRTC", record="station")
}
\keyword{Road_Network}

