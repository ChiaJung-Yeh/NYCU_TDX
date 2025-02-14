\name{Rail_Patronage}
\alias{Rail_Patronage}
\docType{data}
\title{
Retrieve the patronage of railway system.
}
\description{
The function is used to download the patronage data of railway system. This is now currently available for Taiwan Railway (TRA), Taiwan High Speed Rail (THSR), Taipei MRT (TRTC), Kaoshiung MRT (KRTC), and Tayouan Airport MRT (TYMC)!"
}
\usage{
Rail_Patronage(operator, ym, OD, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}. This is currently now only for Taiwan Railway (TRA) and Taipei MRT (TRTC).}
  \item{ym}{The time (year-month) of the data. The format of time should be \code{YYYY-MM}. This argument would be deprecated when data across all the time frame are avialable, such as TRA, THSR, TYMC.}
  \item{OD}{If the OD data is required, please specified 'TRUE'; otherwise, the total station patronage will be downloaded. This is currently only available for Taipei MRT (TRTC).}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{gateIn} \tab Station name of inbound.\cr
      \tab \strong{gateOut} \tab Station name of outbound.\cr
      \tab \strong{gateInComingCnt} \tab Total patronage of inbound.\cr
      \tab \strong{gateOutGoingCnt} \tab Total patronage of outbound.\cr
    }
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
# Download the total patronage data for Taiwan Railway
Rail_Patronage(operator="TRA", ym="2024-01", OD=F)

# Download the OD data for Taipei MRT
Rail_Patronage(operator="TRTC", ym="2024-01", OD=T, out="TRTC_OD.csv")
}
\keyword{datasets}

