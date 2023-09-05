\name{Rail_ODFare}
\alias{Rail_ODFare}
\docType{data}
\title{
Retrieve the fare of each OD pair of railway system
}
\description{
The function is used to retrieve the fare of each OD pair of railway system. Data includes the station ID and name of the OD pair, train type, ticket type, fare class, price, and so forth.
}
\usage{
Rail_ODFare(access_token, operator, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{access_token}{Your access token given by TDX platform. This can be derived from the function \code{get_token()}.}
  \item{operator}{Please fill in the operator code of given railway system. Checking out the appropriate operator code in dataframe \code{TDX_Railway}.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data. Note that the directory must contain ".csv" or ".txt" when exporting text file.}
}
\value{
Some columns definition are listed below.
  \tabular{rlll}{
      \tab \strong{OriginStationID}   \tab Origin station of the OD pair.\cr
      \tab \strong{DestinationStationID}   \tab Destination station of the OD pair.\cr
      \tab \strong{TrainType} \tab Type of train, including local train and express train. See the code table below.\cr
      \tab \strong{TicketType} \tab Type of ticket, including one-way ticket, smart card, commuter ticketand so forth. See the code table below.\cr
      \tab \strong{FareClass} \tab Fare class of the ticket, including adult, student, child, and so forth. See the code table below.\cr
      \tab \strong{CabinClass} \tab Class of the cabin, including standard class, business class, and so forth."\cr
      \tab \strong{Price} \tab Price of the OD fare under specific train type, ticket type, fare class, and cabin class."\cr
    }

Codes of \strong{TrainType} are listed below.
  \tabular{rll}{
    \strong{0}   \tab No distinguishment\cr
    \strong{1}   \tab Local train\cr
    \strong{2}   \tab Express train\cr
  }

Codes of \strong{TicketType} are listed below.
  \tabular{rll}{
    \strong{1}   \tab One-way ticket\cr
    \strong{2}   \tab Round-trip ticket\cr
    \strong{3}   \tab Smart card\cr
    \strong{4}   \tab Multi-ride ticket\cr
    \strong{5}   \tab 30 days periodic ticket\cr
    \strong{6}   \tab 60 days periodic ticket\cr
  }

Codes of \strong{FareClass} are listed below.
  \tabular{rll}{
    \strong{1}   \tab Adult\cr
    \strong{2}   \tab Student\cr
    \strong{3}   \tab Child\cr
    \strong{4}   \tab Senior\cr
    \strong{5}   \tab Disabled\cr
    \strong{6}   \tab Disabled Child\cr
    \strong{7}   \tab Disabled accompany\cr
    \strong{8}   \tab Group\cr
  }

Codes of \strong{CabinClass} are listed below.
  \tabular{rll}{
    \strong{1}   \tab Standard class\cr
    \strong{2}   \tab Business class\cr
    \strong{3}   \tab Non-reserved car\cr
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
# retrieve the general time table of TRA
Rail_ODFare(access_token, operator="TRA")

# retrieve the station time table of TRTC (Taipei Metro)
Rail_ODFare(access_token, operator="TRTC")
}
\keyword{Road_Network}

