\name{Bus_OperationKPI}
\alias{Bus_OperationKPI}
\docType{data}
\title{
Retrieve the operaitonal data of buses
}
\description{
The function is used to retrieve the operaitonal data of all buses, including city buses and intercity buses.
}
\usage{
Bus_OperationKPI(dtname, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{dtname}{The name of the required data. This includes \code{Overall} and \code{CityBus}. Note that data \code{CityBus} provides all operational information in each county; however, detailed data (such as monthly patronage) for all counties are only provided beginning from March 2024. Before then, the datailed data is only offered in special municipality.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{

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
# Retrieve all operational data of all buses
Bus_OperationKPI("Overall", out="BusOperationalData.csv")

# Retrieve all operational data of city buses in each county
Bus_OperationKPI("CityBus")
}
\keyword{datasets}

