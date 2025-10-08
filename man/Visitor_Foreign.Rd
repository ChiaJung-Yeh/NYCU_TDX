\name{Visitor_Foreign}
\alias{Visitor_Foreign}
\docType{data}
\title{
Retrieve the data of international visitors count in Taiwan
}
\description{
The function is used to retrieve the number of international visitors by country since 2001. The number of visitors can be further cross-analysed by key attributes, such as age group, trip purpose, and gender.
}
\usage{
Visitor_Foreign(age=F, purpose=F, gender=F, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{age}{If the cross analysis on age group should be retrieved. This should be set \code{TRUE}.}
  \item{purpose}{If the cross analysis on trip purpose should be retrieved. This should be set \code{TRUE}.}
  \item{gender}{If the cross analysis on gender should be retrieved. This should be set \code{TRUE}. Note that the defaule value all three aforementioned argements are set \code{FALSE}, which only retrieve the visitors count by each country.}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{Trips} \tab Number of international visitors from each country.\cr
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
# Retrieve the international visitors by country
Visitor_Foreign()

# Retrieve the international visitors by country and age group
Visitor_Foreign(age=T, out="VisitorAge.csv")
}
\keyword{datasets}

