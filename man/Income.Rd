\name{Income}
\alias{Income}
\docType{data}
\title{
Retrieve the income with given year
}
\description{
The function is used to download the income (by tax) data of each village in Taiwan. The data is downloaded from \href{https://data.gov.tw/dataset/103066}{Open Data Government}.
}
\usage{
Income(year, out=F)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{year}{Select the year (after 2012).}
  \item{out}{If the result should be exported to the local, please fill in the directory. The default value is FALSE, which does not export the data.}
}
\value{
Columns definition of the results are summarized below.

  \tabular{rlll}{
      \tab \strong{Houses} \tab Number of household for tax.\cr
      \tab \strong{Amount} \tab Amounts of all income.\cr
      \tab \strong{MEAN} \tab Average income (Amount/Houses).\cr
      \tab \strong{MED} \tab Medium of income.\cr
      \tab \strong{Q1} \tab First quantile of income.\cr
      \tab \strong{Q3} \tab Thied quantile of income.\cr
      \tab \strong{SD} \tab Standard deviation of income.\cr
      \tab \strong{CV} \tab Coefficient of variation of income.\cr
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
Income(2021)
}
\keyword{datasets}

