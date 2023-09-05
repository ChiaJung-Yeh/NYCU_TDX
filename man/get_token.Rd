\name{get_token}
\alias{get_token}
\docType{data}
\title{
Retrieve the access token from TDX platform
}
\description{
The function is used to retrive the access token, in order to use the API service of connnecting the transportation data. This access token is given by the TDX platform. The Client Id and Client Secret should be appiled from the platform, and filled in the arguments in this function.

Please first apply the member in \href{https://tdx.transportdata.tw/register}{TDX website.} If you are students or your email contains the domain name of "education (.edu)", please enroll with the education plan. After being a member of TDX, you will soon get the Client Id and Client Secret, please check out in the \href{https://tdx.transportdata.tw/user/dataservice/key}{API key Dashboard.}

This function would first check the validity of access token that is stored in the R default directory file. If the access token is invalid, expired, or not available, then it would automatically generate a new token.

This function can produce the access token with the correct Client Id and Client Secret arguments. It would show \emph{"Connect Successfully! This token will expire in 1 day."} as the token is correctly produced; otherwise, it would retrieve \emph{"Your 'client_id' or 'client_secret' is WRONG!!".} And note that the access token will expire in 1 day.
}
\usage{
get_token(client_id, client_secret)
}
\format{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\arguments{
  \item{client_id}{Client ID applied from TDX website.}
  \item{client_secret}{Client Secret applied from TDX website.}
  \item{store}{The directory to store access token.}
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
get_token("CLIENT_ID", "CLIENT_SECRET")
}
\keyword{datasets}

