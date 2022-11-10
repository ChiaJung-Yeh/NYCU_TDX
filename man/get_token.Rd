\name{get_token}
\alias{get_token}
\docType{data}
\title{
Retrieve the access token from TDX platform
}
\description{
The function is used to retrive the access token, in order to use the API service of connnecting the transportation data. This access token is given by the TDX platform. The Client Id and Client Secret should be appiled from the platform, and filled in the arguments in this function.

Please first apply the member in \href{https://tdx.transportdata.tw/register}{TDX website.} If you are students or your email contains the domain name of "education (.edu)", please enroll with the education plan. After being a member of TDX, you will soon get the Client Id and Client Secret, please check out in the \href{https://tdx.transportdata.tw/user/dataservice/key}{API key Dashboard.}

This function would first check the validity of access token that is stored in the specific file. The file is set in the argument \code{store}. If the access token is invalid, expired, or not available (default: \code{store=NA}), then it would automatically generate a new token. It is strongly recommended to set the directory to store the access token, which would decrease the number of calling API service.

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
get_token("CLIENT_ID", "CLIENT_SECRET", store="C:/Users/Downloads")
}
\keyword{datasets}

