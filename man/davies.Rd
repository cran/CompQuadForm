\name{davies}
\alias{davies}
\title{Davies method}
\description{
Distribution function (survival function in fact) of quadratic forms in normal variables using Davies's method.
}
\usage{
davies(q, lambda, h = rep(1, length(lambda)), delta = rep(0,
         length(lambda)), sigma = 0, lim = 10000, acc = 0.0001)
}
\arguments{
  \item{q}{value point at which distribution function is to be evaluated}
  \item{lambda}{the weights \eqn{\lambda_1, \lambda_2, ..., \lambda_n}, i.e. distinct non-zero characteristic roots of \eqn{A\Sigma}{A.Sigma}}
  \item{h}{respective orders of multiplicity \eqn{n_j} of the \eqn{\lambda}{lambda}s}
  \item{delta}{non-centrality parameters \eqn{\delta_j^2} (should be positive)}
  \item{sigma}{coefficient \eqn{\sigma} of the standard Gaussian}
  \item{lim}{maximum number of integration terms. Realistic values for
    'lim' range from 1,000 if the procedure is to be called repeatedly up
    to 50,000 if it is to be called only occasionally}
  \item{acc}{error bound. Suitable values for 'acc' range from 0.001 to
    0.00005 which should be adequate for most statistical purposes.}
}
\details{
Computes \eqn{P[Q>q]}{P[Q>q]} where \eqn{Q = \sum_{j=1}^r\lambda_jX_j+\sigma X_0}{Q = sum_{j=1}^r lambda_j X_j+ sigma X_0} where \eqn{X_j} are independent random variables having a non-central \eqn{chi^2} distribution with \eqn{n_j} degrees of freedom and non-centrality parameter \eqn{delta_j^2} for \eqn{j=1,...,r} and \eqn{X_0} having a standard Gaussian distribution.
}
\value{
  \item{trace}{vector, indicating performance of procedure, with the
    following components: 1: absolute
    value sum, 2: total number of integration terms, 3: number of
    integrations, 4: integration interval in main integration,
    5: truncation point in initial integration, 6: standard deviation of
  convergence factor term, 7: number of cycles to locate integration parameters}
  \item{ifault}{fault indicator: 0: no error, 1: requested accuracy
    could not be obtained, 2: round-off error possibly significant, 3:
    invalid parameters, 4: unable to locate integration parameters}
  \item{Qq}{\eqn{P[Q>q]}{P[Q>q]}}
}
\references{
P. Duchesne, P. Lafaye de Micheaux, Computing the distribution of quadratic forms: Further comparisons between the Liu-Tang-Zhang approximation and exact methods, \emph{Computational Statistics and Data Analysis}, Volume 54, (2010), 858-862

Davies R.B., Algorithm AS 155: The Distribution of a Linear
  Combination of chi-2 Random Variables, Journal of the Royal
  Statistical Society. Series C (Applied Statistics), 29(3), p. 323-333,
(1980)}
\author{Pierre Lafaye de Micheaux (\email{lafaye@dms.umontreal.ca}) and
  Pierre Duchesne (\email{duchesne@dms.umontreal.ca})}
\examples{
# Some results from Table 3, p.327, Davies (1980)

 round(1 - davies(1, c(6, 3, 1), c(1, 1, 1))$Qq, 4)
 round(1 - davies(7, c(6, 3, 1), c(1, 1, 1))$Qq, 4)
 round(1 - davies(20, c(6, 3, 1), c(1, 1, 1))$Qq, 4)
 
 round(1 - davies(2, c(6, 3, 1), c(2, 2, 2))$Qq, 4)
 round(1 - davies(20, c(6, 3, 1), c(2, 2, 2))$Qq, 4)
 round(1 - davies(60, c(6, 3, 1), c(2, 2, 2))$Qq, 4)
 
 round(1 - davies(10, c(6, 3, 1), c(6, 4, 2))$Qq, 4)
 round(1 - davies(50, c(6, 3, 1), c(6, 4, 2))$Qq, 4)
 round(1 - davies(120, c(6, 3, 1), c(6, 4, 2))$Qq, 4)

 round(1 - davies(20, c(7, 3), c(6, 2), c(6, 2))$Qq, 4)
 round(1 - davies(100, c(7, 3), c(6, 2), c(6, 2))$Qq, 4)
 round(1 - davies(200, c(7, 3), c(6, 2), c(6, 2))$Qq, 4)

 round(1 - davies(10, c(7, 3), c(1, 1), c(6, 2))$Qq, 4)
 round(1 - davies(60, c(7, 3), c(1, 1), c(6, 2))$Qq, 4)
 round(1 - davies(150, c(7, 3), c(1, 1), c(6, 2))$Qq, 4)

 round(1 - davies(70, c(7, 3, 7, 3), c(6, 2, 1, 1), c(6, 2, 6, 2))$Qq, 4)
 round(1 - davies(160, c(7, 3, 7, 3), c(6, 2, 1, 1), c(6, 2, 6, 2))$Qq, 4)
 round(1 - davies(260, c(7, 3, 7, 3), c(6, 2, 1, 1), c(6, 2, 6, 2))$Qq, 4)

 round(1 - davies(-40, c(7, 3, -7, -3), c(6, 2, 1, 1), c(6, 2, 6,
 2))$Qq, 4)
 round(1 - davies(40, c(7, 3, -7, -3), c(6, 2, 1, 1), c(6, 2, 6, 2))$Qq,
 4)
 round(1 - davies(140, c(7, 3, -7, -3), c(6, 2, 1, 1), c(6, 2, 6,
 2))$Qq, 4)

# You should sometimes play with the 'lim' parameter:
davies(0.00001,lambda=0.2)
imhof(0.00001,lambda=0.2)$Qq
davies(0.00001,lambda=0.2, lim=20000)

}
\keyword{distribution} % Probability Distributions and Random Numbers
\keyword{htest} % Statistical Inference
