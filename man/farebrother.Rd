\name{farebrother}
\alias{ruben}
\alias{farebrother}
\title{Ruben/Farebrother method}
\description{
Distribution function (survival function in fact) of quadratic forms in normal variables using Farebrother's algorithm.
}
\usage{
farebrother(q,lambda,h = rep(1, length(lambda)),delta = rep(0, length(lambda)),maxit=100000,eps=10^(-10),mode=1)
}
\arguments{
  \item{q}{value point at which distribution function is to be evaluated}
  \item{lambda}{the weights \eqn{\lambda_1, \lambda_2, ..., \lambda_n}, i.e. the distinct non-zero characteristic roots of \eqn{A\Sigma}{A.Sigma} }
  \item{h}{vector of the respective orders of multiplicity \eqn{m_i} of the \eqn{\lambda}{lambda}s}
  \item{delta}{the non-centrality parameters \eqn{delta_i}}
  \item{maxit}{the maximum number of term K in equation below}
  \item{eps}{the desired level of accuracy}
  \item{mode}{if mode>0 then \eqn{\beta=mode*\lambda_{min}} otherwise \eqn{\beta=\beta_B=2/(1/\lambda_{min}+1/\lambda_{max})}}
}
\details{
Computes P[Q>q] where \eqn{Q=\sum_{j=1}^n\lambda_j\chi^2(m_j,\delta_j^2)}{Q=sum_{j=1}^n lambda_j chi^2(m_j,delta_j^2)}. P[Q<q] is approximated by \eqn{\sum_k=0^{K-1} a_k P[\chi^2(m+2k)<q/\beta]} where \eqn{m=\sum_{j=1}^n m_j} and \eqn{\beta} is an arbitrary constant (as given by argument mode).
}
\value{
  \item{Qq}{\eqn{P[Q>q]}{P[Q>q]}}
}
\references{
P. Duchesne, P. Lafaye de Micheaux, Computing the distribution of quadratic forms: Further comparisons between the Liu-Tang-Zhang approximation and exact methods, \emph{Computational Statistics and Data Analysis}, Volume 54, (2010), 858-862

Farebrother R.W., Algorithm AS 204: The distribution of a Positive Linear
  Combination of chi-squared random variables, Journal of the Royal
  Statistical Society, Series C (applied Statistics), Vol. 33, No. 3
  (1984), p. 332-339}
\author{Pierre Lafaye de Micheaux (\email{lafaye@dms.umontreal.ca}) and
  Pierre Duchesne (\email{duchesne@dms.umontreal.ca})}
\examples{
# Some results from Table 3, p.327, Davies (1980)

 farebrother(1,c(6,3,1),c(1,1,1),c(0,0,0))

}
\keyword{distribution} % Probability Distributions and Random Numbers
\keyword{htest} % Statistical Inference
