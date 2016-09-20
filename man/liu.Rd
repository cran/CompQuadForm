\name{liu}
\alias{liu}
\title{Liu's method}
\description{
Distribution function (survival function in fact) of quadratic forms in
normal variables using Liu et al.'s method.
}
\usage{
liu(q, lambda, h = rep(1, length(lambda)), delta = rep(0, length(lambda)))
}
\arguments{
  \item{q}{value point at which the survival function is to be evaluated}
  \item{lambda}{distinct non-zero characteristic roots of \eqn{A\Sigma}{A.Sigma}, i.e. the \eqn{\lambda_i}'s }
  \item{h}{respective orders of multiplicity \eqn{h_i}'s of the \eqn{\lambda}{lambda}'s}
  \item{delta}{non-centrality parameters \eqn{\delta_i}'s (should be positive)}
}
\details{
New chi-square approximation to the distribution of non-negative
definite quadratic forms in non-central normal variables. 

Computes P[Q>q] where \eqn{Q=\sum_{j=1}^n\lambda_j\chi^2(h_j,\delta_j)}{Q=sum_{j=1}^n lambda_j chi^2(h_j,delta_j)}.

This method does not work as good as the Imhof's method. Thus Imhof's method should be recommended.
}
\value{
  \item{Qq}{\eqn{P[Q>q]}{P[Q>q]}}
}
\references{
P. Duchesne, P. Lafaye de Micheaux, Computing the distribution of quadratic forms: Further comparisons between the Liu-Tang-Zhang approximation and exact methods, \emph{Computational Statistics and Data Analysis}, Volume 54, (2010), 858-862

H. Liu, Y. Tang, H.H. Zhang, A new chi-square approximation
  to the distribution of non-negative definite quadratic forms in
  non-central normal variables, \emph{Computational Statistics and Data Analysis}, Volume 53, (2009), 853-856}
\author{Pierre Lafaye de Micheaux (\email{lafaye@dms.umontreal.ca}) and
  Pierre Duchesne (\email{duchesne@dms.umontreal.ca})}
\examples{
# Some results from Liu et al. (2009)
# Q1 from Liu et al.
round(liu(2, c(0.5, 0.4, 0.1), c(1, 2, 1), c(1, 0.6, 0.8)), 6)
round(liu(6, c(0.5, 0.4, 0.1), c(1, 2, 1), c(1, 0.6, 0.8)), 6)
round(liu(8, c(0.5, 0.4, 0.1), c(1, 2, 1), c(1, 0.6, 0.8)), 6)

# Q2 from Liu et al.
round(liu(1, c(0.7, 0.3), c(1, 1), c(6, 2)), 6)
round(liu(6, c(0.7, 0.3), c(1, 1), c(6, 2)), 6)
round(liu(15, c(0.7, 0.3), c(1, 1), c(6, 2)), 6)

# Q3 from Liu et al.
round(liu(2, c(0.995, 0.005), c(1, 2), c(1, 1)), 6)
round(liu(8, c(0.995, 0.005), c(1, 2), c(1, 1)), 6)
round(liu(12, c(0.995, 0.005), c(1, 2), c(1, 1)), 6)

# Q4 from Liu et al.
round(liu(3.5, c(0.35, 0.15, 0.35, 0.15), c(1, 1, 6, 2), c(6, 2, 6, 2)),
6)
round(liu(8, c(0.35, 0.15, 0.35, 0.15), c(1, 1, 6, 2), c(6, 2, 6, 2)), 6)
round(liu(13, c(0.35, 0.15, 0.35, 0.15), c(1, 1, 6, 2), c(6, 2, 6, 2)), 6)

}
\keyword{distribution} % Probability Distributions and Random Numbers
\keyword{htest} % Statistical Inference
