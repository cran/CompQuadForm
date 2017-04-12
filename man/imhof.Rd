\name{imhof}
\alias{imhof}
\title{Imhof method.}
\description{
Distribution function (survival function in fact) of quadratic forms in normal variables using Imhof's method.
}
\usage{
imhof(q, lambda, h = rep(1, length(lambda)),
      delta = rep(0, length(lambda)),
      epsabs = 10^(-6), epsrel = 10^(-6), limit = 10000)
}
\arguments{
  \item{q}{value point at which the survival function is to be evaluated}
  \item{lambda}{distinct non-zero characteristic roots of \eqn{A\Sigma}{A.Sigma} }
  \item{h}{respective orders of multiplicity of the \eqn{\lambda}{lambda}s}
  \item{delta}{non-centrality parameters (should be positive)}
  \item{epsabs}{absolute accuracy requested}
  \item{epsrel}{relative accuracy requested}
  \item{limit}{determines the maximum number of subintervals in the partition of the given integration interval}
}
\details{
Let \eqn{\boldsymbol{X}=(X_1,\ldots,X_n)'}{\strong{X}=(X_1,...,X_n)'} be a column random vector which follows a multidimensional normal law with mean vector \eqn{\boldsymbol{0}}{\strong{0}} and non-singular covariance matrix \eqn{\boldsymbol{\Sigma}}{\strong{Sigma}}. 
Let \eqn{\boldsymbol{\mu}=(\mu_1,\ldots,\mu_n)'}{\strong{mu}=(mu_1,...,mu_n)'} be a constant vector, and consider the quadratic form
 
\deqn{Q=(\boldsymbol{x}+\boldsymbol{\mu})'\boldsymbol{A}(\boldsymbol{x}+\boldsymbol{\mu})=\sum_{r=1}^m\lambda_r\chi^2_{h_r;\delta_r}.}{Q = (\strong{x}+\strong{mu})'\strong{A}(\strong{x}+\strong{mu}) = sum_{r=1}^m lambda_r chi^2_{h_r;\delta_r}.}

The function \code{imhof} computes \eqn{P[Q>q]}{P[Q>q]}.
 
The \eqn{\lambda_r}'s are the distinct non-zero characteristic roots of
\eqn{A\Sigma}{A.Sigma}, the \eqn{h_r}{h_r}'s their respective orders of
multiplicity, the \eqn{\delta_r}{delta_r}'s  are certain linear combinations
of \eqn{\mu_1,\ldots,\mu_n}{mu_1,...,mu_n} and the
\eqn{\chi^2_{h_r;\delta_r}}{chi^2_{h_r;delta_r}} are independent
\eqn{\chi^2}{chi^2}-variables with \eqn{h_r}{h_r} degrees of freedom and
non-centrality parameter \eqn{\delta_r}{delta_r}. The variable
\eqn{\chi^2_{h,\delta}}{chi^2_{h;delta}} is defined here by the
relation \eqn{\chi^2_{h,\delta}=(X_1 +
  \delta)^2+\sum_{i=2}^hX_i^2}{chi^2_{h,delta}=(X_1 + delta)^2+
  sum_{i=2}^h X_i^2}, where \eqn{X_1,\ldots,X_h}{X_1,...,X_h} are
independent unit normal deviates.
}
\value{
  \item{Qq}{\eqn{P[Q>q]}{P[Q>q]}}
  \item{abserr}{estimate of the modulus of the absolute error, which
    should equal or exceed abs(i - result)}
}
\references{
P. Duchesne, P. Lafaye de Micheaux, Computing the distribution of quadratic forms: Further comparisons between the Liu-Tang-Zhang approximation and exact methods, \emph{Computational Statistics and Data Analysis}, Volume 54, (2010), 858-862

J. P. Imhof, Computing the Distribution of Quadratic Forms in Normal Variables, \emph{Biometrika}, Volume 48, Issue 3/4 (Dec., 1961), 419-426}
\author{Pierre Lafaye de Micheaux (\email{lafaye@dms.umontreal.ca}) and
  Pierre Duchesne (\email{duchesne@dms.umontreal.ca})}
\examples{
# Some results from Table 1, p.424, Imhof (1961)

# Q1 with x = 2
round(imhof(2, c(0.6, 0.3, 0.1))$Qq, 4)

# Q2 with x = 6
round(imhof(6, c(0.6, 0.3, 0.1), c(2, 2, 2))$Qq, 4)

# Q6 with x = 15
round(imhof(15, c(0.7, 0.3), c(1, 1), c(6, 2))$Qq, 4)

}
\keyword{distribution} % Probability Distributions and Random Numbers
\keyword{htest} % Statistical Inference
