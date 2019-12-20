
// [[Rcpp::depends(BH)]]

#include <boost/math/interpolators/cardinal_cubic_b_spline.hpp>  

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
SEXP cubic_spline(SEXP double_values, SEXP estimates)
{
  double* values = REAL(double_values);
  int size = LENGTH(double_values);

  double* estimate_values = REAL(estimates);
  int estimate_size = LENGTH(estimates);

  double step = 1;  // step size

  // We could define an arbitrary start time, but for now we'll just use 0:
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0 , step);

  SEXP res = PROTECT(Rf_allocVector(REALSXP, estimate_size));
  double* resp = REAL(res);

  // Now we can evaluate the spline wherever we please.
  boost::random::uniform_real_distribution<double> absissa(0, size * step);
  for (size_t i = 0; i < (size_t) estimate_size; ++i)
  {
      resp[i] = spline(estimate_values[i]);
  }

  UNPROTECT(1);

  return res;  
}
