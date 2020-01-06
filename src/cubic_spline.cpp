
// [[Rcpp::depends(BH)]]

#include <boost/random/uniform_real_distribution.hpp>
#include <boost/math/interpolators/cardinal_cubic_b_spline.hpp>  

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
SEXP cubic_spline(SEXP double_values, SEXP nr_of_knots)
{
  double* values = REAL(double_values);
  int size = LENGTH(double_values);

  int estimate_size = *INTEGER(nr_of_knots);

  double step = 1L / ((double) size - 1L);  // step size

  // We could define an arbitrary start time, but for now we'll just use 0:
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0 , step);

  SEXP res = PROTECT(Rf_allocVector(REALSXP, estimate_size));
  SEXP der = PROTECT(Rf_allocVector(REALSXP, estimate_size));

  double* resp = REAL(res);
  double* derp = REAL(der);
  
  // evaluate at specified points
  double knot = 0.0;
  for (size_t i = 0; i < (size_t) estimate_size; ++i)
  {
    resp[i] = spline(knot);
    derp[i] = spline.prime(knot);
    knot += step;
  }

  List result = List::create(
    _["knot"]       = res,
    _["derivative"] = der
  );
  
  UNPROTECT(2);

  return result;  
}
