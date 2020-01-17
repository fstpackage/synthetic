
// [[Rcpp::depends(BH)]]

#include <random>

#include <boost/random/uniform_real_distribution.hpp>
#include <boost/random/mersenne_twister.hpp>
#include "boost_b_spline.hpp"  // modify when boost fix is rolled out

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
SEXP random_spline(SEXP control_values, SEXP nr_of_draws, SEXP seed_dbl)
{
  double* values = REAL(control_values);
  int size = LENGTH(control_values);

  int draw_size = *INTEGER(nr_of_draws);
  uint64_t seed = *((uint64_t*) (REAL(seed_dbl)));

  double step = 1.0 / ((double) (size - 1L));  // step size

  double left_derivative = (values[1] - values[0]) / step;
  double right_derivative = (values[size - 1] - values[size - 2]) / step;
  
  // set the control points and endpoint derivatives
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0,
    step, left_derivative, right_derivative);

  // output vector
  SEXP res = PROTECT(Rf_allocVector(REALSXP, draw_size));

  // random number generator
  boost::mt19937 gen;
  gen.seed(seed);
  boost::random::uniform_real_distribution<double> dis(0.0, 1.0);

  double* resp = REAL(res);

  // TODO: try loop unrolling here for performance
  
  // evaluate at random points
  for (size_t i = 0; i < (size_t) draw_size; ++i)
  {
    double random = dis(gen);
    resp[i] = spline(random);
  }

  UNPROTECT(1);

  return res;
}


// [[Rcpp::export]]
SEXP simulate_spline(SEXP control_points, SEXP x_points)
{
  double* values = REAL(control_points);
  double* x_values = REAL(x_points);

  int size = LENGTH(control_points);
  int draw_size = LENGTH(x_points);

  double step = 1.0 / ((double) (size - 1L));  // step size
  double left_derivative = (values[1] - values[0]) / step;
  double right_derivative = (values[size - 1] - values[size - 2]) / step;

  // set the control points and endpoint derivatives
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0,
    step, left_derivative, right_derivative);

  // output vector
  SEXP res = PROTECT(Rf_allocVector(REALSXP, draw_size));
  double* resp = REAL(res);

  // TODO: try loop unrolling here for performance

  // evaluate at specified points
  for (size_t i = 0; i < (size_t) draw_size; ++i)
  {
    resp[i] = spline(x_values[i]);
  }

  UNPROTECT(1);

  return res;
}
