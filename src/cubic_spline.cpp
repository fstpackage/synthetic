
// [[Rcpp::depends(BH)]]

#include <random>

#include <boost/random/uniform_real_distribution.hpp>
#include <boost/random/mersenne_twister.hpp>
#include <boost/math/interpolators/cardinal_cubic_b_spline.hpp>  
#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
SEXP random_dbl_std(SEXP nr_of_draws, SEXP seed_dbl)
{
  int draw_size = *INTEGER(nr_of_draws);
  SEXP res = PROTECT(Rf_allocVector(REALSXP, draw_size));
  double* res_p = REAL(res);

  uint64_t seed = *((uint64_t*) (REAL(seed_dbl)));

  std::random_device rd;  //Will be used to obtain a seed for the random number engine
  std::mt19937 gen(seed); //Standard mersenne_twister_engine seeded with rd()
  std::uniform_real_distribution<> dis(0.0, 1.0);

  for (int i = 0; i < draw_size; ++i) {
    res_p[i] = dis(gen);
  }

  UNPROTECT(1);

  return res;
}


// [[Rcpp::export]]
SEXP random_dbl_boost(SEXP nr_of_draws, SEXP seed_dbl)
{
  int draw_size = *INTEGER(nr_of_draws);
  SEXP res = PROTECT(Rf_allocVector(REALSXP, draw_size));
  double* res_p = REAL(res);

  uint64_t seed = *((uint64_t*) (REAL(seed_dbl)));

  boost::mt19937 gen;
  gen.seed(seed);
  boost::random::uniform_real_distribution<double> dis(0.0, 1.0);

  for (int i = 0; i < draw_size; ++i) {
    res_p[i] = dis(gen);
  }
  
  UNPROTECT(1);
  
  return res;
}


// [[Rcpp::export]]
SEXP cubic_spline(SEXP double_values, SEXP nr_of_draws, SEXP seed_dbl)
{
  double* values = REAL(double_values);
  int size = LENGTH(double_values);

  int draw_size = *INTEGER(nr_of_draws);
  uint64_t seed = *((uint64_t*) (REAL(seed_dbl)));

  double step = 1L / ((double) size - 1L);  // step size

  // We could define an arbitrary start time, but for now we'll just use 0:
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0 , step);

  SEXP res = PROTECT(Rf_allocVector(REALSXP, draw_size));

  boost::mt19937 gen;
  gen.seed(seed);
  boost::random::uniform_real_distribution<double> dis(0.0, 1.0);

  double* resp = REAL(res);

  // evaluate at random points
  for (size_t i = 0; i < (size_t) draw_size; ++i)
  {
    double random = dis(gen);
    resp[i] = spline(random);
  }

  UNPROTECT(1);

  return res;
}
