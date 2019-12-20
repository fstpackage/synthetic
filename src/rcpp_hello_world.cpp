
// [[Rcpp::depends(BH)]]

#include <boost/math/interpolators/cardinal_cubic_b_spline.hpp>  
#include <boost/random/uniform_real_distribution.hpp>
#include <boost/math/tools/roots.hpp>

#include <Rcpp.h>

using namespace Rcpp;


// [[Rcpp::export]]
void cubic_spline(SEXP double_values, SEXP estimates)
{
  double* values = REAL(double_values);
  int size = LENGTH(double_values);

  double* estimate_values = REAL(estimates);
  int estimate_size = LENGTH(estimates);

  double step = 0.01;  // step size

  // We could define an arbitrary start time, but for now we'll just use 0:
  boost::math::interpolators::cardinal_cubic_b_spline<double> spline(values, size, 0 , step);

  // Now we can evaluate the spline wherever we please.
  std::mt19937 gen;
  boost::random::uniform_real_distribution<double> absissa(0, size * step);
  for (size_t i = 0; i < estimate_size; ++i)
  {
      std::cout << "estimate at " << estimate_values[i] << " = " << spline(estimate_values[i]) << std::endl;
  }
}
