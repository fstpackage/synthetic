// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// random_dbl_std
SEXP random_dbl_std(SEXP nr_of_draws, SEXP seed_dbl);
RcppExport SEXP _synthetic_random_dbl_std(SEXP nr_of_drawsSEXP, SEXP seed_dblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type nr_of_draws(nr_of_drawsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type seed_dbl(seed_dblSEXP);
    rcpp_result_gen = Rcpp::wrap(random_dbl_std(nr_of_draws, seed_dbl));
    return rcpp_result_gen;
END_RCPP
}
// new_random3
SEXP new_random3(SEXP nr_of_draws, SEXP seed_dbl);
RcppExport SEXP _synthetic_new_random3(SEXP nr_of_drawsSEXP, SEXP seed_dblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type nr_of_draws(nr_of_drawsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type seed_dbl(seed_dblSEXP);
    rcpp_result_gen = Rcpp::wrap(new_random3(nr_of_draws, seed_dbl));
    return rcpp_result_gen;
END_RCPP
}
// cubic_spline
SEXP cubic_spline(SEXP double_values, SEXP nr_of_draws, SEXP seed_dbl);
RcppExport SEXP _synthetic_cubic_spline(SEXP double_valuesSEXP, SEXP nr_of_drawsSEXP, SEXP seed_dblSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type double_values(double_valuesSEXP);
    Rcpp::traits::input_parameter< SEXP >::type nr_of_draws(nr_of_drawsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type seed_dbl(seed_dblSEXP);
    rcpp_result_gen = Rcpp::wrap(cubic_spline(double_values, nr_of_draws, seed_dbl));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_synthetic_random_dbl_std", (DL_FUNC) &_synthetic_random_dbl_std, 2},
    {"_synthetic_new_random3", (DL_FUNC) &_synthetic_new_random3, 2},
    {"_synthetic_cubic_spline", (DL_FUNC) &_synthetic_cubic_spline, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_synthetic(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
