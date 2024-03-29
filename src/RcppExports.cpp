// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// bout_end
std::vector<double> bout_end(std::vector<int> seq, std::vector<double> activity, std::vector<double> interval, int size, int window_size);
RcppExport SEXP _activPAL_bout_end(SEXP seqSEXP, SEXP activitySEXP, SEXP intervalSEXP, SEXP sizeSEXP, SEXP window_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<int> >::type seq(seqSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type activity(activitySEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type interval(intervalSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    Rcpp::traits::input_parameter< int >::type window_size(window_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(bout_end(seq, activity, interval, size, window_size));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_activPAL_bout_end", (DL_FUNC) &_activPAL_bout_end, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_activPAL(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
