// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// simul
Rcpp::List simul(Rcpp::DataFrame df, Rcpp::CharacterVector ids, int nums, Rcpp::List ls);
RcppExport SEXP _projmanr_simul(SEXP dfSEXP, SEXP idsSEXP, SEXP numsSEXP, SEXP lsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< int >::type nums(numsSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type ls(lsSEXP);
    rcpp_result_gen = Rcpp::wrap(simul(df, ids, nums, ls));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_projmanr_simul", (DL_FUNC) &_projmanr_simul, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_projmanr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
