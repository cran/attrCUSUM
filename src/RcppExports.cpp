// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// C_attrCUSUM_inc_TranPrCt_VSI
double C_attrCUSUM_inc_TranPrCt_VSI(NumericVector Ct_Interval_, NumericVector Cprev_, NumericVector refv_, NumericVector pmf_process, LogicalVector leftmost_closed, LogicalVector rightmost_closed);
RcppExport SEXP attrCUSUM_C_attrCUSUM_inc_TranPrCt_VSI(SEXP Ct_Interval_SEXP, SEXP Cprev_SEXP, SEXP refv_SEXP, SEXP pmf_processSEXP, SEXP leftmost_closedSEXP, SEXP rightmost_closedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Ct_Interval_(Ct_Interval_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Cprev_(Cprev_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type refv_(refv_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pmf_process(pmf_processSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type leftmost_closed(leftmost_closedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type rightmost_closed(rightmost_closedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_attrCUSUM_inc_TranPrCt_VSI(Ct_Interval_, Cprev_, refv_, pmf_process, leftmost_closed, rightmost_closed));
    return rcpp_result_gen;
END_RCPP
}
// C_attrCUSUM_inc_Q_VSI
List C_attrCUSUM_inc_Q_VSI(NumericVector endpoints, NumericVector refv, NumericVector pmf_process, LogicalVector leftmost_closed, LogicalVector rightmost_closed);
RcppExport SEXP attrCUSUM_C_attrCUSUM_inc_Q_VSI(SEXP endpointsSEXP, SEXP refvSEXP, SEXP pmf_processSEXP, SEXP leftmost_closedSEXP, SEXP rightmost_closedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type endpoints(endpointsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type refv(refvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pmf_process(pmf_processSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type leftmost_closed(leftmost_closedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type rightmost_closed(rightmost_closedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_attrCUSUM_inc_Q_VSI(endpoints, refv, pmf_process, leftmost_closed, rightmost_closed));
    return rcpp_result_gen;
END_RCPP
}
// C_attrCUSUM_getEigen
List C_attrCUSUM_getEigen(NumericMatrix Q_mat);
RcppExport SEXP attrCUSUM_C_attrCUSUM_getEigen(SEXP Q_matSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Q_mat(Q_matSEXP);
    rcpp_result_gen = Rcpp::wrap(C_attrCUSUM_getEigen(Q_mat));
    return rcpp_result_gen;
END_RCPP
}
// C_attrCUSUM_inc_InitPr_VSI
NumericMatrix C_attrCUSUM_inc_InitPr_VSI(NumericVector endpoints, NumericVector c_zero, NumericVector refv, NumericVector pmf_process, LogicalVector leftmost_closed, LogicalVector rightmost_closed);
RcppExport SEXP attrCUSUM_C_attrCUSUM_inc_InitPr_VSI(SEXP endpointsSEXP, SEXP c_zeroSEXP, SEXP refvSEXP, SEXP pmf_processSEXP, SEXP leftmost_closedSEXP, SEXP rightmost_closedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type endpoints(endpointsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type c_zero(c_zeroSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type refv(refvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type pmf_process(pmf_processSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type leftmost_closed(leftmost_closedSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type rightmost_closed(rightmost_closedSEXP);
    rcpp_result_gen = Rcpp::wrap(C_attrCUSUM_inc_InitPr_VSI(endpoints, c_zero, refv, pmf_process, leftmost_closed, rightmost_closed));
    return rcpp_result_gen;
END_RCPP
}
// C_attrCUSUM_getATA
List C_attrCUSUM_getATA(NumericMatrix Q_);
RcppExport SEXP attrCUSUM_C_attrCUSUM_getATA(SEXP Q_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type Q_(Q_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_attrCUSUM_getATA(Q_));
    return rcpp_result_gen;
END_RCPP
}
// C_ImportSubseq
NumericVector C_ImportSubseq(NumericVector x_, IntegerVector ind_b_R_, IntegerVector ind_e_R_);
RcppExport SEXP attrCUSUM_C_ImportSubseq(SEXP x_SEXP, SEXP ind_b_R_SEXP, SEXP ind_e_R_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_b_R_(ind_b_R_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_e_R_(ind_e_R_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_ImportSubseq(x_, ind_b_R_, ind_e_R_));
    return rcpp_result_gen;
END_RCPP
}
// C_ImportSubseq_Rind
NumericVector C_ImportSubseq_Rind(NumericVector x_, IntegerVector ind_b_R_, IntegerVector ind_e_R_);
RcppExport SEXP attrCUSUM_C_ImportSubseq_Rind(SEXP x_SEXP, SEXP ind_b_R_SEXP, SEXP ind_e_R_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_b_R_(ind_b_R_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_e_R_(ind_e_R_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_ImportSubseq_Rind(x_, ind_b_R_, ind_e_R_));
    return rcpp_result_gen;
END_RCPP
}
// C_ImportSubseq_Cind
NumericVector C_ImportSubseq_Cind(NumericVector x_, IntegerVector ind_b_C_, IntegerVector ind_e_C_);
RcppExport SEXP attrCUSUM_C_ImportSubseq_Cind(SEXP x_SEXP, SEXP ind_b_C_SEXP, SEXP ind_e_C_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_b_C_(ind_b_C_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ind_e_C_(ind_e_C_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_ImportSubseq_Cind(x_, ind_b_C_, ind_e_C_));
    return rcpp_result_gen;
END_RCPP
}
// C_ndecimal
double C_ndecimal(NumericVector x_, IntegerVector maxndecimal_);
RcppExport SEXP attrCUSUM_C_ndecimal(SEXP x_SEXP, SEXP maxndecimal_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type maxndecimal_(maxndecimal_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_ndecimal(x_, maxndecimal_));
    return rcpp_result_gen;
END_RCPP
}
// C_indicator_x_in_I
double C_indicator_x_in_I(NumericVector x_, NumericVector Interval_, LogicalVector leftmost_closed_, LogicalVector rightmost_closed_, IntegerVector ndec_);
RcppExport SEXP attrCUSUM_C_indicator_x_in_I(SEXP x_SEXP, SEXP Interval_SEXP, SEXP leftmost_closed_SEXP, SEXP rightmost_closed_SEXP, SEXP ndec_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_(x_SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Interval_(Interval_SEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type leftmost_closed_(leftmost_closed_SEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type rightmost_closed_(rightmost_closed_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type ndec_(ndec_SEXP);
    rcpp_result_gen = Rcpp::wrap(C_indicator_x_in_I(x_, Interval_, leftmost_closed_, rightmost_closed_, ndec_));
    return rcpp_result_gen;
END_RCPP
}