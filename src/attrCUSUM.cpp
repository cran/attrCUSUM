// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// Declaration (functions in utils)
NumericVector C_ImportSubseq(NumericVector x_,
                             IntegerVector ind_b_R_,
                             IntegerVector ind_e_R_);
double C_indicator_x_in_I(NumericVector x_,
                          NumericVector Interval_,
                          LogicalVector leftmost_closed,
                          LogicalVector rightmost_closed,
                          IntegerVector ndec_);
double C_ndecimal(NumericVector x_, IntegerVector maxndecimal_);


// [[Rcpp::export]]
double C_attrCUSUM_inc_TranPrCt_VSI(NumericVector Ct_Interval_,
                                      NumericVector Cprev_,
                                      NumericVector refv_,
                                      NumericVector pmf_process,
                                      LogicalVector leftmost_closed,
                                      LogicalVector rightmost_closed) {
  NumericVector Ct_Interval = round(Ct_Interval_, 7);
  NumericVector Ct_candi(1);
  IntegerVector ndec = IntegerVector::create(7);
  double Cprev = as<double>(Cprev_), refv = as<double>(refv_), res = 0;
  int len_pmf = pmf_process.size();
  Ct_candi[0] = fmax(Cprev, 0.0) + (len_pmf - 1) - refv;
  Ct_candi = round(Ct_candi, 7);
  if (Ct_candi[0] < Ct_Interval[0]) {
    return(0);
  }
  Ct_candi[0] = fmax(Cprev, 0.0) + 0 - refv;
  Ct_candi = round(Ct_candi, 7);
  if (Ct_candi[0] > Ct_Interval[1]) {
    return(0);
  }

  for (int j = 0; j != len_pmf; j++) {
    Ct_candi[0] = fmax(Cprev, 0.0) + j - refv;
    Ct_candi = round(Ct_candi, 7);
    if (C_indicator_x_in_I(Ct_candi, Ct_Interval,
                           leftmost_closed, rightmost_closed, ndec)) {
      res += pmf_process[j];
    }
  }
  return(res);
}

// Calculate A Matrix of Transition Probabilities
// for VSI CUSUM Chart for Mean Increase
// [[Rcpp::export]]
List C_attrCUSUM_inc_Q_VSI(NumericVector endpoints,
                           NumericVector refv,
                           NumericVector pmf_process,
                           LogicalVector leftmost_closed,
                           LogicalVector rightmost_closed) {
  int numsubI = endpoints.size() - 1;
  int L = pmf_process.size() - 1;
  NumericVector tol(1);
  tol[0] = endpoints[1] - endpoints[0];
  tol = round(tol, 7);
  refv = round(refv, 7);
  NumericMatrix Q(numsubI, numsubI);
  NumericVector Cprev(1);
  NumericVector Ct_tmp(1);
  NumericVector ind_tmp(1);

  for (int i = 1; i <= numsubI; i++) {
    Cprev[0] = endpoints[i - 1];
    for (int j = 0; j <= L; j++) {
      Ct_tmp[0] = fmax(Cprev[0], 0.0) + j - refv[0];
      Ct_tmp = round(Ct_tmp, 7);
      ind_tmp[0] = (((Ct_tmp[0] + refv[0]) / tol[0]) + 1);
      ind_tmp = round(ind_tmp, 0);
      if (ind_tmp[0] > numsubI) {
        break;
      }
      Q(i - 1, ind_tmp[0] - 1) += pmf_process[j];
    }
  }
  arma::mat Q_arma(Q.begin(), numsubI, numsubI, false);
  arma::mat I(numsubI, numsubI); I.eye();
  arma::mat I_minus_Q(numsubI, numsubI); I_minus_Q = I - Q_arma;
  arma::mat I_minus_Q_inv(numsubI, numsubI);
  I_minus_Q_inv = arma::inv(I_minus_Q);
  arma::mat I_minus_Q_inv_1 = arma::sum(I_minus_Q_inv, 1);
  return(List::create(Named("Q", Q),
                      Named("I_minus_Q", I_minus_Q),
                      Named("I_minus_Q_inv", I_minus_Q_inv),
                      Named("I_minus_Q_inv_1", I_minus_Q_inv_1)));

}

// [[Rcpp::export]]
List C_attrCUSUM_getEigen(NumericMatrix Q_mat) {
  int numsubI = Q_mat.nrow();
  arma::mat Q_arma(Q_mat.begin(), numsubI, numsubI, false);
  arma::cx_vec eig_value;
  arma::cx_mat eig_vec;
  arma::eig_gen(eig_value, eig_vec, Q_arma);

  return(List::create(Named("eig.value", eig_value),
                      Named("eig.vec", eig_vec)));
}


// [[Rcpp::export]]
NumericMatrix C_attrCUSUM_inc_InitPr_VSI(NumericVector endpoints,
                                     NumericVector c_zero,
                                     NumericVector refv,
                                     NumericVector pmf_process,
                                     LogicalVector leftmost_closed,
                                     LogicalVector rightmost_closed) {
  int numsubI = endpoints.size() - 1;
  NumericMatrix initprob(1, numsubI);
  IntegerVector ind_b_R(1), ind_e_R(1);
  NumericVector Cprev(1);
  for (int i = 1; i <= numsubI; i++) {
    ind_b_R[0] = i;
    ind_e_R[0] = i + 1;
    initprob(0, i - 1) = C_attrCUSUM_inc_TranPrCt_VSI(C_ImportSubseq(endpoints,
                                                  ind_b_R, ind_e_R),
                                                  c_zero, refv, pmf_process,
                                                  leftmost_closed, rightmost_closed);
  }
  return(initprob);
}

// [[Rcpp::export]]
List C_attrCUSUM_getATA(NumericMatrix Q_) {
  int numrow = Q_.nrow();
  arma::mat Q(Q_.begin(), numrow, numrow, false);
  arma::mat I(numrow, numrow); I.eye();
  arma::mat I_minus_Q(numrow, numrow); I_minus_Q = I - Q;
  arma::mat I_minus_Q_inv(numrow, numrow); I_minus_Q_inv = arma::inv(I_minus_Q);
  arma::mat I_minus_Q_inv_1 = arma::sum(I_minus_Q_inv, 1);
  return(List::create(Named("I_minus_Q", I_minus_Q),
                      Named("M", I_minus_Q_inv),
                      Named("ATA", I_minus_Q_inv_1)));
}
