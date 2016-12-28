// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericVector C_ImportSubseq(NumericVector x_,
                             IntegerVector ind_b_R_,
                             IntegerVector ind_e_R_) {
  return(NumericVector::import(x_.begin() + as<int>(ind_b_R_) - 1,
                               x_.begin() + as<int>(ind_e_R_)));
}

// [[Rcpp::export]]
NumericVector C_ImportSubseq_Rind(NumericVector x_,
                                  IntegerVector ind_b_R_,
                                  IntegerVector ind_e_R_) {
  return(NumericVector::import(x_.begin() + as<int>(ind_b_R_) - 1,
                               x_.begin() + as<int>(ind_e_R_)));
}

// [[Rcpp::export]]
NumericVector C_ImportSubseq_Cind(NumericVector x_,
                                  IntegerVector ind_b_C_,
                                  IntegerVector ind_e_C_) {
  return(NumericVector::import(x_.begin() + as<int>(ind_b_C_),
                               x_.begin() + as<int>(ind_e_C_) + 1));
}

// [[Rcpp::export]]
double C_ndecimal(NumericVector x_, IntegerVector maxndecimal_) {
  double x = as<double>(x_);
  int maxndecimal = as<int>(maxndecimal_);
  for (int d = 0; d != maxndecimal; d++) {
    if (round(x_, d)[0] == x) {
      return(d);
    }
  }
  return(maxndecimal);
}

// [[Rcpp::export]]
double C_indicator_x_in_I(NumericVector x_,
                          NumericVector Interval_,
                          LogicalVector leftmost_closed_,
                          LogicalVector rightmost_closed_,
                          IntegerVector ndec_) {
  int ndec = as<int>(ndec_);
  NumericVector Interval = round(Interval_, ndec);
  double l = Interval[0], u = Interval[1], x = round(x_, ndec)[0];
  bool leftmost_closed = as<bool>(leftmost_closed_);
  bool rightmost_closed = as<bool>(rightmost_closed_);
  bool Check_x_in_I;

  Check_x_in_I = (leftmost_closed) ? (x >= l) : (x > l);
  if (Check_x_in_I) Check_x_in_I = (rightmost_closed) ? (x <= u) : (x < u);
  return((double)Check_x_in_I);
}


// Under construction

// // [[Rcpp::export]]
// List C_Subintervals_lensubI(NumericVector minv,
//                             NumericVector maxv,
//                             NumericVector lensubI_,
//                             IntegerVector ndec_) {
//   double lensubI = as<double>(lensubI_);
//   int ndec = as<int>(ndec_), numsubI;
//   int dig_lab = (int)fmax(log10(as<double>(maxv)), 0) + 1 + ndec;
//   Function cut("cut"), seq("seq");
//   NumericVector endpoints = seq(Named("from", minv),
//                                 Named("to", maxv),
//                                 Named("by", lensubI));
//   endpoints = round(endpoints, ndec);
//   numsubI = endpoints.size() - 1;
//   NumericVector reprv_l = endpoints.import(endpoints.begin(),
//                                            endpoints.end() - 1);
//   NumericVector reprv_r = endpoints.import(endpoints.begin() + 1,
//                                            endpoints.end());
//   NumericVector reprv_m = (reprv_l + reprv_r) / 2.0;
//
//   CharacterVector I_info_co = cut(Named("x", reprv_m),
//                                 Named("breaks", endpoints),
//                                 Named("right", 0),
//                                 Named("dig.lab", dig_lab));
//   CharacterVector I_info_oc = cut(Named("x", reprv_m),
//                                 Named("breaks", endpoints),
//                                 Named("right", 1),
//                                 Named("dig.lab", dig_lab));
//
//   return(List::create(Named("endpoints", endpoints),
//                       Named("lensubI", lensubI),
//                       Named("numsubI", numsubI),
//                       Named("reprv.l", reprv_l),
//                       Named("reprv.m", reprv_m),
//                       Named("reprv.r", reprv_r),
//                       Named("I.info.co", I_info_co),
//                       Named("I.info.oc", I_info_oc)));
// }

// // [[Rcpp::export]]
// List C_Subintervals_numsubI(NumericVector minv,
//                             NumericVector maxv,
//                             IntegerVector numsubI_,
//                             IntegerVector ndec_) {
//   double lensubI;
//   int numsubI = as<int>(numsubI_), ndec = as<int>(ndec_);
//   int dig_lab = (int)fmax(log10(as<double>(maxv)), 0) + 1 + ndec;
//   Function cut("cut"), seq("seq");
//
//   NumericVector endpoints = seq(Named("from", minv),
//                                 Named("to", maxv),
//                                 Named("length.out", numsubI + 1));
//   endpoints = round(endpoints, ndec);
//   lensubI = endpoints[1] - endpoints[0];
//   NumericVector reprv_l = endpoints.import(endpoints.begin(),
//                                            endpoints.end() - 1);
//   NumericVector reprv_r = endpoints.import(endpoints.begin() + 1,
//                                            endpoints.end());
//   NumericVector reprv_m = (reprv_l + reprv_r) / 2.0;
//   CharacterVector I_info_co = cut(Named("x", reprv_m),
//                                 Named("breaks", endpoints),
//                                 Named("right", 0),
//                                 Named("dig.lab", dig_lab));
//   CharacterVector I_info_oc = cut(Named("x", reprv_m),
//                                 Named("breaks", endpoints),
//                                 Named("right", 1),
//                                 Named("dig.lab", dig_lab));
//   return(List::create(Named("endpoints", endpoints),
//                       Named("lensubI", lensubI),
//                       Named("numsubI", numsubI),
//                       Named("reprv.l", reprv_l),
//                       Named("reprv.m", reprv_m),
//                       Named("reprv.r", reprv_r),
//                       Named("I.info.co", I_info_co),
//                       Named("I.info.oc", I_info_oc)));
// }

// // [[Rcpp::export]]
// arma::mat C_ShrinkMat(NumericMatrix matA_, int shnrow, int shncol) {
//   int numrow = matA_.nrow();
//   int numcol = matA_.ncol();
//   arma::mat matA(matA_.begin(), numrow, numcol, false);
//   matA.reshape(numrow , numcol - shncol);
//   arma::mat matA2 = matA.t();
//   matA2.reshape(numcol - shncol, numrow - shnrow);
//   return(matA2.t());
// }

// // [[Rcpp::export]]
// NumericVector C_round(NumericVector x, NumericVector digits) {
//   return(round(x, as<double>(digits)));
// }


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


/*** R

*/
