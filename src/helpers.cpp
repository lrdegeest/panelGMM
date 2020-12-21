// -*- mode: C++; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil; -*-

// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "RcppArmadillo.h"

// generic matrix-vector multiplication (slightly faster than BLAS)
//' @export
// [[Rcpp::export]]
arma::vec do_mv(const arma::mat& m, const arma::vec& v) {
    return m * v;
}

// calculate Z'uu'Z for each cross section
// need this to calculate weighting matrix S
//' @export
// [[Rcpp::export]]
arma::field<arma::mat> do_ZuuZ(const arma::mat& Z, const arma::vec& e, int n, int t){
    arma::field<arma::mat> ZuuZ_list(n);
    int r = 0;
    for(int i = 0; i < n; i++) {
        arma::mat Zi = Z.rows(r, r + (t - 1));
        arma::vec ei = e.rows(r, r + (t - 1));
        arma::mat ZuuZ = Zi.t() * ei * ei.t() * Zi;
        ZuuZ_list(i) = ZuuZ;
        r = r + t;
    }
    return ZuuZ_list;
}

// calculate Z'u for each cross section
// need this for OIR test
//' @export
// [[Rcpp::export]]
arma::field<arma::mat> do_Zu(const arma::mat& Z, const arma::vec& e, int n, int t){
    arma::field<arma::mat> Zu_list(n);
    int r = 0;
    for(int i = 0; i < n; i++) {
        arma::mat Zi = Z.rows(r, r + (t - 1));
        arma::vec ei = e.rows(r, r + (t - 1));
        arma::mat Zu = Zi.t() * ei;
        Zu_list(i) = Zu;
        r = r + t;
    }
    return Zu_list;
}
