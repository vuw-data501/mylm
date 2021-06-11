#include <Rcpp.h>
using namespace Rcpp;

//' Row Sums
//'
//' @export
// [[Rcpp::export]]
NumericVector rowSumsC(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(nrow);

  for (int i = 0; i < nrow; i++) {
    double total = 0;
    for (int j = 0; j < ncol; j++) {
      total += x(i, j);
    }
    out[i] = total;
  }
  return out;
}

//' Matrix multiplication
//'
//' @param a First matrix
//' @param b Second matrix
//'
//' @export
// [[Rcpp::export]]
NumericMatrix matrix_multiply_c(NumericMatrix a, NumericMatrix b) {
  int na1 = a.nrow();
  int nab = a.ncol();
  int nb2 = b.ncol();
  NumericMatrix ab(na1,nb2);
  int i,j,k;

  for(i=0; i<na1; i++) {
    for(j=0; j<nb2; j++) {
      ab(i,j) = 0;
      for(k=0; k<nab; k++) {
        ab(i,j) += a(i,k)*b(k,j);
      }
    }
  }

  return ab;
}
