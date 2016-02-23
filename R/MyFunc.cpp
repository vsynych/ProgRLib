#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
CharacterVector myFunc(DataFrame x) {
NumericVector col1 = as(x["col1"]);
NumericVector col2 = as(x["col2"]);
NumericVector col3 = as(x["col3"]);
NumericVector col4 = as(x["col4"]);
int n = col1.size();
CharacterVector out(n);
for (int i=0; i 4){
  out[i] = "greater_than_4";
} else {
  out[i] = "lesser_than_4";
}
}
return out;
}