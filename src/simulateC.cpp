#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
float simulateC (int k, int room, int replicate) {
  IntegerVector result (replicate);
  for (int r=0; r<replicate; r++){
    IntegerVector counts(365);
    for (int i=0; i<room; i++) {
      int pos = rand() % 365;
      if (pos < 365 && pos >= 0) counts[pos] ++;
    }
    if (max(counts) >= k) {
      result[r] = TRUE;
    } else result[r] = FALSE;
  }
  float prob = float(sum(result))/float(replicate);
  return prob;
}