
#include <Rcpp.h>
using namespace Rcpp;

//' psmatch from Rcpp
//' @return A list with two vectors (`match_id`, `match_x`).
//' @params x Propensity scores.
//' @examples
//' # Testing the function
//' set.seed(1231)
//' x <- cbind(runif(5))
//' @export
// [[Rcpp::export]]
List ps_match1(const NumericVector & x) {

  int n = static_cast<int>(x.size());

  IntegerVector indices(n);
  NumericVector values(n);

  for (int i = 0; i < n; ++i) {

    // Maximum value
    double cur_best = std::numeric_limits< double >::max();
    int cur_i = 0;

    for (int j = 0; j < n; ++j) {

      // We can't compare to oneself
      if (i == j)
        continue;

      // If it is lower, then update
      if (std::abs(x[i] - x[j]) < cur_best) {

        cur_best = std::abs(x[i] - x[j]);
        cur_i    = j;

      }

    }

    // In the end, we register the result
    indices[i] = cur_i;
    values[i]  = x[cur_i];

  }

  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_x"]  = values
  );

}

// [[Rcpp::export]]
List ps_match2(const NumericVector & x) {

  int n = static_cast<int>(x.size());

  IntegerVector indices(n);
  NumericVector values(n);

  for (int i = 0; i < n; ++i) {

    // Instead of allocating new memory, we can point by reference
    // (saves operations)
    double & cur_best = values[i];
    int    & cur_i    = indices[i];

    cur_best = std::numeric_limits< double >::max();

    cur_i = 0;

    for (int j = 0; j < n; ++j) {

      // We can't compare to oneself
      if (i == j)
        continue;

      // If it is lower, then update
      if (std::abs(x[i] - x[j]) < cur_best) {

        cur_best = std::abs(x[i] - x[j]);
        cur_i    = j;

      }

    }

  }

  for (int i = 0; i < n; ++i)
    values[i] = x[indices[i]];

  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_x"]  = values
  );

}

// [[Rcpp::export]]
List ps_match3(const NumericVector & x) {

  int n = static_cast<int>(x.size());

  IntegerVector indices(n);
  NumericVector values(n);
  values.fill(std::numeric_limits< double >::max());

  for (int i = 0; i < n; ++i) {

    // Instead of allocating new memory, we can point by reference
    // (saves operations)
    double & cur_best = values[i];
    auto & cur_i    = indices[i];

    for (int j = 0; j < i; ++j) {

      // If it is lower, then update
      double d = std::abs(x[i] - x[j]);
      if (d < cur_best) {

        cur_best = d;
        cur_i    = j;

      }

      if (d < values[j]) {

        values[j] = d;
        indices[j] = i;

      }

    }

  }

  for (int i = 0; i < n; ++i)
    values[i] = x[indices[i]];

  return List::create(
    _["match_id"] = indices + 1, // We add one to match R's indices
    _["match_x"]  = values
  );

}


