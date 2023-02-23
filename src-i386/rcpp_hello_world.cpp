
#include <Rcpp.h>
using namespace Rcpp;

//' Hello world from Rcpp
//' @return A list with two vectors (`x`, `y`).
//' @examples
//' rcpp_hello_world()
//' @export
// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
