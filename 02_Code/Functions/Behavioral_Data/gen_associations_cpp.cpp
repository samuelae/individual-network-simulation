#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix gen_associations_cpp(NumericMatrix &am,
                                   IntegerVector cue_inds,
                                   int n_responses = 3,
                                   int n_rep = 3, 
                                   int n_sub = 10) {

  // calculate repetitions
  int repetitions = n_rep * n_sub;
  
  // create associations container
  int n_cues = cue_inds.size();
  IntegerMatrix associations(n_cues * repetitions, n_responses + 3);

  // create response index vector
  IntegerVector response_inds = seq(0, am.ncol() - 1);
  
  // iterate through repetitions: how each cue is encountered
  for(int r = 0; r < repetitions; ++r){
    
    // iterate through cues
    for(int i = 0; i < n_cues; ++i){
      
      // extract edge weights 
      int index = cue_inds[i];
      NumericVector probs = am(index,_);
      
      // store cue
      associations(i + r * n_cues, 0) = std::floor(double(r) / n_rep); // subject id
      associations(i + r * n_cues, 1) = r % n_rep; // repetition id
      associations(i + r * n_cues, 2) = cue_inds[i]; // cue id
      
      // sample associations
      IntegerVector asso_i = sample(response_inds, n_responses, false, probs);
      
      // store associations
      for(int j = 0; j  < n_responses; ++j){
        associations(i + r * n_cues, j + 3) = asso_i[j];
        }
      }
    }
  return associations;
  }

// // [[Rcpp::export]]
// NumericVector sample_cpp(NumericVector x, NumericVector w, int n = 3){
//   NumericVector tmp = sample(x, size = n, replace = false, probs = w);
//   return tmp;
//   }

