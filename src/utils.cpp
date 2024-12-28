#include <Rcpp.h>
#include <cmath>
#include <random>
#include <RcppEigen.h>
#include "utils.h"


using namespace Rcpp;
using namespace Eigen;

// [[Rcpp::depends(RcppEigen)]]



//random normal function using the RNG of the R instance via Rcpp sugar
Eigen::VectorXd init_random_normal(const int n, double mean, double sd) {
  Rcpp::NumericVector v = rnorm(n, mean , sd) ;
  Eigen::VectorXd res = Rcpp::as<Eigen::VectorXd>(v);
  return res;
}


Eigen::VectorXd concatenate(const Eigen::VectorXd& v1, const Eigen::VectorXd& v2) {

  Eigen::VectorXd result(v1.size() + v2.size());
  result.head(v1.size()) = v1;
  result.tail(v2.size()) = v2;

  return result;
}


//gradient function
// [[Rcpp::export]]
Eigen::VectorXd grad_ll_cpp(const Eigen::VectorXd& beta,
                            const Eigen::VectorXd& gamma,
                            const Rcpp::List& m,
                            const std::string& type) {
  const Eigen::MatrixXd X = Rcpp::as<Eigen::MatrixXd>( m["x"]);
  const Eigen::MatrixXd Z = Rcpp::as<Eigen::MatrixXd>(m["z"]);
  const Eigen::VectorXd y = Rcpp::as<Eigen::VectorXd> (m["y"]);
  //Rcpp::Rcout << "gamma: " << gamma << std::endl;

  int n_beta = Rcpp::as<int>(m["dim_beta"]);
  int n_gamma = Rcpp::as<int>(m["dim_gamma"]);

  Eigen::VectorXd mu = X * beta;
  Eigen::VectorXd log_sigma = Z * gamma;

  Eigen::VectorXd sigma = log_sigma.array().min(700).exp();  // limit size to 700 to ensure nurmeric stability and prevent overflow


  Eigen::VectorXd grad_beta(n_beta);
  Eigen::VectorXd grad_gamma(n_gamma);

  Eigen::VectorXd residuals = y - mu;
  Eigen::VectorXd  sigma_squared = sigma.array().square();


  if (type == "location") {
    grad_beta = X.transpose() * residuals.cwiseQuotient(sigma_squared);
    return grad_beta;
  }

  if (type == "scale") {
    Eigen::VectorXd residuals_squared = residuals.array().square();


    Eigen::VectorXd res_sig_ratio = residuals_squared.cwiseQuotient(sigma_squared);


    Eigen::VectorXd term = -1.0 + res_sig_ratio.array();

    Eigen::VectorXd grad_gamma = Z.transpose() * term;



  //  Eigen::VectorXd term1 = Z.colwise().sum();
  //  Eigen::VectorXd term2 = (residuals.cwiseProduct(residuals).cwiseQuotient(sigma.cwiseProduct(sigma))).asDiagonal() * Z;
  //  grad_gamma = term2.colwise().sum() - term1;
    return grad_gamma;
  }
  // Return an empty vector if type is not recognized
  return Eigen::VectorXd(0);
}


//vreates a log uniform value, using RNG from set.seed() in R
double log_uniform(){
  // Initialize the random number generator once


  // Generate a random uniform number and calc the log
  Rcpp::NumericVector vec = runif(1);
  double uniform_value = vec(0);
  // catch extreme case of value being 0 and try again
  if (uniform_value == 0.0) {
   return log_uniform();
   }

  double log_value = std::log(uniform_value);


  return log_value;
}

//clip gradients if necessary, to ensure numeric stability and prevent overflow
Eigen::VectorXd clip_gradients(const Eigen::VectorXd& grad, double clip_value) {
  // Clipping der Gradientenwerte
  return grad.array().min(clip_value).max(-clip_value);
}


//acceptance function
bool acceptance(double log_a_unif, double log_a_criterion){
  return log_a_unif < log_a_criterion;
}



//matrix function for thinning of the chain
NumericMatrix thin_matrix(const NumericMatrix& mat, int thin) {
  int n_rows = mat.nrow();
  int n_cols = mat.ncol();
  int new_n_rows = (n_rows + thin - 1) / thin; // Aufrunden der Anzahl der Zeilen
  NumericMatrix result(new_n_rows, n_cols);

  for (int i = 0; i < new_n_rows; ++i) {
    result.row(i) = mat.row(i * thin);
  }

  return result;
}

