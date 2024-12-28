#ifndef UTILS_H
#define UTILS_H
#include <Rcpp.h>
#include <cmath>
#include <random>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace Eigen;


Eigen::VectorXd init_random_normal(int n, double mean, double sd);

Eigen::VectorXd concatenate(const Eigen::VectorXd& v1, const Eigen::VectorXd& v2);


Eigen::VectorXd grad_ll_cpp(const Eigen::VectorXd& beta,
                            const Eigen::VectorXd& gamma,
                            const Rcpp::List& m,
                            const std::string& type);


double log_uniform();


Eigen::VectorXd clip_gradients(const Eigen::VectorXd& grad, double clip_value);


bool acceptance(double log_a_unif, double log_a_criterion);

NumericMatrix thin_matrix(const NumericMatrix& mat, int thin);



#endif
