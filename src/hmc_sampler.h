#ifndef HMC_SAMPLER_H
#define HMC_SAMPLER_H
#include <Rcpp.h>
#include <cmath>
#include <random>
#include <RcppEigen.h>
#include "utils.h"
//namespace Rcpp;
//using namespace Eigen;

double log_likelihood( Eigen::VectorXd par, Rcpp::List m, bool neg = false);



Rcpp::List leapfrog_cpp(const Rcpp::List& m, int i, int trajectory_length, double stepsize, Eigen::VectorXd v_loc_old, Eigen::VectorXd v_scale_old);


double H(Eigen::VectorXd par , Eigen::VectorXd v, Rcpp::List m);

double log_acceptance_criterion(Eigen::VectorXd prev_x, Eigen::VectorXd Impulse_old, Eigen::VectorXd x, Eigen::VectorXd v, Rcpp::List m);

Rcpp::List hmc_sampler( const Rcpp::List& m, int i, int trajectory_length, double stepsize);

Rcpp::List build_chain_cpp(Rcpp::List m, double stepsize, int trajectory_length, int chain_length, int burn_in, int thin);

Rcpp::List Tuner(Rcpp::List m);

#endif
