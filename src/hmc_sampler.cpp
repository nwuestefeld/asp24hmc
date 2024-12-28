#include <Rcpp.h>
#include <cmath>
#include <random>
#include <RcppEigen.h>
#include "utils.h"
#include "hmc_sampler.h"
using namespace Rcpp;
using namespace Eigen;


// [[Rcpp::depends(RcppEigen)]]
double log_likelihood( Eigen::VectorXd par, Rcpp::List m, bool neg){
  int dim_beta = Rcpp::as<int>(m["dim_beta"]);
  int dim_gamma = Rcpp::as<int>(m["dim_gamma"]);

  Eigen::VectorXd beta, gamma;
  beta = par.head(dim_beta);
  gamma = par.tail(dim_gamma);


  const Eigen::MatrixXd& x = Rcpp::as<Eigen::MatrixXd>(m["x"]);
  const Eigen::MatrixXd& z = Rcpp::as<Eigen::MatrixXd>(m["z"]);
  const Eigen::VectorXd& y = Rcpp::as<Eigen::VectorXd>(m["y"]);

  Eigen::VectorXd mu = x * beta;

  Eigen::VectorXd log_sigma = z * gamma;
  //overflow check
  Eigen::VectorXd sigma = (log_sigma.array() > 700).select(Eigen::VectorXd::Constant(log_sigma.size(), 1.0e+300), log_sigma.array().exp());
  sigma = sigma.array().max(1.0e-10); // Hinzufügen einer kleinen Konstante, um Divisionen durch Null zu vermeiden

 // Rcpp::NumericVector Y = m["y"];

  double result = 0.0;
  for( int i = 0; i< y.size(); ++i){
    // result += R::dnorm(y(i),mu(i),sigma(i), true);
    result += -0.5 * (log(2 * M_PI) + 2 * log(sigma(i)) + (y(i) - mu(i)) * (y(i) - mu(i)) / (sigma(i) * sigma(i)));
  }

 // Rcpp::Rcout << "mu: " << mu.transpose() << std::endl;
  //Rcpp::Rcout << "sigma: " << sigma.transpose() << std::endl;
 // Rcpp::Rcout << "log_likelihood: " << log_likelihood << std::endl;


  return neg ? -result : result;
}



// Leapfrog HMC function
// [[Rcpp::export]]
Rcpp::List leapfrog_cpp(const Rcpp::List& m, int i, int trajectory_length, double stepsize, Eigen::VectorXd v_loc_old, Eigen::VectorXd v_scale_old) {

  //necessary if executing skript is R + use Numeric vector instead of eigen
  //i -= 1;

  //double clip_value = 5;
  //  Rcpp::Rcout << "Initial grad loc: " << grad_loc.transpose() << std::endl;


  Rcpp::List mcmc = m["mcmc"];
  const Eigen::MatrixXd& loc = Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(mcmc["location"]);
  const Eigen::MatrixXd& scale = Rcpp::as<Eigen::Map<Eigen::MatrixXd>>(mcmc["scale"]);




  int n_beta = Rcpp::as<int>(m["dim_beta"]);
  int n_gamma = Rcpp::as<int>(m["dim_gamma"]);

  Eigen::VectorXd v_loc = std::move(v_loc_old);
  Eigen::VectorXd v_scale = std::move(v_scale_old);


  Eigen::VectorXd x_loc = loc.row(i);
  Eigen::VectorXd x_scale = scale.row(i);

  //Update the momentum before loop
  Eigen::VectorXd grad_loc = grad_ll_cpp(x_loc, x_scale, m, "location");
  Eigen::VectorXd grad_scale = grad_ll_cpp(x_loc, x_scale, m, "scale");
////  grad_loc = clip_gradients(grad_loc, clip_value);
//  grad_scale = clip_gradients(grad_scale, clip_value);
//  Rcpp::Rcout << "Initial grad loc: " << grad_loc.transpose() << std::endl;
//  Rcpp::Rcout << "Initial grad scale: " << grad_scale.transpose() << std::endl;

  v_loc += 0.5 * stepsize * grad_loc;
  v_scale += 0.5 * stepsize * grad_scale;



  // Loop over the trajectory
  //Rcpp::Rcout << "trajectory_length: " << trajectory_length << std::endl;

  for (int j = 0; j < trajectory_length; j++) {
    //Rcpp::Rcout << "Iteratrion: " << j+1 << std::endl;

    x_loc += stepsize * v_loc;
    x_scale += stepsize * v_scale;

   // Rcpp::Rcout << std::fixed << std::setprecision(15) << "vloc: " << v_loc << std::endl;
    //Rcpp::Rcout << std::fixed << std::setprecision(15) << "vscale : " << v_scale << std::endl;

    //Rcpp::Rcout << "x_loc im loop: " << x_loc << std::endl;
    grad_loc = grad_ll_cpp(x_loc, x_scale, m, "location");
    grad_scale = grad_ll_cpp(x_loc, x_scale, m, "scale");

    // Clipping the gradients
    //grad_loc = clip_gradients(grad_loc, clip_value);
   // grad_scale = clip_gradients(grad_scale, clip_value);
    v_loc += stepsize * grad_loc;
    v_scale += stepsize * grad_scale;

  }

  x_loc += stepsize * v_loc;
  x_scale += stepsize * v_scale;


  grad_loc = grad_ll_cpp(x_loc, x_scale, m, "location");
  grad_scale = grad_ll_cpp(x_loc, x_scale, m, "scale");

  // Clipping the gradients
  //grad_loc = clip_gradients(grad_loc, clip_value);
 // grad_scale = clip_gradients(grad_scale, clip_value);

  // Update the momentum after loop
  v_loc += 0.5 * stepsize * grad_loc;
  v_scale += 0.5 * stepsize * grad_scale;
  //Rcpp::Rcout << "v_loc nach loop: " << v_loc << std::endl;



  //return the list
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("location") = Rcpp::wrap(x_loc),
    Rcpp::Named("scale") = Rcpp::wrap(x_scale),
    Rcpp::Named("v_loc") = Rcpp::wrap(v_loc),
    Rcpp::Named("v_scale") = Rcpp::wrap(v_scale)
  );
  return output;
}

//Hamiltonian Function
double H(Eigen::VectorXd par , Eigen::VectorXd v, Rcpp::List m){


  //caluclate the negative log likelihood
  double ll = log_likelihood(par, m, true);

  // split the vector in location and scale
  int n_beta = Rcpp::as<int>(m["dim_beta"]);
  int n_gamma = Rcpp::as<int>(m["dim_gamma"]);

Eigen::VectorXd v_loc = v.head(n_beta);
Eigen::VectorXd v_scale = v.tail(n_gamma);

  //calculate the Kinetic Energy
  double K = 0.5 * (v_loc.array().square().sum() + v_scale.array().square().sum());


  return ll + K;
}


//function of the log acceptance criterion
// [[Rcpp::export]]
double log_acceptance_criterion(Eigen::VectorXd prev_x, Eigen::VectorXd Impulse_old, Eigen::VectorXd x, Eigen::VectorXd v, Rcpp::List m){
    double H_old = H(prev_x, Impulse_old, m);
    double H_new = H(x, v, m);

    if (std::isnan(H_old) || std::isnan(H_new)) {
      // Wenn ein NaN auftritt, lehne den Vorschlag ab
      return -std::numeric_limits<double>::infinity();
    }


    //compare
    return std::min(0.0, -(H_new - H_old));

}

//function of the hmc sampling
// [[Rcpp::export]]
Rcpp::List hmc_sampler( const Rcpp::List& m, int i, int trajectory_length, double stepsize){

  //init the objects of processing

  //sublists of m$mcmc
  const Rcpp::List mcmc = m["mcmc"];
  const Rcpp::List coeffs = m["coeffs"];

  //location matrix and scale matrix
  const Eigen::MatrixXd location_matrix = Rcpp::as<Eigen::MatrixXd>(mcmc["location"]);
  const Eigen::MatrixXd scale_matrix = Rcpp::as<Eigen::MatrixXd>(mcmc["scale"]);

  //init starting value as last object
  const Eigen::VectorXd x_loc_old = location_matrix.row(i);
  const Eigen::VectorXd x_scale_old = scale_matrix.row(i);

  //definie dimensions
  int n_beta = Rcpp::as<int>(m["dim_beta"]);
  int n_gamma = Rcpp::as<int>(m["dim_gamma"]);


  //init random normal starting value for velocity
  Eigen::VectorXd v_loc = init_random_normal(n_beta, 0.0, 1.0);
  Eigen::VectorXd v_scale = init_random_normal(n_gamma, 0.0, 1.0);

  //execute the leapfrog
  Rcpp::List result = leapfrog_cpp(m,i,trajectory_length,stepsize, v_loc, v_scale);



  //grab the proposals for the x and v from the leapfrog
 const  Eigen::VectorXd& x_loc_new = Rcpp::as<Eigen::VectorXd>(result["location"]);
 const  Eigen::VectorXd& x_scale_new = Rcpp::as<Eigen::VectorXd>(result["scale"]);
 const  Eigen::VectorXd& v_loc_new = Rcpp::as<Eigen::VectorXd>(result["v_loc"]);
 const Eigen::VectorXd&  v_scale_new = Rcpp::as<Eigen::VectorXd>(result["v_scale"]);

// Rcpp::Rcout << "x_loc_new: " << x_loc_new<< std::endl;


  //concatenate to vectors for Hamiltonian
  Eigen::VectorXd x_old = concatenate(x_loc_old, x_scale_old);
  Eigen::VectorXd x_new = concatenate(x_loc_new, x_scale_new);
  Eigen::VectorXd Impulse_old = concatenate(v_loc, v_scale);
  Eigen::VectorXd Impulse_new = concatenate(v_loc_new, v_scale_new);



  //random unform for acceptance rate
  double log_a_unif = log_uniform();
  //Rcpp::Rcout << "log_a_unif: " << log_a_unif << std::endl;


  //define the log acceptance criterion based on the values
  double log_a_criterion = log_acceptance_criterion(x_old, Impulse_old, x_new, Impulse_new, m);
  //decide
  bool accept = acceptance(log_a_unif, log_a_criterion);
 // Rcpp::Rcout << "log_a_criterion: " << log_a_criterion << std::endl;
 // Rcpp::Rcout << "log_a_unif " << log_a_unif << std::endl;
 // Rcpp::Rcout << "accept: " << accept << std::endl;

// Rcpp::Rcout << "v: " << Impulse_new << std::endl;

  if(accept){
    //export the new values
    Rcpp::List output = Rcpp::List::create(
      Rcpp::Named("location") = x_loc_new,
      Rcpp::Named("scale") = x_scale_new,
      Rcpp::Named("accept") = accept
    );
    return output;
  }
  else{
    //export the old values
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("location") = x_loc_old,
    Rcpp::Named("scale") = x_scale_old,
    Rcpp::Named("accept") = accept
  );
    return output;
  }

}


// [[Rcpp::export]]
Rcpp::List build_chain_cpp(Rcpp::List m, double stepsize, int trajectory_length, int chain_length, int burn_in, int thin){


  //init the m object parts and sublists
  Rcpp::List mcmc = m["mcmc"];
  NumericMatrix location_matrix = mcmc["location"];
  NumericMatrix scale_matrix = mcmc["scale"];

  //grab the initial coefficients
  Rcpp::List coeffs = m["coeffs"];
  Rcpp::NumericVector location_vector = coeffs["location"];
  Rcpp::NumericVector scale_vector = coeffs["scale"];
 //  Rcpp::Rcout << "location_vector: " << location_vector << std::endl;


  location_matrix(0, _) = location_vector;
  scale_matrix(0, _) = scale_vector;

  //init counter for accepted samples
  int count_accept = 0;

  //loop the sampler
  for(int i = 0; i<chain_length; i++){
    Rcpp::List result = hmc_sampler( m, i,trajectory_length, stepsize);
    bool accept = result["accept"];




    //fill the values into the matrices
    location_matrix(i+1, _) = Rcpp::as<Rcpp::NumericVector>(result["location"]);
    scale_matrix(i+1, _) = Rcpp::as<Rcpp::NumericVector>(result["scale"]);

    if(accept){
      count_accept++;
    }
  }



  //calculate acceptance rate

  double accepted = static_cast<double>(count_accept) / chain_length;
  mcmc["accepted"] = accepted;
  //Rcpp::Rcout << "accepted: " << accepted << std::endl;

  //remove first entry
  Rcpp::NumericMatrix location_matrix_1 = location_matrix(Rcpp::Range(1, location_matrix.nrow() - 1), Rcpp::_);
  Rcpp::NumericMatrix scale_matrix_1 = scale_matrix(Rcpp::Range(1, scale_matrix.nrow() - 1), Rcpp::_);




  //calc the remaining dimensions after removing first entry (bisschen overkill)
  int n_cols_location = location_matrix_1.ncol();
  int n_cols_scale = scale_matrix_1.ncol();
  int n_rows_location = location_matrix_1.nrow();
  int n_rows_scale = scale_matrix_1.nrow();

  //burn in
  Rcpp::NumericMatrix location_matrix_burned = location_matrix_1(Range(burn_in, n_rows_location - 1), _);
  Rcpp::NumericMatrix scale_matrix_burned = scale_matrix_1(Range(burn_in, n_rows_scale - 1), _);




  //thining
  //init the matrices for thining
  Rcpp::NumericMatrix location_matrix_final;
  Rcpp::NumericMatrix scale_matrix_final;



  if(thin != 0){
    //thinning
    location_matrix_final = thin_matrix(location_matrix_burned, thin);
    scale_matrix_final = thin_matrix(scale_matrix_burned, thin);
  } else {
    //no thinning
    location_matrix_final = location_matrix_burned;
    scale_matrix_final = scale_matrix_burned;
  }




  //point estimation for the coefficients
  //switch to Eigen to use matrix multiplication
  Eigen::MatrixXd mat_loc = Rcpp::as<Eigen::MatrixXd>(location_matrix_final);
  Eigen::MatrixXd mat_scale = Rcpp::as<Eigen::MatrixXd>(scale_matrix_final);

  //calculate the colwise means
  Eigen::VectorXd point_estimate_loc = mat_loc.colwise().mean();
  Eigen::VectorXd point_estimate_scale = mat_scale.colwise().mean();

  //update the coeffs
  coeffs["location"] = Rcpp::wrap(point_estimate_loc);
  coeffs["scale"] = Rcpp::wrap(point_estimate_scale);
  m["coeffs"] = coeffs;

  //save the chains
  mcmc["location"] = location_matrix_final;
  mcmc["scale"] = scale_matrix_final;
  m["mcmc"] = mcmc;


  return m;
}


