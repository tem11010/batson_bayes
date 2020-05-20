#include <Rcpp.h>
#include <iostream>
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
double probcpp(bool cog, int c, int m, double d){
  if(cog){
    return log(c)+d-log(c*exp(d)+m);
  }else{
    return log(m)-log(c*exp(d)+m);
  }
}


// x, col 2 should be number cognizable, 3 be total, 4 be cognizable
// [[Rcpp::export]]
double Lk_cpp(double d,NumericMatrix x, bool L){
  double lglkr=0;
  for(int i=0; i < x.nrow();++i){
    lglkr = lglkr + probcpp(x(i,3),x(i,1),x(i,2)-x(i,1),d);
  }
  if(L){
    return lglkr;
  }else{
    return exp(lglkr);
  }
}

// [[Rcpp::export]]
double Priorcpp(double theta, double mu, double std) {
  return 1/(sqrt(2*M_PI)*std)*exp(-pow((theta-mu),2)/(2*pow(std,2)));
}

// [[Rcpp::export]]
List make_posterior(NumericMatrix x, int niter, 
                             double theta_start_val, double theta_proposal_sd){
  NumericVector theta(niter);
  double current_theta;
  double new_theta;
  double r;
  double thresh;
  double acpt;
  int acptn=0;
 
  theta[0] = theta_start_val;
  
  for(int i = 1; i < niter; ++i){
    
    current_theta = theta[i-1];
    
    new_theta = rnorm(1,current_theta, theta_proposal_sd)[0];
    
    
    r = log(Priorcpp(new_theta,0,2))+Lk_cpp(new_theta,x, 1)-log(Priorcpp(current_theta,0,2))-
      Lk_cpp(current_theta,x,1);
    
    
    thresh = log(runif(1)[0]);
    
    
    
    if(thresh<r){
      theta[i] = new_theta;
      acptn+=1;
      //std::cout << acptn << std::endl;
    } else{
      theta[i] = current_theta;
    }
    
    
  }
  //std::cout << acptn << std:: endl;
  acpt = static_cast<double>(acptn)/niter;
  //std::cout << acpt << std:: endl;
  return List::create(Named("theta")=theta,Named("accept_rate")=acpt);
}


//this is a comment






