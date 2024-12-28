# asp24hmc

## Introduction
**asp24hmc** provides an implementation of the Hamiltonian Monte Carlo (HMC) algorithm in R. 
This package allows users to perform Bayesian inference using HMC for regression models with specified location and scale components.

## Key Features
- Hamiltonian Markov Chain Monte Carlo implementation:  Our R package uses HMC to estimate parameters for location scale regression models.
- Methods: Build-in function for inference
- Automatic parameter-tuning: Our algorithm picks automatically the best paramters
- Fast chain building: Our implementation leverages C++ files for speed
- Simulation Study


## Installation

To install the `asp24hmc` package clone this repository and use the `devtools` package from CRAN. Make sure you have `devtools` installed to run the following command from inside the folder on you local machine:

```r
devtools::install()
```
## Example Usage
```r
# Load necessary libraries (assuming asp24hmc is installed)
library(asp24hmc)

# Prepare the iris dataset for a regression example
data(iris)

# Define a location model and a scale model
location_formula <- Sepal.Length ~ Sepal.Width + Petal.Length
scale_formula <- ~ Sepal.Width  # For simplicity, we'll use Sepal.Width for the scale model

# Fit the model using HMC
# Using some reasonable parameters for this example
result <- GLSR_HMC(
    location = location_formula,
    scale = scale_formula,
    data = iris,
    chain_length = 5000,
    stepsize = 0.01,
    trajectory_length = 1,
    burn_in = 1000,
    thin = 1
)

# Print the results
print(result)
```


## Authors:
Arne Tillmann
Lukas Brüwer
Nils Wüstefeld


