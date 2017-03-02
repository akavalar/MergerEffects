# MergerEffects
Merger Effects Estimation in R

## Requirements & Installation
In order to use the package, make sure you have the following packages installed:

1. `nleqslv` (non-linear equation solver)
2. `numDeriv` (calculation of numerical derivatives, i.e. Jacobian matrices)
3. `foreach` & `doParallel` (parallelized execution of the code)

Install the MergerEffects package by running something like this:
`install.packages("[your path]/MergerEffects_1.0.tar.gz", repos = NULL, type="source", lib=.libPaths())`

## Structure of the package
![Tree Structure](/Structure.png?raw=true "Nested hierarchy of MergerEffects functions")

The main user-facing function are `mergerSimulate()` and `writeToFile()`, especially if one wants to iterate over all possible combinations of input parameters.

## Features
- Easy to use, fast, flexible, efficient ("Compute once!"), modular and therefore extendable.
- Allows for synergies, partial ownership, etc.
- Allows for linear, Almost Ideal Demand System (AIDS), and linear-logarithmic demand systems
- Allows for serial vs. parallel execution
- Allows for diagnostic checks (disabled by default)
  - "Have we found the optimum?", "Is our optimum indeed a maximum?", etc.
  - Slows down the execution because of the numerical calculation of the Jacobian matrix of first derivatives
- Can fine-tune some parameters of the underlying non-linear solver

## Inputs
Here I assume you are using `mergerSimulate()` to simulate a merger involving N products and M companies.
- q0: vector of N quantities
- p0: vector of N prices
- margins: vector of N margins
- ownPre: pre-merger NxM matrix of shares between 0 and 1; each row represents one product, with the individual entries representing how much of that product's profit goes to which company (each product's entries should add up to 1)
- ownPost: a list of all possible post-merger NxM matrices of shares between 0 and 1 indicating how profit shares changed as a result of the merger; if using only one matrix, can use either the matrix directly or can put it in the list
- conPre: pre-merger NxM matrix of 0's and 1's; each row represents one product and must have one 1 entry indicating the company that sets the price of that product, with all other entries being 0
- conPost: a list of all possible post-merger NxM matrices of 0's and 1's indicating the merger-induced change in price control for each product; if one matrix, can use either the matrix directly or can put it in the list
- PoD: a list of all possible NxN matrices showing the proportions of diversion; all entries in a given row in the matrix must add up to 1 and the diagonal elements must all be 0; matrices can be replaced by "quantity" or "nests" strings, in which case a quantity-based PoD matrix and/or the nests-based PoD matrix are automatically calculated and used by the code; use a list even if it contains just one matrix or one string!
- delta: curvature parameter of the linear demand functions only; either scalar or vector of arbitrary length
- shape: choice of the the demand system (1 = linear; 2 = AIDS; 3 = LinLog); either scalar or vector of up to length 3
- marketElasticity: should be self-explanatory (but make sure elasticities are negative); either scalar or vector of arbitrary length
- gamma: scalar used in the calculation of the nests-based PoD matrix
- e_1, e_2, ..., e_N: less-than-or-equal-to-N number of either scalars or vectors of arbitrary length defining all possible eficiencies ("savings") for each product (if some e_i not supplied, default value = 0)
- cores: scalar, number of cores used (default value = 0, i.e. sequential computation)
- diagnostics: scalar, logging of diagnostic info (default value = 0, no logging)
- tol: scalar, maximum tolerance level (default value = 1e-6)
- retries: scalar, maximum number of attempts when numerically optimizing functions

## Outputs
The output of the `mergerSimulate()` function is a dataframe containing both the results of each iteration, the (varying) data and potentially also diagnostic information associated with that particular iteration of the model. Each cell of the dataframe is a list.

Variables stored inside the dataframe that begin with *check* are returned only if logging of diagnostic info is enabled (i.e., if diagnostic = 1). Variables starting with *data* indicate the (varying) data inputs used with a particular iteration of the model.
The `writeToFile()` and `writeToFile2()` functions will output the indices and the varying input data to a pipe-delimited CSV file.

## What is a merger simulation?
A decent understanding of the theory of a differentiated Bertrand games is assumed. For more information, review the internal logic of the code and the comments found therein. Also, have a look at the sample code (and the accompanying sample data files).

As a rough sketch, every merger simulation consists of two parts:

**Step 1: "Calibration"**

- Using observed actual industry data, we tease out the parameters of N demand functions for N products in the pre-merger world. By observing the equilibrium outcomes, we can infer what kind of demand system generated such outcomes.
- Data required:
  - Actual prices, quantities, margins/costs for each of N products
  - Ownership and control structure
  - Degree of competition between all products ("diversion ratios")
  - Market elasticity, i.e. aggregate elasticity of total demand with respect to a proportional increase in all prices
	
**Step 2: "Simulation"**

- Armed with the knowledge of consumer behavior (i.e. knowing how the demand system looks like), we simulate the merger by changing the ownership and control structure of the economy. This allows us to find the new optimal prices and quantities in the post-merger environment using the same demand functions identified in Step 1.
- Data required:
  - New ownership and control structure
  - Potentially new lower costs due to merger synergies
  
I might write a more detailed memo on the theory behind merger simulations at some point in the future.
