# Measurement-Invariance
R code for conducting simulation and analysis about measurement invariance

## Repositories content
### Detecting Violations of Factorial Invariance Using Data-Based Specification Searches A Monte Carlo Study
  In this folder, you can find the simulation process conducted by R. Due to to the different software which the author conducted the simulation, you have to read the procedure of the simulation from creating sample to analysis firstly.
  
  The R script named raw_function contains original function for generating data and analysis including loop function, apply function and function with parallel computing. fin_funtion R script includes the final version of function for the project.
  
  Next, let's talk about condition. Firstly, the all_condition_info R script includes the brief introduction of each simulation condition from generating dataset to conducting CFA.
  
  The code conducting simulation is old and worse than the Eunju Jung & Myeongsun Yoon (2016) one. Please use the latest function and code.
  
### Comparisons of Three Empirical Methods for Partial Factorial Invariance: Forward, Backward, and Factor-Ratio Tests

### Permutation Randomization Methods for Testing Measurement Equivalence and Detecting Differential Item Functioning in Multiple-Group Confirmatory Factor Analysis

## Reference
1. Myeongsun Yoon & Roger E. Millsap (2007) Detecting Violations of Factorial Invariance Using Data-Based Specification Searches: A Monte Carlo Study, Structural Equation Modeling: A Multidisciplinary Journal, 14:3, 435-463, DOI: [10.1080/10705510701301677](https://www.tandfonline.com/doi/full/10.1080/10705510701301677)

2. Eunju Jung & Myeongsun Yoon (2016) Comparisons of Three Empirical Methods for Partial Factorial Invariance: Forward, Backward, and Factor-Ratio Tests, Structural Equation Modeling: A Multidisciplinary Journal, 23:4, 567-584, DOI: [10.1080/10705511.2015.1138092](https://www.tandfonline.com/doi/full/10.1080/10705511.2015.1138092)

3. Jorgensen, T. D., Kite, B. A., Chen, P. Y., & Short, S. D. (2018). Permutation randomization methods for testing measurement equivalence and detecting differential item functioning in multiple-group confirmatory factor analysis. Psychological methods, 23(4), 708. DOI:[https://doi.org/10.1037/met0000152](https://psycnet.apa.org/doiLanding?doi=10.1037%2Fmet0000152)