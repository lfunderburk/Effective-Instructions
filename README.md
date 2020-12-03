# Effective-Instructions
Author: David Freeman, Erik Kimbrough, and Garrett Petersen, and Hanh Tong  
 

This repository contains the data file and part of the statistical analysis for the paper ["Instructions"](https://link.springer.com/article/10.1007/s40881-018-0059-0?error=cookies_not_supported&error=cookies_not_supported&code=8fe07e78-e48f-4541-b975-ebbceaa213b5&code=cde031c5-f019-4626-96fb-600f2162fe77).  

This code contains the following analysis:
- Average Non-money maximizing behavior (NMB) and average quiz score, by treatment.
- Fisher's exact test of no association between treatments and NMB.
- Kruskal-Wallis test to compare distribution of quiz score across treatments.
- Goodman and Kruskal's gamma: tests of association between quiz scores and NMB.
- Wilcoxon rank-sum tests: test whether some treatments improved quiz scores.
- Logistic regressions:
  - a model of NMB as a logistic function of treatment.
  - a model of NMB as a logistic-linear function of quiz score, treatment, and their interactions.
- A linear regression to model treatment effects on quiz scores.
- Robust (HC1) 95% confidence intervals.
- Mediation analysis.
