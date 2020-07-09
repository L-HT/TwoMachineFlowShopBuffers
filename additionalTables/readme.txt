Abbreviations for the algorithms:
---------------------------------

HVNS: Hybrid Variable Neighborhood Search
HVNS-tuned: HVNS with irace-tuned parameters
DABC: Discrete Artificial Bee Colony
DABC-tuned: DABC with irace-tuned parameters
ACOLS-LOWEVA: refers to the Ant Colony Optimization (ACO) used in the
experiments
ILS-NEH: Iterated Local Search with modified NEH heuristic. Also 
abbreviated as 2BF-ILS.


Description of the files that can be found here:
------------------------------------------------

avgRPDPerSize.csv: 
Contains the average ratio (RPD(2BF-ILS) / RPD(algorithm)) for each of 
the algorithms 2BF-ILS is compared with, split by problem size (50, 100 
and 150 job instances).

bestResults.csv:
Contains the best makespan values for each instance that was found in 
all runs (with all algorithms and over all repetitions). These are the
values used to calculate the RPD.

ecdfFE_rel.csv:
Contains the relative AUC values for the ECDF curves 
for the FE (function evaluations) time measure. These scaled such that 
a value is better the closer to 1 it is.

ecdfNT_rel.csv:
Contains the relative AUC values for the ECDF curves 
for the NT (normalized time) time measure. These scaled such that a 
value is better the closer to 1 it is.

endPerformance.csv:
Contains the average makespan the algorithms obtained at the end of the
time limit for each instance.

ertFE_rel.csv:
Contains the relative AUC values for the ERT curves 
for the FE (function evaluations) time measure. These scaled such that 
a value is better the closer to 1 it is.

ertNT_rel.csv:
Contains the relative AUC values for the ERT curves 
for the NT (normalized time) time measure. These scaled such that 
a value is better the closer to 1 it is.

johnsonBounds.csv:
Contains the Johnson bounds (obtained by applying Johnson's algorithm
on each instance without taking the buffer constraints into account)
for each instance.

pcFE_rel.csv
Contains the relative AUC values for the PC curves 
for the FE (function evaluations) time measure. These scaled such that 
a value is better the closer to 1 it is.

pcNT_rel.csv
Contains the relative AUC values for the PC curves 
for the NT (normalized) time measure. These scaled such that 
a value is better the closer to 1 it is.

performance100000.csv:
Contains the average makespan the algorithms obtained at after
100,000 function evaluations for each algorithm.

relativeAUC50/100/150.csv:
Contains the relative AUC values for each of the graphical
evaluation measured, averaged over all instances with 50/100/150 jobs.

statTest50/100/150.csv:
Contains the results of the two-sided sign test used for comparing
the algorithms, limited to the instances with 50/100/150 jobs.
Each cell contains two values: The first value is the test result at
the point in time where each algorithm has performed 100,000 function
evaluations. The second value is the test result at the end of the
time limit. The direction the arrow (<< / >>) points at indicates which
of the compared algorithms obtained better results (here ">>" refers
to the algorithm in the respective column). Only results with two
arrows (<< / >>) are statistically significant. Results with one arrow
(< / >) are cases where the p-value is under 0.05, but not 
statistically significant due to Bonferroni correction.
