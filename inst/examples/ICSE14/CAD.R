# Example from Section 2 of 
# E. Letier, D. Stefan, E. Barr
# Uncertainty, Risk and Information Value 
# in Software Requirements and Architecture
# Proceedings ICSE 2014

library(moda)
library(triangle)

# Decision: keeping legacy CAD or developing a new CAD
DList = list(
  System = c('legacy', 'new')
  )

# Eliciting the 90% Confidence Intervals
Cost_CI = matrix(nrow = 2, ncol = 2)
rownames(Cost_CI) = DList$'System'
colnames(Cost_CI) = c('lower', 'upper')
Cost_CI['legacy', ] = c(0, 0)
Cost_CI['new', ] = c(1, 5)

Benefit_CI = matrix(nrow = 2, ncol = 2)
rownames(Benefit_CI) = DList$'System'
colnames(Benefit_CI) = c('lower', 'upper')
Benefit_CI['legacy', ] = c(0.9, 1.1)
Benefit_CI['new', ] = c(1, 9) 

# Simulating costs and benefits 
N = 10^4

Cost_sim = matrix(nrow = N, ncol = 2)
colnames(Cost_sim) = DList$'System'
Cost_sim[ , 'legacy'] = rnorm(N, mean(Cost_CI['legacy', ]), 
                              sd_from_90CI(Cost_CI['legacy', 1], Cost_CI['legacy', 2]))
Cost_sim[ , 'new'] = rnorm(N, mean(Cost_CI['new', ]), 
                              sd_from_90CI(Cost_CI['new', 1], Cost_CI['new', 2]))
# Cost is truncated at zero
Cost_sim[Cost_sim<0]=0


Benefit_sim = matrix(nrow = N, ncol = 2)
colnames(Benefit_sim) = DList$'System'
Benefit_sim[ , 'legacy'] = rnorm(N, mean(Benefit_CI['legacy', ]), 
                              sd_from_90CI(Benefit_CI['legacy', 1], Benefit_CI['legacy', 2]))
Benefit_sim[ , 'new'] = rnorm(N, mean(Benefit_CI['new', ]), 
                           sd_from_90CI(Benefit_CI['new', 1], Benefit_CI['new', 2]))
# Benefit is truncated at zero
Benefit_sim[Benefit_sim<0]=0

# Performing the cost-benefit analysis
cba_results = cba(Cost_sim, Benefit_sim)
print(cba_results)


