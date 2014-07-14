library(moda)
library(triangle)

library(microbenchmark)

# 1. Defining the decision space
# ===============================

# Listing the decisions
Decisions = list(
  LocationFinding = c("GPS", "Radio Triangulation"),
  HardwarePlatform = c("Nexus I (HTC)", "Droid (Motorola)"),
  FileSharing = c("OpenIntents", "In House"),
  ReportSyncing = c("Explicit", "Implicit"),
  ChatProtocol = c("XMPP (Open Fire)", "In House"),
  MapAccess = c("On demand (Google)", "Cached On Server", "Preloaded (ESRI)"),
  Connectivity = c("Wifi", "3G on Nexus I", "3G on Droid", "Bluetooth"),
  Database = c("MySQL", "sqLite"),
  ArchitecturalPattern = c("Facade", "Peer-to-peer", "Push-based"),
  DataExchangeFormat = c("XML", "Compressed XML", "Unformatted data")
  )

# Generating all alternatives as absolute decision vectors
A = all_alternatives(Decisions, type = "absolute")

n_options = n_options(Decisions)
n_alt = n_alternatives(Decisions)

# 2. Declaring goals, their weights, and utility levels
# ====================================================== 

Goals = c("Ramp Up Time", "Cost", "Development Time", "Deployment Time",
          "Battery Usage", "Response Time", "Reliability")
n_goals = length(Goals)

# Specifying goals weigths
weight = vector(length = length(Goals))
names(weight) = Goals
weight["Ramp Up Time"] = 2
weight["Cost"] = 1
weight["Development Time"] = 2
weight["Deployment Time"] = 2
weight["Battery Usage"] = 9
weight["Response Time"] = 7
weight["Reliability"] = 3


# Specifying the goals utility levels
levels = matrix(nrow = length(Goals), ncol = 3)
rownames(levels) = Goals
colnames(levels) = c('zeroUtility', 'oneUtility', 'must')
levels["Battery Usage", ] = c(111, 24, 52)
levels["Response Time", ] = c(2850, 203, 882)
levels["Reliability", ] = c(535, 792, 721)
levels["Ramp Up Time", ] = c(83, 31, 58)
levels["Cost", ] = c(2250, 550, 1290)
levels["Development Time", ] = c(149, 61, 111)
levels["Deployment Time", ] = c(72, 21, 38)

# shortand notations
zeroUtility = function(g){levels[g, 'zeroUtility']}
oneUtility = function(g){levels[g, 'oneUtility']}
must = function(g){levels[g, 'must']}

# Whether the goal is to be maximized or minimized
modality = function(g){
  if (levels[g, 'oneUtility'] > levels[g, 'zeroUtility']){return('max')}
  else {return('min')}
}

# 3. Generating the parameters simulations
# ========================================

# Nbr of simulations
N = 10^4

# Reading the parameters data (min, max and mode of triangular distribution)
tryCatch(
  {options_data = read.csv("data.csv", header = TRUE)},
  warning = function(warn){
    message(warn)
    message("\n Remember to set the working directory to the source file location")
  }
)
options_data = options_data[ ,2:22] # dropping the first column with option names

message("Generating parameters simulations")
t0 = Sys.time()
set.seed(20140604)
contrib_sim = array(dim = c(N, n_options, n_goals))
for (i in 1:n_options){
  for (j in 1:n_goals){
    contrib_sim[ , i, j] = rtriangle(N,
                                     a = options_data[i, 3*(j-1)+1],
                                     b = options_data[i, 3*(j-1)+3],
                                     c = options_data[i, 3*(j-1)+2]
                                     )
  }
}
t1 = Sys.time()
message("Parameters simulations generated")
print(t1 - t0)

# 4. Defining the objectives evaluation function
# =============================================

#' Evaluates a single SAS architecture with concrete parameter values
#' 
#' @param d an absolute decsion vector for the SAS problem
#' @param contrib a O X G matrix of option contributions;
#' contrib[o, g] denotes the contribution of option o to goal g
#' 
#' @return a vector (Utility, ProjectFailure)
#' 
evaluate_SAS = function(d, contrib){

  goal_level = vector(length = n_goals)
  goal_utility = vector(length = n_goals)
  goal_failure = vector(length = n_goals)
  names(goal_level) = names(goal_utility) = names(goal_failure) = Goals
  
  for (g in 1: n_goals){
    goal_level[g] = sum(contrib[d, g])
    goal_utility[g] = linear_utility(goal_level[g], zeroUtility(g), oneUtility(g))
    goal_failure[g] = is.failure(goal_level[g], must(g), modality(g))
  }
  
  Utility = sum(weight * goal_utility)
  ProjectFailure = any(goal_failure)
  
  return(c(Utility = Utility, ProjectFailure = ProjectFailure))
}

# Testing evaluate_SAS performance
speed_evaluate_SAS = microbenchmark(
  evaluate_SAS(A[1, ], contrib_sim[1, , ]),
  evaluate_SAS(A[nrow(A), ], contrib_sim[N, , ]),
  evaluate_SAS(A[nrow(A)/2, ], contrib_sim[N/2, , ])
)
# 1 eval = 400 microseconds
# => 10^4 eval = 4 000 000 microseconds = 4 seconds
# => 6912 alternatives with N = 10^4 => over 7 hours!
# with N = 10^3: 45 minutes; N = 100, 4.5 mintues
# Too slow but the function is easy to write
# and we can use it to test faster vectorized implementation


# 5. Defining the simulation function
# ====================================

#' Simulate a single alternative d for all scenarios in contrib_sim
#' 
#' @param d an absolute decsion vector for the SAS problem
#' 
#' @return
#' A matrix of dim (N, 2) where N is the number of scenarios in contrib_sim.
#' The first column gives the Utility of d in each scenario;
#' the second column denotes gives ProfjectFailure of d in each scenario
#' 
#'  @details
#'  The function assumes the following variables in the global environment:
#'  \itemize{
#'  \item contrib_sim
#'  \item weight
#'  \item levels
#'  }
#' 
simulate_SAS = function(d){
  
  N = dim(contrib_sim)[1]
  
  goal_level = matrix(nrow = N, ncol = n_goals)
  goal_utility = matrix(nrow = N, ncol = n_goals)
  goal_failure = matrix(nrow = N, ncol = n_goals)
  
  for (g in 1: n_goals){
    goal_level[ , g] = rowSums(contrib_sim[ , d, g])
    goal_utility[ , g] = linear_utility(goal_level[ , g], zeroUtility(g), oneUtility(g))
    goal_failure[ , g] = is.failure(goal_level[ , g], must(g), modality(g))
  }
  
  # Utility is the weighted sum of the goal utility levels
  Utility = sumproduct(weight, goal_utility)
  
  # ProjectFailure is true if at least one goal fails
  ProjectFailure = rowSums(goal_failure) > 0
  
  return(cbind(Utility, ProjectFailure))

}

# Testing simulate_SAS is consistent with evaluate_SAS
stopifnot( 
  all.equal(simulate_SAS(A[1, ])[1, ],
            evaluate_SAS(A[1, ], contrib_sim[1, , ]))
)
stopifnot( 
  all.equal(simulate_SAS(A[nrow(A), ])[N, ],
            evaluate_SAS(A[nrow(A), ], contrib_sim[N, , ]))
)
stopifnot( 
  all.equal(simulate_SAS(A[nrow(A)/2, ])[N/2, ],
            evaluate_SAS(A[nrow(A)/2, ], contrib_sim[N/2, , ]))
)

# Measure simulate_SAS speed
speed_simulate_SAS = microbenchmark(
  simulate_SAS(A[1, ]),
  simulate_SAS(A[ncol(A), ]),
  times = 10
)
# 40 milliseconds to simulate 1 alternative with N = 10^4
# => 4.6 minutes for all 6912 alternatives

# 6. Simulating all alternatives
# ==============================

message("Starting the simulation")
t0 = Sys.time()

Expected_Utility = vector(length = n_alt)
Project_Risk = vector(length = n_alt)

cat("Progress: ")
for (i in 1: n_alt){
  print_progress(i, n_alt)
  sim_result = simulate_SAS(A[i, ])
  Expected_Utility[i] = mean(sim_result[ , 1])
  Project_Risk[i] = mean(sim_result[ , 2])
}

t1 = Sys.time()
message("Simulation completed")
print(t1 - t0)

# 7. Shortlisting candidate architectures
# #######################################

Criteria = cbind(Project_Risk, Expected_Utility)

message("Starting shortlising")
t0 = Sys.time()
shortlist = shortlist(Decisions, Criteria, 
                      mode = c("min", "max"), 
                      margin = c(0.01, 0.5)
                      )
t1 = Sys.time()
print(shortlist)
print(t1 - t0)


# NOTE:
# If we use margin  = c(0.01, 0.1) as in paper
# we obtain 4 shortlised architectures (instead of 10 in paper)
# This is because of a difference in how pareto strip was defined/computed.
# In the paper model, two decisions names were also inverted.


# 8. Analysing Value of Information
# #################################

# Since, to save space and time, we didn't store 
# the simulations of all alternatives, we first need
# to resimulate the shortlisted alternatives.

shortlisted = shortlist$which_alternatives
Shortlist = A[shortlisted, ]
rownames(Shortlist) = paste("SL", 1:nrow(Shortlist), sep = "")
n_shortlist = nrow(Shortlist)

Shortlist_Utility = matrix(nrow = N, ncol = n_shortlist)
Shortlist_ProjectFailure = matrix(nrow = N, ncol = n_shortlist)

for (i in 1:n_shortlist){
  sim_result = simulate_SAS(Shortlist[i, ])
  Shortlist_Utility[ , i] = sim_result[ , 1]
  Shortlist_ProjectFailure[ , i] = sim_result[ , 2]
}
# We should also show the expected utility and risks for the shortlisted arch.

# Total Perfect Information
# =========================

evtpi_and_risk = evtpi_and_risk(Shortlist_Utility, Shortlist_ProjectFailure)
print(evtpi_and_risk)

# Perfect Information about Goals Levels
# ======================================

message("Computing evppi for goals levels")
goals_evppi_and_risk = matrix(nrow = n_shortlist * n_goals, ncol = 2)
colnames(goals_evppi_and_risk) = c("evppi", "delta_riks")
rownames(goals_evppi_and_risk) = vector(mode = "character", 
                                        length = n_shortlist * n_goals)

for (i in 1:n_shortlist){
  for (g in 1: n_goals){
    goal_sim = rowSums(contrib_sim[ , Shortlist[i, ], g])
    index = (i-1) * n_goals + g
    rownames(goals_evppi_and_risk)[index] = 
      paste(Goals[g], "(", rownames(Shortlist)[i], ")", sep = "")
    goals_evppi_and_risk[index,  ] =
      evppi_and_risk(goal_sim, Shortlist_Utility, Shortlist_ProjectFailure)    
  }
}
evppi_order = order(goals_evppi_and_risk[ , 1], decreasing = TRUE)
goals_evppi_and_risk = goals_evppi_and_risk[evppi_order, ]

print("Goals with top ten evppi:")
print(goals_evppi_and_risk[1:10, ])

