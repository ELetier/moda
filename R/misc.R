#' Print progress through a loop
#' 
#' @param pos number of the current iteration
#' @param end total number of iternations
#' 
#' @examples
#' N = 10^4
#' cat("Progress: ")
#' for (i in 1:N){
#'    print_progress(i, N)
#'    # insert loop body here
#' }
#' 
#' @export
print_progress = function(pos, end) {
  progress = round(100 * pos / end)
  previous = round(100 * (pos - 1) /end)
  if (progress != previous){
    cat(paste(progress, "% ", sep = ""))
    if (progress %% 10 == 0) cat("\n")
  } 
}

#' Standard Deviation of a 90\% Confidence Interval
#' 
#' Generates the standard deviation from the lower and upper bounds
#' of a 90\% confidence interval
#' 
#' @param lower the lower bound of the 90\% confidence interval
#' @param upper the upper bound of the 90\% confidence interval
#' 
#' @export
sd_from_90CI = function(lower, upper){
  (upper - lower)/3.29
}
