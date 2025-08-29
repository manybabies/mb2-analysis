library(lme4)
library(lmerTest)
library(buildmer)

# Define the function to fit models in the specified pruning order
prune_lmer_model <- function(data,age_cohort_predictor_name="age_cohort_c") {
  # Shared part of the formula
  shared_formula <- paste0("Average ~ condition_c * outcome_c * ",age_cohort_predictor_name)
  
  # Define the list of formulas in pruning order, with only the random effects changing
  formulas <- list(
    full_model = paste(shared_formula, "+ (condition_c * outcome_c | lab_id)"),
    prun1_model = paste(shared_formula, "+ (1+condition_c+condition_c : outcome_c | lab_id)"),
    prun2_model = paste(shared_formula, "+ (1+condition_c : outcome_c | lab_id)"),
    prun3_model = paste(shared_formula, "+ (condition_c * outcome_c || lab_id)"),
    prun4_model = paste(shared_formula, "+ (1+condition_c+condition_c : outcome_c || lab_id)"),
    prun5_model = paste(shared_formula, "+ (1+condition_c : outcome_c || lab_id)"),
    prun6_model = paste(shared_formula, "+ (1 | lab_id)")
  )
  
  # Iterate over the formulas and fit the model
  for (i in seq_along(formulas)) {
    formula <- formulas[[i]]
    
    # Print the formula being tried
    cat(paste0("Trying model: ", names(formulas)[i], "\n"))
    
    # Fit the model
    model <- tryCatch({
      lmer(as.formula(formula), data = data, REML = FALSE,
           control=lmerControl(optimizer="bobyqa",
                               optCtrl=list(maxfun=2e5)))
    }, warning = function(w) {
      if(grepl('failed to converge', w$message)) {
        cat(paste0("Model ", names(formulas)[i], " failed to converge with warning: ", w$message, "\n"))
      }
      #invokeRestart("muffleWarning")  # Muffle the warning so it doesn't stop execution
      return(NULL)
    }, error = function(e) {
      cat(paste0("Model ", names(formulas)[i], " failed with error: ", e$message, "\n"))
      return(NULL)
    })
    
    # Check for convergence only if model fitting was successful
    if (!is.null(model)) {
      if (!is.null(model@optinfo$derivs)) {
        if(converged(model)) { 
          cat(paste0("Model ", names(formulas)[i], " converged.\n"))
          return(model)
        }
      } else {
        cat(paste0("Model ", names(formulas)[i], " did not converge properly (no derivatives found).\n"))
      }
    }
  }
  
  # Return NULL if no model converges
  cat("No model converged. Using the intercept-only model despite singular fit\n")
  print(i)
  lmer(as.formula(formulas[[i]]), data = data, REML = FALSE,
       control=lmerControl(optimizer="bobyqa",
                           optCtrl=list(maxfun=2e5)))
}
