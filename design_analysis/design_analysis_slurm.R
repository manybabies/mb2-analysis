library(tidyverse)
library(brms)
inv.logit = function(x) {exp(x)/(1 + exp(x))}

priors <-c(set_prior("normal(0, 2)", class = "Intercept"),
              set_prior("normal(0, 2)", class = "b"),
              set_prior("normal(0, .1)", class = "sd"),
              set_prior("lkj(2)", class = "L"))

simulate.df = function(num_subjects, num_labs) {
  
  # parameters
  condition_beta = runif(1, .36, 1.45) # small to medium effects
  age_beta = runif(1, -.20, .20) 
  age_condition_beta = runif(1, -.20, .20)
  sigma = runif(1, .5, 2) # may want to change this
  minage = 18
  maxage = 27
  intercept_mean = 1
  intercept_sd = .25
  lab_intercept_sd = .1
  lab_slope_sd = .1

  d = expand.grid(subj = 1:num_subjects,
                  lab = 1:num_labs) %>%
      mutate(condition = ifelse(subj %% 2 == 0, .5, -.5),
             age = round(runif(n(), minage, maxage))) 
  
  d$age = d$age - mean(d$age)
  
  d = tibble(lab = 1:num_labs,
                lab_intercept = rnorm(num_labs, 0, lab_intercept_sd),
                lab_condition_slope = rnorm(num_labs, 0, lab_slope_sd)) %>%
    right_join(d)
  
  d = d %>%
    mutate(intercept = rnorm(n(), intercept_mean, intercept_sd),
           simulated_datapoint = intercept + 
                        lab_intercept + 
                        condition * (condition_beta + lab_condition_slope) + 
                        age * age_beta + 
                        age * condition * age_condition_beta +
                        rnorm(n(), 0, sigma) 
            )
  return(d)
}

x = simulate.df(20, 22)


run_simulation <- function(it) {
    newd_ = simulate.df(20, 22)
    l.new = brm(data=newd_,
        simulated_datapoint ~ condition + age + condition:age + (condition * age | lab),
        prior=priors,
        chains=6,
        cores=1,
        iter = 40, #000,
        save_all_pars=T,
        control = list(adapt_delta = 0.99))


    l.null = brm(data=newd_, 
        simulated_datapoint ~ age + condition:age + (condition * age | lab),
        prior=priors,
        chains=6,
        cores=1,
        iter=40, #000,
        save_all_pars = T,
        control = list(adapt_delta = 0.99))
    
    bf.new.null = as.numeric(bayes_factor(l.new, l.null)[1])
    
    newd.sum = group_by(newd_, condition) %>% 
      summarise(m=inv.logit(mean(simulated_datapoint)))

    result = tibble(it=it, bf=bf.new.null,
          cond1_mean = newd.sum$m[1],
          cond2_mean = newd.sum$m[2],
          lower=fixef(l.new)["condition", ]["Q2.5"], 
          upper=fixef(l.new)["condition", ]["Q97.5"])
    
    #write_csv(result, file=paste("bf_design_analysis_results", as.character(it), sep="/"), append = T)
	  
    return(result)
}

library(rslurm)
sjob <- slurm_apply(run_simulation, tibble(it=1:2), jobname = 'mb2-analysis_sim_bf',
                    nodes = 1, cpus_per_node = 2, submit = TRUE)

res <- get_slurm_out(sjob, outtype = 'table')
head(res, 3)

#df = run_simulation(1)
write_csv(res, file="design_analysis_results.csv", append = T)

