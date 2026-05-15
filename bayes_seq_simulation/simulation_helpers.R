# Sequential Bayesian updating simulation helpers

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
  library(posterior)
  library(here)
})

source(here("bayes_seq_simulation", "simulation_constants.R"))

priors <- c(
  set_prior("uniform(0, 1)", lb = 0, ub = 1, class = "Intercept"),
  set_prior("normal(0, 0.1)",  class = "b"),
  set_prior("normal(0, 0.05)", class = "sd"),
  set_prior("lkj(2)",          class = "L")
)


extract_diagnostics <- function(fit, max_treedepth) {
  # Per-parameter stats — explicit calls to avoid summarise_draws auto-naming,
  # which overrides LHS names when the inner function returns a named value
  # (quantile -> "2.5%", mcse_quantile -> "mcse_q50") and silently drops them.
  draws <- tryCatch(as_draws_array(fit, variable = "b_condition_c"),
                    error = function(e) NULL)
  if (!is.null(draws)) {
    vec         <- as.numeric(draws)
    b_mean      <- mean(vec)
    b_sd        <- sd(vec)
    b_q025      <- unname(quantile(vec, 0.025))
    b_q975      <- unname(quantile(vec, 0.975))
    b_mcse_mean <- posterior::mcse_mean(draws)
    b_mcse_med  <- posterior::mcse_quantile(draws, 0.5)
    b_ess_bulk  <- posterior::ess_bulk(draws)
    b_ess_tail  <- posterior::ess_tail(draws)
    b_rhat      <- posterior::rhat(draws)
  } else {
    b_mean <- b_sd <- b_q025 <- b_q975 <- b_mcse_mean <- b_mcse_med <-
      b_ess_bulk <- b_ess_tail <- b_rhat <- NA_real_
  }

  # Sampler health
  np <- tryCatch(nuts_params(fit), error = function(e) NULL)
  if (!is.null(np)) {
    n_div  <- sum(np$Value[np$Parameter == "divergent__"], na.rm = TRUE)
    tree   <- np$Value[np$Parameter == "treedepth__"]
    n_tree <- sum(tree >= max_treedepth, na.rm = TRUE)
  } else {
    n_div <- NA_integer_; n_tree <- NA_integer_
  }

  # Global rhat / ess across all sampled params
  global <- tryCatch(
    posterior::summarise_draws(fit, rhat = posterior::rhat,
                                    ess_bulk = posterior::ess_bulk),
    error = function(e) NULL
  )
  max_rhat <- if (!is.null(global)) suppressWarnings(max(global$rhat, na.rm = TRUE)) else NA_real_
  min_ess  <- if (!is.null(global)) suppressWarnings(min(global$ess_bulk, na.rm = TRUE)) else NA_real_

  list(
    b_condition_mean      = b_mean,
    b_condition_sd        = b_sd,
    b_condition_q025      = b_q025,
    b_condition_q975      = b_q975,
    b_condition_mcse_mean = b_mcse_mean,
    b_condition_mcse_med  = b_mcse_med,
    b_condition_ess_bulk  = b_ess_bulk,
    b_condition_ess_tail  = b_ess_tail,
    b_condition_rhat      = b_rhat,
    n_divergent           = n_div,
    n_max_treedepth       = n_tree,
    max_rhat_any          = max_rhat,
    min_ess_bulk_any      = min_ess
  )
}


# ===========================================================================
# Run ONE ordering: sequentially add labs, fit, BF, log, stop when BF crosses
# thresholds. Returns a tibble with one row per step attempted.
#
# `lab_files_map` is a named character vector mapping lab_id -> RDS path
# (per-lab summarized tibbles produced by simulation_precompute.R).
# ===========================================================================
run_one_ordering <- function(outer_idx, lab_order, lab_files_map,
                             mcmc, seed_base, high, low, min_n_labs) {

  formula <- bf(prop_exit ~ 1 + condition_c + age_mo_c +
                  condition_c : age_mo_c +
                  (1 + condition_c + age_mo_c + condition_c : age_mo_c | lab_id))

  acc  <- NULL
  rows <- vector("list", length(lab_order))

  for (i in seq_along(lab_order)) {
    lab_id   <- lab_order[i]
    lab_data <- readRDS(lab_files_map[[lab_id]])
    acc      <- bind_rows(acc, lab_data)

    n_labs                <- i
    n_participants_added  <- length(unique(lab_data$participant_lab_id))
    n_participants_cum    <- length(unique(acc$participant_lab_id))
    base_row <- tibble(
      outer_idx                 = outer_idx,
      inner_idx                 = i,
      lab_id_added              = lab_id,
      n_labs_cumulative         = n_labs,
      n_participants_added      = n_participants_added,
      n_participants_cumulative = n_participants_cum,
      fit_error_message         = NA_character_
    )

    # Skip BF for the first few labs (random effects underidentified).
    if (n_labs < min_n_labs) {
      rows[[i]] <- base_row %>% mutate(
        evid_ratio_h0 = NA_real_, bf_10 = NA_real_, log_bf_10 = NA_real_,
        stopped = FALSE, stop_reason = NA_character_,
        fit_seconds = NA_real_, seed = NA_integer_
      )
      next
    }

    seed <- as.integer(seed_base + i)
    t0   <- Sys.time()
    fit  <- tryCatch(
      brm(
        formula      = formula,
        data         = acc,
        family       = gaussian(),
        prior        = priors,
        save_pars    = save_pars(all = TRUE),
        sample_prior = TRUE,
        iter         = mcmc$iter,
        warmup       = mcmc$warmup,
        chains       = mcmc$chains,
        cores        = mcmc$cores,
        seed         = seed,
        control      = list(adapt_delta   = mcmc$adapt_delta,
                            max_treedepth = mcmc$max_treedepth,
                            stepsize      = mcmc$stepsize),
        refresh      = 0,
        silent       = 2
      ),
      error = function(e) e
    )
    fit_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    if (inherits(fit, "error")) {
      rows[[i]] <- base_row %>% mutate(
        evid_ratio_h0 = NA_real_, bf_10 = NA_real_, log_bf_10 = NA_real_,
        stopped = FALSE, stop_reason = "fit_error",
        fit_seconds = fit_seconds, seed = seed,
        fit_error_message = as.character(fit$message)
      )
      next
    }

    # Savage-Dickey BF for condition_c = 0 (requires sample_prior = TRUE).
    # hypothesis() returns Evid.Ratio = BF_01; invert to BF_10.
    bf <- tryCatch({
      h             <- hypothesis(fit, "condition_c = 0", class = "b")
      evid_ratio_h0 <- h$hypothesis$Evid.Ratio
      bf_10         <- 1 / evid_ratio_h0
      list(evid_ratio_h0 = evid_ratio_h0, bf_10 = bf_10, log_bf_10 = log(bf_10))
    }, error = function(e) {
      list(evid_ratio_h0 = NA_real_, bf_10 = NA_real_, log_bf_10 = NA_real_)
    })

    diag <- extract_diagnostics(fit, mcmc$max_treedepth)

    sreason <- if (is.na(bf$bf_10))    NA_character_
               else if (bf$bf_10 >= high) "high"
               else if (bf$bf_10 <= low)  "low"
               else                       NA_character_
    stopped <- !is.na(sreason)

    rows[[i]] <- base_row %>% mutate(
      evid_ratio_h0 = bf$evid_ratio_h0,
      bf_10         = bf$bf_10,
      log_bf_10     = bf$log_bf_10,
      !!!diag,
      stopped       = stopped,
      stop_reason   = sreason,
      fit_seconds   = fit_seconds,
      seed          = seed
    )

    rm(fit); gc(verbose = FALSE)
    if (stopped) break
  }

  # If we exhausted the lab list without ever crossing a BF threshold, mark
  # the terminal row so the analyst can distinguish "ran out of data" from
  # "stopped early".
  if (i == length(lab_order) && !isTRUE(rows[[i]]$stopped)) {
    rows[[i]] <- rows[[i]] %>%
      mutate(stopped = TRUE, stop_reason = "max_labs")
  }

  bind_rows(rows)
}
