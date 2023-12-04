#' Simulate data from a Structural Causal Model (SCM) used in the research report
#'
#' Simulate data from a Structural Causal Model (SCM) used in the research report. It uses 2 confounders and allows for one change in confounder effects.
#'
#'@param timepoints Numeric input specifying the number of timepoints to sample. Used for naming the variables.
#'@param burnin Numeric input specifying the number of burnin timepoints to use for convergence. Used for naming the variables. Together with timepoints determines the total number of timepoints sampled.
#'@param N Numeric input specifying the sample size.
#'@param ndat Numeric input specifying the number of datasets to simulate.
#'@param phi 2x2 matrix specifying the lagged effects (i.e. autoregressions on the diagonals and cross-lagged effects on the off-diagonals).
#'@param betac 2x2 matrix specifying the initial effect of the confounders.
#'@param betac2 Optional 2x2 matrix specifying the changed effects of the confounders.
#'@param time_beta_change Optional numeric input specifying the timepoint at which the effects of the confounders change.
#'@param psi Atomic vector with length=2 specifying the residual variances of the variables.
#'@param intercepts Atomic vector with length=2 specifying the intercepts.
#'@param meanc Atomic vector with length=2 specifying the means of the confounders.
#'@param varC Atomic vector with length=2 specifying the variances of the confounders.
#'@param seed Optional argument specifying the seed to use before resampling.

#' @returns An ndat x 1 dataset with ndat nested simulated datasets, each of size N x (timepoints+burnin+3).
#' @examples
#' \dontrun{
#' # set phi matrix
#'phixx <- 0.2
#'phixy <- 0.15
#'phiyy <- 0.30
#'phiyx <- 0.10
#'phi <- matrix(c(phixx, phixy,
#'                phiyx, phiyy),
#'              nrow = 2,
#'              byrow=T)

#'# speciy residual variances
#'psix <- 1
#'psiy <- 1
#'psixy <- c(psix, psiy)

#'# specify variances of confounders
#'psic1 <- 1
#'psic2 <- 1
#'psic <- c(psic1, psic2)
#'# specify effects of confounders
#'betaxc1 <- 0.3
#'betaxc2 <- 0.8
#'
#'betayc1 <- 0.5
#'betayc2 <- 0.2
#'
#'# put in matrix
#'betac_1 <- matrix(c(betaxc1, betaxc2,
#'                    betayc1, betayc2),
#'                  nrow = 2,
#'                  byrow = T)
#'
#'data <- sim_scm(
#'  timepoints = 5,
#'  burnin = 5,
#'  N = 500,
#'  ndat = 10,
#'  phi = phi,
#'  betac = betac_1,
#'  betac2 = NULLL,
#'  time_beta_change = NULL,
#'  psi = psixy,
#'  intercepts = c(0,0),
#'  meanc = c(0,0),
#'  varC = c(0,0),
#'  seed = 42)
#'}
#'@export
sim_scm <- function(timepoints = 5,
                    burnin = 45,
                    N = 500,
                    ndat = 1,
                    phi,
                    betac,
                    betac2 = NULL,
                    time_beta_change = NULL,
                    psi,
                    intercepts = c(0,0),
                    meanc = c(0,0),
                    varC = c(1,1),
                    seed = NULL
){
  if (!is.null(betac2) & is.null(time_beta_change)){
    cli::cli_abort(c("The function does not know when to change the effect of the confounders.",
                     "i" = "Please provide a time for the new effects."))
  }
  if (is.null(betac2) & is.null(time_beta_change)){
    betac2 <- betac
    time_beta_change <- 1 # time_beta_change will be at t=1
  }
  if(!is.null(seed)){
    set.seed(seed)
  }
  tot_timepoints <- burnin+timepoints
  variable.names <- purrr::map(.x = c(-(burnin-1):(timepoints)),
                               function(x) purrr::map2(.x = c("x", "y"),
                                                       .y = x,
                                                       .f = paste0)) %>%
    unlist() %>%
    c("ID", ., "C1", "C2")
  datas <- tibble::tibble(data = rep(NA, ndat))
  for (i in 1:ndat){
    # dataset i
    tab <- matrix(nrow=N,ncol=2*tot_timepoints)

    # values on confounders
    c <- MASS::mvrnorm(n = N, mu = meanc, Sigma = diag(varC))

    for (j in 1:N){
      # datamatrix for person j
      mat <- matrix(NA,
                    nrow = 2,
                    ncol = tot_timepoints)

      # residuals of innovations
      error <- MASS::mvrnorm(n = tot_timepoints,
                             mu = rep(0, 2),
                             Sigma = diag(psi))
      mat[,1] <- intercepts + betac%*%c[j,] + error[1,]
      # rest of burnin period + first beta
      for (k in 2:(burnin+time_beta_change-1)){
        mat[, k] <- intercepts + phi%*%mat[,k-1] + betac%*%c[j,] + error[k,]
      }
      # second beta
      for (k in (burnin+time_beta_change):tot_timepoints){
        mat[, k] <- intercepts + phi%*%mat[,k-1] + betac2%*%c[j,] + error[k,]
      }
      tab[j, ] <- c(mat)
    }
    # add ID and confounder values
    tab <- cbind(1:N, tab, c) %>%
      tibble::as_tibble()
    # nest dataset
    datas[i, ] <- tidyr::nest(tab)
  }
  # set variable names
  datas$data <- purrr::map(datas$data,
                           purrr::set_names,
                           variable.names)
  return(datas)
}
