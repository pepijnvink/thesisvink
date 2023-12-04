# create test dataframe
## set phi matrix
phixx <- 0.2
phixy <- 0.15
phiyy <- 0.30
phiyx <- 0.10
phi <- matrix(c(phixx, phixy,
                phiyx, phiyy),
              nrow = 2,
              byrow=T)

## speciy residual variances
psix <- 1
psiy <- 1
psixy <- c(psix, psiy)

## specify variances of confounders
psic1 <- 1
psic2 <- 1
psic <- c(psic1, psic2)

# specify effects of confounders
betaxc1 <- 0.3
betaxc2 <- 0.8

betayc1 <- 0.5
betayc2 <- 0.2

# put in matrix
betac_1 <- matrix(c(betaxc1, betaxc2,
                    betayc1, betayc2),
                  nrow = 2,
                  byrow = T)

test_that("Check data types", {
  expect_s3_class(sim_scm(
    timepoints = 5,
    burnin = 5,
    N = 10,
    ndat=5,
    phi = phi,
    betac = betac_1,
    betac2 = NULL,
    time_beta_change = NULL,
    psi = psixy,
    intercepts = c(0,0),
    meanc = c(0,0),
    varC = c(0,0),
    seed = 42), "data.frame")
expect_s3_class(sim_scm(
  timepoints = 5,
  burnin = 5,
  N = 10,
  ndat=5,
  phi = phi,
  betac = betac_1,
  betac2 = NULL,
  time_beta_change = NULL,
  psi = psixy,
  intercepts = c(0,0),
  meanc = c(0,0),
  varC = c(0,0),
  seed = 42)$data[[1]], "data.frame")
})
