#' Function to generate data
#'
#' @param N An integer representing the number of subjects in the trial. Default is 500.
#' @param em_strength. A numeric value representing the effect modifier strength. Default
#' is 0.1.
#' @param overlap_param. A numeric value representing the overlap between the AB and AC trial
#' populations. Default is 0.5.
#' @param shared_em. A logical value indicating whether the shared effect modifier assumption
#' is true or not. Default is TRUE.
#' @param seed. An integer value representing the random seed. Default is 123.
#'
#' @return A list of data.frames. Consists of AB_IPD which represents the IPD from the AB trial,
#' and AC_aggregate which represents the aggregate data from the AC trial.
#'
#' @examples
#' gen_data()
#'
#' @importFrom magrittr %>%
#' @export

gen_data <- function(N = 500, em_strength = 0.5, overlap_param = 0.5, shared_em = TRUE, seed = 123) {
  set.seed(seed)

  mu_X1 <- 1
  sigma_X1 <- 0.5

  mu_X2 <- 0.5
  sigma_X2 <- 0.1

  b_0 <- 1
  b_X1 <- em_strength * sigma_X1
  b_X2 <- em_strength * sigma_X2
  b_trt <- -2

  c_0 <- 1.5
  if (shared_em == TRUE) {
    c_X1 <- b_X1
    c_X2 <- b_X2
  } else {
    c_X1 <- shared_em * sigma_X1
    c_X2 <- shared_em * sigma_X2
  }
  c_trt <- -1.5

  # AB IPD
  AB_IPD <- data.frame(
    X1 = rnorm(N, mu_X1, sigma_X1),
    X2 = rnorm(N, mu_X2, sigma_X2),
    treatment = c(rep("A", N / 2), rep("B", N / 2))
  ) %>%
  dplyr::mutate(
    yprob = 1 / (1 + exp(-(
      b_0 +
        (b_trt + b_X1 * (X1 - mu_X1) +  b_X2 * (X2 - mu_X2)) * (treatment == "B")
    ))),
    y = rbinom(N, 1, yprob)
  ) %>%
  dplyr::select(-yprob)

  AC_IPD <- data.frame(
    X1 = rnorm(N, (1.1 + (1 - overlap_param)^2) * mu_X1, 0.75 * sigma_X1),
    X2 = rnorm(N, (1.1 + (1 - overlap_param)^2) * mu_X2, 0.75 * sigma_X2),
    treatment = c(rep("A", N / 2), rep("C", N / 2))
  ) %>%
  dplyr::mutate(
    yprob = 1 / (1 + exp(-(
      c_0 +
        (c_trt + c_X1 * (X1 - mu_X1) +  c_X2 * (X2 - mu_X2)) * (treatment == "C")
    ))),
    y = rbinom(N, 1, yprob)
  ) %>%
  dplyr::select(-yprob)

  AC_aggregate <- cbind(
    AC_IPD %>%
      dplyr::summarise(
        X1_mean = mean(X1),
        X1_sd = sd(X1),
        X2_mean = mean(X2),
        X2_sd = sd(X2)
      ),

    AC_IPD %>%
      dplyr::filter(treatment == "A") %>%
      dplyr::summarise(
        y_A_sum = sum(y),
        y_A_mean = mean(y),
        N_A = dplyr::n()
      ),

    AC_IPD %>%
      dplyr::filter(treatment == "C") %>%
      dplyr::summarise(
        y_C_sum = sum(y),
        y_C_mean = mean(y),
        N_C = dplyr::n()
      )
  )

  list(AB_IPD = AB_IPD, AC_aggregate = AC_aggregate)
}

