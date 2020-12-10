#' Simulate sanger chromatogram density data
#'
#' @param sequence A string of nucleotides to use (GATC)
#' @param sd The standard deviation of the peak width
#' @param noise What proportion of the signal to make from random noise
#' @param degrade What proportion of the signal should be lost by the end
#'
#' @return A tibble with positions (20 per base) and G/A/T/C signal
#' @export
#'
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom stats dnorm
#' @importFrom stats runif
#' @importFrom magrittr %>%
#'
#' @examples
#' simulate_sanger_data("GAATTC")
simulate_sanger_data <- function(sequence, sd=5, noise=0.1, degrade=0.8) {

  trace_length <- 20*(nchar(sequence)+2)
  start_signal <- rep(0,trace_length)

  sds <- rep(sd, trace_length)

  if (degrade > 0) {
    sds <- seq(from=sd, to=sd*(1/degrade), length.out=trace_length)
  }

  tibble::tibble(
    POS=1:trace_length,
    G=start_signal,A=start_signal,T=start_signal,C=start_signal
  ) -> base_data

  for (i in 1:nchar(sequence)) {
    base <- substr(sequence,i,i)
    position <- 20 * (i+1)
    add_base(base_data,base,position,sds[i]) -> base_data
  }

  add_noise(base_data,noise) -> base_data
  degrade_signal(base_data, degrade) -> base_data

  return(base_data)
}


add_base <- function(data, base, position, sd) {
  data %>%
    dplyr::mutate(
      "{base}" := !!sym(base) + dnorm(1:nrow(data), mean=position, sd=sd)
    ) %>%
    return()
}

add_noise <- function(data,noise) {

  data %>% select(-POS) %>% max() -> biggest_signal

  for (i in c("G","A","T","C")) {
    data %>%
      dplyr::mutate(
        "{i}" := !!sym(i) + as.numeric(smooth(runif(nrow(data), min = 0, max=biggest_signal*noise)))
      ) -> data
  }

  return(data)

}

degrade_signal <- function(data,degrade) {
  return(data)
}
