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
#' @importFrom stats dnorm density smooth runif
#' @importFrom magrittr %>%
#'
#' @examples
#' simulate_sanger_data("GAATTC")
simulate_sanger_data <- function(sequence, sd=5, noise=0.1, degrade=0.8) {

  if (sd<=0) {
    stop("SD must be more than zero")
  }

  if (noise<0) {
    stop("Noise must be greater than or equal to zero")
  }


  if (any(degrade>1, degrade<=0)) {
    stop("Degrade must be between 0 and 1")
  }


  trace_length <- 20*(nchar(sequence)+1)
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
    position <- 20 * i
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

  # Fix for spurious check() warning
  NULL -> POS

  data %>% dplyr::select(-POS) %>% max() -> biggest_signal

  for (i in c("G","A","T","C")) {
    data %>%
      dplyr::mutate(
        "{i}" := !!sym(i) + as.numeric(smooth(runif(nrow(data), min = 0, max=biggest_signal*noise)))
      ) -> data
  }

  return(data)

}

degrade_signal <- function(data,degrade) {

  seq(from=1, to=degrade, length.out=nrow(data)) -> degrade_values

  for (i in c("G","A","T","C")) {
    data %>%
      dplyr::mutate(
        "{i}" := !!sym(i) * degrade_values
      ) -> data
  }

  return(data)
}
