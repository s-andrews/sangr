#' Merge multiple sanger datasets together
#'
#' @param ... Multiple base density datasets from simulate_sanger_data()
#'
#' @return A merged base density dataset
#' @export
#'
#'@importFrom dplyr group_by summarise ungroup
#'
#' @examples
#' merge_sanger_data(simulate_sanger_data("GAG"),simulate_sanger_data("GTG"))
#'
merge_sanger_data <- function(...) {
  datasets <- list(...)

  NULL -> G -> A -> T -> C -> POS

  do.call(dplyr::bind_rows, datasets) -> merged_data

  merged_data %>%
    dplyr::group_by(POS) %>%
    dplyr::summarise(
      G=mean(G),
      A=mean(A),
      T=mean(T),
      C=mean(C)
    ) %>%
    dplyr::ungroup() %>%
    return()

}
