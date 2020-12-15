#' Draw a chromatogram from simulated data
#'
#' @param data The per base density data from simulate_sanger_data
#' @param sequence A string containing the expected sequence. Called from the density data if not provided
#' @param palette The colours used for the ACGT traces. Can be "ABI" or "Geneious"
#'
#' @return A ggplot object containing the chromatogram
#' @export
#'
#'@importFrom ggplot2 ggplot geom_line scale_x_continuous theme scale_colour_manual element_blank
#'@importFrom tidyr pivot_longer
#'@importFrom dplyr filter group_by ungroup slice arrange pull
#'
#'
#' @examples
#' draw_chromatogram(simulate_sanger_data("GAATTC"), "GAATTC")
draw_chromatogram <- function(data, sequence=NULL, palette="ABI") {

  # This avoids the spurious warnings from check()
  NULL -> POS -> base

  # If the sequence is null then we need to call it from the data
  if (is.null(sequence)) {
    data %>%
      tidyr::pivot_longer(
        cols=-POS,
        names_to="base",
        values_to="density"
      ) %>%
      dplyr::arrange(POS,desc(density)) %>%
      dplyr::filter(POS %in% seq(from=20,by=20,to=max(test$POS)-1)) %>%
      dplyr::group_by(POS) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::pull(base) %>%
      paste0(collapse = "") -> sequence
  }

  strsplit(sequence,"")[[1]] -> labels

  palette_colours <- c("green2","blue2","black","red2")

  if(palette == "Geneious") {
    palette_colours <- c("red2","blue2","darkgoldenrod2","green2")
  }

  data %>%
    tidyr::pivot_longer(
      cols=-POS,
      names_to="base",
      values_to="density"
    ) %>%
      ggplot2::ggplot(ggplot2::aes(x=POS,y=density, colour=base)) +
      ggplot2::geom_line(size=1.2, show.legend = FALSE) +
      ggplot2::scale_x_continuous(breaks = seq(from=20,by=20,length.out=length(labels)), labels = labels) +
      ggplot2::scale_colour_manual(values = palette_colours) +
      ggplot2::theme(axis.title = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank()) %>%
    return()

}
