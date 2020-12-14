#' Draw a chromatogram from simulated data
#'
#' @param data The per base density data from simulate_sanger_data
#' @param sequence A string containing the expected sequence
#'
#' @return A ggplot object containing the chromatogram
#' @export
#'
#' @examples
#' draw_chromatogram(simulate_sanger_data("GAATTC"), "GAATTC")
draw_chromatogram <- function(data, sequence) {

  strsplit(sequence,"")[[1]] -> labels

  data %>%
    pivot_longer(
      cols=-POS,
      names_to="base",
      values_to="density"
    ) %>%
      ggplot(aes(x=POS,y=density, colour=base)) +
      geom_line(size=1.2, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(from=40,by=20,length.out=length(labels)), labels = labels) +
      scale_colour_manual(values = c("green2","blue2","black","red2")) +
      theme(axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) %>%
    return()

}
