ggmort <- function(x, legend_title) {
  ggplot(x, aes(
    x = .data$regione,
    y = .data$variation,
    fill = .data$strata
  )) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_bar(position = "dodge", stat = "identity") +
    labs(
      y = "Variazione Percentuale",
      x = "Nome regione",
      fill = legend_title
    ) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1),
      panel.background = element_blank()
    )
}
