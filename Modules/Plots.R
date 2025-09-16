bivariate_plot <- function(df, x, y, title = "" , subtitle_prefix = "") {
  
  # Calculate correlation manually with [[ ]] to get column by string
  cor_result <- cor.test(df[[deparse(substitute(x))]], df[[deparse(substitute(y))]], method = "pearson")
  cor_value <- round(cor_result$estimate, 2)
  
  ggplot(df, aes(x = {{x}}, y = {{y}})) +
    geom_point(size = 3, color = "steelblue") +
    labs(
      title = title,
      subtitle = paste0(subtitle_prefix, ": r = ", cor_value),
      x = deparse(substitute(x)),
      y = deparse(substitute(y))
    ) +
    geom_text(
      aes(label = ID),
      vjust = -1, hjust = 1, size = 3
    ) +
    geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "blue", linewidth = 0.8) +
    theme_minimal()
}
