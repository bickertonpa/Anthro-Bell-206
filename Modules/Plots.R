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

lmPrintout <- function(step_model, digits = 2, mult = " × ") {
  stopifnot(inherits(step_model, "lm"))
  
  coefs <- stats::coef(step_model)
  terms <- names(coefs)
  resp  <- all.vars(stats::formula(step_model))[1]
  fmt   <- function(x) format(round(x, digits), trim = TRUE, scientific = FALSE)
  
  has_intercept <- "(Intercept)" %in% terms
  
  # Start equation
  if (has_intercept) {
    intercept <- coefs["(Intercept)"]
    eq <- paste0(resp, " = ", fmt(intercept))
    idx <- setdiff(seq_along(coefs), match("(Intercept)", terms))
  } else {
    eq <- paste0(resp, " = ")
    idx <- seq_along(coefs)
  }
  
  # Add predictor terms (if any)
  if (length(idx) > 0) {
    first <- !has_intercept
    for (i in idx) {
      sign <- if (first) {
        if (coefs[i] < 0) "-" else ""     # no leading " + " for very first term
      } else {
        if (coefs[i] < 0) " - " else " + "
      }
      eq <- paste0(eq, sign, fmt(abs(coefs[i])), mult, terms[i])
      first <- FALSE
    }
  }
  
  r2 <- summary(step_model)$r.squared
  paste(eq, sprintf("  |  R\u00B2 = %.2f", r2))
}