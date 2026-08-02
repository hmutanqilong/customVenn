if (requireNamespace("customVenn", quietly = TRUE)) {
  library(customVenn)
} else if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  source(file.path("R", "plot_custom_venn.R"))
}

if (!requireNamespace("eulerr", quietly = TRUE)) {
  message("Skipping eulerr validation because eulerr is not installed.")
  quit(status = 0)
}

balanced_2 <- list(A = 1:100, B = 57:200)
balanced_3 <- list(A = 1:80, B = 41:140, C = 101:220)

validate_fit <- function(x, label) {
  fit <- eulerr::euler(x, shape = "circle")
  counts <- venn_region_counts(x)
  message(label)
  message("  customVenn union count: ", sum(counts$count))
  message("  eulerr diagError: ", signif(fit$diagError, 4))
  message("  eulerr stress: ", signif(fit$stress, 4))
  invisible(fit)
}

validate_fit(balanced_2, "balanced 2-set validation")
validate_fit(balanced_3, "balanced 3-set validation")

if (file.exists("venn_diagram_gene_lst.rds")) {
  imbalanced_3 <- readRDS("venn_diagram_gene_lst.rds")
  validate_fit(imbalanced_3, "imbalanced RDS 3-set validation")
}
