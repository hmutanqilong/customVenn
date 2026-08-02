test_that("two-set balanced input keeps raw radius proportions", {
  x <- list(A = 1:100, B = 57:200)

  p <- suppressWarnings(plot_custom_venn(x))
  circles <- attr(p, "custom_venn_circles")

  expect_s3_class(p, "ggplot")
  expect_false(isTRUE(attr(p, "custom_venn_scaled")))
  expect_equal(max(circles$r) / min(circles$r), sqrt(144 / 100), tolerance = 1e-8)
})

test_that("two-set imbalanced input compresses radius proportions", {
  x <- list(A = 1:20, B = 1:2000)

  p <- suppressWarnings(plot_custom_venn(x, max_radius_ratio = 2.5, label = "none"))
  circles <- attr(p, "custom_venn_circles")

  expect_true(isTRUE(attr(p, "custom_venn_scaled")))
  expect_equal(max(circles$r) / min(circles$r), 2.5, tolerance = 1e-8)
})

test_that("three-set balanced input keeps raw radius proportions", {
  x <- list(A = 1:80, B = 41:140, C = 101:220)

  p <- suppressWarnings(plot_custom_venn(x))
  circles <- attr(p, "custom_venn_circles")

  expect_false(isTRUE(attr(p, "custom_venn_scaled")))
  expect_equal(max(circles$r) / min(circles$r), sqrt(120 / 80), tolerance = 1e-8)
})

test_that("three-set imbalanced rds input compresses radius proportions", {
  rds_path <- file.path("..", "..", "venn_diagram_gene_lst.rds")
  skip_if_not(file.exists(rds_path))

  x <- readRDS(rds_path)
  p <- suppressWarnings(plot_custom_venn(x, max_radius_ratio = 2.5, label = "none"))
  circles <- attr(p, "custom_venn_circles")

  expect_true(isTRUE(attr(p, "custom_venn_scaled")))
  expect_equal(max(circles$r) / min(circles$r), 2.5, tolerance = 1e-8)
})

test_that("default styling uses NPG colors and text layers only", {
  x <- list(A = 1:5, B = 3:8, C = 5:10)

  p <- suppressWarnings(plot_custom_venn(x))
  circles <- attr(p, "custom_venn_circles")
  geom_classes <- vapply(p$layers, function(layer) class(layer$geom)[1], character(1))

  expect_equal(circles$fill, c("#E64B35FF", "#4DBBD5FF", "#00A087FF"))
  expect_true(any(geom_classes == "GeomText"))
  expect_false(any(geom_classes == "GeomLabel"))
})

test_that("three-set input order maps to fixed top-left top-right bottom positions", {
  x <- list(A = 1:52, B = 1:136, C = 1:6497)

  p <- suppressWarnings(plot_custom_venn(x))
  circles <- attr(p, "custom_venn_circles")

  circle_a <- circles[circles$set == "A", ]
  circle_b <- circles[circles$set == "B", ]
  circle_c <- circles[circles$set == "C", ]

  expect_lt(circle_a$x0, circle_b$x0)
  expect_gt(circle_a$y0, circle_c$y0)
  expect_gt(circle_b$y0, circle_c$y0)
  expect_lt(abs(circle_a$y0 - circle_b$y0), 0.25 * max(circles$r))
  expect_lt(abs(circle_c$x0), 0.5 * max(circles$r))

  set_labels <- attr(p, "custom_venn_set_labels")
  expect_lt(set_labels$x[set_labels$set == "A"], circle_a$x0)
  expect_equal(set_labels$x[set_labels$set == "B"], circle_b$x0, tolerance = 1e-8)
  expect_equal(set_labels$x[set_labels$set == "C"], circle_c$x0, tolerance = 1e-8)
  expect_gt(set_labels$y[set_labels$set == "A"], circle_a$y0)
  expect_gt(set_labels$y[set_labels$set == "B"], circle_b$y0)
  expect_lt(set_labels$y[set_labels$set == "C"], circle_c$y0)
})

expect_region_label_boxes_inside <- function(p) {
  circles <- attr(p, "custom_venn_circles")
  labels <- attr(p, "custom_venn_labels")
  counts <- attr(p, "custom_venn_counts")
  labels <- labels[counts$count > 0, ]

  expect_true(all(c("label_width", "label_height") %in% names(labels)))

  for (i in seq_len(nrow(labels))) {
    members <- strsplit(gsub("_only$", "", labels$region[i]), "&", fixed = TRUE)[[1]]
    members <- members[members %in% circles$set]
    corners <- expand.grid(
      x = labels$x[i] + c(-0.5, 0.5) * labels$label_width[i],
      y = labels$y[i] + c(-0.5, 0.5) * labels$label_height[i]
    )

    for (member in members) {
      circle <- circles[circles$set == member, ]
      distances <- sqrt((corners$x - circle$x0)^2 + (corners$y - circle$y0)^2)
      expect_lte(max(distances), circle$r * 0.98)
    }
  }
}

region_members_for_test <- function(region, set_names) {
  only_match <- set_names[paste0(set_names, "_only") == region]
  if (length(only_match) == 1) {
    return(only_match)
  }

  members <- strsplit(region, "&", fixed = TRUE)[[1]]
  members[members %in% set_names]
}

expect_region_label_anchors_in_semantic_regions <- function(p) {
  circles <- attr(p, "custom_venn_circles")
  labels <- attr(p, "custom_venn_labels")
  counts <- attr(p, "custom_venn_counts")
  labels <- labels[counts$count > 0, ]

  for (i in seq_len(nrow(labels))) {
    members <- region_members_for_test(labels$region[i], circles$set)
    excluded <- setdiff(circles$set, members)

    for (member in members) {
      circle <- circles[circles$set == member, ]
      distance <- sqrt((labels$x[i] - circle$x0)^2 + (labels$y[i] - circle$y0)^2)
      expect_lte(distance, circle$r * 0.999)
    }

    for (member in excluded) {
      circle <- circles[circles$set == member, ]
      distance <- sqrt((labels$x[i] - circle$x0)^2 + (labels$y[i] - circle$y0)^2)
      expect_gte(distance, circle$r * 1.001)
    }
  }
}

expect_region_label_boxes_in_semantic_regions <- function(p) {
  circles <- attr(p, "custom_venn_circles")
  labels <- attr(p, "custom_venn_labels")
  counts <- attr(p, "custom_venn_counts")
  labels <- labels[counts$count > 0, ]

  for (i in seq_len(nrow(labels))) {
    members <- region_members_for_test(labels$region[i], circles$set)
    excluded <- setdiff(circles$set, members)
    corners <- expand.grid(
      x = labels$x[i] + c(-0.5, 0.5) * labels$label_width[i],
      y = labels$y[i] + c(-0.5, 0.5) * labels$label_height[i]
    )

    for (member in members) {
      circle <- circles[circles$set == member, ]
      distances <- sqrt((corners$x - circle$x0)^2 + (corners$y - circle$y0)^2)
      expect_lte(max(distances), circle$r * 0.98)
    }

    for (member in excluded) {
      circle <- circles[circles$set == member, ]
      distances <- sqrt((corners$x - circle$x0)^2 + (corners$y - circle$y0)^2)
      expect_gte(min(distances), circle$r * 1.001)
    }
  }
}

expect_region_label_boxes_do_not_overlap <- function(p) {
  labels <- attr(p, "custom_venn_labels")
  counts <- attr(p, "custom_venn_counts")
  labels <- labels[counts$count > 0, ]

  if (nrow(labels) < 2) {
    return(invisible(TRUE))
  }

  for (i in seq_len(nrow(labels) - 1)) {
    for (j in seq.int(i + 1, nrow(labels))) {
      separated_x <- abs(labels$x[i] - labels$x[j]) >=
        (labels$label_width[i] + labels$label_width[j]) / 2
      separated_y <- abs(labels$y[i] - labels$y[j]) >=
        (labels$label_height[i] + labels$label_height[j]) / 2
      expect_true(separated_x || separated_y)
    }
  }
}

test_that("region label text boxes stay inside every member circle in imbalanced layouts", {
  cases <- list(
    imbalanced_2 = list(A = 1:20, B = 1:2000),
    imbalanced_3 = list(
      burden = 1:52,
      dandelion = 1:136,
      gwas = 1:6497
    )
  )

  for (x in cases) {
    expect_region_label_boxes_inside(suppressWarnings(plot_custom_venn(x, label = "count", region_label_size = 2.5)))
    expect_region_label_boxes_inside(suppressWarnings(plot_custom_venn(x, label = "prop", region_label_size = 1.5)))
  }
})

test_that("three-set labels use the correct disjoint semantic regions in the real RDS order", {
  rds_path <- file.path("..", "..", "venn_diagram_gene_lst.rds")
  skip_if_not(file.exists(rds_path))

  raw <- readRDS(rds_path)
  x <- list(
    Burden = raw$burden,
    GWAS = raw$gwas,
    DANDELION = raw$dandelion
  )

  p <- suppressWarnings(plot_custom_venn(x, label = "count", region_label_size = 6))
  expect_region_label_anchors_in_semantic_regions(p)

  small_text_plot <- suppressWarnings(plot_custom_venn(x, label = "count", region_label_size = 1.5))
  expect_region_label_boxes_in_semantic_regions(small_text_plot)

  set_labels <- attr(p, "custom_venn_set_labels")
  circles <- attr(p, "custom_venn_circles")
  expect_lt(set_labels$x[set_labels$set == "Burden"], circles$x0[circles$set == "Burden"])
  expect_equal(set_labels$x[set_labels$set == "GWAS"], circles$x0[circles$set == "GWAS"], tolerance = 1e-8)
  expect_equal(set_labels$x[set_labels$set == "DANDELION"], circles$x0[circles$set == "DANDELION"], tolerance = 1e-8)
  expect_gt(set_labels$y[set_labels$set == "Burden"], circles$y0[circles$set == "Burden"])
  expect_gt(set_labels$y[set_labels$set == "GWAS"], circles$y0[circles$set == "GWAS"])
  expect_lt(set_labels$y[set_labels$set == "DANDELION"], circles$y0[circles$set == "DANDELION"])
})

test_that("auto fit keeps full text boxes in semantic regions", {
  rds_path <- file.path("..", "..", "venn_diagram_gene_lst.rds")
  skip_if_not(file.exists(rds_path))

  raw <- readRDS(rds_path)
  x <- list(
    Burden = raw$burden,
    GWAS = raw$gwas,
    DANDELION = raw$dandelion
  )

  fixed_plot <- suppressWarnings(plot_custom_venn(
    x,
    label = "count",
    region_label_size = 6,
    region_label_fit = "fixed"
  ))
  fixed_labels <- attr(fixed_plot, "custom_venn_labels")
  expect_true(all(fixed_labels$size == 6))
  expect_true(any(attr(fixed_plot, "custom_venn_label_issues")$region == "Burden&DANDELION"))

  auto_plot <- plot_custom_venn(
    x,
    label = "count",
    region_label_size = 6
  )
  auto_labels <- attr(auto_plot, "custom_venn_labels")
  adjustment <- attr(auto_plot, "custom_venn_layout_adjustment")

  expect_region_label_boxes_in_semantic_regions(auto_plot)
  expect_s3_class(adjustment, "data.frame")
  expect_true(isTRUE(adjustment$auto_fit))
  expect_true(adjustment$circle_adjusted || adjustment$shrink_applied)
  expect_lte(auto_labels$size[auto_labels$region == "Burden&DANDELION"], 6)
  expect_error(
    plot_custom_venn(x, label = "count", region_label_fit = "shrink"),
    "one of"
  )
})

test_that("region label text boxes avoid each other in current RDS layout at feasible fixed size", {
  rds_path <- file.path("..", "..", "venn_diagram_gene_lst.rds")
  skip_if_not(file.exists(rds_path))

  x <- readRDS(rds_path)

  expect_region_label_boxes_do_not_overlap(suppressWarnings(plot_custom_venn(x, label = "count", region_label_size = 2.5)))
  expect_region_label_boxes_do_not_overlap(suppressWarnings(plot_custom_venn(x, label = "prop", region_label_size = 1.5)))
})

test_that("region label size remains fixed even when labels cannot all fit", {
  rds_path <- file.path("..", "..", "venn_diagram_gene_lst.rds")
  skip_if_not(file.exists(rds_path))

  x <- readRDS(rds_path)

  expect_warning(
    plot_custom_venn(x, label = "count", region_label_size = 5, region_label_fit = "fixed"),
    "requested fixed font size"
  )
  expect_warning(
    plot_custom_venn(x, label = "prop", region_label_size = 5, region_label_fit = "fixed"),
    "requested fixed font size"
  )
  expect_warning(
    plot_custom_venn(x, label = "both", region_label_size = 5, region_label_fit = "fixed"),
    "requested fixed font size"
  )
  count_plot <- suppressWarnings(plot_custom_venn(x, label = "count", region_label_size = 5, region_label_fit = "fixed"))
  prop_plot <- suppressWarnings(plot_custom_venn(x, label = "prop", region_label_size = 5, region_label_fit = "fixed"))
  both_plot <- suppressWarnings(plot_custom_venn(x, label = "both", region_label_size = 5, region_label_fit = "fixed"))

  expect_true(all(attr(count_plot, "custom_venn_labels")$size == 5))
  expect_true(all(attr(prop_plot, "custom_venn_labels")$size == 5))
  expect_true(all(attr(both_plot, "custom_venn_labels")$size == 5))
  expect_true(is.data.frame(attr(count_plot, "custom_venn_label_issues")))
  expect_gt(nrow(attr(prop_plot, "custom_venn_label_issues")), 0)
  expect_gt(nrow(attr(both_plot, "custom_venn_label_issues")), 0)
})

test_that("label modes count, prop, both, and none are supported", {
  x <- list(A = 1:5, B = 3:8)

  count_plot <- suppressWarnings(plot_custom_venn(x, label = "count"))
  prop_plot <- suppressWarnings(plot_custom_venn(x, label = "prop"))
  both_plot <- suppressWarnings(plot_custom_venn(x, label = "both"))
  none_plot <- suppressWarnings(plot_custom_venn(x, label = "none"))

  expect_true(all(grepl("^[0-9,]+$", attr(count_plot, "custom_venn_labels")$label)))
  expect_true(all(grepl("^[0-9,]+\\n\\([0-9.]+%\\)$", attr(prop_plot, "custom_venn_labels")$label)))
  expect_true(all(grepl("^[0-9,]+\\n\\([0-9.]+%\\)$", attr(both_plot, "custom_venn_labels")$label)))
  expect_equal(nrow(attr(none_plot, "custom_venn_labels")), 0)
})

test_that("manual radius and style parameters are reflected in plot metadata", {
  x <- list(A = 1:5, B = 3:8)

  p <- suppressWarnings(plot_custom_venn(
    x,
    scale = "manual",
    manual_radius = c(A = 1, B = 2),
    fill = c(A = "red", B = "blue"),
    alpha = 0.25,
    border_color = c(A = "black", B = "gray50"),
    border_linewidth = 1.3,
    region_label_color = "purple",
    set_label_color = c(A = "orange", B = "green")
  ))
  circles <- attr(p, "custom_venn_circles")

  expect_equal(circles$r, c(1, 2))
  expect_equal(circles$fill, c("red", "blue"))
  expect_equal(circles$alpha, c(0.25, 0.25))
  expect_equal(circles$border_color, c("black", "gray50"))
  expect_equal(circles$border_linewidth, c(1.3, 1.3))
  expect_equal(attr(p, "custom_venn_labels")$color[1], "purple")
})
