#' Count Venn diagram regions
#'
#' @param x A named list of two or three vectors.
#'
#' @return A tibble with one row per disjoint Venn region.
#' @export
venn_region_counts <- function(x) {
  sets <- clean_venn_sets(x)
  set_names <- names(sets)
  union_items <- Reduce(union, sets)
  union_n <- length(union_items)

  if (length(sets) == 2) {
    a <- sets[[1]]
    b <- sets[[2]]
    regions <- list(
      setdiff(a, b),
      setdiff(b, a),
      intersect(a, b)
    )
    region_names <- c(
      paste0(set_names[1], "_only"),
      paste0(set_names[2], "_only"),
      paste(set_names, collapse = "&")
    )
  } else {
    a <- sets[[1]]
    b <- sets[[2]]
    c <- sets[[3]]
    regions <- list(
      setdiff(a, union(b, c)),
      setdiff(b, union(a, c)),
      setdiff(c, union(a, b)),
      setdiff(intersect(a, b), c),
      setdiff(intersect(a, c), b),
      setdiff(intersect(b, c), a),
      Reduce(intersect, sets)
    )
    region_names <- c(
      paste0(set_names[1], "_only"),
      paste0(set_names[2], "_only"),
      paste0(set_names[3], "_only"),
      paste(set_names[c(1, 2)], collapse = "&"),
      paste(set_names[c(1, 3)], collapse = "&"),
      paste(set_names[c(2, 3)], collapse = "&"),
      paste(set_names, collapse = "&")
    )
  }

  counts <- lengths(regions)
  tibble::tibble(
    region = region_names,
    count = as.integer(counts),
    prop = counts / union_n,
    members = regions
  )
}

#' Draw a custom scaled Venn diagram
#'
#' @param x A named list of two or three vectors.
#' @param scale Circle scaling mode. `"auto"` keeps raw area-derived radius
#'   proportions until they exceed `max_radius_ratio`, then compresses them.
#' @param max_radius_ratio Maximum visual radius ratio in auto mode.
#' @param manual_radius Optional manual radii for `scale = "manual"`.
#' @param manual_area_factor Optional manual area factors for `scale = "manual"`.
#' @param label Region label mode. `"count"` shows counts only; `"prop"` and
#'   `"both"` show `counts\\n(prop)`; `"none"` hides region labels.
#' @param fill Fill colors for sets.
#' @param alpha Fill transparency.
#' @param border_color Circle border colors.
#' @param border_linewidth Circle border line widths.
#' @param set_label_color,set_label_size,set_label_fontface,set_label_family
#'   Set-name text styling.
#' @param region_label_color,region_label_size,region_label_fontface,region_label_family
#'   Region text styling.
#' @param region_label_fit Region label sizing policy. `"auto"` tries to keep
#'   `region_label_size` by adjusting the visual circle layout, then shrinks
#'   only labels that still cannot fit. `"fixed"` keeps the requested circle
#'   layout and `region_label_size`; unresolved issues are reported as warnings.
#' @param region_label_min_size Minimum region label size used by
#'   `region_label_fit = "auto"` when local shrinking is needed.
#' @param label_positions Optional data frame with `region`, `x`, and `y`.
#' @param set_label_positions Optional data frame with `set`, `x`, and `y`.
#' @param overlap Fraction of pairwise radius sums used as center distances.
#'   For three sets, input order maps to fixed positions: first set left-top,
#'   second set right-top, third set bottom.
#' @param prop_accuracy Accuracy passed to [scales::percent()] for prop labels.
#'
#' @return A ggplot object. Region label placement issues caused by fixed font
#'   size constraints are recorded in `attr(plot, "custom_venn_label_issues")`.
#'   The function warns when any such issue is present.
#' @importFrom rlang .data
#' @export
plot_custom_venn <- function(
  x,
  scale = c("auto", "raw", "manual"),
  max_radius_ratio = 2.5,
  manual_radius = NULL,
  manual_area_factor = NULL,
  label = c("count", "prop", "both", "none"),
  fill = NULL,
  alpha = 0.4,
  border_color = "black",
  border_linewidth = 0.7,
  set_label_color = "black",
  set_label_size = 5,
  set_label_fontface = "bold",
  set_label_family = "",
  region_label_color = "black",
  region_label_size = 5,
  region_label_fontface = "plain",
  region_label_family = "",
  region_label_fit = c("auto", "fixed"),
  region_label_min_size = 1,
  label_positions = NULL,
  set_label_positions = NULL,
  overlap = 0.62,
  prop_accuracy = 0.1
) {
  scale <- match.arg(scale)
  label <- match.arg(label)
  region_label_fit <- match.arg(region_label_fit)
  if (!is.numeric(region_label_min_size) || length(region_label_min_size) != 1 ||
      !is.finite(region_label_min_size) || region_label_min_size <= 0) {
    rlang::abort("`region_label_min_size` must be a positive finite number.")
  }

  sets <- clean_venn_sets(x)
  set_names <- names(sets)
  n_sets <- length(sets)
  set_sizes <- lengths(sets)
  radius_info <- compute_visual_radius(
    set_sizes,
    scale = scale,
    max_radius_ratio = max_radius_ratio,
    manual_radius = manual_radius,
    manual_area_factor = manual_area_factor,
    set_names = set_names
  )

  initial_circles <- layout_venn_circles(set_names, set_sizes, radius_info$r, overlap)
  counts <- venn_region_counts(sets)

  layout_fit <- fit_venn_layout_for_labels(
    initial_circles,
    counts,
    label,
    prop_accuracy,
    label_positions,
    region_label_size,
    region_label_fit,
    region_label_min_size,
    overlap
  )
  circles <- layout_fit$circles
  circles$raw_radius_ratio <- as.numeric(radius_info$raw_radius_ratio[set_names])
  circles$scaled <- radius_info$scaled
  circles$fill <- normalize_set_param(fill, set_names, npg_fill(n_sets), "fill")
  circles$alpha <- normalize_set_param(alpha, set_names, alpha, "alpha")
  circles$border_color <- normalize_set_param(border_color, set_names, border_color, "border_color")
  circles$border_linewidth <- normalize_set_param(border_linewidth, set_names, border_linewidth, "border_linewidth")

  set_labels <- make_set_labels(circles, set_label_positions)
  set_labels$color <- normalize_set_param(set_label_color, set_names, set_label_color, "set_label_color")
  set_labels$size <- normalize_set_param(set_label_size, set_names, set_label_size, "set_label_size")
  set_labels$fontface <- normalize_set_param(set_label_fontface, set_names, set_label_fontface, "set_label_fontface")
  set_labels$family <- normalize_set_param(set_label_family, set_names, set_label_family, "set_label_family")
  set_label_dims <- label_dimensions(set_labels$set, set_labels$size)
  set_labels$label_width <- set_label_dims$width
  set_labels$label_height <- set_label_dims$height

  region_labels <- layout_fit$labels
  if (nrow(region_labels) > 0) {
    region_labels$color <- rep(region_label_color, length.out = nrow(region_labels))
    region_labels$fontface <- rep(region_label_fontface, length.out = nrow(region_labels))
    region_labels$family <- rep(region_label_family, length.out = nrow(region_labels))
  }
  label_issues <- region_label_issues(region_labels, circles)
  if (nrow(label_issues) > 0) {
    warning(
      "Some region labels cannot fit cleanly at the requested fixed font size; ",
      "adjust `region_label_size` or circle scaling/layout parameters.",
      call. = FALSE
    )
  }

  x_limits <- range(c(
    circles$x0 - circles$r,
    circles$x0 + circles$r,
    set_labels$x - set_labels$label_width / 2,
    set_labels$x + set_labels$label_width / 2
  )) + c(-0.35, 0.35)
  y_limits <- range(c(
    circles$y0 - circles$r,
    circles$y0 + circles$r,
    set_labels$y - set_labels$label_height / 2,
    set_labels$y + set_labels$label_height / 2
  )) + c(-0.35, 0.35)

  p <- ggplot2::ggplot() +
    ggforce::geom_circle(
      data = circles,
      ggplot2::aes(
        x0 = .data$x0,
        y0 = .data$y0,
        r = .data$r,
        fill = .data$fill,
        alpha = .data$alpha,
        color = .data$border_color,
        linewidth = .data$border_linewidth
      )
    )

  if (nrow(region_labels) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = region_labels,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          label = .data$label,
          color = .data$color,
          size = .data$size,
          fontface = .data$fontface,
          family = .data$family
        )
      )
  }

  p <- p +
    ggplot2::geom_text(
      data = set_labels,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        label = .data$set,
        color = .data$color,
        size = .data$size,
        fontface = .data$fontface,
        family = .data$family
      )
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::coord_fixed(xlim = x_limits, ylim = y_limits, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 15, 10, 15)
    )

  attr(p, "custom_venn_circles") <- circles
  attr(p, "custom_venn_counts") <- counts
  attr(p, "custom_venn_labels") <- region_labels
  attr(p, "custom_venn_label_issues") <- label_issues
  attr(p, "custom_venn_set_labels") <- set_labels
  attr(p, "custom_venn_scaled") <- radius_info$scaled
  attr(p, "custom_venn_layout_adjustment") <- layout_fit$adjustment
  p
}

clean_venn_sets <- function(x) {
  if (!is.list(x)) {
    rlang::abort("`x` must be a named list.")
  }
  if (!length(x) %in% c(2, 3)) {
    rlang::abort("`x` must contain 2 or 3 sets.")
  }
  if (is.null(names(x)) || any(names(x) == "") || anyNA(names(x))) {
    rlang::abort("`x` must be a named list with non-empty set names.")
  }
  if (anyDuplicated(names(x))) {
    rlang::abort("Set names must be unique.")
  }

  cleaned <- lapply(x, function(value) {
    value <- as.character(value)
    value <- value[!is.na(value)]
    unique(value)
  })

  if (any(lengths(cleaned) == 0)) {
    rlang::abort("All sets must be non-empty after removing duplicates and NA values.")
  }

  cleaned
}

compute_visual_radius <- function(
  set_sizes,
  scale,
  max_radius_ratio,
  manual_radius,
  manual_area_factor,
  set_names
) {
  if (!is.numeric(max_radius_ratio) || length(max_radius_ratio) != 1 ||
      is.na(max_radius_ratio) || max_radius_ratio < 1) {
    rlang::abort("`max_radius_ratio` must be a single number greater than or equal to 1.")
  }

  raw_radius_ratio <- sqrt(set_sizes / min(set_sizes))
  names(raw_radius_ratio) <- set_names

  if (scale == "manual") {
    if (!is.null(manual_radius) && !is.null(manual_area_factor)) {
      rlang::abort("Use only one of `manual_radius` or `manual_area_factor`.")
    }
    if (!is.null(manual_radius)) {
      r <- as.numeric(normalize_set_param(manual_radius, set_names, NULL, "manual_radius"))
    } else if (!is.null(manual_area_factor)) {
      area_factor <- as.numeric(normalize_set_param(manual_area_factor, set_names, NULL, "manual_area_factor"))
      r <- sqrt(area_factor)
    } else {
      rlang::abort("`scale = \"manual\"` requires `manual_radius` or `manual_area_factor`.")
    }
    if (any(!is.finite(r)) || any(r <= 0)) {
      rlang::abort("Manual radii or area factors must be positive finite numbers.")
    }
    names(r) <- set_names
    return(list(r = r, raw_radius_ratio = raw_radius_ratio, scaled = TRUE))
  }

  if (scale == "raw" || max(raw_radius_ratio) <= max_radius_ratio) {
    return(list(r = raw_radius_ratio, raw_radius_ratio = raw_radius_ratio, scaled = FALSE))
  }

  gamma <- log(max_radius_ratio) / log(max(raw_radius_ratio))
  r <- raw_radius_ratio ^ gamma
  names(r) <- set_names
  list(r = r, raw_radius_ratio = raw_radius_ratio, scaled = TRUE)
}

layout_venn_circles <- function(set_names, set_sizes, r, overlap) {
  if (!is.numeric(overlap) || length(overlap) != 1 || is.na(overlap) ||
      overlap <= 0 || overlap >= 1) {
    rlang::abort("`overlap` must be a single number between 0 and 1.")
  }

  if (length(r) == 2) {
    d <- sum(r) * overlap
    x0 <- c(-d / 2, d / 2)
    y0 <- c(0, 0)
  } else {
    d12 <- (r[1] + r[2]) * overlap
    d13 <- (r[1] + r[3]) * overlap
    d23 <- (r[2] + r[3]) * overlap
    x3 <- (d13^2 - d23^2) / (2 * d12)
    y3_sq <- d13^2 - (x3 + d12 / 2)^2
    y3 <- -sqrt(max(y3_sq, 0))
    x0 <- c(-d12 / 2, d12 / 2, x3)
    y0 <- c(0, 0, y3)
    x0 <- x0 - mean(x0)
    y0 <- y0 - mean(y0)
  }

  tibble::tibble(
    set = set_names,
    size = as.integer(set_sizes),
    x0 = as.numeric(x0),
    y0 = as.numeric(y0),
    r = as.numeric(r)
  )
}

make_set_labels <- function(circles, set_label_positions = NULL) {
  centroid_x <- mean(circles$x0)
  centroid_y <- mean(circles$y0)
  dx <- circles$x0 - centroid_x
  dy <- circles$y0 - centroid_y
  distance <- sqrt(dx^2 + dy^2)
  distance[distance == 0] <- 1

  labels <- tibble::tibble(
    set = circles$set,
    x = circles$x0 + dx / distance * (circles$r + 0.45),
    y = circles$y0 + dy / distance * (circles$r + 0.45)
  )

  if (nrow(circles) == 2) {
    labels$x <- circles$x0
    labels$y <- circles$y0 + circles$r + 0.45
  } else if (nrow(circles) == 3) {
    labels$x <- c(
      circles$x0[1] - circles$r[1] * 0.95,
      circles$x0[2],
      circles$x0[3]
    )
    labels$y <- c(
      circles$y0[1] + circles$r[1] + 0.55,
      circles$y0[2] + circles$r[2] + 0.55,
      circles$y0[3] - circles$r[3] - 0.55
    )
  }

  if (!is.null(set_label_positions)) {
    labels <- override_positions(labels, set_label_positions, key = "set")
  }

  labels
}

fit_venn_layout_for_labels <- function(
  initial_circles,
  counts,
  label,
  prop_accuracy,
  label_positions,
  region_label_size,
  region_label_fit,
  region_label_min_size,
  overlap
) {
  fixed_labels <- make_region_labels(
    initial_circles,
    counts,
    label,
    prop_accuracy,
    label_positions,
    region_label_size,
    "fixed",
    region_label_min_size
  )
  fixed_issues <- region_label_issues(fixed_labels, initial_circles)

  if (region_label_fit == "fixed" || label == "none" || !is.null(label_positions)) {
    return(list(
      circles = initial_circles,
      labels = fixed_labels,
      adjustment = layout_adjustment_row(
        auto_fit = FALSE,
        circle_adjusted = FALSE,
        shrink_applied = FALSE,
        initial_circles = initial_circles,
        final_circles = initial_circles,
        initial_overlap = overlap,
        final_overlap = overlap,
        issue_count_before = nrow(fixed_issues),
        issue_count_after = nrow(fixed_issues)
      )
    ))
  }

  best <- list(
    circles = initial_circles,
    labels = fixed_labels,
    issues = fixed_issues,
    overlap = overlap,
    score = layout_fit_score(fixed_issues, initial_circles, initial_circles, overlap, overlap)
  )

  if (nrow(fixed_issues) > 0) {
    candidates <- label_fit_layout_candidates(initial_circles, fixed_issues, overlap)
    for (i in seq_len(nrow(candidates))) {
      candidate_r <- initial_circles$r * candidates$multipliers[[i]]
      candidate_circles <- layout_venn_circles(
        initial_circles$set,
        initial_circles$size,
        candidate_r,
        candidates$overlap[i]
      )
      candidate_labels <- make_region_labels(
        candidate_circles,
        counts,
        label,
        prop_accuracy,
        label_positions,
        region_label_size,
        "fixed",
        region_label_min_size
      )
      candidate_issues <- region_label_issues(candidate_labels, candidate_circles)
      candidate_score <- layout_fit_score(
        candidate_issues,
        initial_circles,
        candidate_circles,
        overlap,
        candidates$overlap[i]
      )
      if (candidate_score < best$score) {
        best <- list(
          circles = candidate_circles,
          labels = candidate_labels,
          issues = candidate_issues,
          overlap = candidates$overlap[i],
          score = candidate_score
        )
      }
    }
  }

  shrink_applied <- FALSE
  if (nrow(best$issues) > 0) {
    shrunk_labels <- make_region_labels(
      best$circles,
      counts,
      label,
      prop_accuracy,
      label_positions,
      region_label_size,
      "shrink",
      region_label_min_size
    )
    shrunk_issues <- region_label_issues(shrunk_labels, best$circles)
    shrink_applied <- any(shrunk_labels$size < region_label_size)
    if (nrow(shrunk_issues) <= nrow(best$issues)) {
      best$labels <- shrunk_labels
      best$issues <- shrunk_issues
    }
  }

  list(
    circles = best$circles,
    labels = best$labels,
    adjustment = layout_adjustment_row(
      auto_fit = nrow(fixed_issues) > 0,
      circle_adjusted = !isTRUE(all.equal(initial_circles$r, best$circles$r)) ||
        !isTRUE(all.equal(initial_circles$x0, best$circles$x0)) ||
        !isTRUE(all.equal(initial_circles$y0, best$circles$y0)),
      shrink_applied = shrink_applied,
      initial_circles = initial_circles,
      final_circles = best$circles,
      initial_overlap = overlap,
      final_overlap = best$overlap,
      issue_count_before = nrow(fixed_issues),
      issue_count_after = nrow(best$issues)
    )
  )
}

layout_fit_score <- function(issues, initial_circles, candidate_circles, initial_overlap, candidate_overlap) {
  semantic_issues <- sum(issues$issue %in% c(
    "outside_semantic_region",
    "box_outside_semantic_region",
    "outside_member_circle"
  ))
  overlap_issues <- sum(issues$issue == "overlaps_region_label")
  radius_change <- sum(abs(log(candidate_circles$r / initial_circles$r)))
  overlap_change <- abs(candidate_overlap - initial_overlap)

  semantic_issues * 100000 +
    overlap_issues * 10000 +
    radius_change * 100 +
    overlap_change * 10
}

label_fit_layout_candidates <- function(circles, issues, overlap) {
  candidates <- list()
  add_candidate <- function(multipliers, candidate_overlap) {
    candidate_overlap <- min(max(candidate_overlap, 0.35), 0.9)
    key <- paste(
      paste(round(multipliers, 3), collapse = ","),
      round(candidate_overlap, 3),
      sep = "|"
    )
    candidates[[key]] <<- list(multipliers = multipliers, overlap = candidate_overlap)
  }

  base <- rep(1, nrow(circles))
  add_candidate(base, overlap)
  for (candidate_overlap in unique(c(overlap * 0.92, overlap, overlap * 1.08))) {
    add_candidate(base, candidate_overlap)
  }

  issue_regions <- unique(issues$region[issues$issue %in% c(
    "outside_semantic_region",
    "box_outside_semantic_region",
    "outside_member_circle"
  )])
  for (region in issue_regions) {
    members <- region_members(region, circles$set)
    excluded <- setdiff(circles$set, members)
    for (expand in c(1.12, 1.25, 1.4)) {
      multipliers <- base
      multipliers[circles$set %in% members] <- expand
      multipliers[circles$set %in% excluded] <- 0.96
      for (candidate_overlap in unique(c(overlap * 0.92, overlap, overlap * 1.08))) {
        add_candidate(multipliers, candidate_overlap)
      }
    }
  }

  tibble::tibble(
    multipliers = lapply(candidates, `[[`, "multipliers"),
    overlap = vapply(candidates, `[[`, numeric(1), "overlap")
  )
}

layout_adjustment_row <- function(
  auto_fit,
  circle_adjusted,
  shrink_applied,
  initial_circles,
  final_circles,
  initial_overlap,
  final_overlap,
  issue_count_before,
  issue_count_after
) {
  tibble::tibble(
    auto_fit = auto_fit,
    circle_adjusted = circle_adjusted,
    shrink_applied = shrink_applied,
    initial_overlap = initial_overlap,
    final_overlap = final_overlap,
    initial_radii = list(stats::setNames(initial_circles$r, initial_circles$set)),
    final_radii = list(stats::setNames(final_circles$r, final_circles$set)),
    issue_count_before = issue_count_before,
    issue_count_after = issue_count_after
  )
}

make_region_labels <- function(
  circles,
  counts,
  label,
  prop_accuracy,
  label_positions = NULL,
  region_label_size = 5,
  region_label_fit = "fixed",
  region_label_min_size = 1
) {
  if (label == "none") {
    return(tibble::tibble(region = character(), x = numeric(), y = numeric(), label = character()))
  }

  set_names <- circles$set
  centroid <- c(mean(circles$x0), mean(circles$y0))

  if (nrow(circles) == 2) {
    positions <- tibble::tibble(
      region = c(
        paste0(set_names[1], "_only"),
        paste0(set_names[2], "_only"),
        paste(set_names, collapse = "&")
      ),
      x = c(circles$x0[1] - circles$r[1] * 0.38, circles$x0[2] + circles$r[2] * 0.38, mean(circles$x0)),
      y = c(circles$y0[1], circles$y0[2], mean(circles$y0))
    )
  } else {
    only_positions <- lapply(seq_len(3), function(i) {
      v <- c(circles$x0[i], circles$y0[i]) - centroid
      d <- sqrt(sum(v^2))
      if (d == 0) {
        v <- c(0, 1)
        d <- 1
      }
      c(circles$x0[i], circles$y0[i]) + v / d * circles$r[i] * 0.32
    })
    only_positions <- do.call(rbind, only_positions)

    pair_position <- function(i, j, k) {
      midpoint <- c(mean(circles$x0[c(i, j)]), mean(circles$y0[c(i, j)]))
      away <- midpoint - c(circles$x0[k], circles$y0[k])
      d <- sqrt(sum(away^2))
      if (d == 0) {
        away <- c(0, 1)
        d <- 1
      }
      midpoint + away / d * min(circles$r[c(i, j)]) * 0.16
    }

    pair_positions <- rbind(
      pair_position(1, 2, 3),
      pair_position(1, 3, 2),
      pair_position(2, 3, 1)
    )

    positions <- tibble::tibble(
      region = c(
        paste0(set_names[1], "_only"),
        paste0(set_names[2], "_only"),
        paste0(set_names[3], "_only"),
        paste(set_names[c(1, 2)], collapse = "&"),
        paste(set_names[c(1, 3)], collapse = "&"),
        paste(set_names[c(2, 3)], collapse = "&"),
        paste(set_names, collapse = "&")
      ),
      x = c(only_positions[, 1], pair_positions[, 1], centroid[1]),
      y = c(only_positions[, 2], pair_positions[, 2], centroid[2])
    )
  }

  positions <- dplyr::left_join(positions, counts, by = "region")
  positions$label <- format_region_label(positions$count, positions$prop, label, prop_accuracy)
  positions <- positions[, c("region", "x", "y", "label", "count", "prop")]
  positions$size <- rep(region_label_size, length.out = nrow(positions))
  positions <- add_region_label_dimensions(positions)
  positions <- keep_region_labels_inside_member_circles(
    positions,
    circles,
    region_label_fit = region_label_fit,
    region_label_min_size = region_label_min_size
  )

  if (!is.null(label_positions)) {
    positions <- override_positions(positions, label_positions, key = "region")
  }

  positions
}

keep_region_labels_inside_member_circles <- function(
  positions,
  circles,
  region_label_fit = "fixed",
  region_label_min_size = 1
) {
  placed <- positions[0, ]
  member_count <- vapply(positions$region, function(region) {
    length(region_members(region, circles$set))
  }, numeric(1))
  placement_order <- order(-member_count, -positions$count)

  for (i in placement_order) {
    members <- region_members(positions$region[i], circles$set)
    if (length(members) == 0) {
      next
    }

    if (region_label_fit == "shrink") {
      replacement <- shrink_and_place_region_label(
        label_row = positions[i, ],
        circles = circles,
        members = members,
        avoid_labels = placed,
        min_size = region_label_min_size
      )
      positions[i, ] <- replacement
      placed <- rbind(placed, positions[i, ])
      next
    }

    if (region_label_box_inside_semantic(positions[i, ], circles, members, margin = 0.9) &&
        !label_overlaps_any(positions[i, ], placed)) {
      placed <- rbind(placed, positions[i, ])
      next
    }

    preferred <- c(positions$x[i], positions$y[i])
    replacement <- find_region_label_position(
      circles = circles,
      include = members,
      preferred = preferred,
      label_width = positions$label_width[i],
      label_height = positions$label_height[i],
      avoid_labels = placed
    )
    positions$x[i] <- replacement[1]
    positions$y[i] <- replacement[2]
    placed <- rbind(placed, positions[i, ])
  }

  positions
}

shrink_and_place_region_label <- function(label_row, circles, members, avoid_labels, min_size) {
  max_size <- label_row$size
  sizes <- seq(max_size, min_size, by = -0.1)
  if (length(sizes) == 0 || utils::tail(sizes, 1) > min_size) {
    sizes <- c(sizes, min_size)
  }

  best <- NULL
  for (size in sizes) {
    candidate <- label_row
    candidate$size <- size
    candidate <- add_region_label_dimensions(candidate)

    replacement <- find_region_label_position(
      circles = circles,
      include = members,
      preferred = c(label_row$x, label_row$y),
      label_width = candidate$label_width,
      label_height = candidate$label_height,
      avoid_labels = avoid_labels
    )
    candidate$x <- replacement[1]
    candidate$y <- replacement[2]
    best <- candidate

    if (region_label_box_inside_semantic(candidate, circles, members, margin = 0.98) &&
        !label_overlaps_any(candidate, avoid_labels)) {
      return(candidate)
    }
  }

  best
}

region_label_issues <- function(positions, circles) {
  if (nrow(positions) == 0) {
    return(tibble::tibble(
      region = character(),
      issue = character(),
      label = character(),
      x = numeric(),
      y = numeric(),
      size = numeric()
    ))
  }

  rows <- list()
  for (i in seq_len(nrow(positions))) {
    members <- region_members(positions$region[i], circles$set)
    if (length(members) == 0) {
      next
    }
    if (!region_label_anchor_inside_semantic(positions[i, ], circles, members)) {
      rows[[length(rows) + 1]] <- label_issue_row(positions[i, ], "outside_semantic_region")
    }
    if (!region_label_box_inside_semantic(positions[i, ], circles, members, margin = 0.98)) {
      rows[[length(rows) + 1]] <- label_issue_row(positions[i, ], "box_outside_semantic_region")
    }
    if (!region_label_box_inside(positions[i, ], circles, members, margin = 0.98)) {
      rows[[length(rows) + 1]] <- label_issue_row(positions[i, ], "outside_member_circle")
    }
    previous <- positions[seq_len(i - 1), , drop = FALSE]
    if (nrow(previous) > 0 && label_overlaps_any(positions[i, ], previous)) {
      rows[[length(rows) + 1]] <- label_issue_row(positions[i, ], "overlaps_region_label")
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      region = character(),
      issue = character(),
      label = character(),
      x = numeric(),
      y = numeric(),
      size = numeric()
    ))
  }

  dplyr::bind_rows(rows)
}

label_issue_row <- function(label_row, issue) {
  tibble::tibble(
    region = label_row$region,
    issue = issue,
    label = label_row$label,
    x = label_row$x,
    y = label_row$y,
    size = label_row$size
  )
}

region_label_boxes_inside_member_circles <- function(positions, circles) {
  vapply(seq_len(nrow(positions)), function(i) {
    members <- region_members(positions$region[i], circles$set)
    if (length(members) == 0) {
      return(TRUE)
    }
    region_label_box_inside(positions[i, ], circles, members, margin = 0.98)
  }, logical(1))
}

region_label_box_inside <- function(label_row, circles, members, margin = 0.98) {
  corners <- label_box_corners(label_row)
  all(vapply(members, function(member) {
    circle <- circles[circles$set == member, ]
    distances <- sqrt((corners$x - circle$x0)^2 + (corners$y - circle$y0)^2)
    max(distances) <= circle$r * margin
  }, logical(1)))
}

region_label_anchor_inside_semantic <- function(label_row, circles, members, margin = 1) {
  included <- circles[circles$set %in% members, ]
  excluded <- circles[!circles$set %in% members, ]

  inside_included <- all(vapply(seq_len(nrow(included)), function(i) {
    distance <- sqrt((label_row$x - included$x0[i])^2 + (label_row$y - included$y0[i])^2)
    distance <= included$r[i] * margin
  }, logical(1)))
  outside_excluded <- TRUE
  if (nrow(excluded) > 0) {
    outside_excluded <- all(vapply(seq_len(nrow(excluded)), function(i) {
      distance <- sqrt((label_row$x - excluded$x0[i])^2 + (label_row$y - excluded$y0[i])^2)
      distance >= excluded$r[i] / margin
    }, logical(1)))
  }

  inside_included && outside_excluded
}

region_label_box_inside_semantic <- function(
  label_row,
  circles,
  members,
  margin = 0.98,
  exclude_margin = 1.001
) {
  region_label_box_inside(label_row, circles, members, margin = margin) &&
    region_label_box_outside_excluded(label_row, circles, members, margin = exclude_margin)
}

region_label_box_outside_excluded <- function(label_row, circles, members, margin = 1) {
  excluded <- circles[!circles$set %in% members, ]
  if (nrow(excluded) == 0) {
    return(TRUE)
  }

  corners <- label_box_corners(label_row)
  all(vapply(seq_len(nrow(excluded)), function(i) {
    distances <- sqrt((corners$x - excluded$x0[i])^2 + (corners$y - excluded$y0[i])^2)
    min(distances) >= excluded$r[i] * margin
  }, logical(1)))
}

label_box_corners <- function(label_row) {
  expand.grid(
    x = label_row$x + c(-0.5, 0.5) * label_row$label_width,
    y = label_row$y + c(-0.5, 0.5) * label_row$label_height
  )
}

region_members <- function(region, set_names) {
  only_match <- set_names[paste0(set_names, "_only") == region]
  if (length(only_match) == 1) {
    return(only_match)
  }

  members <- strsplit(region, "&", fixed = TRUE)[[1]]
  members[members %in% set_names]
}

find_region_label_position <- function(
  circles,
  include,
  preferred,
  label_width = 0,
  label_height = 0,
  avoid_labels = NULL
) {
  included <- circles[circles$set %in% include, ]
  excluded <- circles[!circles$set %in% include, ]

  x_range <- range(circles$x0 - circles$r, circles$x0 + circles$r)
  y_range <- range(circles$y0 - circles$r, circles$y0 + circles$r)
  candidates <- expand.grid(
    x = seq(x_range[1], x_range[2], length.out = 241),
    y = seq(y_range[1], y_range[2], length.out = 241)
  )
  all_candidates <- candidates

  semantic_box <- candidate_boxes_inside_circles(candidates, included, label_width, label_height, margin = 0.98) &
    candidate_boxes_outside_circles(candidates, excluded, label_width, label_height, margin = 1.001)
  semantic_anchor <- candidate_anchors_inside_circles(candidates, included, margin = 0.999) &
    candidate_anchors_outside_circles(candidates, excluded, margin = 1.001)
  member_box <- candidate_boxes_inside_circles(candidates, included, label_width, label_height, margin = 0.9)
  member_anchor <- candidate_anchors_inside_circles(candidates, included, margin = 1)

  if (any(semantic_box)) {
    candidates <- candidates[semantic_box, , drop = FALSE]
  } else if (any(semantic_anchor)) {
    candidates <- candidates[semantic_anchor, , drop = FALSE]
  } else if (any(member_box)) {
    candidates <- candidates[member_box, , drop = FALSE]
  } else if (any(member_anchor)) {
    candidates <- candidates[member_anchor, , drop = FALSE]
  } else {
    candidates <- all_candidates
  }

  exclude_penalty <- rep(0, nrow(candidates))
  if (nrow(excluded) > 0) {
    for (i in seq_len(nrow(excluded))) {
      distance <- sqrt((candidates$x - excluded$x0[i])^2 + (candidates$y - excluded$y0[i])^2)
      exclude_penalty <- exclude_penalty + pmax(0, excluded$r[i] - distance)^2
    }
  }

  include_penalty <- rep(0, nrow(candidates))
  for (i in seq_len(nrow(included))) {
    distance <- sqrt((candidates$x - included$x0[i])^2 + (candidates$y - included$y0[i])^2)
    include_penalty <- include_penalty + pmax(0, distance - included$r[i])^2
  }

  preferred_distance <- sqrt((candidates$x - preferred[1])^2 + (candidates$y - preferred[2])^2)
  center_distance <- Reduce(`+`, lapply(seq_len(nrow(included)), function(i) {
    sqrt((candidates$x - included$x0[i])^2 + (candidates$y - included$y0[i])^2) / included$r[i]
  })) / nrow(included)
  overlap_penalty <- label_overlap_penalty(candidates, label_width, label_height, avoid_labels)
  score <- (exclude_penalty + include_penalty) * 10000 +
    overlap_penalty * 1000 +
    preferred_distance +
    center_distance * 0.1
  best <- candidates[which.min(score), ]

  c(best$x, best$y)
}

candidate_anchors_inside_circles <- function(candidates, circles, margin = 1) {
  inside <- rep(TRUE, nrow(candidates))
  if (nrow(circles) == 0) {
    return(inside)
  }

  for (i in seq_len(nrow(circles))) {
    distance <- sqrt((candidates$x - circles$x0[i])^2 + (candidates$y - circles$y0[i])^2)
    inside <- inside & distance <= circles$r[i] * margin
  }
  inside
}

candidate_anchors_outside_circles <- function(candidates, circles, margin = 1) {
  outside <- rep(TRUE, nrow(candidates))
  if (nrow(circles) == 0) {
    return(outside)
  }

  for (i in seq_len(nrow(circles))) {
    distance <- sqrt((candidates$x - circles$x0[i])^2 + (candidates$y - circles$y0[i])^2)
    outside <- outside & distance >= circles$r[i] * margin
  }
  outside
}

candidate_boxes_inside_circles <- function(candidates, circles, label_width, label_height, margin = 0.98) {
  inside <- rep(TRUE, nrow(candidates))
  if (nrow(circles) == 0) {
    return(inside)
  }

  for (i in seq_len(nrow(circles))) {
    inside <- inside & box_centers_inside_circle(
      candidates,
      circles[i, ],
      label_width,
      label_height,
      margin
    )
  }
  inside
}

candidate_boxes_outside_circles <- function(candidates, circles, label_width, label_height, margin = 1) {
  outside <- rep(TRUE, nrow(candidates))
  if (nrow(circles) == 0) {
    return(outside)
  }

  corner_offsets <- expand.grid(
    dx = c(-0.5, 0.5) * label_width,
    dy = c(-0.5, 0.5) * label_height
  )

  for (i in seq_len(nrow(circles))) {
    outside_circle <- rep(TRUE, nrow(candidates))
    for (j in seq_len(nrow(corner_offsets))) {
      distance <- sqrt(
        (candidates$x + corner_offsets$dx[j] - circles$x0[i])^2 +
          (candidates$y + corner_offsets$dy[j] - circles$y0[i])^2
      )
      outside_circle <- outside_circle & distance >= circles$r[i] * margin
    }
    outside <- outside & outside_circle
  }

  outside
}

label_overlap_penalty <- function(candidates, label_width, label_height, avoid_labels) {
  penalty <- rep(0, nrow(candidates))
  if (is.null(avoid_labels) || nrow(avoid_labels) == 0) {
    return(penalty)
  }

  left <- candidates$x - label_width / 2
  right <- candidates$x + label_width / 2
  bottom <- candidates$y - label_height / 2
  top <- candidates$y + label_height / 2

  for (i in seq_len(nrow(avoid_labels))) {
    avoid_left <- avoid_labels$x[i] - avoid_labels$label_width[i] / 2
    avoid_right <- avoid_labels$x[i] + avoid_labels$label_width[i] / 2
    avoid_bottom <- avoid_labels$y[i] - avoid_labels$label_height[i] / 2
    avoid_top <- avoid_labels$y[i] + avoid_labels$label_height[i] / 2

    overlap_width <- pmax(0, pmin(right, avoid_right) - pmax(left, avoid_left))
    overlap_height <- pmax(0, pmin(top, avoid_top) - pmax(bottom, avoid_bottom))
    penalty <- penalty + overlap_width * overlap_height
  }

  penalty
}

label_overlaps_any <- function(label_row, placed) {
  if (nrow(placed) == 0) {
    return(FALSE)
  }

  any(vapply(seq_len(nrow(placed)), function(i) {
    abs(label_row$x - placed$x[i]) <
      (label_row$label_width + placed$label_width[i]) / 2 &&
      abs(label_row$y - placed$y[i]) <
        (label_row$label_height + placed$label_height[i]) / 2
  }, logical(1)))
}

box_centers_inside_circle <- function(candidates, circle, label_width, label_height, margin) {
  corner_offsets <- expand.grid(
    dx = c(-0.5, 0.5) * label_width,
    dy = c(-0.5, 0.5) * label_height
  )
  inside <- rep(TRUE, nrow(candidates))
  for (i in seq_len(nrow(corner_offsets))) {
    distance <- sqrt(
      (candidates$x + corner_offsets$dx[i] - circle$x0)^2 +
        (candidates$y + corner_offsets$dy[i] - circle$y0)^2
    )
    inside <- inside & distance <= circle$r * margin
  }
  inside
}

add_region_label_dimensions <- function(labels) {
  dims <- label_dimensions(labels$label, labels$size)
  labels$label_width <- dims$width
  labels$label_height <- dims$height
  labels
}

label_dimensions <- function(label, size) {
  lines <- strsplit(label, "\n", fixed = TRUE)
  max_chars <- vapply(lines, function(x) max(nchar(x), 1), numeric(1))
  line_count <- lengths(lines)
  tibble::tibble(
    width = max_chars * size * 0.043,
    height = line_count * size * 0.105
  )
}

format_region_label <- function(count, prop, label, prop_accuracy) {
  count_label <- scales::comma(count)
  prop_label <- scales::percent(prop, accuracy = prop_accuracy)

  switch(
    label,
    count = count_label,
    prop = paste0(count_label, "\n(", prop_label, ")"),
    both = paste0(count_label, "\n(", prop_label, ")"),
    none = character()
  )
}

override_positions <- function(base, override, key) {
  if (!is.data.frame(override) || !all(c(key, "x", "y") %in% names(override))) {
    rlang::abort(paste0("Position override must be a data frame with `", key, "`, `x`, and `y`."))
  }
  idx <- match(base[[key]], override[[key]])
  replace <- !is.na(idx)
  base$x[replace] <- override$x[idx[replace]]
  base$y[replace] <- override$y[idx[replace]]
  base
}

normalize_set_param <- function(value, set_names, default, arg) {
  if (is.null(value)) {
    value <- default
  }
  if (is.null(value)) {
    rlang::abort(paste0("`", arg, "` is required."))
  }
  if (!is.null(names(value)) && any(names(value) != "")) {
    missing <- setdiff(set_names, names(value))
    if (length(missing) > 0) {
      rlang::abort(paste0("`", arg, "` is missing values for: ", paste(missing, collapse = ", ")))
    }
    return(unname(value[set_names]))
  }
  if (length(value) == 1) {
    return(rep(value, length(set_names)))
  }
  if (length(value) != length(set_names)) {
    rlang::abort(paste0("`", arg, "` must have length 1 or length ", length(set_names), "."))
  }
  value
}

npg_fill <- function(n) {
  c("#E64B35FF", "#4DBBD5FF", "#00A087FF")[seq_len(n)]
}
