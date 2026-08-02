# customVenn

`customVenn` draws two- and three-set circular Venn diagrams with `ggplot2`.
Region labels are computed from the true set membership, while circle radii can
use raw area-derived proportions, automatic compression, or manual values.
For three-set diagrams, list order controls the fixed layout: the first set is
drawn at left-top, the second at right-top, and the third at bottom.

The default fill palette uses Nature/NPG-style colors:

```r
c("#E64B35FF", "#4DBBD5FF", "#00A087FF")
```

## Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("hmutanqilong/customVenn")
```

Load the package:

```r
library(customVenn)
```

## Quick Start

Use a named list of two or three vectors. Duplicates and `NA` values are removed
before counting regions.

```r
sets2 <- list(
  Burden = paste0("gene", 1:80),
  GWAS = paste0("gene", 41:160)
)

plot_custom_venn(sets2)
```

For three sets, the input order fixes the visual positions:

```r
sets3 <- list(
  Burden = paste0("gene", 1:52),
  GWAS = paste0("gene", c(21:32, 1000:7432)),
  DANDELION = paste0("gene", c(1:6, 33:46, 200:269))
)

plot_custom_venn(sets3, region_label_size = 6)
```

The first set is drawn left-top, the second right-top, and the third at bottom.

## Region Counts

Use `venn_region_counts()` when you need the exact disjoint region counts before
plotting.

```r
venn_region_counts(sets3)
```

The returned `members` column contains the elements assigned to each disjoint
region.

## Labels

Set `label = "prop"` or `label = "both"` to show region text as
`counts\n(prop)`, for example `6,433\n(98.6%)`. By default,
`region_label_fit = "auto"` tries to keep `region_label_size` by adjusting the
visual circle layout; if a label still cannot fit, only that local label is
shrunk. Use `region_label_fit = "fixed"` to keep the requested circle layout and
font size unchanged; unresolved label placement issues are returned in
`attr(p, "custom_venn_label_issues")`.

```r
p <- plot_custom_venn(
  sets3,
  label = "both",
  region_label_size = 6,
  region_label_fit = "auto"
)

attr(p, "custom_venn_layout_adjustment")
attr(p, "custom_venn_label_issues")
```

Use `label = "none"` to hide region labels.

## Scaling Modes

- `scale = "raw"` uses radius ratios from `sqrt(set_size / min_set_size)`.
- `scale = "auto"` keeps raw ratios when they are moderate and compresses them
  when the largest radius ratio exceeds `max_radius_ratio`.
- `scale = "manual"` uses `manual_radius` or `manual_area_factor`.

Counts and proportions are always calculated from the original set membership.
When auto or manual scaling changes circle sizes, the diagram is a readable
visual summary rather than a strict area-proportional plot.

```r
plot_custom_venn(sets3, scale = "raw")

plot_custom_venn(sets3, scale = "auto", max_radius_ratio = 2.5)

plot_custom_venn(
  sets3,
  scale = "manual",
  manual_radius = c(Burden = 1.2, GWAS = 2.2, DANDELION = 1.4)
)
```

## Styling

All circles remain circular. Common visual parameters can be supplied as a
single value or a named vector aligned to the set names.

```r
plot_custom_venn(
  sets3,
  fill = c(Burden = "#E64B35FF", GWAS = "#4DBBD5FF", DANDELION = "#00A087FF"),
  alpha = 0.35,
  border_color = c(Burden = "black", GWAS = "gray20", DANDELION = "black"),
  border_linewidth = 0.9,
  set_label_size = 6,
  set_label_fontface = "bold",
  region_label_size = 5,
  region_label_color = "black"
)
```

You can override individual text positions with data frames:

```r
plot_custom_venn(
  sets3,
  label_positions = data.frame(region = "Burden_only", x = -1.3, y = 0.7),
  set_label_positions = data.frame(set = "GWAS", x = 1.2, y = 3.4)
)
```

## Help

```r
?customVenn
?plot_custom_venn
?venn_region_counts
```
