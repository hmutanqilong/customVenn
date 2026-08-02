\docType{package}
\name{customVenn-package}
\alias{customVenn}
\alias{customVenn-package}
\title{customVenn: Custom Scaled Venn Diagrams with ggplot2}
\description{
Draw two- and three-set circular Venn diagrams with true disjoint-region
counts, controllable visual circle scaling, and ggplot2 output.
}
\details{
The main plotting function is \code{\link{plot_custom_venn}}. It accepts a
named list of two or three vectors and returns a ggplot object. Region counts
are computed from true set membership, while circle radii can use raw,
compressed, or manual visual scaling.

For three-set diagrams, list order fixes the layout: the first set is drawn
left-top, the second right-top, and the third at bottom. Region labels are
placed as text in the correct disjoint semantic regions. With
\code{region_label_fit = "auto"}, the plot can adapt the visual circle layout
to keep labels readable; \code{region_label_fit = "fixed"} keeps the requested
layout and reports unresolved label placement issues.
}
\seealso{
\code{\link{plot_custom_venn}},
\code{\link{venn_region_counts}}
}
\keyword{package}
