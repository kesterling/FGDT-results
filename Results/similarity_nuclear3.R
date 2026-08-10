# ==============================================================================
# Structured Group Simulation Results - Nuclear Power (3 Options)
#
# Builds a faceted histogram comparing the distribution of pairwise cosine
# similarity scores across three experimental designs (N = 600, N = 60, N = 6
# participants) and three topics ("standpoints"), from text generated in
# repeated trials.
#
# This is the 3-topic counterpart to make_similarity_figure.R (the 4-topic
# version). Everything is identical except: (1) there are three standpoint
# columns instead of four, and (2) the figure title reflects "3 Options".
#
# Input: for each design, a subdirectory containing three CSV files
#   similarityMatrix_standpoint0.csv ... similarityMatrix_standpoint2.csv
# Each file is a square, headerless matrix of cosine similarities between
# trials (one topic's texts), with dimension = number of trials for that
# design (600-person design: 20 trials; 60-person: 30 trials; 6-person: 50
# trials).
#
# Recoding applied to every similarity score before plotting:
#   - a value of exactly 1        -> 0  (self-similarity / degenerate matches)
#   - a value less than 0.10      -> 0  (near-zero / noise)
#
# Y-AXIS TRUNCATION (automatic, data-driven, PER ROW):
# A structural zero-inflation (many recoded-to-zero pairs) can produce a zero
# bin that dwarfs every other bar in its row, which would force that row's
# y-axis to stretch so far that the non-zero distributions become invisible.
# To avoid that while keeping every panel WITHIN a row on an identical,
# comparable y-scale:
#   1. Each row (design) gets its own y-axis ceiling, set from the tallest
#      NON-ZERO bin found anywhere in that row (with headroom). Rows are
#      allowed to differ from each other so each design is shown at a scale
#      appropriate to it. This uses facet_grid(scales = "free_y"), which
#      shares the y-scale across all columns WITHIN a row while letting it
#      vary BETWEEN rows -- standard ggplot2 behavior, no extra packages
#      needed.
#   2. Any bin -- in practice, only ever the zero bin -- whose true count
#      exceeds ITS ROW's ceiling is drawn clipped at that ceiling (by
#      capping the plotted value itself, not the axis), marked with a small
#      "broken axis" (//) indicator, and labeled with its true count.
# This is fully automatic and requires no hard-coded panel list, so it will
# behave the same way on future data sets.
#
# Mean/SD/N annotation: drawn at a fixed position in the middle of every
# panel, so its placement is identical and comparable across all panels
# regardless of whether that particular panel happens to be truncated.
#
# Output: similarity_histogram_by_design_standpoint_3topic.png/.pdf saved
# alongside this script.
# ==============================================================================

# ---- Packages ----------------------------------------------------------------
required_pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2")
missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- Working directory --------------------------------------------------------
# Assumes this script lives in the same folder as the trial_id_* subdirectories.
# Works whether the script is run via Rscript or sourced from RStudio.
this_file <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    args <- commandArgs(trailingOnly = FALSE)
    f <- sub("^--file=", "", args[grepl("^--file=", args)])
    if (length(f) > 0) normalizePath(f) else NA_character_
  }
}, error = function(e) NA_character_)
if (!is.na(this_file) && nzchar(this_file)) setwd(dirname(this_file))

# ---- Study design: subdirectories, design labels, and trial counts -----------
designs <- tibble::tibble(
  trial_dir    = c(dirs[1],dirs[2],dirs[3]),
  design_label = factor(c("N = 600", "N = 60", "N = 6"),
                         levels = c("N = 600", "N = 60", "N = 6")),
  n_trials     = c(20L, 30L, 50L)
)

standpoints <- tibble::tibble(
  file_tag          = paste0("standpoint", 0:2),
  standpoint_label  = factor(c("Expand", "Maintain", "Phase Out"),
                              levels = c("Expand", "Maintain", "Phase Out"))
)

BINWIDTH   <- 0.02  # bin width chosen so a bin is centered exactly on 0
Y_HEADROOM <- 1.20   # per-row y-axis ceiling = tallest non-zero bin in that row x this factor

# Fixed annotation anchor (as a FRACTION of a row's own y ceiling), used
# identically in every panel: x sits just to the right of the zero bin for
# the truncation label.
LABEL_X_OFFSET       <- BINWIDTH * 0.8
TRUNCATION_LABEL_YFR <- 0.90

# Mean/SD/N box: centered in every panel (x = middle of the shared x range,
# y = half of that row's own y ceiling).
STATS_BOX_YFR <- 0.5

# ---- Helper: read one similarity matrix, recode, and pull the lower triangle --
read_lower_tri <- function(path, expected_n) {
  m <- as.matrix(read.csv(path, header = FALSE))
  stopifnot(nrow(m) == expected_n, ncol(m) == expected_n)

  v <- m[lower.tri(m, diag = FALSE)]
  v[v == 1]   <- 0   # exact 1s -> 0
  v[v < 0.10] <- 0   # anything below 0.10 (incl. negative values) -> 0
  v
}

# ---- Assemble the long-format data set for all 9 design x standpoint panels --
combos <- tidyr::crossing(designs, standpoints)

sim_data <- purrr::pmap_dfr(
  combos,
  function(trial_dir, design_label, n_trials, file_tag, standpoint_label) {
    path <- file.path(trial_dir, paste0("similarityMatrix_", file_tag, ".csv"))
    vals <- read_lower_tri(path, n_trials)
    tibble::tibble(
      design            = design_label,
      standpoint        = standpoint_label,
      cosine_similarity = vals
    )
  }
)

# ---- Per-panel summary stats for the NON-ZERO similarity scores --------------
panel_stats <- sim_data %>%
  dplyr::filter(cosine_similarity != 0) %>%
  dplyr::group_by(design, standpoint) %>%
  dplyr::summarise(
    mean_val = mean(cosine_similarity),
    sd_val   = sd(cosine_similarity),
    n_val    = dplyr::n(),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(
    label = sprintf("Mean = %.2f\nSD = %.2f\nN = %d", mean_val, sd_val, n_val)
  )

# ---- Shared bin edges across every panel (a bin centered on 0) ---------------
max_val <- max(sim_data$cosine_similarity)
edges   <- seq(-BINWIDTH / 2, max_val + BINWIDTH, by = BINWIDTH)

# ---- Pre-compute histogram counts for every panel -----------------------------
hist_df <- sim_data %>%
  dplyr::group_by(design, standpoint) %>%
  dplyr::group_modify(~ {
    h <- hist(.x$cosine_similarity, breaks = edges, plot = FALSE, right = FALSE)
    tibble::tibble(bin_center = h$mids, count = h$counts)
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is_zero_bin = abs(bin_center) < BINWIDTH / 4)

zero_bin_center <- hist_df$bin_center[hist_df$is_zero_bin][1]
first_standpoint <- levels(hist_df$standpoint)[1]
x_mid <- mean(range(edges))

# ---- Per-row y-axis ceiling: tallest NON-ZERO bin anywhere in that row -------
row_ceilings <- hist_df %>%
  dplyr::filter(!is_zero_bin) %>%
  dplyr::group_by(design) %>%
  dplyr::summarise(row_ceiling = max(count), .groups = "drop") %>%
  dplyr::mutate(
    row_ceiling = ifelse(!is.finite(row_ceiling) | row_ceiling <= 0, 1, row_ceiling),
    row_ceiling = row_ceiling * Y_HEADROOM
  )

# Cap the plotted bar height itself at the row's ceiling (this is what
# actually produces the truncation -- facet_grid(scales = "free_y") then
# auto-ranges each row to fit this already-capped data, no explicit
# per-row numeric axis limits required).
hist_df <- hist_df %>%
  dplyr::left_join(row_ceilings, by = "design") %>%
  dplyr::mutate(display_count = pmin(count, row_ceiling))

# A zero-height "anchor" point in every row (placed in the first standpoint
# column) so that row's free y-scale always extends up to its ceiling, even
# in rows where no bar (after capping) actually reaches that high.
row_anchors <- row_ceilings %>%
  dplyr::mutate(
    standpoint = factor(first_standpoint, levels = levels(hist_df$standpoint)),
    bin_center = zero_bin_center
  )

# ---- Panels whose true bin height exceeds their row's ceiling ----------------
# In practice this is only ever the zero bin, but the check is generic (not
# hard-coded to the zero bin) so it applies automatically to future data.
truncated_df <- hist_df %>%
  dplyr::filter(count > row_ceiling)

# Small double-diagonal "//" break mark for each truncated bar.
break_marks <- truncated_df %>%
  dplyr::select(design, standpoint, bin_center, row_ceiling) %>%
  dplyr::mutate(mark_id = list(1:2)) %>%
  tidyr::unnest(mark_id) %>%
  dplyr::mutate(
    shift = ifelse(mark_id == 1, -BINWIDTH * 0.16, BINWIDTH * 0.16),
    x     = bin_center + shift - BINWIDTH * 0.14,
    xend  = bin_center + shift + BINWIDTH * 0.14,
    y     = row_ceiling * 0.955,
    yend  = row_ceiling * 1.0
  )

# Label with the true count, placed to the right of the clipped bar so it
# doesn't bleed past the left axis spine when the truncated bin is at x = 0.
truncated_labels <- truncated_df %>%
  dplyr::mutate(
    x     = bin_center + LABEL_X_OFFSET,
    y     = row_ceiling * TRUNCATION_LABEL_YFR,
    label = paste0("n = ", count)
  )

# Mean/SD/N box: same fixed, centered (x, y-fraction-of-that-row's-ceiling)
# anchor in every panel, regardless of whether that particular panel is
# truncated.
stats_labels <- panel_stats %>%
  dplyr::left_join(row_ceilings, by = "design") %>%
  dplyr::mutate(
    x = x_mid,
    y = row_ceiling * STATS_BOX_YFR
  )

# ---- Build the figure ----------------------------------------------------------
p <- ggplot(hist_df, aes(x = bin_center, y = display_count, fill = is_zero_bin)) +
  geom_col(width = BINWIDTH, color = "white", linewidth = 0.1) +
  geom_blank(data = row_anchors, aes(x = bin_center, y = row_ceiling), inherit.aes = FALSE) +
  scale_fill_manual(
    values = c(`TRUE` = "maroon", `FALSE` = "grey65"),
    guide  = "none"
  ) +
  geom_vline(
    data = panel_stats,
    aes(xintercept = mean_val),
    color = "black", linewidth = 0.5, inherit.aes = FALSE
  ) +
  geom_label(
    data = stats_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, hjust = 0.5, vjust = 0.5,
    size = 2.6, lineheight = 0.9,
    label.size = 0, alpha = 0.75, fill = "white"
  ) +
  geom_segment(
    data = break_marks,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE, color = "white", linewidth = 1.8, lineend = "butt"
  ) +
  geom_segment(
    data = break_marks,
    aes(x = x, xend = xend, y = y, yend = yend),
    inherit.aes = FALSE, color = "black", linewidth = 0.8, lineend = "butt"
  ) +
  geom_label(
    data = truncated_labels,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, hjust = 0, vjust = 1,
    size = 2.6, fontface = "bold",
    label.size = 0, alpha = 0.75, fill = "white"
  ) +
  facet_grid(rows = vars(design), cols = vars(standpoint), scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
  labs(
    title = "Structured Group Simulation Results - Nuclear Power (Random)",
    x = "Cosine Similarity",
    y = "Count"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", hjust = 0.5)
  )

# Note: facet_grid(scales = "free_y") shares the y-axis across every column
# WITHIN a row (so the three standpoints for a given design stay directly
# comparable) while letting it vary BETWEEN rows (so each design is shown
# at its own appropriate scale). The x-scale (and the shared bin edges
# feeding every panel) stay fixed across the whole figure.

print(p)

# ---- Save the figure -----------------------------------------------------------
ggsave("similarity_nuclear3.png", plot = p,
       width = 12, height = 8, dpi = 300)
ggsave("similarity_nuclear3.pdf", plot = p,
       width = 12, height = 8)
