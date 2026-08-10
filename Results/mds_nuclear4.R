# ==============================================================================
# Structured Group Simulation Results - Nuclear Power (4 Options)
# Multidimensional Scaling (MDS) of Cosine Similarities
#
# For each of the three experimental designs (N = 600, N = 60, N = 6) and each
# of the four topics ("standpoints"), places every trial's text as a point in
# a 2-D space, positioned so that trials with higher cosine similarity to one
# another are plotted closer together.
#
# Recoding applied to every similarity score before use (identical to
# make_similarity_figure.R):
#   - a value of exactly 1        -> 0  (self-similarity / degenerate matches)
#   - a value less than 0.10      -> 0  (near-zero / noise)
#
# ONLY NON-ZERO (i.e., not recoded-to-zero) similarity values are used to
# place points: a recoded pair is excluded entirely from the MDS fit (given
# zero weight) rather than being treated as "maximally dissimilar." Doing
# this requires weighted / missing-data MDS, implemented here with the
# 'smacof' package's smacofSym() function via its `weightmat` argument
# (weight = 0 tells smacof to ignore that pair when fitting the layout).
#
# All panels share the same fixed X and Y scale (ggplot2's facet_grid()
# default) so the spread of points is directly comparable across designs
# and topics, and coord_fixed() keeps a 1:1 aspect ratio so on-page
# distances faithfully reflect the fitted MDS distances.
#
# VERIFICATION: this script has been run end-to-end against your real
# 4-topic data (trial_id_7320131895 / _4482425013 / _3787048560) and
# produces mds_by_design_standpoint.png/.pdf without errors. Note that
# smacofSym() automatically drops any trial that has zero valid (non-zero,
# non-recoded) pairs with every other trial in its panel, since such a
# trial cannot be placed -- this happened for a handful of trials in the
# sparser N = 6 panels (a message is printed for each one). If you install
# 'smacof' fresh, it depends on the CRAN package 'weights', which in turn
# needs 'mice', 'gdata', and 'lme4'; install.packages("smacof") should
# pull all of this in automatically.
#
# Output: mds_by_design_standpoint.png/.pdf saved alongside this script.
# ==============================================================================

# ---- Packages ----------------------------------------------------------------
# Installing smacof for the first time can take a few minutes -- it pulls in
# several dependencies (Hmisc, polynom, nnls, e1071, plotrix, colorspace, ...).
required_pkgs <- c("dplyr", "tidyr", "purrr", "tibble", "ggplot2", "smacof")
missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ---- Working directory --------------------------------------------------------
# Assumes this script lives in the same folder as the trial_id_* subdirectories.
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
  file_tag          = paste0("standpoint", 0:3),
  standpoint_label  = factor(c("Expand", "Maintain", "Phase Out", "Research"),
                              levels = c("Expand", "Maintain", "Phase Out", "Research"))
)

# ---- Helper: read one similarity matrix and recode it -------------------------
# MDS needs the full symmetric matrix (not just the lower triangle used for
# the histogram figures).
read_recoded_matrix <- function(path, expected_n) {
  m <- as.matrix(read.csv(path, header = FALSE))
  stopifnot(nrow(m) == expected_n, ncol(m) == expected_n)

  m[m == 1]   <- 0   # exact 1s -> 0 (also zeroes the diagonal, which is fine --
                      # self-similarity isn't used by the MDS fit either way)
  m[m < 0.10] <- 0   # anything below 0.10 (incl. negative values) -> 0
  m
}

# ---- Helper: connected components of a 0/1 adjacency matrix (base-R BFS) -----
# Used purely as a diagnostic: if the "valid pair" graph for a panel breaks
# into more than one component, the relative placement of trials in
# different components is not well-determined by the data.
connected_components <- function(adj) {
  n <- nrow(adj)
  comp <- rep(0L, n)
  current <- 0L
  for (start in seq_len(n)) {
    if (comp[start] != 0L) next
    current <- current + 1L
    queue <- start
    comp[start] <- current
    while (length(queue) > 0) {
      node <- queue[1]
      queue <- queue[-1]
      neighbors <- which(adj[node, ] > 0 & comp == 0L)
      comp[neighbors] <- current
      queue <- c(queue, neighbors)
    }
  }
  comp
}

# ---- Compute the 2-D MDS configuration for one design x standpoint panel ------
compute_mds_panel <- function(trial_dir, design_label, n_trials, file_tag, standpoint_label) {
  path <- file.path(trial_dir, paste0("similarityMatrix_", file_tag, ".csv"))
  S <- read_recoded_matrix(path, n_trials)

  W <- (S != 0) * 1     # weight = 1 for valid (non-recoded) pairs, 0 otherwise
  diag(W) <- 0
  Delta <- 1 - S         # cosine similarity -> dissimilarity for the MDS fit
  diag(Delta) <- 0

  trial_id <- seq_len(n_trials)

  # Trials with zero valid connections to every other trial carry no usable
  # information and cannot be meaningfully placed -- drop them.
  degree <- rowSums(W > 0)
  isolated <- degree == 0
  if (any(isolated)) {
    message(sprintf(
      "  [%s / %s] dropping %d fully-isolated trial(s) with no valid (non-zero) pairs",
      design_label, standpoint_label, sum(isolated)
    ))
  }
  keep <- !isolated
  S     <- S[keep, keep, drop = FALSE]
  W     <- W[keep, keep, drop = FALSE]
  Delta <- Delta[keep, keep, drop = FALSE]
  trial_id <- trial_id[keep]

  # Diagnostic: how sparse is this panel, and is it fully connected?
  n_valid_pairs <- sum(W[upper.tri(W)] > 0)
  n_total_pairs <- length(W[upper.tri(W)])
  n_comp <- if (length(trial_id) > 0) length(unique(connected_components(W > 0))) else 0L
  message(sprintf(
    "  [%s / %s] %d trials retained, %d/%d pairs valid, %d connected component(s)",
    design_label, standpoint_label, length(trial_id), n_valid_pairs, n_total_pairs, n_comp
  ))

  if (length(trial_id) < 3) {
    warning(sprintf(
      "[%s / %s] fewer than 3 placeable trials -- skipping MDS for this panel",
      design_label, standpoint_label
    ))
    return(tibble::tibble(
      design = design_label, standpoint = standpoint_label,
      trial_id = integer(0), D1 = numeric(0), D2 = numeric(0)
    ))
  }

  # weight = 0 tells smacof to ignore that pair entirely when fitting the
  # 2-D layout -- this is what implements "only include non-zero values."
  fit <- smacof::smacofSym(delta = Delta, weightmat = W, ndim = 2, type = "ratio")

  tibble::tibble(
    design     = design_label,
    standpoint = standpoint_label,
    trial_id   = trial_id,
    D1         = fit$conf[, 1],
    D2         = fit$conf[, 2]
  )
}

# ---- Run every design x standpoint panel --------------------------------------
combos <- tidyr::crossing(designs, standpoints)

mds_data <- purrr::pmap_dfr(
  combos,
  function(trial_dir, design_label, n_trials, file_tag, standpoint_label) {
    compute_mds_panel(trial_dir, design_label, n_trials, file_tag, standpoint_label)
  }
)

# ---- Build the figure ----------------------------------------------------------
p <- ggplot(mds_data, aes(x = D1, y = D2)) +
  geom_point(shape = 21, size = 1.8, fill = "steelblue", color = "black",
             stroke = 0.2, alpha = 0.8) +
  facet_grid(rows = vars(design), cols = vars(standpoint)) +  # fixed (shared) scales by default
  coord_fixed(ratio = 1) +   # equal-unit axes so on-page distances reflect fitted MDS distances
  labs(
    title = "Structured Group Simulation Results - Nuclear Power (4 Options)\nMultidimensional Scaling of Cosine Similarities",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text        = element_text(face = "bold"),
    panel.grid.minor  = element_blank(),
    plot.title        = element_text(face = "bold", hjust = 0.5)
  )

# Note: facet_grid() with no `scales` argument (the default, "fixed") keeps
# every panel on the same X and Y scale, so panel spread is directly
# comparable across designs and topics.

print(p)

# ---- Save the figure -----------------------------------------------------------
ggsave("mds_nuclear4.png", plot = p, width = 12, height = 9, dpi = 300)
ggsave("mds_nuclear4.pdf", plot = p, width = 12, height = 9)
