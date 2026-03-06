################################################################################
# Project-wide configuration
################################################################################
#
# Centralized colors, labels, modelsummary defaults, and graphics parameters.
# Sourced by each analysis script via source(here("scripts/00_config.R")).
# Also sourced by .Rprofile for interactive convenience.
#
################################################################################

# Color schemes ----------------------------------------------------------------
# For gear
ps_color <- "#005C99"
ll_color <- "#E36C4A"

all_gears <- c(ps = ps_color,
               ll = ll_color)

# For species
alb_color <- "#7C6DB0"
bet_color <- "#B33A3A"
skj_color <- "#5A5A5A"
yft_color <- "#D9A441"

all_spp <- c(alb = alb_color,
             bet = bet_color,
             skj = skj_color,
             yft = yft_color,
             all = "black")

# Labels -----------------------------------------------------------------------
spp_labs <- c("alb" = "Albacore",
              "bet" = "Bigeye",
              "skj" = "Skipjack",
              "yft" = "Yellowfin")

# modelsummary defaults --------------------------------------------------------
gof_omit <- "^R2$|With|IC|RMSE|FE|Std"
tab_stars <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

# Graphics defaults ------------------------------------------------------------
lw <- 0.3
pt_size <- 2

# Table note building blocks ---------------------------------------------------
note_obs <- "The unit of observation is a grid cell by year."
note_fe  <- "All model specifications include fixed effects by year and grid cell."
note_ybar <- "$\\\\bar{Y}_{pre}$ indicates the mean of each outcome variable in the pre-closure period."

# Helper functions -------------------------------------------------------------
make_small <- function(path) {
  lines <- readLines(path)
  idx <- which(grepl("\\\\centering", lines))[1]
  lines <- append(lines, "\\small", after = idx)
  writeLines(lines, path)
}

wrap_notes <- function(path) {
  lines <- readLines(path)

  note_idx <- grep("\\\\multicolumn\\{\\d+\\}\\{l\\}\\{\\\\rule", lines)
  if (length(note_idx) == 0) return(invisible(NULL))

  note_texts <- sub(
    "^\\\\multicolumn\\{\\d+\\}\\{l\\}\\{\\\\rule\\{0pt\\}\\{[^}]+\\}(.*)\\}\\\\\\\\$",
    "\\1",
    lines[note_idx]
  )

  lines <- lines[-note_idx]

  centering_idx <- which(grepl("^\\\\centering$", lines))[1]
  lines <- append(lines, "\\begin{threeparttable}", after = centering_idx)

  end_tabular_idx <- which(grepl("^\\\\end\\{tabular\\}$", lines))
  notes_block <- c(
    "\\begin{tablenotes}",
    "\\small",
    paste0("\\item ", note_texts),
    "\\end{tablenotes}",
    "\\end{threeparttable}"
  )
  lines <- append(lines, notes_block, after = end_tabular_idx)

  writeLines(lines, path)
}

es_save <- function(plot, name, prefix, width = 10, height = 6) {
  ggplot2::ggsave(plot = plot,
                  filename = here::here("content", "img", paste0(prefix, "_", name, "_es.png")),
                  width = width, height = height)
}
