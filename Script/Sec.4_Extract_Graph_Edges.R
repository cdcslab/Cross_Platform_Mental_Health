library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(patchwork)


# ------------------------------------------------------------
# 2. Paths and general parameters
# ------------------------------------------------------------

base_dir <- "Insert-base-dir/path/Data/" # Insert path to "Data" folder

# Significance threshold
alpha <- 0.05

# Multiple-testing correction
adjust_method <- "BH"


# ------------------------------------------------------------
# 3. Define the final year used for each platform
# ------------------------------------------------------------

target_years <- data.table(
  Platform = c( "Facebook", "Instagram", "Reddit", "Tumblr","YouTube"),
  year = c(
    2024L,
    2021L,
    2024L,
    2024L,
    2024L
  )
)


# ------------------------------------------------------------
# 4. Define transition files
# ------------------------------------------------------------

transition_files <- data.table(
  Platform = c(
    "Facebook",
    "Instagram",
    "Reddit",
    "Tumblr",
    "YouTube"
  ),
  file = c(
    file.path(base_dir, "Transition_Probs/transitions_probs_year_FB.csv"),
    file.path(base_dir, "Transition_Probs/transitions_probs_year_instagram.csv"),
    file.path(base_dir, "Transition_Probs/transitions_probs_year_reddit.csv"),
    file.path(base_dir, "Transition_Probs/transitions_probs_year_tumblr.csv"),
    file.path(base_dir, "Transition_Probs/transitions_probs_year_YT.csv")
  )
)

transition_files <- merge(
  transition_files,
  target_years,
  by = "Platform"
)


# ------------------------------------------------------------
# 5. Load observed transition counts
# ------------------------------------------------------------

transition_list <- lapply(
  seq_len(nrow(transition_files)),
  function(i) {
    
    platform_i <- transition_files$Platform[i]
    year_i <- transition_files$year[i]
    file_i <- transition_files$file[i]
    
    dt <- fread(file_i)
    
    # Check that all required variables are available
    required_columns <- c(
      "Category",
      "next_Category",
      "year",
      "N"
    )
    
    missing_columns <- setdiff(
      required_columns,
      names(dt)
    )
    
    if (length(missing_columns) > 0L) {
      stop(
        paste(
          "Missing columns in",
          basename(file_i),
          ":",
          paste(missing_columns, collapse = ", ")
        )
      )
    }
    
    # Keep only the target year
    dt <- dt[
      year == year_i,
      .(
        Category = as.character(Category),
        next_Category = as.character(next_Category),
        year = as.integer(year),
        N = as.numeric(N)
      )
    ]
    
    dt[, Platform := platform_i]
    
    # Aggregate possible duplicate rows
    dt <- dt[
      ,
      .(N = sum(N)),
      by = .(
        Platform,
        year,
        Category,
        next_Category
      )
    ]
    
    dt
  }
)

transitions <- rbindlist(
  transition_list,
  use.names = TRUE
)

rm(transition_list)


# ------------------------------------------------------------
# 6. Load category frequencies
# ------------------------------------------------------------


cat_freqs <- fread(paste0(base_dir, "cat_freqs_all_platforms.csv"))

# Keep only the platform-year combinations used in the final networks
cat_freqs <- cat_freqs[target_years, on = .(Platform, year),nomatch = 0L]


# ------------------------------------------------------------
# 9. Create the complete transition space
# ------------------------------------------------------------

# Every possible ordered pair of categories is included.
# Importantly, self-loops are retained here.
edge_grid <- cat_freqs[,{categories <- sort(unique(Category))
CJ(Category = categories, next_Category = categories, unique = TRUE)},
by = .(Platform, year)]


# ------------------------------------------------------------
# 10. Add observed transition counts
# ------------------------------------------------------------

edge_tests <- merge(
  edge_grid,
  transitions,
  by = c(
    "Platform",
    "year",
    "Category",
    "next_Category"
  ),
  all.x = TRUE
)

# Missing combinations correspond to zero observed transitions
edge_tests[
  is.na(N),
  N := 0
]


# ------------------------------------------------------------
# 11. Add destination-category prevalence
# ------------------------------------------------------------

destination_frequencies <- cat_freqs[,.(Platform, 
                                        year, 
                                        next_Category = Category,
                                        p_null = p)]

edge_tests <- merge(
  edge_tests,
  destination_frequencies,
  by = c("Platform", "year", "next_Category"),
  all.x = TRUE)


# ------------------------------------------------------------
# 12. Compute empirical conditional transition probabilities
# ------------------------------------------------------------

# N_out is the number of transitions originating from category i.
# Self-loops are included in this total.
edge_tests[, N_out := sum(N), by = .(Platform, year, Category)]

# Empirical conditional transition probability:
# P(next category = j | current category = i)
edge_tests[, p_obs := fifelse(N_out > 0, N / N_out, NA_real_)]


# ------------------------------------------------------------
# 13. Compute expectations under the null model
# ------------------------------------------------------------

# Under the null model, the probability of reaching category j
# is equal to its empirical prevalence, independently of i.
edge_tests[, N_expected := N_out * p_null]

# Difference between observed and expected transition probability
edge_tests[, excess_probability := p_obs - p_null]

# Observed-to-expected ratio
edge_tests[, enrichment := fifelse(N_out > 0 & p_null > 0, p_obs / p_null, NA_real_)]

# Log2 enrichment is useful as an effect-size measure.
# It is left undefined for unobserved transitions.
edge_tests[, log2_enrichment := fifelse(N > 0 & !is.na(enrichment) & enrichment > 0,
                                        log2(enrichment), NA_real_)]


# ------------------------------------------------------------
# 14. One-sided exact binomial tests
# ------------------------------------------------------------

# Null hypothesis:
#
# H0: P(j | i) = q_j
#
# Alternative hypothesis:
#
# H1: P(j | i) > q_j
#
# where q_j is the observed relative frequency of destination
# category j within the corresponding platform-year.

edge_tests[, p_value := NA_real_]

edge_tests[N_out > 0, p_value := pbinom(q = N - 1, size = N_out, prob = p_null, lower.tail = FALSE)]


# ------------------------------------------------------------
# 15. Identify self-loops
# ------------------------------------------------------------

edge_tests[, self_loop := Category == next_Category]


# ------------------------------------------------------------
# 16. Multiple-testing correction
# ------------------------------------------------------------

# Only cross-category transitions constitute hypotheses for the
# final transition network. Self-loops remain in the model but
# are not part of the graph's edge-selection procedure.

edge_tests[,p_adj := NA_real_]

edge_tests[!self_loop & !is.na(p_value),
           p_adj := p.adjust(p_value, method = adjust_method),
           by = .(Platform, year)]


# ------------------------------------------------------------
# 17. Select statistically over-represented transitions
# ------------------------------------------------------------

edge_tests[, significant := !self_loop & !is.na(p_adj) & p_adj < alpha & p_obs > p_null]

#fwrite(edge_tests, paste0("Insert/Path/", "edge_tests.csv"))
