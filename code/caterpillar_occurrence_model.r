# =========================================================
# PACKAGES
# =========================================================

library(glmmTMB)
library(emmeans)   # Marginal means and contrasts
library(dplyr)     # Data manipulation

# =========================================================
# SECTION 1: DATA PREPARATION
# =========================================================

# Temporary reading in of CC data for 5 sites for 2024-2026
dataset = read.csv('../caterpillars-analysis-public/data/fullDataset_2026-07-14.csv') %>%
  filter(Name %in% c("Eno River State Park",
                     "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                     "NC Botanical Garden",
                     "UNC Chapel Hill Campus",
                     "Prairie Ridge Ecostation"),
         Year %in% 2024:2026)


# Caterpillars Count! raw data subsetted to the 5 NC sites and the years 2024-2026
#dataset = read.csv('data/cc_cicada_analysis_data_2024-2026.csv')

# Cicada density index by site
cicadaLevels = read.csv("data/cicada_noise_by_site_on_day143.csv") %>%
  mutate(Name = case_when(site == "eno" ~ "Eno River State Park",
                          site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                          site == "ncbg" ~ "NC Botanical Garden",
                          site == "unc" ~ "UNC Chapel Hill Campus",
                          site == "pridge" ~ "Prairie Ridge Ecostation")) %>%
  select(Name, cicadaIndex)

# NOTE: The "cicada period" is designated as between days 121-159 (i.e. thru June 7).
# See the cicada amplitude by week plot created in creating_cicada_output.r

dat = dataset %>%
  group_by(Name, ID, PlantFK, LocalDate, julianday, Year, ObservationMethod, WetLeaves, Group, sciName) %>%
  # we will include sawfly larvae in 'caterpillars' for our purposes
  summarize(occurrence = ifelse(sum(Quantity[Group == 'caterpillar' | (Group == 'bee' & Sawfly)], na.rm = T), 1, 0)) %>%
  filter(julianday %in% 121:212) %>% # May 1 to July 31
  mutate(Period = ifelse(julianday >= 121 & julianday < 160, "cicada", "post_cicada"),
         Site = factor(Name),
         Week = ceiling((julianday - 4)/7)) %>% 
  left_join(cicadaLevels, by = 'Name')
  

# Expected columns in dat:
#   occurrence          : integer 0/1 (caterpillar present/absent)
#   Year                : integer 2024, 2025, or 2026
#   Period              : character "cicada" or "post_cicada"
#   site_cicada_density : numeric, constant within site across all rows
#   Site                : site identifier, 5 levels
#   PlantFK              : branch code, unique within and across sites
#   Week                : integer calendar week of survey
#   ObservationMethod   : character "Visual" or "Beat sheet"

# ---------------------------------------------------------
# 1a. Year as factor with orthogonal planned contrasts
# ---------------------------------------------------------
dat$Year <- factor(dat$Year, levels = c("2024", "2025", "2026"))

# Two orthogonal contrasts:
#   cicada_vs_noncicada : 2024 vs. average of 2025 and 2026
#                         Primary test of cicada year effect
#   yr1_vs_yr2          : 2025 vs. 2026
#                         Tests for carry-over effects or year-to-year recovery

contrasts(dat$Year) <- cbind(
  cicada_vs_noncicada = c( 1.0, -0.5, -0.5),
  yr1_vs_yr2          = c( 0.0,  0.5, -0.5)
)

# Verify orthogonality: dot product of the two contrast vectors should be 0
cat("Contrast dot product (should be 0):",
    sum(c(1.0, -0.5, -0.5) * c(0.0, 0.5, -0.5)), "\n")

# ---------------------------------------------------------
# 1b. Period as factor with cicada period as reference level
# ---------------------------------------------------------
# With "cicada" as reference, the Period main effect and its interaction
# with Year contrasts represent the post-cicada vs. cicada-period difference,
# which is the comparison of primary interest.
dat$Period <- factor(dat$Period, levels = c("cicada", "post_cicada"))

# ---------------------------------------------------------
# 1c. Survey method as factor
# ---------------------------------------------------------
dat$ObservationMethod <- factor(dat$ObservationMethod, levels = c("Visual", "Beat sheet"))


# ---------------------------------------------------------
# 1d. Center continuous predictors
# ---------------------------------------------------------

# Center cicada density at its grand mean so that the main effect coefficient
# represents the effect at mean cicada density across sites, not at zero density.
# Do NOT scale (divide by SD) — the raw units of cicada density are interpretable.
# Compute mean across the 5 unique site cicada density values
# Each site contributes equally regardless of how many observations it has
site_cicada_values <- dat %>%
  distinct(Site, cicadaIndex) %>%
  pull(cicadaIndex)

cicada_center <- mean(site_cicada_values)

# Now center the variable
dat$cicadaIndex_c <- dat$cicadaIndex - cicada_center

# Center Week at its grand mean across the full season.
# This makes the model intercept interpretable as occurrence at the 
# mid-season average rather than at week zero.
week_center <- mean(unique(dat$Week))
dat$Week_c  <- dat$Week - week_center

# =========================================================
# SECTION 2: PRIMARY MODEL
# =========================================================

m1 <- glmmTMB(
  occurrence ~
    
    # --- Core inferential terms -------------------------------------------
  
  Year * Period +
    # Year contributes two terms via the planned contrasts set above:
    #   Yearcicada_vs_noncicada      : mean occurrence, cicada vs. non-cicada years
    #   Yearyr1_vs_yr2               : mean occurrence, year+1 vs. year+2
    # Period contributes one term:
    #   Periodpost_cicada            : post-cicada vs. cicada period, pooled across years
    # The interaction contributes two terms — these are the key hypothesis tests:
    #   Yearcicada_vs_noncicada:Periodpost_cicada  --- PRIMARY CICADA EFFECT TEST
    #     Does the magnitude of the cicada-to-post-cicada seasonal shift in
    #     caterpillar occurrence differ between the cicada year and the average
    #     of the two non-cicada years? A positive coefficient means caterpillar
    #     occurrence rose MORE from cicada to post-cicada period in 2024 than
    #     in non-cicada years, consistent with suppressed predation during the
    #     cicada period.
    #   Yearyr1_vs_yr2:Periodpost_cicada           --- CARRY-OVER EFFECT TEST
    #     Does the seasonal shift differ between year+1 and year+2?
    
    # --- Site-level dose-response ------------------------------------------
  
  cicadaIndex_c +
    # Does between-site variation in cicada density predict overall caterpillar
    # occurrence across all years and periods?
    # CAUTION: this effect is estimated from between-site variation at 5 sites
    # only. Confidence intervals will appear precise but should be interpreted
    # cautiously — see Section 3 and accompanying notes.
    
    # --- Nuisance / control terms ------------------------------------------
  
  Week_c +          # Linear within-season temporal trend. Captures residual
    # phenological change not already captured by Period.
    
    ObservationMethod +   # Systematic difference in caterpillar detection probability
    # between Visual and beat sheet methods. Expected direction:
    # beat sheet typically detects more caterpillars than Visual survey.
    
    # --- Random effects ----------------------------------------------------
  
  (1 | Site) +
    # Site-level random intercept. Accounts for between-site differences in
    # baseline caterpillar occurrence not captured by cicadaIndex_c.
    # IMPORTANT NOTE: estimated from only 5 groups. The variance component
    # will be imprecise. Singular fit (variance near zero) is possible and
    # should be reported transparently rather than treated as a model failure.
    # Its primary function here is to prevent pseudoreplication of the
    # cicadaIndex_c effect by ensuring branch-level observations within
    # a site are not treated as independent evidence about between-site
    # relationships.
    
    (1 | PlantFK),
  # Branch-level random intercept, effectively nested within site via the
  # unique PlantFK coding above. Captures consistent differences among
  # branches in caterpillar occurrence probability (e.g., due to host tree
  # species, canopy position). Well-supported: ~200-300 branches total,
  # each surveyed approximately 33 times (11 weeks x 3 years).
  
  data   = dat,
  family = binomial(link = "logit")
)

summary(m1)

# =========================================================
# SECTION 3: MODEL DIAGNOSTICS
# =========================================================

# ---------------------------------------------------------
# 3a. Inspect variance components
# ---------------------------------------------------------
VarCorr(m1)

# Key things to check:
# 1. Is the Site variance component very small (near zero)?
#    This means cicadaIndex_c and other fixed effects are accounting
#    for most between-site variation. Acceptable — report it.
# 2. Is the Branch variance component of reasonable magnitude?
#    Expect it to be positive and non-trivial given that branches
#    differ consistently in caterpillar-hosting quality.

# ---------------------------------------------------------
# 3b. Check residual temporal autocorrelation within branches
# ---------------------------------------------------------
# The branch random intercept absorbs stable between-branch differences
# but not week-to-week residual correlation. Check whether autocorrelation
# remains in residuals after model fitting.

dat$resid <- residuals(m1, type = "response")

# Run ACF check for a sample of branch-year combinations
# Select branches from one site with complete weekly observations
acf_check <- dat %>%
  filter(
    Site    == levels(factor(dat$Site))[1],
    Year    == "2024",
    ObservationMethod == "Visual"             # one method at a time for clarity
  ) %>%
  arrange(PlantFK, Week)

# Plot ACF for the first branch as a representative diagnostic
example <- acf_check %>%
  filter(PlantFK == unique(acf_check$PlantFK)[1]) %>%
  arrange(Week)

if (nrow(example) >= 6) {
  acf(example$resid,
      main = "Residual ACF: example branch, 2024, Visual survey")
}
# If autocorrelation at lag 1 is substantial (|r| > ~0.2),
# consider the AR(1) extension in Section 5.

# =========================================================
# SECTION 4: INTERPRETATION
# =========================================================

# ---------------------------------------------------------
# 4a. Primary test: read directly from model summary
# ---------------------------------------------------------
# In the summary(m1) fixed effects table, look for:
#   Yearcicada_vs_noncicada:Periodpost_cicada
# This is the coefficient for the primary cicada effect.
# A positive value means caterpillar occurrence increased MORE from 
# cicada to post-cicada period in 2024 than in non-cicada years,
# consistent with cicada-mediated suppression of predation during
# the cicada period.

# ---------------------------------------------------------
# 4b. Marginal estimated probabilities by Year and Period
# ---------------------------------------------------------
emm_YP <- emmeans(m1, ~ Period | Year, type = "response")
# type = "response" back-transforms log-odds to probability scale
print(emm_YP)

# Period contrast (post_cicada vs. cicada odds ratio) within each year
# This shows the seasonal shift within each year separately
pairs(emm_YP, reverse = TRUE)
# In the cicada year (2024), if the odds ratio is substantially larger
# than in non-cicada years, this supports the cicada hypothesis.

# ---------------------------------------------------------
# 4c. Cicada density effect across years
# ---------------------------------------------------------
# To Visualize the cicadaIndex_c effect, generate predictions across
# the observed range of cicada densities
density_range <- data.frame(
  cicadaIndex_c = seq(
    min(dat$cicadaIndex_c),
    max(dat$cicadaIndex_c),
    length.out = 50
  ),
  Period        = factor("cicada",    levels = levels(dat$Period)),
  Year          = factor("2024",      levels = levels(dat$Year)),
  Week_c        = 0,
  ObservationMethod = factor("Visual",    levels = levels(dat$ObservationMethod))
)

density_range$predicted <- predict(m1,
                                   newdata  = density_range,
                                   type     = "response",
                                   re.form  = NA)   # population-level prediction,
# ignoring random effects
# Plot predicted occurrence probability vs. cicada density
plot(predicted ~ cicadaIndex_c, data = density_range, type = "l",
     xlab = "Cicada density (centered)",
     ylab = "Predicted caterpillar occurrence probability",
     main = "Cicada density effect (cicada period, 2024, Visual survey)")

# =========================================================
# SECTION 5: OPTIONAL EXTENSIONS
# =========================================================

# ---------------------------------------------------------
# 5a. Year x Cicada Density interaction (dose-response test)
# ---------------------------------------------------------
# Tests whether the between-year difference in caterpillar occurrence
# scales with site-level cicada density.
# This adds 2 coefficients (2 Year contrasts x cicadaIndex_c).
# Treat as EXPLORATORY given only 5 sites.

m2 <- glmmTMB(
  occurrence ~
    Year * Period +
    Year * cicadaIndex_c +   # Does the cicada year effect on caterpillar
    # occurrence scale with cicada density?
    Week_c +
    ObservationMethod +
    (1 | Site) +
    (1 | PlantFK),
  data   = dat,
  family = binomial(link = "logit")
)

AIC(m1, m2) 

# Support for Model2

summary(m2)

# ==============================================
# SECTION 6: GRAPHING
# ==============================================

library(ggplot2)
library(patchwork)   # for combining panels
library(scales)

# Underlying caterpillar phenology patterns across the 5 sites
# ---------------------------------------------------------
# Visual encoding scheme
#
# The goal is redundant encoding: 2024 differs from
# 2025/2026 in color, line weight, AND line type, so
# the distinction survives greyscale printing and
# readers with color vision deficiency.
#
# 2024: vivid red-orange, thick, solid
# 2025: muted steel blue, thin, solid   } similar to
# 2026: lighter steel blue, thin, dashed } each other
#
# Color distance between 2025 and 2026 is intentionally
# small. Color distance between 2024 and both non-cicada
# years is large — warm vs. cool, vivid vs. muted.
# ---------------------------------------------------------

year_colors_occ  <- c("2024" = "#CC3311",
                      "2025" = "#5B8DB8",
                      "2026" = "#5B8DB8")

year_labels_occ  <- c("2024" = "2024 (cicada year)",
                      "2025" = "2025 (year +1)",
                      "2026" = "2026 (year +2)")

year_linewidths  <- c("2024" = 1.5, "2025" = 0.9, "2026" = 0.9)
year_linetypes   <- c("2024" = "solid", "2025" = "solid",  "2026" = "dashed")
year_pointsizes  <- c("2024" = 2.3,    "2025" = 1.8,       "2026" = 1.8)

# ---------------------------------------------------------
# Weekly observed occurrence rates
# Averaged across all branches and both survey methods
# within each Site × Year × Week combination.
#
# Note: beat sheet surveys show systematically lower
# detection rates than visual surveys (model coefficient
# -0.33). Averaging across methods is acceptable for
# visualizing seasonal patterns since the method effect
# is a fixed offset that does not distort curve shape.
# To restrict to visual surveys only, add:
#   filter(ObservationMethod == "Visual")
# before the group_by.
# ---------------------------------------------------------

weekly_occ <- dat %>%
  group_by(Site, Year, julianday) %>%
  summarise(
    mean_occ = mean(occurrence),
    n_obs    = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    # Factor levels: 2025 and 2026 drawn first, 2024 drawn
    # last so it sits on top where lines overlap
    Year = factor(Year, levels = c("2025", "2026", "2024")),
    Site = factor(Site)
  ) %>%
  filter(n_obs > 40,
         Site != "UNC Chapel Hill Campus"   #optional to focus on 4 panels
         ) # filter out some days with just a few random surveys

# ---------------------------------------------------------
# Cicada period bounds for background shading
#
# Derived from the Period variable in dat so the shaded
# window reflects your actual data coding.
# The same shaded region is applied to all five panels:
# in 2024 it marks when cicadas were present; in 2025
# and 2026 it marks the equivalent seasonal window for
# visual comparison.
# ---------------------------------------------------------

cicadaWeekBegin = 18
cicadaWeekEnd = 22

cicadaDayBegin = 121
cicadaDayEnd = 160

# ---------------------------------------------------------
# Optional: abbreviated site labels
#
# Long site names will be wrapped by label_wrap_gen()
# below, but if you want custom short labels, define
# them here as a named vector matching levels(weekly_occ$Site)
#
site_short <- c(
   "Triangle Land Conservancy - Johnston Mill Nature Preserve" = "Johnston Mill",
   #"UNC Chapel Hill Campus" = "UNC Chapel Hill",
   "Eno River State Park" = "Eno River State Park",
   "NC Botanical Garden" = "NC Botanical Garden",
   "Prairie Ridge Ecostation" = "Prairie Ridge Ecostation"
)
# Then replace labeller = label_wrap_gen(width = 28) with
# labeller = labeller(Site = site_short)
# ---------------------------------------------------------

# ---------------------------------------------------------
# Figure
# ---------------------------------------------------------

fig_weekly <- ggplot(
  weekly_occ,
  aes(x     = julianday,
      y     = mean_occ,
      color = Year,
      group = Year)
) +
  
  # Cicada period background shading
  # annotate() applies identically to all facets
  annotate(
    "rect",
    xmin  = cicadaDayBegin,
    xmax  = cicadaDayEnd,
    ymin  = -Inf,
    ymax  = Inf,
    fill  = "gray85",
    alpha = 0.65
  ) +
  
  # Lines — linewidth and linetype vary by year
  geom_line(aes(linewidth = Year,
                linetype  = Year)) +
  
  # Points — size varies by year
  geom_point(aes(size = Year)) +
  
  # -------------------------------------------------------
# Scales
# -------------------------------------------------------

scale_color_manual(
  values = year_colors_occ,
  labels = year_labels_occ,
  # Reorder labels to match desired legend order 2024, 2025, 2026
  # even though factor levels are ordered for drawing
  breaks = c("2024", "2025", "2026"),
  name   = NULL
) +
  scale_linewidth_manual(
    values = year_linewidths,
    guide  = "none"
  ) +
  scale_linetype_manual(
    values = year_linetypes,
    guide  = "none"
  ) +
  scale_size_manual(
    values = year_pointsizes,
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 5)
  ) +
  
  # -------------------------------------------------------
# Facet: one panel per site, 2 columns
# 5 panels → rows of 2, 2, 1; lower-right cell empty
# label_wrap_gen wraps long site names at word boundaries
# -------------------------------------------------------

facet_wrap(
  ~ Site,
  ncol     = 2,
  labeller = labeller(Site = site_short)
) +
  
  # -------------------------------------------------------
# Labels
# -------------------------------------------------------

labs(
  x       = "Day of year",
  y       = "Caterpillar occurrence probability",
  #caption = paste0(
  #  "Shaded region: cicada period (mid-May to early June).\n",
  #  "Dashed vertical line marks end of cicada period.\n",
  #  "Points and lines show observed weekly means averaged ",
  #  "across branches and survey methods."
  #)
) +
  
  # -------------------------------------------------------
# Theme
# -------------------------------------------------------

theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(linewidth = 0.3,
                                     color     = "grey88"),
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(2.2, "cm"),
    legend.text       = element_text(size = 10),
    strip.background  = element_rect(fill  = "grey95",
                                     color = "grey70"),
    strip.text        = element_text(size = 9, face = "bold"),
    plot.caption      = element_text(size  = 8,
                                     color = "grey45",
                                     hjust = 0)
  ) +
  
  # -------------------------------------------------------
# Legend: single color legend with overridden aesthetics
# so each key swatch shows the correct line weight,
# line type, and point size for that year
# -------------------------------------------------------

guides(
  color = guide_legend(
    nrow         = 1,
    override.aes = list(
      linewidth = c(1.5, 0.7, 0.7),
      linetype  = c("solid", "solid", "dashed"),
      size      = c(2.3, 1.3, 1.3)
    )
  )
)

fig_weekly

# ---------------------------------------------------------
# Save
# Height 9 inches accommodates 3 rows of panels plus
# x-axis label and bottom legend comfortably.
# Increase height to 10 if site name wrapping adds lines.
# ---------------------------------------------------------

ggsave(
  "weekly_caterpillar_occurrence_by_site.pdf",
  fig_weekly,
  width  = 7.5,
  height = 9.0,
  units  = "in"
)




# -------------------------------------------------------
# Site-level observed occurrence means for data overlay
# -------------------------------------------------------
# These are the raw observed proportions — useful for showing the
# actual data behind the model predictions. With 5 sites they are
# few enough to plot individually without overloading the figure.

site_year_means <- dat %>%
  group_by(Site, Year) %>%
  summarise(
    mean_occurrence   = mean(occurrence),
    cicadaIndex_c     = first(cicadaIndex_c),   # constant within site
    .groups = "drop"
  )

site_year_period_means <- dat %>%
  group_by(Site, Year, Period) %>%
  summarise(
    mean_occurrence   = mean(occurrence),
    cicadaIndex_c     = first(cicadaIndex_c),
    .groups = "drop"
  )

# Recover original (uncentered) cicada density scale for axis labeling
# attr() retrieves the centering value stored by scale()
cicada_center <- mean(dat$cicadaIndex, na.rm = TRUE)
dat$cicadaIndex_c <- dat$cicadaIndex - cicada_center

site_year_means$cicada_density_orig        <- site_year_means$cicadaIndex_c        + cicada_center
site_year_period_means$cicada_density_orig <- site_year_period_means$cicadaIndex_c + cicada_center

# -------------------------------------------------------
# Consistent color scheme and labels used across all plots
# -------------------------------------------------------
year_colors <- c("2024" = "#CC3311",   # orange-red: cicada year
                 "2025" = "#0072B2",   # blue: year+1
                 "2026" = "#009E73")   # teal: year+2

year_labels <- c("2024" = "2024 (cicada year)",
                 "2025" = "2025 (year +1)",
                 "2026" = "2026 (year +2)")

# -------------------------------------------------------
# Generate prediction grid across cicada density range
# Fixed at: cicada period, mean week, visual survey method
# Population-level (re.form = NA ignores random effects)
# -------------------------------------------------------

cicada_seq <- seq(min(dat$cicadaIndex_c),
                  max(dat$cicadaIndex_c),
                  length.out = 100)

pred_grid <- expand.grid(
  cicadaIndex_c     = cicada_seq,
  Year              = factor(c("2024", "2025", "2026"),
                             levels = levels(dat$Year)),
  Period            = factor("cicada", levels = levels(dat$Period)),
  Week_c            = 0,
  ObservationMethod = factor("Visual", levels = levels(dat$ObservationMethod))
)

# CRITICAL: prediction grid must carry the same contrast structure
# as the fitted model, otherwise predictions will be wrong
contrasts(pred_grid$Year) <- contrasts(dat$Year)

# Predict on link scale, then transform manually so CIs are
# computed on log-odds scale before back-transforming —
# this ensures CIs are bounded within [0,1]
preds <- predict(m2,
                 newdata  = pred_grid,
                 type     = "link",
                 se.fit   = TRUE,
                 re.form  = NA)

pred_grid <- pred_grid %>%
  mutate(
    fit                  = preds$fit,
    se                   = preds$se.fit,
    lower_95             = plogis(fit - 1.96 * se),
    upper_95             = plogis(fit + 1.96 * se),
    predicted            = plogis(fit),
    cicada_density_orig  = cicadaIndex_c + cicada_center
  )

# -------------------------------------------------------
# Plot
# -------------------------------------------------------

p1 <- ggplot(pred_grid,
             aes(x     = cicada_density_orig,
                 y     = predicted,
                 color = Year,
                 fill  = Year)) +
  
  # Confidence ribbons — kept semi-transparent; their width honestly
  # communicates the uncertainty in a relationship from 5 sites
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              alpha = 0.15, color = NA) +
  
  # Prediction lines
  geom_line(linewidth = 2.0) +
  
  # Observed site-year means overlaid as points
  # With 5 sites these are legible and add transparency about the
  # raw data structure underlying the model
  geom_point(data  = site_year_means,
             aes(x = cicada_density_orig,
                 y = mean_occurrence,
                 color = Year),
             size  = 3,
             shape = 16) +
  
  scale_color_manual(values = year_colors, labels = year_labels) +
  scale_fill_manual( values = year_colors, labels = year_labels,
                     guide  = "none") +
  
  labs(
    x        = "Site cicada density index",
    y        = "Caterpillar occurrence",
    color    = NULL,
    #caption  = "Lines: model predictions (cicada period, visual survey, mean week)\nPoints: observed site means"
  ) +
  
  theme_bw(base_size = 30) +
  theme(legend.position = "bottom",
        legend.text      = element_text(size = 16))

p1



# -------------------------------------------------------
# Plot the cicada-year prediction MINUS the non-cicada-year
# average prediction across the cicada density gradient.
# This directly visualizes the interaction coefficient.
# A line with positive slope = higher caterpillar occurrence
# in the cicada year at high-density sites relative to
# non-cicada-year baseline.
# -------------------------------------------------------

# Separate prediction grids per year
make_preds <- function(yr) {
  grid <- data.frame(
    cicadaIndex_c     = cicada_seq,
    Year              = factor(yr, levels = levels(dat$Year)),
    Period            = factor("cicada", levels = levels(dat$Period)),
    Week_c            = 0,
    ObservationMethod = factor("Visual", levels = levels(dat$ObservationMethod))
  )
  contrasts(grid$Year) <- contrasts(dat$Year)
  p <- predict(m2, newdata = grid, type = "link",
               se.fit = TRUE, re.form = NA)
  data.frame(cicadaIndex_c = cicada_seq,
             fit = p$fit, se = p$se.fit, Year = yr)
}

preds_all <- bind_rows(lapply(c("2024", "2025", "2026"), make_preds))

# Compute non-cicada-year average prediction (arithmetic mean of 2025 and 2026
# on link scale, then difference from 2024)
preds_wide <- preds_all %>%
  select(cicadaIndex_c, Year, fit) %>%
  tidyr::pivot_wider(names_from = Year, values_from = fit) %>%
  mutate(
    noncicada_avg = (`2025` + `2026`) / 2,
    difference    = `2024` - noncicada_avg,   # on log-odds scale
    cicada_density_orig = cicadaIndex_c + cicada_center
  )

# For the SE of the difference, propagate uncertainty properly.
# In a simple linear combination, Var(a - (b+c)/2) = Var(a) + Var((b+c)/2)
# Here we use delta method approximation via predict se values.
preds_se_wide <- preds_all %>%
  select(cicadaIndex_c, Year, se) %>%
  tidyr::pivot_wider(names_from = Year, values_from = se) %>%
  mutate(
    se_difference = sqrt(`2024`^2 + ((`2025`^2 + `2026`^2) / 4))
  )

preds_diff <- left_join(preds_wide, preds_se_wide,
                        by = "cicadaIndex_c") %>%
  mutate(
    lower_95 = difference - 1.96 * se_difference,
    upper_95 = difference + 1.96 * se_difference
  )

# -------------------------------------------------------
# Plot on log-odds (link) scale — the difference is most
# naturally interpreted there since the interaction
# coefficient IS on the log-odds scale
# -------------------------------------------------------

p2 <- ggplot(preds_diff,
             aes(x = cicada_density_orig, y = difference)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              fill = "#D55E00", alpha = 0.2) +
  
  geom_line(color = "#D55E00", linewidth = 1.1) +
  
  labs(
    x       = "Site cicada density index",
    y       = "Cicada year vs. non-cicada average\n(log-odds difference)",
    #caption = "Positive values indicate relatively higher caterpillar\noccurrence in the cicada year at a given site density"
  ) +
  
  theme_bw(base_size = 12)

p2



# -------------------------------------------------------
# Marginal predicted probabilities for each Year x Period
# combination, at mean cicada density and mean week,
# averaged over survey method
# -------------------------------------------------------

emm_YP <- emmeans(m2,
                  specs = ~ Period | Year,
                  type  = "response",
                  at    = list(cicadaIndex_c = 0, Week_c = 0))

emm_df <- as.data.frame(emm_YP) %>%
  rename(predicted = prob,
         lower_95  = asymp.LCL,
         upper_95  = asymp.UCL) %>%
  mutate(
    Year   = factor(Year, levels = c("2024", "2025", "2026")),
    Period = factor(Period,
                    levels = c("cicada", "post_cicada"),
                    labels = c("Cicada\nperiod", "Post-cicada\nperiod"))
  )

# Observed site-period-year means for overlay
# Recode Period labels to match emmeans output
site_year_period_plot <- site_year_period_means %>%
  mutate(
    Period_label = factor(Period,
                          levels = c("cicada", "post_cicada"),
                          labels = c("Cicada\nperiod", "Post-cicada\nperiod"))
  )

p3 <- ggplot(emm_df,
             aes(x     = Period,
                 y     = predicted,
                 color = Year,
                 group = Year)) +
  
  # Confidence intervals
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width    = 0.08,
                position = position_dodge(width = 0.15)) +
  
  # Connecting lines
  geom_line(position = position_dodge(width = 0.15),
            linewidth = 0.9) +
  
  # Marginal mean points
  geom_point(size     = 3,
             position = position_dodge(width = 0.15)) +
  
  # Observed site means overlaid as smaller, semi-transparent points
  # Jittered slightly to avoid overplotting across sites
  geom_point(data  = site_year_period_plot,
             aes(x = Period_label,
                 y = mean_occurrence,
                 color = Year),
             size     = 1.8,
             alpha    = 0.5,
             shape    = 1,          # open circles distinguish from model estimates
             position = position_dodge(width = 0.15)) +
  
  scale_color_manual(values = year_colors, labels = year_labels) +
  
  labs(
    x       = NULL,
    y       = "Predicted caterpillar occurrence probability",
    color   = NULL,
    #caption = "Filled points and lines: model marginal means at mean cicada density\nOpen points: observed site means"
  ) +
  
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.text      = element_text(size = 10))

p3



# -------------------------------------------------------
# Combine Option 1 (cicada density x Year) and Option 3
# (Year x Period) into a single manuscript-ready figure.
# Shared legend extracted and placed at bottom.
# -------------------------------------------------------

# Extract shared legend from p1
# (requires cowplot or can be done within patchwork)
combined <- (p1 + theme(legend.position = "none")) +
  (p3 + theme(legend.position = "none")) +
  plot_layout(ncol = 2, widths = c(1.1, 0.9)) +
  plot_annotation(
    tag_levels = "A",
    theme = theme(plot.tag = element_text(face = "bold"))
  )

# Add shared legend below both panels
# Extract legend from p1 as a grob
legend_grob <- cowplot::get_legend(
  p1 + theme(legend.position = "bottom",
             legend.direction = "horizontal")
)

cowplot::plot_grid(combined,
                   legend_grob,
                   ncol    = 1,
                   rel_heights = c(1, 0.08))