# =========================================================
# PACKAGES
# =========================================================

library(glmmTMB)
library(emmeans)   # Marginal means and contrasts
library(dplyr)     # Data manipulation

# =========================================================
# SECTION 1: DATA PREPARATION
# =========================================================

# Caterpillars Count! raw data subsetted to the 5 NC sites and the years 2024-2026
dataset = read.csv('data/cc_cicada_analysis_data_2024-2026.csv')

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
         Week = (julianday - 4)/7) %>% 
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
dat$cicada_density_c <- as.numeric(
  scale(dat$cicadaIndex, center = TRUE, scale = FALSE)
)

# Center Week at its grand mean across the full season.
# This makes the model intercept interpretable as occurrence at the 
# mid-season average rather than at week zero.
dat$Week_c <- dat$Week - mean(dat$Week)

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
  
  cicada_density_c +
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
    # baseline caterpillar occurrence not captured by cicada_density_c.
    # IMPORTANT NOTE: estimated from only 5 groups. The variance component
    # will be imprecise. Singular fit (variance near zero) is possible and
    # should be reported transparently rather than treated as a model failure.
    # Its primary function here is to prevent pseudoreplication of the
    # cicada_density_c effect by ensuring branch-level observations within
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
#    This means cicada_density_c and other fixed effects are accounting
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
# To Visualize the cicada_density_c effect, generate predictions across
# the observed range of cicada densities
density_range <- data.frame(
  cicada_density_c = seq(
    min(dat$cicada_density_c),
    max(dat$cicada_density_c),
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
plot(predicted ~ cicada_density_c, data = density_range, type = "l",
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
# This adds 2 coefficients (2 Year contrasts x cicada_density_c).
# Treat as EXPLORATORY given only 5 sites.

m2 <- glmmTMB(
  occurrence ~
    Year * Period +
    Year * cicada_density_c +   # Does the cicada year effect on caterpillar
    # occurrence scale with cicada density?
    Week_c +
    ObservationMethod +
    (1 | Site) +
    (1 | PlantFK),
  data   = dat,
  family = binomial(link = "logit")
)

AIC(m1, m2)
# Prefer m2 only if delta AIC > 2 AND the interaction aligns with
# the a priori dose-response prediction (not just any improvement).

# ---------------------------------------------------------
# 5b. AR(1) extension if residual autocorrelation is substantial
# ---------------------------------------------------------
# glmmTMB supports AR(1) via ar1(), which requires the time variable
# to be a consecutive integer factor within each grouping unit.

# Create consecutive time index within each Branch x Year combination.
# Branches with missing weeks will violate the "consecutive" assumption
# — check for and handle gaps before using this extension.

dat <- dat %>%
  group_by(PlantFK, Year, ObservationMethod) %>%
  arrange(Week) %>%
  mutate(time_idx = factor(row_number())) %>%
  ungroup()

# AR(1) within Branch x Year x ObservationMethod combinations.
# This replaces the simple branch random intercept with an AR(1) structure
# that both captures autocorrelation AND allows for branch-level variation.
m1_ar1 <- glmmTMB(
  occurrence ~
    Year * Period +
    cicada_density_c +
    Week_c +
    ObservationMethod +
    (1 | Site) +
    ar1(time_idx + 0 | PlantFK:Year:ObservationMethod),
  # AR(1) within each branch-year-method time series
  # The + 0 suppresses a fixed intercept within the ar1 term
  data   = dat,
  family = binomial(link = "logit")
)

AIC(m1, m1_ar1)
# If this model fails to converge (likely given the large number of
# Branch:Year:ObservationMethod groups), retain m1 with a note acknowledging
# potential residual autocorrelation as a limitation.