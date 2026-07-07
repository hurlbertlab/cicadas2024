# Script for analyzing clay caterpillar predation
library(dplyr)
library(gsheet)
library(lubridate)
library(stringr)
library(glmmTMB)
library(emmeans)

# Cicada density index by site data
cicadaLevels = read.csv("data/cicada_noise_by_site_on_day143.csv") %>%
  mutate(Name = case_when(site == "eno" ~ "Eno River State Park",
                          site == "jmill" ~ "Triangle Land Conservancy - Johnston Mill Nature Preserve",
                          site == "ncbg" ~ "NC Botanical Garden",
                          site == "unc" ~ "UNC Chapel Hill Campus",
                          site == "pridge" ~ "Prairie Ridge Ecostation")) %>%
  select(Name, cicadaIndex)

mean_cicada_across_sites = mean(cicadaLevels$cicadaIndex)

# Read in clay caterpillar predation data
url = "https://docs.google.com/spreadsheets/d/1hi7iyi7xunriU2fvFpgNVDjO5ERML4IUgzTkQjLVYCo/edit?gid=0#gid=0"
#ForestCover = read.csv("data/ForestCover.csv")
#LandscapeCover = read.csv("data/sites_2022-09-19.csv")
#LandscapePrairie = read.csv("data/prairieridgeforest.csv")


dat_clay = gsheet2tbl(url) %>%
  mutate(DeployDate = as.Date(DeployDate, format = "%m/%d/%Y"),
         CollectionDate = as.Date(CollectionDate, format = "%m/%d/%Y"),
         year = str_extract(DeployDate, "\\d+"),
         # Floor to Monday of the sampling week, then subtract 1 day to get Sunday.
         # A sampling bout occurs during a week Monday-Friday, with the average
         # collection date across sites occurring on a Wednesday.
         # Sunday represents the average midpoint of caterpillar deployment
         # and is chosen as the day of year to represent predation activity.
         collect_sunday = floor_date(CollectionDate, unit = "week", week_start = 1) - days(1),
         bout_doy = yday(collect_sunday),
         Period = ifelse(bout_doy >= 121 & bout_doy < 160, "cicada", "post_cicada"),
         
  ) %>%
  
  # Assign sequential bout numbers within each year
  group_by(year) %>%
  mutate(
    # dense_rank correctly assigns the same bout number to all observations
    # sharing the same collect_sunday within a year, which is all sites
    # visited during the same sampling week
    Bout = dense_rank(collect_sunday)
  ) %>%
  ungroup() %>%
  left_join(cicadaLevels, by = 'Name') %>%
    # center cicada index values
  mutate(cicadaIndex_c = cicadaIndex - mean_cicada_across_sites) %>%
    # remove clay caterpillar rows for clay caterpillars that were never re-found
  filter(`Not Found` == 0)

# Center the bout_doy
bout_doy_center <- dat_clay %>%
  distinct(year, Bout, bout_doy) %>%
  pull(bout_doy) %>%
  mean()

cat("Bout doy centering value (Sunday-based):", round(bout_doy_center, 1), "\n")

dat_clay$bout_doy_c = dat_clay$bout_doy - bout_doy_center

# ---------------------------------------------------------
# Factor coding — Year and Period
# ---------------------------------------------------------
dat_clay$Year <- factor(dat_clay$year,
                             levels = c("2024", "2025", "2026"))

contrasts(dat_clay$Year) <- cbind(
  cicada_vs_noncicada = c( 1.0, -0.5, -0.5),
  yr1_vs_yr2          = c( 0.0,  0.5, -0.5)
)

dat_clay$Period <- factor(dat_clay$Period)

# Check observations per branch — should be up to 12 (4 bouts × 3 years)
# Fewer than 12 indicates missing bouts or not-found exclusions
branch_obs <- dat_clay %>%
  group_by(Code) %>%
  summarise(n_obs = n(), .groups = "drop")

cat("Observations per branch:\n")
print(summary(branch_obs$n_obs))

# At one site (UNC), 20 of 30 branch codes were consistent across all three years; the remaining 10 were sampled in 2024 and 2025 only, and a different 10 in 2026 only, with replacement branches on similar tree species. All unique codes were treated as distinct units in the branch random effect.


# =========================================================
# PRIMARY MODEL
# =========================================================

m_bird1 <- glmmTMB(
  Bird ~
    
    Year * Period +
    # Primary temporal inference.
    # Key term: Yearcicada_vs_noncicada:Periodpost_cicada
    # Tests whether the seasonal shift in bird strike rate from
    # cicada to post-cicada period is larger or smaller in 2024
    # than in non-cicada years.
    # Predicted direction under trophic cascade hypothesis:
    # bird strike rate should be suppressed during the cicada
    # period in 2024 relative to baseline, then recover once
    # cicadas disappear. This would produce a LARGER
    # post-cicada vs. cicada increase in 2024 than in
    # non-cicada years — a POSITIVE interaction coefficient.
    # Note: this is the most confounded comparison due to the
    # seasonal increase in foraging intensity during chick-rearing,
    # which would independently drive a post-cicada increase.
    # Non-cicada years provide the empirical baseline for this
    # natural increase, making the interaction the appropriate
    # test rather than the main Period effect.
    
    Year * cicadaIndex_c +
    # Dose-response inference.
    # Key term: Yearcicada_vs_noncicada:cicadaIndex_c
    # Tests whether the between-year difference in bird strike
    # rate scales with site-level cicada density.
    # Predicted direction: NEGATIVE.
    # Sites with higher cicada density should show relatively
    # LOWER bird strike rates in 2024 compared to non-cicada
    # years, as birds switch attention to the abundant cicada
    # resource. This is the least confounded of the three
    # inferential lenses (between-site comparison at the same
    # calendar time) and therefore the most interpretable.
    #
    # CRITICAL SIGN CHECK with caterpillar occurrence model:
    # If Yearcicada_vs_noncicada:cicadaIndex_c is NEGATIVE here
    # and was POSITIVE in the caterpillar occurrence model (m2),
    # the two results are mechanistically coherent:
    # more cicadas → fewer bird strikes → more caterpillars.
    
    bout_doy_c +
    # Linear temporal trend across the 4 bouts.
    # Accounts for residual within-season variation in bird
    # strike rate not captured by the Period step change.
    # With only 4 time points this term is modest — AIC
    # comparison with m_bird0 (below) determines whether
    # it adds meaningful explanatory power.
    
    (1 | Name) +
    # Site-level random intercept.
    # Prevents pseudoreplication of the cicadaIndex_c effect.
    # Estimated from 5 groups — imprecise but necessary.
    # Singular fit should be reported transparently.
    
    (1 | Code),
  # Branch-level random intercept nested within site.
  # Captures consistent differences among branches in baseline
  # bird strike probability due to branch architecture,
  # position, visibility, or host tree characteristics.
  # ~150 branches × up to 12 observations each — well supported.
  
  data   = dat_clay,
  family = binomial(link = "logit")
)

summary(m_bird1)
VarCorr(m_bird1)

# =========================================================
# OPTIONAL EXTENSION: Circle random intercept
# =========================================================
# Include only if the Circle inspection above suggests
# meaningful spatial clustering within sites

m_bird1_circle <- glmmTMB(
  Bird ~
    Year * Period +
    Year * cicadaIndex_c +
    Week_c +
    (1 | Name) +
    (1 | Name:Circle) +   # Circle nested within Site
    (1 | Code),
  data   = dat_clay,
  family = binomial(link = "logit")
)

AIC(m_bird1, m_bird1_circle)

# =========================================================
# COMPARISON WITH MODEL OMITTING Week_c
# =========================================================
m_bird0 <- glmmTMB(
  Bird ~
    Year * Period +
    Year * cicadaIndex_c +
    (1 | Name) +
    (1 | Code),
  data   = dat_clay,
  family = binomial(link = "logit")
)

AIC(m_bird0, m_bird1)

# =========================================================
# SIGN COMPARISON ACROSS MODELS
# =========================================================
# The mechanistic coherence of results across both models
# is the most important single interpretive check.

coef_caterpillar <- fixef(m2)$cond[
  "Yearcicada_vs_noncicada:cicadaIndex_c"]
coef_bird        <- fixef(m_bird1)$cond[
  "Yearcicada_vs_noncicada:cicadaIndex_c"]

cat("\nCross-model sign comparison:\n")
cat("Caterpillar occurrence (m2)  - cicada year × density:",
    round(coef_caterpillar, 3), "\n")
cat("Bird strike rate (m_bird1)   - cicada year × density:",
    round(coef_bird, 3), "\n")
cat("Signs opposite as predicted (negative bird, positive caterpillar)?",
    sign(coef_caterpillar) != sign(coef_bird), "\n")

# =========================================================
# MARGINAL MEANS FOR VISUALIZATION
# =========================================================

# Year × Period marginal means
emm_bird_YP <- emmeans(m_bird1,
                       specs = ~ Period | Year,
                       type  = "response",
                       at    = list(cicadaIndex_c = 0,
                                    Week_c        = 0))
print(emm_bird_YP)
pairs(emm_bird_YP, reverse = TRUE)

# Predicted strike rate across cicada density gradient by year
pred_grid_bird <- expand.grid(
  cicadaIndex_c = seq(min(dat_clay$cicadaIndex_c),
                      max(dat_clay$cicadaIndex_c),
                      length.out = 100),
  Year          = factor(c("2024", "2025", "2026"),
                         levels = levels(dat_clay$Year)),
  Period        = factor("cicada",
                         levels = levels(dat_clay$Period)),
  Week_c        = 0
)

contrasts(pred_grid_bird$Year) <- contrasts(dat_clay$Year)

preds_bird <- predict(m_bird1,
                      newdata = pred_grid_bird,
                      type    = "link",
                      se.fit  = TRUE,
                      re.form = NA)

pred_grid_bird <- pred_grid_bird %>%
  mutate(
    fit             = preds_bird$fit,
    se              = preds_bird$se.fit,
    lower_95        = plogis(fit - 1.96 * se),
    upper_95        = plogis(fit + 1.96 * se),
    predicted       = plogis(fit),
    cicada_density_orig = cicadaIndex_c + cicada_center
  )