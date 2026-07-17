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


# ---------------------------------------------------------
# Bout_ID: unique site × year × period combination
# ---------------------------------------------------------
dat_clay$Bout_ID <- interaction(dat_clay$Name,
                                dat_clay$year,
                                dat_clay$Period,
                                drop = TRUE)

cat("Number of bouts:", nlevels(dat_clay$Bout_ID), "\n")
# Expected: 30 (5 sites × 3 years × 2 periods)

dat_clay %>%
  group_by(Bout_ID) %>%
  summarise(n = n(), .groups = "drop") %>%
  pull(n) %>%
  summary()

# ---------------------------------------------------------
# Total strike events
# ---------------------------------------------------------
cat("Total bird strikes:", sum(dat_clay$Bird), "\n")
cat("Total observations:", nrow(dat_clay), "\n")
cat("Overall strike rate:",
    round(mean(dat_clay$Bird) * 100, 1), "%\n")

dat_clay %>%
  group_by(year, Period) %>%
  summarise(
    n_deployed = n(),
    n_strikes  = sum(Bird),
    rate       = round(mean(Bird) * 100, 1),
    .groups    = "drop"
  ) %>%
  print()

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
    
    (1 | Bout_ID),
  # Bout-level random intercept (site × year × period).
  # Captures within-bout clustering: clay caterpillars deployed
  # together share the same local bird community, weather
  # conditions, and potential for repeat visits by active
  # individual birds. More relevant than branch-level clustering
  # given ~1.6 expected strikes per branch across all visits.
  # Note: Bout_ID is nested within Site by construction,
  # so these two random effects form an implicit hierarchy.
  
  data   = dat_clay,
  family = binomial(link = "logit")
)

summary(m_bird1)
VarCorr(m_bird1)


# ---------------------------------------------------------
# Compare with site-only random effect model
# ---------------------------------------------------------
# With only 30 bouts, the bout variance component may also
# be near zero. If so, the simplest defensible model has
# only the site random intercept.

m_bird0 <- glmmTMB(
  Bird ~
    Year * Period +
    Year * cicadaIndex_c +
    bout_doy_c +
    (1 | Name),
  data   = dat_clay,
  family = binomial(link = "logit")
)

AIC(m_bird0, m_bird1)
VarCorr(m_bird0)

# df      AIC
# m_bird0 11 1573.433
# m_bird1 12 1575.028

# Adding Bout_ID random effect does not add much, preferred (equivalent) model is the simpler m_bird0.

# =========================================================
# COMPARISON WITH MODEL OMITTING bout_doy_c
# =========================================================
m_bird0b <- glmmTMB(
  Bird ~
    Year * Period +
    Year * cicadaIndex_c +
    (1 | Name),
  data   = dat_clay,
  family = binomial(link = "logit")
)

AIC(m_bird0, m_bird0b)

#          df      AIC
# m_bird0  11 1573.433
# m_bird0b 10 1571.679

# Removing bout_doy_c decreases AIC by 1.75. Again the direction favors the simpler model. The binary Period variable is capturing all the meaningful temporal variation in strike rate across the season — the additional linear trend within periods adds noise rather than signal with only 4 time points.

# Side by side coefficient comparison for key terms
key_terms <- c("Yearcicada_vs_noncicada:cicadaIndex_c",
               "Yearcicada_vs_noncicada:Periodpost_cicada",
               "Yearyr1_vs_yr2",
               "Periodpost_cicada")

coefs_caterpillar <- summary(m2)$coefficients$cond[key_terms, ]
coefs_bird        <- summary(m_bird0b)$coefficients$cond[key_terms, ]

cat("\nCaterpillar occurrence model:\n")
print(round(coefs_caterpillar[, c("Estimate", "Std. Error", "Pr(>|z|)")], 3))

cat("\nBird strike model:\n")
print(round(coefs_bird[, c("Estimate", "Std. Error", "Pr(>|z|)")], 3))

# =========================================================
# MARGINAL MEANS FOR VISUALIZATION
# =========================================================

# Year × Period marginal means
emm_bird_YP <- emmeans(m_bird0b,
                       specs = ~ Period | Year,
                       type  = "response",
                       at    = list(cicadaIndex_c = 0))
print(emm_bird_YP)
pairs(emm_bird_YP, reverse = TRUE)


emm_bird_df <- as.data.frame(emm_bird_YP) %>%
  rename(predicted = prob,
         lower_95  = asymp.LCL,
         upper_95  = asymp.UCL) %>%
  mutate(
    Period_label = factor(
      Period,
      levels = c("cicada", "post_cicada"),
      labels = c("Cicada\nperiod", "Post-cicada\nperiod")
    ),
    Year = factor(Year, levels = c("2024", "2025", "2026"))
  )


# Predicted strike rate across cicada density gradient by year
pred_grid_bird <- expand.grid(
  cicadaIndex_c = seq(min(dat_clay$cicadaIndex_c),
                      max(dat_clay$cicadaIndex_c),
                      length.out = 100),
  Year          = factor(c("2024", "2025", "2026"),
                         levels = levels(dat_clay$Year)),
  Period        = factor("cicada",
                         levels = levels(dat_clay$Period))
)

contrasts(pred_grid_bird$Year) <- contrasts(dat_clay$Year)

preds_bird <- predict(m_bird0b,
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


###################################################
# Plotting
###################################################

# --- Bird strike ~ cicada density ---
pB <- ggplot(pred_grid_bird,
             aes(x = cicada_density_orig, y = predicted,
                 color = Year, fill = Year)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 2.0) +
  geom_point(data  = site_means_bird,
             aes(x = cicada_density_orig,
                 y = mean_strike),
             size  = 2.5, shape = 16) +
  scale_color_manual(values = year_colors, labels = year_labels) +
  scale_fill_manual( values = year_colors, labels = year_labels,
                     guide  = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(
    x     = "Site cicada density index",
    y     = "Bird strike probability",
    color = NULL
  ) +
  theme_bw(base_size = 24) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 24)
    #plot.title       = element_text(size = 11, face = "bold")
  )

pB

pD <- ggplot(emm_bird_df,
             aes(x     = Period_label,
                 y     = predicted,
                 color = Year,
                 group = Year)) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width    = 0.08,
                position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25),
            linewidth = 2) +
  geom_point(size     = 2.5,
             position = position_dodge(width = 0.25)) +
  geom_point(data     = site_period_bird,
             aes(x    = Period_label,
                 y    = mean_strike,
                 color = Year),
             size     = 1.5,
             alpha    = 0.5,
             shape    = 1,
             position = position_dodge(width = 0.25)) +
  scale_color_manual(values = year_colors, labels = year_labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(
    x     = NULL,
    y     = "Bird strike probability",
    color = NULL
  ) +
  theme_bw(base_size = 24) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 24)
    #plot.title       = element_text(size = 11, face = "bold")
  )

pD

