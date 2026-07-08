# This script makes graphs to visualize the interaction effects of
# 1) the caterpillar occurrence model, and
# 2) the clay caterpillar bird strike model

source('caterpillar_occurrence_model.r')
source('clay_caterpillar_model.r')


# =========================================================
# PACKAGES
# =========================================================

library(ggplot2)
library(dplyr)
library(emmeans)
library(patchwork)
library(scales)     # for percent_format

# =========================================================
# SHARED AESTHETICS
# =========================================================

year_colors <- c("2024" = "#D55E00",
                 "2025" = "#0072B2",
                 "2026" = "#009E73")

year_labels <- c("2024" = "2024 (cicada year)",
                 "2025" = "2025 (year +1)",
                 "2026" = "2026 (year +2)")

# Base theme applied to all panels
# Legend position controlled at combination stage
theme_fig <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 10),
    plot.title       = element_text(size = 11, face = "bold")
  )

# =========================================================
# SECTION 1: OBSERVED SITE-LEVEL MEANS FOR DATA OVERLAY
# =========================================================

# --- Caterpillar model ---
# Density plots: site × year means within cicada period only,
# to match predictions which are held at Period = "cicada"
site_means_cat <- dat %>%
  filter(Period == "cicada") %>%
  group_by(Site, Year) %>%
  summarise(
    mean_occ            = mean(occurrence),
    cicada_density_orig = first(cicadaIndex_c) + cicada_center,
    .groups = "drop"
  )

# Period plots: site × year × period means across all observations
site_period_cat <- dat %>%
  group_by(Site, Year, Period) %>%
  summarise(
    mean_occ = mean(occurrence),
    .groups  = "drop"
  ) %>%
  mutate(
    Period_label = factor(
      Period,
      levels = c("cicada", "post_cicada"),
      labels = c("Cicada\nperiod", "Post-cicada\nperiod")
    ),
    Year = factor(Year, levels = c("2024", "2025", "2026"))
  )

# --- Bird strike model ---
site_means_bird <- dat_clay %>%
  filter(Period == "cicada") %>%
  group_by(Name, Year) %>%
  summarise(
    mean_strike         = mean(Bird),
    cicada_density_orig = first(cicadaIndex_c) + cicada_center,
    .groups = "drop"
  )

site_period_bird <- dat_clay %>%
  group_by(Name, Year, Period) %>%
  summarise(
    mean_strike = mean(Bird),
    .groups     = "drop"
  ) %>%
  mutate(
    Period_label = factor(
      Period,
      levels = c("cicada", "post_cicada"),
      labels = c("Cicada\nperiod", "Post-cicada\nperiod")
    ),
    Year = factor(Year, levels = c("2024", "2025", "2026"))
  )

# =========================================================
# SECTION 2: CICADA DENSITY × YEAR PREDICTIONS
# Panels A and B
# Predictions held at: cicada period, mean temporal covariate
# Population-level (re.form = NA)
# =========================================================

# Shared cicada density sequence — same 5 sites in both models
# so range is identical across both datasets
cicada_seq <- seq(
  min(dat$cicadaIndex_c),
  max(dat$cicadaIndex_c),
  length.out = 100
)

# --- Panel A: Caterpillar occurrence ---
grid_cat_dens <- expand.grid(
  cicadaIndex_c     = cicada_seq,
  Year              = factor(c("2024", "2025", "2026"),
                             levels = levels(dat$Year)),
  Period            = factor("cicada",
                             levels = levels(dat$Period)),
  Week_c            = 0,
  ObservationMethod = factor("Visual",
                             levels = levels(dat$ObservationMethod))
)
contrasts(grid_cat_dens$Year) <- contrasts(dat$Year)

preds_cat_dens <- predict(m2,
                          newdata = grid_cat_dens,
                          type    = "link",
                          se.fit  = TRUE,
                          re.form = NA)

grid_cat_dens <- grid_cat_dens %>%
  mutate(
    predicted           = plogis(preds_cat_dens$fit),
    lower_95            = plogis(preds_cat_dens$fit - 1.96 * preds_cat_dens$se.fit),
    upper_95            = plogis(preds_cat_dens$fit + 1.96 * preds_cat_dens$se.fit),
    cicada_density_orig = cicadaIndex_c + cicada_center
  )

# --- Panel B: Bird strike ---
grid_bird_dens <- expand.grid(
  cicadaIndex_c = cicada_seq,
  Year          = factor(c("2024", "2025", "2026"),
                         levels = levels(dat_clay$Year)),
  Period        = factor("cicada",
                         levels = levels(dat_clay$Period))
)
contrasts(grid_bird_dens$Year) <- contrasts(dat_clay$Year)

preds_bird_dens <- predict(m_bird0b,
                           newdata = grid_bird_dens,
                           type    = "link",
                           se.fit  = TRUE,
                           re.form = NA)

grid_bird_dens <- grid_bird_dens %>%
  mutate(
    predicted           = plogis(preds_bird_dens$fit),
    lower_95            = plogis(preds_bird_dens$fit - 1.96 * preds_bird_dens$se.fit),
    upper_95            = plogis(preds_bird_dens$fit + 1.96 * preds_bird_dens$se.fit),
    cicada_density_orig = cicadaIndex_c + cicada_center
  )

# =========================================================
# SECTION 3: YEAR × PERIOD MARGINAL MEANS
# Panels C and D
# Evaluated at mean cicada density (cicadaIndex_c = 0)
# and mean temporal covariate
# =========================================================

# --- Panel C: Caterpillar occurrence ---
# ObservationMethod held at "Visual" for interpretability;
# averaging over methods would be equally defensible
emm_cat_YP <- emmeans(m2,
                      specs = ~ Period | Year,
                      type  = "response",
                      at    = list(cicadaIndex_c     = 0,
                                   Week_c            = 0,
                                   ObservationMethod = "Visual"))

emm_cat_df <- as.data.frame(emm_cat_YP) %>%
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

# --- Panel D: Bird strike ---
emm_bird_YP <- emmeans(m_bird0b,
                       specs = ~ Period | Year,
                       type  = "response",
                       at    = list(cicadaIndex_c = 0))

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

# =========================================================
# SECTION 4: BUILD INDIVIDUAL PANELS
# =========================================================

# --- Panel A: Caterpillar occurrence ~ cicada density ---
pA <- ggplot(grid_cat_dens,
             aes(x = cicada_density_orig, y = predicted,
                 color = Year, fill = Year)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.0) +
  # Observed site × year means (cicada period only)
  # 5 points per year — few enough to be individually legible
  geom_point(data  = site_means_cat,
             aes(x = cicada_density_orig,
                 y = mean_occ),
             size  = 2.5, shape = 16) +
  scale_color_manual(values = year_colors, labels = year_labels) +
  scale_fill_manual( values = year_colors, labels = year_labels,
                     guide  = "none") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(
    x     = "Site cicada density index",
    y     = "Caterpillar occurrence probability",
    color = NULL,
    title = "A"
  ) +
  theme_fig

# --- Panel B: Bird strike ~ cicada density ---
pB <- ggplot(grid_bird_dens,
             aes(x = cicada_density_orig, y = predicted,
                 color = Year, fill = Year)) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.0) +
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
    color = NULL,
    title = "B"
  ) +
  theme_fig

# --- Panel C: Caterpillar occurrence ~ Year × Period ---
pC <- ggplot(emm_cat_df,
             aes(x     = Period_label,
                 y     = predicted,
                 color = Year,
                 group = Year)) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width    = 0.08,
                position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25),
            linewidth = 0.9) +
  geom_point(size     = 2.5,
             position = position_dodge(width = 0.25)) +
  # Observed site × year × period means as open circles
  geom_point(data     = site_period_cat,
             aes(x    = Period_label,
                 y    = mean_occ,
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
    y     = "Caterpillar occurrence probability",
    color = NULL,
    title = "C"
  ) +
  theme_fig

# --- Panel D: Bird strike ~ Year × Period ---
pD <- ggplot(emm_bird_df,
             aes(x     = Period_label,
                 y     = predicted,
                 color = Year,
                 group = Year)) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width    = 0.08,
                position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25),
            linewidth = 0.9) +
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
    color = NULL,
    title = "D"
  ) +
  theme_fig

# =========================================================
# SECTION 5: COMBINE INTO 2 × 2 FIGURE
# =========================================================

# patchwork collects the shared legend from all panels
# and places it at the bottom
combined_fig <- (pA | pB) / (pC | pD) +
  plot_layout(guides = "collect") &
  theme(legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.text      = element_text(size = 10),
        legend.key.width = unit(1.5, "cm"))

combined_fig

# Save at dimensions appropriate for a two-column manuscript figure
ggsave("cicada_main_effects.pdf",
       combined_fig,
       width  = 7.0,
       height = 6.5,
       units  = "in")